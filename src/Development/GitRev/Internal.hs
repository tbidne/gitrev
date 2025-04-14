{-# LANGUAGE QuasiQuotes #-}

module Development.GitRev.Internal
  ( -- * Git
    gitHashMay,
    gitShortHashMay,
    gitBranchMay,
    gitDescribeMay,
    gitDirty,
    gitDirtyTracked,
    gitCommitCountMay,
    gitCommitDateMay,

    -- * Modifiers
    unknownFallback,
    envFallback,
    envUnknownFallback,
  )
where

import Control.Exception
  ( Exception (fromException),
    SomeAsyncException (SomeAsyncException),
    SomeException,
    catchJust,
    throwIO,
    toException,
  )
import Control.Monad (when, (<=<))
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Syntax (addDependentFile)
import System.Directory.OsPath
  ( doesDirectoryExist,
    doesFileExist,
    findExecutable,
    getCurrentDirectory,
  )
import System.Environment qualified as Env
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.File.OsPath qualified as FileIO
import System.OsPath (OsPath, osp, (</>))
import System.OsPath qualified as OsPath
import System.OsString qualified as OsString
import System.Process (readProcessWithExitCode)

gitHashMay :: Q (Maybe String)
gitHashMay = runGit ["rev-parse", "HEAD"] IdxNotUsed

gitShortHashMay :: Q (Maybe String)
gitShortHashMay = runGit ["rev-parse", "--short", "HEAD"] IdxNotUsed

gitBranchMay :: Q (Maybe String)
gitBranchMay = runGit ["rev-parse", "--abbrev-ref", "HEAD"] IdxNotUsed

gitDescribeMay :: Q (Maybe String)
gitDescribeMay = runGit ["describe", "--long", "--always"] IdxNotUsed

gitDirty :: Q Bool
gitDirty = nonEmpty <$> runGit ["status", "--porcelain"] IdxUsed

gitDirtyTracked :: Q Bool
gitDirtyTracked =
  nonEmpty <$> runGit ["status", "--porcelain", "--untracked-files=no"] IdxUsed

nonEmpty :: Maybe String -> Bool
nonEmpty Nothing = False
nonEmpty (Just "") = False
nonEmpty (Just _) = True

gitCommitCountMay :: Q (Maybe String)
gitCommitCountMay = runGit ["rev-list", "HEAD", "--count"] IdxNotUsed

gitCommitDateMay :: Q (Maybe String)
gitCommitDateMay = runGit ["log", "HEAD", "-1", "--format=%cd"] IdxNotUsed

unknownFallback :: Q (Maybe String) -> Q String
unknownFallback m = fmap (fromMaybe "UNKNOWN") m

envFallback :: String -> Q (Maybe String) -> Q (Maybe String)
envFallback var m =
  m >>= \case
    Just r -> pure $ Just r
    Nothing -> lookupEnvQ var

envUnknownFallback :: String -> Q (Maybe String) -> Q String
envUnknownFallback var m =
  unknownFallback $
    m >>= \case
      Just r -> pure $ Just r
      Nothing -> lookupEnvQ var

lookupEnvQ :: String -> Q (Maybe String)
lookupEnvQ s = runIO (Env.lookupEnv s)

-- | Run git with the given arguments and no stdin, returning the
-- stdout output. If git isn't available or something goes wrong,
-- return the second argument.
runGit :: [String] -> IndexUsed -> Q (Maybe String)
runGit args useIdx = do
  let oops :: SomeException -> IO (ExitCode, String, String)
      oops _e = pure (ExitFailure 1, "", "")
  gitFound <- runIO $ isJust <$> findExecutable [osp|git|]
  if gitFound
    then do
      -- a lot of bookkeeping to record the right dependencies
      pwd <- runIO getDotGit
      let hd = pwd </> [osp|HEAD|]
          index = pwd </> [osp|index|]
          packedRefs = pwd </> [osp|packed-refs|]
      hdFp <- OsPath.decodeUtf hd
      hdExists <- runIO $ doesFileExist hd
      when hdExists $ do
        addDependentFile hdFp
        -- the HEAD file either contains the hash of a detached head
        -- or a pointer to the file that contains the hash of the head
        T.splitAt 5 <$> runIO (readFileUtf8 hd) >>= \case
          -- pointer to ref
          ("ref: ", relRef) -> do
            relRefOs <- OsPath.encodeUtf $ T.unpack relRef
            let ref = pwd </> tillNewLineOsPath relRefOs
            refExists <- runIO $ doesFileExist ref
            refFp <- OsPath.decodeUtf ref
            when refExists $ addDependentFile refFp
          -- detached head
          _hash -> pure ()
      -- add the index if it exists to set the dirty flag
      indexExists <- runIO $ doesFileExist index
      when (indexExists && useIdx == IdxUsed) $ do
        indexFp <- OsPath.decodeUtf index
        addDependentFile indexFp
      -- if the refs have been packed, the info we're looking for
      -- might be in that file rather than the one-file-per-ref case
      -- handled above
      packedExists <- runIO $ doesFileExist packedRefs
      when packedExists $ do
        packedRefsFp <- OsPath.decodeUtf packedRefs
        addDependentFile packedRefsFp
      runIO $ do
        (code, out, _err) <- readProcessWithExitCode "git" args "" `catchSync` oops
        case code of
          ExitSuccess -> pure $ Just (tillNewLineStr out)
          ExitFailure _ -> pure Nothing
    else pure Nothing

tillNewLineStr :: String -> String
tillNewLineStr = takeWhile (\c -> c /= '\n' && c /= '\r')

tillNewLineOsPath :: OsPath -> OsPath
tillNewLineOsPath = OsString.takeWhile (\c -> c /= nl && c /= cr)
  where
    nl = OsString.unsafeFromChar '\n'
    cr = OsString.unsafeFromChar '\r'

-- | Determine where our @.git@ directory is, in case we're in a
-- submodule.
getDotGit :: IO OsPath
getDotGit = do
  pwd <- getGitRoot
  let dotGit = pwd </> [osp|.git|]
      oops = pure dotGit -- it's gonna fail, that's fine
  isDir <- doesDirectoryExist dotGit

  if isDir
    then pure dotGit
    else do
      isFile <- doesFileExist dotGit
      if isFile
        then do
          T.splitAt 8 <$> readFileUtf8 dotGit >>= \case
            ("gitdir: ", relDir) -> do
              relDirOs <- OsPath.encodeUtf $ T.unpack relDir
              isRelDir <- doesDirectoryExist relDirOs
              if isRelDir
                then pure relDirOs
                else oops
            _ -> oops
        else oops

readFileUtf8 :: OsPath -> IO Text
readFileUtf8 =
  (either throwIO pure)
    . TEnc.decodeUtf8'
    <=< FileIO.readFile'

-- | Get the root directory of the Git repo.
getGitRoot :: IO OsPath
getGitRoot = do
  pwd <- getCurrentDirectory
  (code, out, _) <-
    readProcessWithExitCode "git" ["rev-parse", "--show-toplevel"] ""
  out' <- OsPath.encodeUtf out
  case code of
    ExitSuccess -> pure $ tillNewLineOsPath out'
    ExitFailure _ -> pure pwd -- later steps will fail, that's fine

-- | Type to flag if the git index is used or not in a call to runGit
data IndexUsed
  = -- | The git index is used
    IdxUsed
  | -- | The git index is /not/ used
    IdxNotUsed
  deriving stock (Eq)

catchSync :: IO a -> (SomeException -> IO a) -> IO a
catchSync = catchIf isSyncException

catchIf ::
  (Exception e) =>
  (e -> Bool) ->
  IO a ->
  (e -> IO a) ->
  IO a
catchIf p = catchJust (\e -> if p e then Just e else Nothing)

isSyncException :: (Exception e) => e -> Bool
isSyncException e = case fromException (toException e) of
  Just SomeAsyncException {} -> False
  Nothing -> True
