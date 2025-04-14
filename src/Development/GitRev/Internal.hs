{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Internal module.
--
-- @since 1.40.0
module Development.GitRev.Internal
  ( -- * Git
    GitError (..),
    gitHashQ,
    gitShortHashQ,
    gitBranchQ,
    gitDescribeQ,
    gitDirtyQ,
    gitDirtyTrackedQ,
    gitCommitCountQ,
    gitCommitDateQ,

    -- * Modifiers
    liftFalse,
    liftDefString,
    liftError,
    envFallback,
  )
where

import Control.Exception
  ( Exception (displayException, fromException),
    SomeAsyncException (SomeAsyncException),
    SomeException,
    catchJust,
    throwIO,
    toException,
  )
import Control.Monad (when, (<=<))
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Syntax (Lift, addDependentFile)
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

-- | @since 1.40.0
gitHashQ :: Q (Either GitError String)
gitHashQ = runGit ["rev-parse", "HEAD"] IdxNotUsed

-- | @since 1.40.0
gitShortHashQ :: Q (Either GitError String)
gitShortHashQ = runGit ["rev-parse", "--short", "HEAD"] IdxNotUsed

-- | @since 1.40.0
gitBranchQ :: Q (Either GitError String)
gitBranchQ = runGit ["rev-parse", "--abbrev-ref", "HEAD"] IdxNotUsed

-- | @since 1.40.0
gitDescribeQ :: Q (Either GitError String)
gitDescribeQ = runGit ["describe", "--long", "--always"] IdxNotUsed

-- | @since 1.40.0
gitDirtyQ :: Q (Either GitError Bool)
gitDirtyQ = fmap nonEmpty <$> runGit ["status", "--porcelain"] IdxUsed

-- | @since 1.40.0
gitDirtyTrackedQ :: Q (Either GitError Bool)
gitDirtyTrackedQ =
  fmap nonEmpty <$> runGit ["status", "--porcelain", "--untracked-files=no"] IdxUsed

-- | @since 1.40.0
gitCommitCountQ :: Q (Either GitError String)
gitCommitCountQ = runGit ["rev-list", "HEAD", "--count"] IdxNotUsed

-- | @since 1.40.0
gitCommitDateQ :: Q (Either GitError String)
gitCommitDateQ = runGit ["log", "HEAD", "-1", "--format=%cd"] IdxNotUsed

-- | Maps 'GitError' to string @UNKNOWN@.
--
-- @since 1.40.0
liftDefString :: Q (Either GitError String) -> Q String
liftDefString m = fmap (either (const "UNKNOWN") id) m

-- | Maps 'GitError' to 'False'.
--
-- @since 1.40.0
liftFalse :: Q (Either GitError Bool) -> Q Bool
liftFalse m = m >>= either (pure . const False) (pure)

-- | Calls 'error' on 'GitError'.
--
-- @since 1.40.0
liftError :: Q (Either GitError String) -> Q String
liftError m = fmap (either (error . displayException) id) m

-- | @envFallback var m@ looks up environment variable @var@ when @m@ returns
-- fails. Returns the original error if the lookup also fails.
--
-- @since 1.40.0
envFallback :: String -> Q (Either GitError String) -> Q (Either GitError String)
envFallback var m = do
  m >>= \case
    Right r -> pure $ Right r
    Left err ->
      lookupEnvQ var >>= \case
        Just x -> pure $ Right x
        Nothing -> pure $ Left err

nonEmpty :: String -> Bool
nonEmpty "" = False
nonEmpty _ = True

lookupEnvQ :: String -> Q (Maybe String)
lookupEnvQ s = runIO (Env.lookupEnv s)

-- | Errors that can be encountered with git.
--
-- @since 1.40.0
data GitError
  = -- | @since 1.40.0
    GitNotFound
  | -- | @since 1.40.0
    GitRunError String
  deriving stock (Eq, Lift, Show)

-- | @since 1.40.0
instance Exception GitError where
  displayException GitNotFound = "Git executable not found"
  displayException (GitRunError s) = "Git error: " ++ s

-- | Run git with the given arguments and no stdin, returning the
-- stdout output.
runGit :: [String] -> IndexUsed -> Q (Either GitError String)
runGit args useIdx = do
  let oops :: SomeException -> IO (ExitCode, String, String)
      oops ex = pure (ExitFailure 1, "", displayException ex)
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
        (code, out, err) <- readProcessWithExitCode "git" args "" `catchSync` oops
        case code of
          ExitSuccess -> pure $ Right (tillNewLineStr out)
          ExitFailure _ -> pure $ Left $ GitRunError err
    else pure $ Left GitNotFound

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
