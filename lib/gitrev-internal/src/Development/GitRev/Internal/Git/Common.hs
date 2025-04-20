{-# LANGUAGE QuasiQuotes #-}

-- | Provides utilities for querying git.
--
-- @since 0.1
module Development.GitRev.Internal.Git.Common
  ( GitError (..),
    IndexUsed (..),
    GitProcessArgs (..),
    runGitPostprocess,

    -- * Misc
    tillNewLineOsPath,
    nonEmpty,
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
import Development.GitRev.Internal.OsString qualified as OsStringI
import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Syntax (Lift, addDependentFile)
import System.Directory.OsPath
  ( doesDirectoryExist,
    doesFileExist,
    findExecutable,
    getCurrentDirectory,
  )
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.File.OsPath qualified as FileIO
import System.OsPath (OsPath, osp, (</>))
import System.OsString qualified as OsString
import System.Process (readProcessWithExitCode)

-- | Errors that can be encountered with git.
--
-- @since 0.1
data GitError p
  = -- | @since 0.1
    GitNotFound
  | -- | @since 0.1
    GitRunError p
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Lift,
      -- | @since 0.1
      Show
    )

-- | Parameters for running our git process below. Allows us to parameterize
-- over String and OsString.
data GitProcessArgs p = MkGitProcessArgs
  { -- | Empty path i.e. "".
    emptyPath :: p,
    -- | Git name i.e. "git".
    gitExeName :: p,
    -- | Process function.
    runProcessFn :: p -> [p] -> p -> IO (ExitCode, p, p),
    -- | Conversion from String. Used in error reporting hence should be
    -- total i.e. lenient encodes, if necessary.
    strToP :: String -> p
  }

-- | Run git with the given arguments and no stdin, returning the
-- stdout output.
runGitPostprocess ::
  forall p.
  GitProcessArgs p ->
  -- | Post-processing on the result.
  (p -> p) ->
  -- | Args to run with git.
  [p] ->
  -- | Whether the index is used.
  IndexUsed ->
  Q (Either (GitError p) p)
runGitPostprocess gitProcessArgs postProcess args useIdx = do
  let oops :: SomeException -> IO (ExitCode, p, p)
      oops ex = pure (ExitFailure 1, gpaEmptyPath, gpaStrToP $ displayException ex)
  gitFound <- runIO $ isJust <$> findExecutable [osp|git|]
  if gitFound
    then do
      -- a lot of bookkeeping to record the right dependencies
      pwd <- runIO getDotGit
      let hd = pwd </> [osp|HEAD|]
          index = pwd </> [osp|index|]
          packedRefs = pwd </> [osp|packed-refs|]
      hdFp <- OsStringI.decodeThrowM hd
      hdExists <- runIO $ doesFileExist hd
      when hdExists $ do
        addDependentFile hdFp
        -- the HEAD file either contains the hash of a detached head
        -- or a pointer to the file that contains the hash of the head
        T.splitAt 5 <$> runIO (readFileUtf8 hd) >>= \case
          -- pointer to ref
          ("ref: ", relRef) -> do
            relRefOs <- OsStringI.encodeThrowM $ T.unpack relRef
            let ref = pwd </> tillNewLineOsPath relRefOs
            refExists <- runIO $ doesFileExist ref
            refFp <- OsStringI.decodeThrowM ref
            when refExists $ addDependentFile refFp
          -- detached head
          _hash -> pure ()
      -- add the index if it exists to set the dirty flag
      indexExists <- runIO $ doesFileExist index
      when (indexExists && useIdx == IdxUsed) $ do
        indexFp <- OsStringI.decodeThrowM index
        addDependentFile indexFp
      -- if the refs have been packed, the info we're looking for
      -- might be in that file rather than the one-file-per-ref case
      -- handled above
      packedExists <- runIO $ doesFileExist packedRefs
      when packedExists $ do
        packedRefsFp <- OsStringI.decodeThrowM packedRefs
        addDependentFile packedRefsFp
      runIO $ do
        (code, out, err) <-
          gpaRunProcessFn gpaGitExeName args gpaEmptyPath `catchSync` oops
        case code of
          ExitSuccess -> pure $ Right (postProcess out)
          ExitFailure _ -> pure $ Left $ GitRunError err
    else pure $ Left GitNotFound
  where
    gpaEmptyPath = emptyPath gitProcessArgs
    gpaGitExeName = gitExeName gitProcessArgs
    gpaRunProcessFn = runProcessFn gitProcessArgs
    gpaStrToP = strToP gitProcessArgs

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
              relDirOs <- OsStringI.encodeThrowM $ T.unpack relDir
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
  out' <- OsStringI.encodeThrowM out
  case code of
    ExitSuccess -> pure $ tillNewLineOsPath out'
    ExitFailure _ -> pure pwd -- later steps will fail, that's fine

nonEmpty :: (Eq a, Monoid a) => a -> Bool
nonEmpty = not . (== mempty)

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
