{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      :  $Header$
-- Copyright   :  (c) 2015 Adam C. Foltzer, 2025 Thomas Bidne
-- License     :  BSD3
-- Maintainer  :  tbidne@protonmail.com
--
-- Provides utilities for querying git for String' and
-- 'System.OsString.OsString'.
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
    trySync,
  )
where

import Control.Exception
  ( Exception (displayException, fromException),
    SomeAsyncException (SomeAsyncException),
    SomeException,
    throwIO,
    toException,
    tryJust,
  )
import Control.Monad (unless, (>=>))
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes, isJust)
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

-- | Parameters for running our git process below. Allows us to parameterize
-- over String and OsString.
data GitProcessArgs str = MkGitProcessArgs
  { -- | Conversion from String. Used in error reporting hence should be
    -- total i.e. lenient encodes, if necessary.
    fromStringTotal :: String -> str,
    -- | Args for acquiring git root i.e. ["rev-parse", "--show-toplevel"].
    gitRootArgs :: [str],
    -- | Runs git with parameter args.
    runProcessGit :: [str] -> IO (ExitCode, str, str),
    -- | Encode p to OsPath. IO due to possibility of failure i.e. we want
    -- errors to throw.
    toOsPath :: str -> IO OsPath,
    -- | Conversion to String. Used in error reporting hence should be
    -- total i.e. lenient encodes, if necessary.
    toStringTotal :: str -> String
  }

-- | Run git with the given arguments and no stdin, returning the
-- stdout output.
runGitPostprocess ::
  forall str.
  GitProcessArgs str ->
  -- | Post-processing on the result.
  (str -> str) ->
  -- | Args to run with git.
  [str] ->
  -- | Whether the index is used.
  IndexUsed ->
  Q (Either (GitError str) str)
runGitPostprocess
  gpArgs@MkGitProcessArgs {runProcessGit, fromStringTotal}
  postProcess
  args
  useIdx = do
    runIO (trySync getFilesAndGitResult) >>= \case
      Left ex -> pure $ Left $ MkGitError (fromStringTotal $ displayException ex)
      Right (filesToTrack, (ec, out, err)) -> do
        -- REVIEW: Do we want to add these even if git fails? Doing it for now
        -- because that was the previous behavior...
        --
        -- Also, note that if addDependentOsPath fails (i.e. a bug), it will
        -- cause everything to fail.
        traverse_ addDependentOsPath filesToTrack
        case ec of
          ExitFailure _ -> pure $ Left $ MkGitError err
          ExitSuccess -> pure $ Right (postProcess out)
    where
      -- Try to do as much IO in here as possible so it is easy to prevent
      -- exceptions from slipping later on by surrounding this with trySync.
      --
      -- This is why we return the list of dependent files to track, rather
      -- than handling them in-place. addDependentFile cannot be called
      -- inside IO, but we want all IO logic in one-place, if possible.
      -- Hence we do all the IO things here, return the files we want to
      -- track, then add them once we are in Q.
      getFilesAndGitResult :: IO ([OsPath], (ExitCode, str, str))
      getFilesAndGitResult = do
        gitFound <- isJust <$> findExecutable [osp|git|]

        unless gitFound $ throwIO GitExeNotFound

        filesToTrack <- getGitFiles

        result <- runProcessGit args
        pure (filesToTrack, result)

      -- Return the files to track later with addDependentFile. We split this
      -- up so that we can stuff all the IO logic behind try, then later use
      -- addDependentFile once we are in Q.
      getGitFiles :: IO [OsPath]
      getGitFiles = do
        -- a lot of bookkeeping to record the right dependencies
        pwd <- getDotGit gpArgs
        let hd = pwd </> [osp|HEAD|]
            index = pwd </> [osp|index|]
            packedRefs = pwd </> [osp|packed-refs|]
        hdExists <- doesFileExist hd
        headAndRefs <- whenList hdExists $ do
          -- the HEAD file either contains the hash of a detached head
          -- or a pointer to the file that contains the hash of the head
          contents <- readFileUtf8 hd
          case T.splitAt 5 contents of
            -- pointer to ref
            ("ref: ", relRef) -> do
              relRefOs <- OsStringI.encodeThrowM $ T.unpack relRef
              let ref = pwd </> tillNewLineOsPath relRefOs
              refExists <- doesFileExist ref
              pure $
                if refExists
                  then [Just hd, Just ref]
                  else [Just hd]
            -- detached head
            _hash -> pure [Just hd]
        -- add the index if it exists to set the dirty flag
        indexExists <- doesFileExist index
        packedExists <- doesFileExist packedRefs
        let mIdxFile = whenJust (indexExists && useIdx == IdxUsed) index
            -- if the refs have been packed, the info we're looking for
            -- might be in that file rather than the one-file-per-ref case
            -- handled above
            mPackedRefFiles = whenJust packedExists packedRefs
        pure (catMaybes $ mIdxFile : mPackedRefFiles : headAndRefs)

-- | @since 0.1
tillNewLineOsPath :: OsPath -> OsPath
tillNewLineOsPath = OsString.takeWhile (\c -> c /= nl && c /= cr)
  where
    nl = OsString.unsafeFromChar '\n'
    cr = OsString.unsafeFromChar '\r'

-- | Determine where our @.git@ directory is, in case we're in a
-- submodule.
getDotGit :: forall str. GitProcessArgs str -> IO OsPath
getDotGit gpArgs = do
  pwd <- getGitRoot gpArgs
  let dotGit = pwd </> [osp|.git|]
  isDir <- doesDirectoryExist dotGit

  if isDir
    then pure dotGit
    else do
      isFile <- doesFileExist dotGit

      unless isFile $ throwIO $ DotGitNotFound dotGit

      contents <- readFileUtf8 dotGit
      case T.splitAt 8 contents of
        ("gitdir: ", relDir) -> do
          relDirOs <- OsStringI.encodeThrowM $ T.unpack relDir
          isRelDir <- doesDirectoryExist relDirOs
          if isRelDir
            then pure relDirOs
            else throwIO $ DotGitFileNotDir dotGit relDirOs
        _ -> throwIO $ DotGitFileBadPrefix dotGit contents

readFileUtf8 :: OsPath -> IO Text
readFileUtf8 = FileIO.readFile' >=> either throwIO pure . TEnc.decodeUtf8'

-- | Get the root directory of the Git repo.
getGitRoot :: forall str. GitProcessArgs str -> IO OsPath
getGitRoot
  MkGitProcessArgs
    { gitRootArgs,
      toOsPath,
      toStringTotal,
      runProcessGit
    } = do
    (code, out, _) <- runProcessGit gitRootArgs
    case code of
      ExitSuccess -> tillNewLineOsPath <$> toOsPath out
      ExitFailure _ -> do
        mCwd <-
          trySync getCurrentDirectory <&> \case
            Left _ -> Nothing
            Right cwd -> Just cwd
        throwIO $ GitRootNotFound mCwd (toStringTotal <$> gitRootArgs)

-- | Like when, except returns the empty list rather than unit. Monomorphic
-- on list (rather than e.g. Monoid) for clarity.
whenList :: forall f a. (Applicative f) => Bool -> f [a] -> f [a]
whenList False _ = pure []
whenList True xs = xs

-- | Similar to when, excepts lifts the param to Maybe. Monomorphic
-- on Maybe (rather than e.g. Alternative) for clarity.
whenJust :: forall a. Bool -> a -> Maybe a
whenJust False _ = Nothing
whenJust True x = Just x

nonEmpty :: forall a. (Eq a, Monoid a) => a -> Bool
nonEmpty = (/= mempty)

-- | Type to flag if the git index is used or not in a call to @runGitQ@.
--
-- @since 0.1
data IndexUsed
  = -- | The git index is used.
    --
    -- @since 0.1
    IdxUsed
  | -- | The git index is /not/ used.
    --
    -- @since 0.1
    IdxNotUsed
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | Errors that can be encountered with git. The argument is a string-like
-- message.
--
-- @since 0.1
newtype GitError str = MkGitError str
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Lift,
      -- | @since 0.1
      Show
    )

-- | Internal error. If this is thrown, it will be turned into its string
-- representation then wrapped in GitError. This exists entirely to improve
-- error messages / make some code here clearer.
data InternalError
  = -- | .git file started with unexpected prefix.
    DotGitFileBadPrefix OsPath Text
  | -- | directory pointed to by .git file was not found.
    DotGitFileNotDir OsPath OsPath
  | -- | .git dir/file not found.
    DotGitNotFound OsPath
  | -- | Git exe not found.
    GitExeNotFound
  | -- | Git root not found.
    GitRootNotFound (Maybe OsPath) [String]
  deriving stock (Show)

instance Exception InternalError where
  displayException = \case
    DotGitFileBadPrefix dotGit txt ->
      T.unpack . mconcat $
        [ "Expected .git file '",
          T.pack $ OsStringI.decodeLenient dotGit,
          "' to start with prefix 'gitdir: ', received: '",
          txt,
          "'"
        ]
    DotGitFileNotDir dotGit path ->
      mconcat
        [ "Directory listed in .git file '",
          OsStringI.decodeLenient dotGit,
          "' does not exist: '",
          OsStringI.decodeLenient path,
          "'"
        ]
    DotGitNotFound dotGit ->
      mconcat
        [ "File or directory does not exist: '",
          OsStringI.decodeLenient dotGit,
          "'"
        ]
    GitExeNotFound -> "Git exe not found"
    GitRootNotFound mCurrDir args ->
      mconcat
        [ "Failed running git with args: ",
          show args,
          ", when trying to find git root",
          currDirTxt
        ]
      where
        currDirTxt = case mCurrDir of
          Nothing -> "."
          Just cwd ->
            mconcat
              [ " in directory '",
                OsStringI.decodeLenient cwd,
                "'"
              ]

trySync :: forall a. IO a -> IO (Either SomeException a)
trySync = tryIf isSyncException

tryIf :: forall e a. (Exception e) => (e -> Bool) -> IO a -> IO (Either e a)
tryIf p = tryJust (\e -> if p e then Just e else Nothing)

isSyncException :: forall e. (Exception e) => e -> Bool
isSyncException e = case fromException (toException e) of
  Just SomeAsyncException {} -> False
  Nothing -> True

addDependentOsPath :: OsPath -> Q ()
addDependentOsPath = OsStringI.decodeThrowM >=> addDependentFile
