{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      :  $Header$
-- Copyright   :  (c) 2015 Adam C. Foltzer
-- License     :  BSD3
-- Maintainer  :  acfoltzer@galois.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Some handy Template Haskell splices for including the current git
-- hash and branch in the code of your project. Useful for including
-- in panic messages, @--version@ output, or diagnostic info for more
-- informative bug reports.
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > import Development.GitRev
-- >
-- > panic :: String -> a
-- > panic msg = error panicMsg
-- >   where panicMsg =
-- >           concat [ "[panic ", $(gitBranch), "@", $(gitHash)
-- >                  , " (", $(gitCommitDate), ")"
-- >                  , " (", $(gitCommitCount), " commits in HEAD)"
-- >                  , dirty, "] ", msg ]
-- >         dirty | $(gitDirty) = " (uncommitted files present)"
-- >               | otherwise   = ""
-- >
-- > main = panic "oh no!"
--
-- > % cabal exec runhaskell Example.hs
-- > Example.hs: [panic master@2ae047ba5e4a6f0f3e705a43615363ac006099c1 (Mon Jan 11 11:50:59 2016 -0800) (14 commits in HEAD) (uncommitted files present)] oh no!
module Development.GitRev
  ( gitBranch,
    gitCommitCount,
    gitCommitDate,
    gitDescribe,
    gitDirty,
    gitDirtyTracked,
    gitHash,
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
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Language.Haskell.TH (ExpQ, Q, conE, runIO, stringE)
import Language.Haskell.TH.Syntax (addDependentFile, falseName, trueName)
import System.Directory.OsPath
  ( doesDirectoryExist,
    doesFileExist,
    findExecutable,
    getCurrentDirectory,
  )
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.File.OsPath qualified as FileIO
import System.OsPath (OsPath, osp, (</>))
import System.OsPath qualified as OsPath
import System.OsString qualified as OsString
import System.Process (readProcessWithExitCode)

-- | Run git with the given arguments and no stdin, returning the
-- stdout output. If git isn't available or something goes wrong,
-- return the second argument.
runGit :: [String] -> String -> IndexUsed -> Q String
runGit args def useIdx = do
  let oops :: SomeException -> IO (ExitCode, String, String)
      oops _e = pure (ExitFailure 1, def, "")
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
          ExitSuccess -> pure (tillNewLineStr out)
          ExitFailure _ -> pure def
    else pure def

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

-- | Return the hash of the current git commit, or @UNKNOWN@ if not in
-- a git repository
gitHash :: ExpQ
gitHash =
  stringE =<< runGit ["rev-parse", "HEAD"] "UNKNOWN" IdxNotUsed

-- | Return the branch (or tag) name of the current git commit, or @UNKNOWN@
-- if not in a git repository. For detached heads, this will just be
-- "HEAD"
gitBranch :: ExpQ
gitBranch =
  stringE =<< runGit ["rev-parse", "--abbrev-ref", "HEAD"] "UNKNOWN" IdxNotUsed

-- | Return the long git description for the current git commit, or
-- @UNKNOWN@ if not in a git repository.
gitDescribe :: ExpQ
gitDescribe =
  stringE =<< runGit ["describe", "--long", "--always"] "UNKNOWN" IdxNotUsed

-- | Return @True@ if there are non-committed files present in the
-- repository
gitDirty :: ExpQ
gitDirty = do
  output <- runGit ["status", "--porcelain"] "" IdxUsed
  case output of
    "" -> conE falseName
    _ -> conE trueName

-- | Return @True@ if there are non-commited changes to tracked files
-- present in the repository
gitDirtyTracked :: ExpQ
gitDirtyTracked = do
  output <- runGit ["status", "--porcelain", "--untracked-files=no"] "" IdxUsed
  case output of
    "" -> conE falseName
    _ -> conE trueName

-- | Return the number of commits in the current head
gitCommitCount :: ExpQ
gitCommitCount =
  stringE =<< runGit ["rev-list", "HEAD", "--count"] "UNKNOWN" IdxNotUsed

-- | Return the commit date of the current head
gitCommitDate :: ExpQ
gitCommitDate =
  stringE =<< runGit ["log", "HEAD", "-1", "--format=%cd"] "UNKNOWN" IdxNotUsed

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
