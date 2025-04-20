{-# LANGUAGE CPP #-}

-- | "Development.GitRev.Internal.Environment" for 'OsString'.
--
-- @since 0.1
module Development.GitRev.Internal.Environment.OsString
  ( LookupEnvError (..),
    envValQ,
    runInEnvDirQ,
    withEnvValQ,
  )
where

import Control.Exception (Exception (displayException))
import Development.GitRev.Internal.OsString qualified as OsStringI
import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Syntax (Lift)
import System.Directory.OsPath qualified as Dir
import System.OsPath (OsString)
#if MIN_VERSION_process(1, 6, 26)
import System.Process.Environment.OsString qualified as Process
#else
import System.Environment qualified as Env
#endif

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> :set -XTemplateHaskell
-- >>> import Development.GitRev.Typed.OsString (qToCode)
-- >>> import Language.Haskell.TH (Q, runIO, runQ)
-- >>> import System.Environment (setEnv)
-- >>> import System.OsPath (osp)
-- >>> import System.OsString (osstr)

-- | Performs an environment variable lookup in 'Q'.
--
-- ==== __Examples__
--
-- >>> setEnv "SOME_VAR" "val"
-- >>> $$(qToCode $ envValQ [osstr|SOME_VAR|])
-- Right "val"
--
-- @since 0.1
envValQ ::
  -- | The environment variable @k@.
  OsString ->
  -- | The result @v@ or an error.
  Q (Either LookupEnvError OsString)
envValQ var = withEnvValQ var pure

-- | Runs the given 'Q'-action under the directory @d@ pointed to by the
-- given environment variable.
--
-- ==== __Examples__
--
-- >>> import System.Directory.OsPath (listDirectory)
-- >>> setEnv "SOME_DIR" "./lib/gitrev/src"
-- >>> $$(qToCode $ runInEnvDirQ [osstr|SOME_DIR|] $ runIO (listDirectory [osp|./|]))
-- Right ["Development"]
--
-- @since 0.1
runInEnvDirQ ::
  forall a.
  -- | The environment variable @k@ that should point to some directory
  -- @d@.
  OsString ->
  -- | The 'Q' action @q@.
  Q a ->
  -- | The result of running @q@ in directory @d@.
  Q (Either LookupEnvError a)
runInEnvDirQ var m = withEnvValQ var $ \repoDir -> do
  currDir <- runIO Dir.getCurrentDirectory
  runIO $ Dir.setCurrentDirectory repoDir
  r <- m
  runIO $ Dir.setCurrentDirectory currDir
  pure $ r

-- | Environment variable lookup failure. The value is the variable we
-- attempted to look up.
--
-- @since 0.1
newtype LookupEnvError = MkLookupEnvError OsString
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Lift,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception LookupEnvError where
  displayException (MkLookupEnvError var) =
    "Failed to lookup environment variable: " ++ OsStringI.decodeLenient var

-- | Runs a 'Q'-action on the result of an environment variable, if it exists.
--
-- ==== __Examples__
--
-- >>> import System.Directory.OsPath (listDirectory)
-- >>> setEnv "SOME_DIR" "./lib/gitrev/src"
-- >>> $$(qToCode $ withEnvValQ [osstr|SOME_DIR|] (runIO . listDirectory))
-- Right ["Development"]
--
-- @since 0.1
withEnvValQ ::
  forall a.
  -- | The environment variable @k@ to lookup.
  OsString ->
  -- | Function to run on @k@'s /value/ if @k@ exists.
  (OsString -> Q a) ->
  Q (Either LookupEnvError a)
withEnvValQ var onEnv = do
  lookupEnvQ var >>= \case
    Nothing -> pure $ Left $ MkLookupEnvError var
    Just result -> Right <$> onEnv result

lookupEnvQ :: OsString -> Q (Maybe OsString)
lookupEnvQ = runIO . lookupEnv

lookupEnv :: OsString -> IO (Maybe OsString)
#if MIN_VERSION_process(1, 6, 26)
lookupEnv = Process.getEnv
#else
lookupEnv os = do
  fp <- OsStringI.decodeThrowM os
  r <- Env.lookupEnv fp
  traverse OsStringI.encodeThrowM r
#endif
