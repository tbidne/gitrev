{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

-- | "Development.GitRev.Internal.Environment" for 'OsString'.
--
-- @since 0.1
module Development.GitRev.Internal.Environment.OsString
  ( EnvError (..),
    envValQ,
    runInEnvDirQ,
    withEnvValQ,
  )
where

import Control.Exception (Exception (displayException))
import Control.Monad (join)
import Development.GitRev.Internal.Git.Common qualified as GitC
import Development.GitRev.Internal.OsString qualified as OsStringI
import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Syntax (Lift)
import System.Directory.OsPath qualified as Dir
import System.OsString (OsString, osstr)
#if MIN_VERSION_process(1, 6, 26)
import System.Process.Environment.OsString qualified as Process
#else
import System.Environment qualified as Env
#endif

-- $setup
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
  Q (Either EnvError OsString)
envValQ var = withEnvValQ var pure

-- | Runs the given 'Q'-action under the directory @d@ pointed to by the
-- given environment variable.
--
-- ==== __Examples__
--
-- >>> import System.Directory.OsPath (listDirectory)
-- >>> setEnv "SOME_DIR" "./src"
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
  Q (Either EnvError a)
runInEnvDirQ var m = fmap join $ withEnvValQ var $ \repoDir -> do
  -- Try to change directory
  eCurrDir <- runIO $ GitC.trySync $ do
    currDir <- Dir.getCurrentDirectory
    Dir.setCurrentDirectory repoDir
    pure currDir

  let mkErr = Left . MkEnvError var (Just repoDir)

  case eCurrDir of
    Left ex -> do
      let rsn =
            [osstr|Could not set directory: |]
              <> OsStringI.encodeLenient (displayException ex)
      pure $ mkErr rsn
    Right currDir -> do
      r <- m
      eResult <- runIO $ GitC.trySync $ Dir.setCurrentDirectory currDir
      case eResult of
        Left ex -> do
          let rsn =
                [osstr|Could not restore directory: |]
                  <> OsStringI.encodeLenient (displayException ex)
          pure $ mkErr rsn
        Right _ -> pure $ Right r

-- | Environment variable lookup failure.
--
-- @since 0.1
data EnvError = MkEnvError
  { -- | The environment variable.
    --
    -- @since 0.1
    var :: OsString,
    -- | The value of the environment variable, if it exists.
    --
    -- @since 0.1
    value :: Maybe OsString,
    -- | Text reason for the failure.
    --
    -- @since 0.1
    reason :: OsString
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Lift,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception EnvError where
  displayException err =
    mconcat
      [ "Environment error with env variable '",
        OsStringI.decodeLenient $ var err,
        "', value ",
        OsStringI.decodeLenient valStr,
        ": ",
        OsStringI.decodeLenient $ reason err
      ]
    where
      valStr = case value err of
        Nothing -> [osstr|<none>|]
        Just value -> [osstr|'|] <> value <> [osstr|'|]

-- | Runs a 'Q'-action on the result of an environment variable, if it exists.
--
-- ==== __Examples__
--
-- >>> import System.Directory.OsPath (listDirectory)
-- >>> setEnv "SOME_DIR" "./src"
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
  Q (Either EnvError a)
withEnvValQ var onEnv = do
  lookupEnvQ var >>= \case
    Nothing -> pure $ Left $ MkEnvError var Nothing [osstr|No such var found.|]
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
