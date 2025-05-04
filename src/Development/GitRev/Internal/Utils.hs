{-# LANGUAGE QuantifiedConstraints #-}

-- | Utils module.
--
-- @since 0.1
module Development.GitRev.Internal.Utils
  ( -- * Either projections
    projectStringUnknown,
    Common.projectConst,
    Common.projectFalse,
    Common.projectError,
    Common.projectErrorMap,
    Common.projectLeft,

    -- * Composing errors
    GitRevError (..),

    -- ** Functions
    runGitInEnvDirQ,

    -- ** Mapping utilities
    embedGitError,
    embedEnvError,
    embedTextError,
    Common.joinFirst,
  )
where

import Control.Exception (Exception (displayException))
import Data.Bifunctor (Bifunctor (first))
import Data.Text (Text)
import Data.Text qualified as T
import Development.GitRev.Internal.Environment (EnvError)
import Development.GitRev.Internal.Environment qualified as Env
import Development.GitRev.Internal.Git (GitError)
import Development.GitRev.Internal.Utils.Common qualified as Common
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Syntax (Lift)

-- $setup
-- >>> import Development.GitRev.Typed (qToCode)
-- >>> import Development.GitRev.Internal.Git (GitError (..), gitDirtyQ, gitHashQ)
-- >>> import Development.GitRev.Internal.Environment (EnvError (..))
-- >>> import Language.Haskell.TH (Q, runIO, runQ)
-- >>> import System.Environment (setEnv)

-- | Projects 'Left' to the string @UNKNOWN@.
--
-- ==== __Examples__
--
-- >>> $$(qToCode $ projectStringUnknown (pure $ Left ()))
-- "UNKNOWN"
--
-- @since 0.1
projectStringUnknown ::
  forall f e.
  (Functor f) =>
  f (Either e String) ->
  f String
projectStringUnknown = Common.projectConst "UNKNOWN"

-- | General error type for anything that can go wrong when running
-- @gitrev-typed@ splices.
--
-- @since 0.1
data GitRevError
  = -- | Git error.
    --
    -- @since 0.1
    GitRevErrorGit GitError
  | -- | Environment variable lookup error.
    --
    -- @since 0.1
    GitRevErrorEnv EnvError
  | -- | Catch-all for anything else that can go wrong.
    --
    -- @since 0.1
    GitRevErrorText Text
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Lift,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception GitRevError where
  displayException (GitRevErrorGit ge) = displayException ge
  displayException (GitRevErrorEnv x) = displayException x
  displayException (GitRevErrorText txt) = T.unpack txt

-- | Runs the git action under the directory @d@ pointed to by the
-- given environment variable.
--
-- ==== __Examples__
--
-- >>> setEnv "SOME_DIR" "./"
-- >>> $$(qToCode $ runGitInEnvDirQ "SOME_DIR" gitHashQ)
-- Right ...
--
-- @since 0.1
runGitInEnvDirQ ::
  forall a.
  -- | Environment variable pointing to a directory path, in which we run
  -- the git process.
  String ->
  -- | Git process to run.
  Q (Either GitError a) ->
  -- | The result.
  Q (Either GitRevError a)
runGitInEnvDirQ var =
  fmap (Common.joinFirst GitRevErrorEnv GitRevErrorGit)
    . Env.runInEnvDirQ var

-- | Embeds a 'GitError' in the larger 'GitRevError'.
--
-- ==== __Examples__
--
-- >>> :{
--   let q :: Q (Either GitError ())
--       q = pure (Left $ MkGitError "not found")
--   in runQ $ embedGitError q
-- :}
-- Left (GitRevErrorGit (MkGitError {reason = "not found"}))
--
-- @since 0.1
embedGitError ::
  forall f p a.
  ( Bifunctor p,
    Functor f
  ) =>
  -- | .
  f (p GitError a) ->
  f (p GitRevError a)
embedGitError = fmap (first GitRevErrorGit)

-- | Embeds an 'EnvError' in the larger 'GitRevError'.
--
-- ==== __Examples__
--
-- >>> :{
--   let q :: Q (Either EnvError ())
--       q = pure (Left $ MkEnvError "VAR" Nothing "VAR does not exist")
--   in runQ $ embedEnvError q
-- :}
-- Left (GitRevErrorEnv (MkEnvError {var = "VAR", value = Nothing, reason = "VAR does not exist"}))
--
-- @since 0.1
embedEnvError ::
  forall f p a.
  ( Bifunctor p,
    Functor f
  ) =>
  -- | .
  f (p EnvError a) ->
  f (p GitRevError a)
embedEnvError = fmap (first GitRevErrorEnv)

-- | Embeds a 'Text' in the larger 'GitRevError'.
--
-- ==== __Examples__
--
-- >>> :{
--   let q :: Q (Either Text ())
--       q = pure (Left "Something went wrong")
--   in runQ $ embedTextError q
-- :}
-- Left (GitRevErrorText "Something went wrong")
--
-- @since 0.1
embedTextError ::
  forall f p a.
  ( Bifunctor p,
    Functor f
  ) =>
  -- | .
  f (p Text a) ->
  f (p GitRevError a)
embedTextError = fmap (first GitRevErrorText)
