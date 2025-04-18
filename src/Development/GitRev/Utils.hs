{-# LANGUAGE CPP #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- | Utils module.
--
-- @since 2.0
module Development.GitRev.Utils
  ( -- * Combining Q actions lazily
    QFirst (..),
    firstSuccessQ,

    -- * Either projections
    projectStringUnknown,
    projectString,
    projectFalse,
    projectError,
    projectErrorMap,

    -- * Composing errors
    GitOrLookupEnvError (..),

    -- ** Functions
    runGitInEnvDirQ,

    -- ** Mapping utilities
    embedGitError,
    embedLookupEnvError,
    joinLookupEnvGitErrors,
    joinGitLookupEnvErrors,
  )
where

import Control.Exception (Exception (displayException))
import Control.Monad (join)
import Data.Bifunctor (Bifunctor (first))
#if MIN_VERSION_base(4, 18, 0)
import Data.Foldable1 (Foldable1 (foldMap1))
#endif
import Data.List.NonEmpty (NonEmpty ((:|)))
import Development.GitRev.Utils.Environment (LookupEnvError)
import Development.GitRev.Utils.Environment qualified as Env
import Development.GitRev.Utils.Git (GitError)
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Syntax (Lift)

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import Development.GitRev.Typed (qToCode)
-- >>> import Development.GitRev.Utils.Git (GitError (..), gitDirtyQ, gitHashQ)
-- >>> import Development.GitRev.Utils.Environment (LookupEnvError (..))
-- >>> import Language.Haskell.TH (Q, runIO, runQ)
-- >>> import System.Environment (setEnv)

-- | Wrapper for 'Q' over 'Either' with a lazier 'Semigroup'. With this, we
-- can run:
--
-- @
--   MkQFirst q1 <> MkQFirst q2
-- @
--
-- This will only execute @q2@ if @q1@ returns 'Left', unlike 'Q'\'s normal
-- 'Semigroup' instance.
data QFirst e a = MkQFirst {unQFirst :: Q (Either e a)}

-- | @since 2.0
instance Semigroup (QFirst e a) where
  MkQFirst q1 <> q2 =
    MkQFirst $
      q1 >>= \case
        Right x -> pure $ Right x
        Left _ -> unQFirst q2

-- | @firstSuccessQ q qs@ takes the first @qi@ in @q : qs@ that returns
-- 'Right', without executing any @qj@ for @j > i@. If there are no
-- 'Right'\'s, returns the final result.
--
-- ==== __Examples__
--
-- >>> :{
--    $$( qToCode $
--          firstSuccessQ
--            (pure (Left GitNotFound))
--            [ gitHashQ,
--              error "oh no"
--            ]
--      )
-- :}
-- Right ...
--
-- @since 2.0
firstSuccessQ :: forall e a. Q (Either e a) -> [Q (Either e a)] -> Q (Either e a)
firstSuccessQ q qs = unQFirst $ foldMap1 MkQFirst (q :| qs)

-- | Projects 'Left' to the string @UNKNOWN@.
--
-- ==== __Examples__
--
-- >>> :{
--   let gitHashUnknownQ :: Q String
--       gitHashUnknownQ = projectStringUnknown gitHashQ
--   -- inling gitHashUnknownQ here due to stage restriction
--   in $$(qToCode $ projectStringUnknown gitHashQ)
-- :}
-- ...
--
-- >>> $$(qToCode $ projectStringUnknown (pure $ Left ()))
-- "UNKNOWN"
--
-- @since 2.0
projectStringUnknown ::
  forall f e.
  (Functor f) =>
  f (Either e String) ->
  f String
projectStringUnknown = projectString "UNKNOWN"

-- | Projects 'Left' to the given string.
--
-- ==== __Examples__
--
-- >>> :{
--   let gitHashDefStringQ :: Q String
--       gitHashDefStringQ = projectString "FAILURE" gitHashQ
--   in $$(qToCode $ projectString "FAILURE" gitHashQ)
-- :}
-- ...
--
-- >>> $$(qToCode $ projectString "FAILURE" (pure $ Left ()))
-- "FAILURE"
--
-- @since 2.0
projectString ::
  forall f e.
  (Functor f) =>
  String ->
  f (Either e String) ->
  f String
projectString = projectLeft . const

-- | Projects 'Left' to 'False'.
--
-- ==== __Examples__
--
-- >>> :{
--   let gitDirtyDefFalseQ :: Q Bool
--       gitDirtyDefFalseQ = projectFalse gitDirtyQ
--   in $$(qToCode $ projectFalse gitDirtyQ)
-- :}
-- ...
--
-- >>> $$(qToCode $ projectFalse (pure $ Left ()))
-- False
--
-- @since 2.0
projectFalse :: forall f e. (Functor f) => f (Either e Bool) -> f Bool
projectFalse = projectLeft (const False)

-- | Projects 'Left' via 'error', rendering via 'displayException'. Hence
-- an error will cause a compilation failure.
--
-- ==== __Examples__
--
-- >>> :{
--   let gitHashOrDieQ :: Q String
--       gitHashOrDieQ = projectError gitHashQ
--   in $$(qToCode $ projectError gitHashQ)
-- :}
-- ...
--
-- @since 2.0
projectError :: forall f e a. (Exception e, Functor f) => f (Either e a) -> f a
projectError = projectErrorMap displayException

-- | Projects 'Left' via 'error', rendering via the given function. Hence
-- an error will cause a compilation failure.
--
-- ==== __Examples__
--
-- >>> :{
--   let gitHashOrDieQ :: Q String
--       gitHashOrDieQ = (projectErrorMap show) gitHashQ
--   in $$(qToCode $ (projectErrorMap show) gitHashQ)
-- :}
-- ...
--
-- @since 2.0
projectErrorMap ::
  forall f e a.
  (Functor f) =>
  (e -> String) ->
  f (Either e a) ->
  f a
projectErrorMap onErr = projectLeft (error . onErr)

projectLeft :: forall f e a. (Functor f) => (e -> a) -> f (Either e a) -> f a
projectLeft f = fmap (either f id)

-- | Git or env lookup error.
--
-- @since 2.0
data GitOrLookupEnvError
  = -- | @since 2.0
    GitOrLookupEnvGit GitError
  | -- | @since 2.0
    GitOrLookupEnvLookupEnv LookupEnvError
  deriving stock
    ( -- | @since 2.0
      Lift,
      -- | @since 2.0
      Show
    )

-- | @since 2.0
instance Exception GitOrLookupEnvError where
  displayException (GitOrLookupEnvGit ge) = displayException ge
  displayException (GitOrLookupEnvLookupEnv x) = displayException x

-- | @runGitInEnvDirQ var q@ runs @q@ in the directory given by the
-- environment variable.
--
-- ==== __Examples__
--
-- >>> setEnv "SOME_DIR" "./"
-- >>> $$(qToCode $ runGitInEnvDirQ "SOME_DIR" gitHashQ)
-- Right ...
--
-- @since 2.0
runGitInEnvDirQ ::
  forall a.
  -- | Environment variable pointing to a directory path, in which we run
  -- the git process.
  String ->
  -- | Git process to run.
  Q (Either GitError a) ->
  -- | The result.
  Q (Either GitOrLookupEnvError a)
runGitInEnvDirQ var = joinErrors . Env.runInEnvDirQ var
  where
    joinErrors = fmap joinLookupEnvGitErrors

-- | Utility function for joining lookup and git errors.
--
-- ==== __Examples__
--
-- >>> :{
--   let e :: Either LookupEnvError (Either GitError ())
--       e = Right (Left GitNotFound)
--   in joinLookupEnvGitErrors e
-- :}
-- Left (GitOrLookupEnvGit GitNotFound)
--
-- @since 2.0
joinLookupEnvGitErrors ::
  forall p a.
  ( Bifunctor p,
    forall e. Monad (p e)
  ) =>
  -- | .
  p LookupEnvError (p GitError a) ->
  p GitOrLookupEnvError a
joinLookupEnvGitErrors =
  join
    . embedGitError
    . first GitOrLookupEnvLookupEnv

-- | Utility function for joining git and lookup errors.
--
-- ==== __Examples__
--
-- >>> :{
--   let e :: Either GitError (Either LookupEnvError ())
--       e = Right (Left $ MkLookupEnvError "VAR")
--   in joinGitLookupEnvErrors e
-- :}
-- Left (GitOrLookupEnvLookupEnv (MkLookupEnvError "VAR"))
--
-- @since 2.0
joinGitLookupEnvErrors ::
  forall p a.
  ( Bifunctor p,
    forall e. Monad (p e)
  ) =>
  -- | .
  p GitError (p LookupEnvError a) ->
  p GitOrLookupEnvError a
joinGitLookupEnvErrors =
  join
    . embedLookupEnvError
    . first GitOrLookupEnvGit

-- | Utility function for lifting a 'GitError' to the larger
-- 'GitOrLookupEnvError'.
--
-- ==== __Examples__
--
-- >>> :{
--   let q :: Q (Either GitError ())
--       q = pure (Left GitNotFound)
--   in runQ $ embedGitError q
-- :}
-- Left (GitOrLookupEnvGit GitNotFound)
--
-- @since 2.0
embedGitError ::
  forall f p a.
  ( Bifunctor p,
    Functor f
  ) =>
  -- | .
  f (p GitError a) ->
  f (p GitOrLookupEnvError a)
embedGitError = fmap (first GitOrLookupEnvGit)

-- | Utility function for lifting a 'LookupEnvError' to the larger
-- 'GitOrLookupEnvError'.
--
-- ==== __Examples__
--
-- >>> :{
--   let q :: Q (Either LookupEnvError ())
--       q = pure (Left $ MkLookupEnvError "VAR")
--   in runQ $ embedLookupEnvError q
-- :}
-- Left (GitOrLookupEnvLookupEnv (MkLookupEnvError "VAR"))
--
-- @since 2.0
embedLookupEnvError ::
  forall f p a.
  ( Bifunctor p,
    Functor f
  ) =>
  -- | .
  f (p LookupEnvError a) ->
  f (p GitOrLookupEnvError a)
embedLookupEnvError = fmap (first GitOrLookupEnvLookupEnv)

#if !MIN_VERSION_base(4, 18, 0)
-- Copied from base. Technically not the same as the import above since
-- that one works for all Foldable1, not just NonEmpty, but we only use it
-- here for NonEmpty, so whatever.
foldMap1 :: (Semigroup m) => (a -> m) -> NonEmpty a -> m
foldMap1 f (x :| xs) = go (f x) xs
  where
    go y [] = y
    go y (z : zs) = y <> go (f z) zs
#endif
