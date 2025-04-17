{-# LANGUAGE CPP #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- | Utils module.
--
-- @since 2.0
module Development.GitRev.Utils
  ( -- * Combining Q actions lazily
    QFirst (..),
    firstRight,

    -- * Basic Either lifting
    liftDefString,
    liftFalse,
    liftError,

    -- * Composing errors
    GitOrLookupEnvError (..),

    -- ** Lifted functions
    envValQ,
    runGitInEnvDirQ,

    -- ** Lifting utilities
    liftGitError,
    liftLookupEnvError,
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
import Development.GitRev.Utils.Git (GitError)
import Development.GitRev.Utils.LookupEnv (LookupEnvError)
import Development.GitRev.Utils.LookupEnv qualified as LookupEnv
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Syntax (Lift)

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import Development.GitRev.Typed (qToCode)
-- >>> import Development.GitRev.Utils.Git (GitError (..), gitDirtyQ, gitHashQ)
-- >>> import Development.GitRev.Utils.LookupEnv (LookupEnvError (..))
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

-- | @firstRight q qs@ takes the first @qi@ in @q : qs@ that returns
-- 'Right', without executing any @qj@ for @j > i@. If there are no
-- 'Right'\'s, returns the final result.
--
-- ==== __Examples__
--
-- >>> :{
--    $$( qToCode $
--          firstRight
--            (pure (Left GitNotFound))
--            [ gitHashQ,
--              error "oh no"
--            ]
--      )
-- :}
-- Right ...
--
-- @since 2.0
firstRight :: Q (Either e a) -> [Q (Either e a)] -> Q (Either e a)
firstRight q qs = unQFirst $ foldMap1 MkQFirst (q :| qs)

-- | Lifts 'Left' to string @UNKNOWN@.
--
-- ==== __Examples__
--
-- >>> :{
--   let gitHashDefStringQ :: Q String
--       gitHashDefStringQ = liftDefString gitHashQ
--   -- inling gitHashDefStringQ here due to stage restriction
--   in $$(qToCode $ liftDefString gitHashQ)
-- :}
-- ...
--
-- @since 2.0
liftDefString :: (Functor f) => f (Either e String) -> f String
liftDefString = fmap (either (const "UNKNOWN") id)

-- | Lifts 'Left' to 'False'.
--
-- ==== __Examples__
--
-- >>> :{
--   let gitDirtyDefFalseQ :: Q Bool
--       gitDirtyDefFalseQ = liftFalse gitDirtyQ
--   in $$(qToCode $ liftFalse gitDirtyQ)
-- :}
-- ...
--
-- @since 2.0
liftFalse :: (Functor f) => f (Either e Bool) -> f Bool
liftFalse = fmap (either (const False) id)

-- | Calls 'error' on 'Left'.
--
-- ==== __Examples__
--
-- >>> :{
--   let gitHashOrDieQ :: Q String
--       gitHashOrDieQ = liftError gitHashQ
--   in $$(qToCode $ liftError gitHashQ)
-- :}
-- ...
--
-- @since 2.0
liftError :: (Exception e, Functor f) => f (Either e a) -> f a
liftError = fmap (either (error . displayException) id)

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

-- | Performs an environment variable lookup in 'Q'. Wrapper for
-- 'LookupEnv.envValQ' that lifts 'LookupEnvError' to 'GitOrLookupEnvError'
-- for convenience.
--
-- ==== __Examples__
--
-- >>> setEnv "SOME_VAR" "val"
-- >>> $$(qToCode $ envValQ "SOME_VAR")
-- Right "val"
--
-- @since 2.0
envValQ ::
  -- | Environment variable @k@ to lookup.
  String ->
  -- | The result @v@ or an error.
  Q (Either GitOrLookupEnvError String)
envValQ = liftLookupEnvError . LookupEnv.envValQ

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
  -- | Environment variable pointing to a directory path, in which we run
  -- the git process.
  String ->
  -- | Git process to run.
  Q (Either GitError a) ->
  -- | The result.
  Q (Either GitOrLookupEnvError a)
runGitInEnvDirQ var = joinErrors . LookupEnv.runInEnvDirQ var
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
  ( Bifunctor p,
    forall e. Monad (p e)
  ) =>
  -- | .
  p LookupEnvError (p GitError a) ->
  p GitOrLookupEnvError a
joinLookupEnvGitErrors =
  join
    . liftGitError
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
  ( Bifunctor p,
    forall e. Monad (p e)
  ) =>
  -- | .
  p GitError (p LookupEnvError a) ->
  p GitOrLookupEnvError a
joinGitLookupEnvErrors =
  join
    . liftLookupEnvError
    . first GitOrLookupEnvGit

-- | Utility function for lifting a 'GitError' to the larger
-- 'GitOrLookupEnvError'.
--
-- ==== __Examples__
--
-- >>> :{
--   let q :: Q (Either GitError ())
--       q = pure (Left GitNotFound)
--   in runQ $ liftGitError q
-- :}
-- Left (GitOrLookupEnvGit GitNotFound)
--
-- @since 2.0
liftGitError ::
  ( Bifunctor p,
    Functor f
  ) =>
  -- | .
  f (p GitError a) ->
  f (p GitOrLookupEnvError a)
liftGitError = fmap (first GitOrLookupEnvGit)

-- | Utility function for lifting a 'LookupEnvError' to the larger
-- 'GitOrLookupEnvError'.
--
-- ==== __Examples__
--
-- >>> :{
--   let q :: Q (Either LookupEnvError ())
--       q = pure (Left $ MkLookupEnvError "VAR")
--   in runQ $ liftLookupEnvError q
-- :}
-- Left (GitOrLookupEnvLookupEnv (MkLookupEnvError "VAR"))
--
-- @since 2.0
liftLookupEnvError ::
  ( Bifunctor p,
    Functor f
  ) =>
  -- | .
  f (p LookupEnvError a) ->
  f (p GitOrLookupEnvError a)
liftLookupEnvError = fmap (first GitOrLookupEnvLookupEnv)

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
