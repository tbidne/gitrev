{-# LANGUAGE CPP #-}

-- | Provides 'QFirst' type.
--
-- @since 0.1
module Development.GitRev.Internal.QFirst
  ( -- * Combining Q actions lazily
    QFirst (..),
    mkQFirst,
    firstSuccessQ,
    Exceptions (..),
    mkExceptions,
  )
where

import Control.Exception (Exception (displayException))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.Foldable (Foldable (fold))
#if MIN_VERSION_base(4, 18, 0)
import Data.Foldable1 (Foldable1 (foldMap1))
#endif
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Text.Lazy.Builder.Int qualified as TLBI
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Syntax (Lift)

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import Development.GitRev.Typed (qToCode)
-- >>> import Development.GitRev.Internal.Git (GitError (..), gitDirtyQ, gitHashQ)
-- >>> import Development.GitRev.Internal.Environment (EnvLookupError (..))
-- >>> import Language.Haskell.TH (Q, runIO, runQ)
-- >>> import System.Environment (setEnv)

-- | Collects multiple exceptions.
--
-- @since 0.1
newtype Exceptions e = MkExceptions {unExceptions :: (NonEmpty e)}
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Functor,
      -- | @since 0.1
      Lift,
      -- | @since 0.1
      Show
    )
  deriving newtype
    ( -- | @since 0.1
      Applicative,
      -- | @since 0.1
      Monad,
      -- | @since 0.1
      Semigroup
    )

-- | @since 0.1
instance (Exception e) => Exception (Exceptions e) where
  displayException (MkExceptions errs) =
    mconcat
      [ "Exception(s):",
        renderErrs errs
      ]
    where
      renderErrs =
        T.unpack
          . TL.toStrict
          . TLB.toLazyText
          . fold
          . fmap renderErr
          . NE.zip @Int (1 :| [2 ..])

      renderErr (idx, e) =
        (\b -> "\n" <> TLBI.decimal idx <> ". " <> b)
          . TLB.fromText
          . T.strip
          . T.pack
          . displayException
          $ e

-- | Wraps a type in 'Exceptions'.
--
-- @since 0.1
mkExceptions :: forall e. e -> Exceptions e
mkExceptions = MkExceptions . NE.singleton

-- | Wrapper for 'Q' over 'Either' with a lazier 'Semigroup'. With this, we
-- can run:
--
-- @
--   mkQFirst q1 <> mkQFirst q2
-- @
--
-- This will only execute @q2@ if @q1@ returns 'Left', unlike 'Q'\'s normal
-- 'Semigroup' instance.
--
-- 'QFirst' also collects all errors in 'Exceptions'.
--
-- @since 0.1
newtype QFirst e a = MkQFirst {unQFirst :: Q (Either (Exceptions e) a)}
  deriving stock
    ( -- | @since 0.1
      Functor
    )

-- | @since 0.1
instance Semigroup (QFirst e a) where
  MkQFirst q1 <> q2 =
    MkQFirst $
      q1 >>= \case
        Right x -> pure $ Right x
        Left errs -> first (errs <>) <$> unQFirst q2

-- | @since 0.1
instance Applicative (QFirst e) where
  pure = mkQFirst . pure . Right

  MkQFirst q1 <*> q2 =
    MkQFirst $
      q1 >>= \case
        Left errs -> pure $ Left errs
        Right f -> second f <$> unQFirst q2

-- | @since 0.1
instance Monad (QFirst e) where
  MkQFirst q1 >>= k =
    MkQFirst $
      q1 >>= \case
        Left errs -> pure $ Left errs
        Right x -> unQFirst $ k x

-- | @since 0.1
instance MonadIO (QFirst e) where
  liftIO = MkQFirst . fmap Right . liftIO

-- | @since 0.1
instance Bifunctor QFirst where
  bimap f g (MkQFirst q) = MkQFirst $ fmap (bimap (fmap f) g) q

-- | Wraps a 'Q' computation in 'QFirst'.
--
-- @since 0.1
mkQFirst :: forall e a. Q (Either e a) -> QFirst e a
mkQFirst = MkQFirst . fmap (first mkExceptions)

-- | @firstSuccessQ q qs@ takes the first @qi@ in @q : qs@ that returns
-- 'Right', without executing any @qj@ for @j > i@. If there are no
-- 'Right'\'s, returns all errors.
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
-- @since 0.1
firstSuccessQ ::
  forall e a.
  Q (Either e a) ->
  [Q (Either e a)] ->
  Q (Either (Exceptions e) a)
firstSuccessQ q qs = unQFirst $ foldMap1 mkQFirst (q :| qs)

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
