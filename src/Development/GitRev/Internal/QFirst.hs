{-# LANGUAGE CPP #-}

-- | Provides 'QFirst' type.
--
-- @since 0.1
module Development.GitRev.Internal.QFirst
  ( -- * Combining Q actions lazily
    QFirst (..),
    mkQFirst,
    unQFirst,
    firstSuccessQ,
    Errors (..),
    mkErrors,
    unErrors,
  )
where

import Control.Exception (Exception (displayException), SomeException)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (Bifunctor (bimap, first, second))
#if MIN_VERSION_base(4, 18, 0)
import Data.Foldable1 (Foldable1 (foldMap1))
#endif
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Text.Lazy.Builder.Int qualified as TLBI
import Development.GitRev.Internal.Git.Common qualified as Common
import GHC.Records (HasField (getField))
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Syntax (Lift)

-- $setup
-- >>> import Development.GitRev.Typed (qToCode)
-- >>> import Development.GitRev.Internal.Git (GitError (..), gitDirtyQ, gitHashQ)
-- >>> import Development.GitRev.Internal.Environment (EnvError (..))
-- >>> import Language.Haskell.TH (Q, runIO, runQ)
-- >>> import System.Environment (setEnv)

-- | Collects multiple errors. Intended for displaying multiple
-- exceptions via 'displayException'.
--
-- @since 0.1
newtype Errors e = MkErrors (NonEmpty e)
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Foldable,
      -- | @since 0.1
      Functor,
      -- | @since 0.1
      Lift,
      -- | @since 0.1
      Show,
      -- | @since 0.1
      Traversable
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
instance HasField "unErrors" (Errors e) (NonEmpty e) where
  getField = unErrors

-- | @since 0.1
instance (Exception e) => Exception (Errors e) where
  displayException (MkErrors errs) =
    mconcat
      [ "Exception(s):",
        renderErrs errs
      ]
    where
      renderErrs =
        T.unpack
          . TL.toStrict
          . TLB.toLazyText
          . foldMap renderErr
          . NE.zip @Int (1 :| [2 ..])

      renderErr (idx, e) =
        (\b -> "\n" <> TLBI.decimal idx <> ". " <> b)
          . TLB.fromText
          . T.strip
          . T.pack
          . displayException
          $ e

-- | Unwraps 'Errors'.
--
-- @since 0.1
unErrors :: forall e. Errors e -> NonEmpty e
unErrors (MkErrors e) = e

-- | Wraps a type in 'Errors'.
--
-- @since 0.1
mkErrors :: forall e. NonEmpty e -> Errors e
mkErrors = MkErrors

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
-- If both actions fail, then both errors will be returned via 'Errors'.
--
-- === Warning: exceptions
--
-- In order for 'QFirst' to work as expected, the underlying 'Q' action
-- should /not/ throw exceptions. Uncaught exceptions will not be caught
-- by 'QFirst', hence the intended "try multiple 'Q'-actions until we have a
-- success" pattern will not work.
--
-- @since 0.1
newtype QFirst e a = MkQFirst (Q (Either (Errors e) a))
  deriving stock
    ( -- | @since 0.1
      Functor
    )

-- | @since 0.1
instance HasField "unQFirst" (QFirst e a) (Q (Either (Errors e) a)) where
  getField = unQFirst

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

-- | Catches synchronous exceptions.
--
-- @since 0.1
instance (e ~ SomeException) => MonadIO (QFirst e) where
  liftIO = MkQFirst . liftIO . fmap (first mapError) . Common.trySync
    where
      mapError = MkErrors . (:| [])

-- | @since 0.1
instance Bifunctor QFirst where
  bimap f g (MkQFirst q) = MkQFirst $ fmap (bimap (fmap f) g) q

-- | Unwraps 'QFirst'.
--
-- @since 0.1
unQFirst :: forall e a. QFirst e a -> Q (Either (Errors e) a)
unQFirst (MkQFirst q) = q

-- | Wraps a 'Q' computation in 'QFirst'.
--
-- @since 0.1
mkQFirst :: forall e a. Q (Either e a) -> QFirst e a
mkQFirst = MkQFirst . fmap (first (mkErrors . NE.singleton))

-- | @firstSuccessQ qs@ takes the first @qi@ in @qs@ that returns
-- 'Right', without executing any @qj@ for @j > i@. If there are no
-- 'Right's, returns all errors.
--
-- ==== __Examples__
--
-- >>> :{
--    $$( qToCode $
--          firstSuccessQ $
--            (pure $ Left $ MkGitError "not found")
--              :| [ gitHashQ,
--                   error "oh no"
--                 ]
--      )
-- :}
-- Right ...
--
-- @since 0.1
firstSuccessQ ::
  forall e a.
  NonEmpty (Q (Either e a)) -> Q (Either (Errors e) a)
firstSuccessQ = unQFirst . foldMap1 mkQFirst

#if !MIN_VERSION_base(4, 18, 0)
-- Copied from base. Technically not the same as the import above since
-- that one works for all Foldable1, not just NonEmpty, but we only use it
-- here for NonEmpty, so whatever.
foldMap1 :: forall a m. (Semigroup m) => (a -> m) -> NonEmpty a -> m
foldMap1 f (x :| xs) = go (f x) xs
  where
    go y [] = y
    go y (z : zs) = y <> go (f z) zs
#endif
