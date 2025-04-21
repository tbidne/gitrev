{-# LANGUAGE QuasiQuotes #-}

module Utils
  ( -- * TH
    E (..),

    -- * String
    qSemigroup,
    qFirstSemigroup,
    qFirstSuccess,
    qFirstSuccess2,
    qFirstSuccessAllLefts,

    -- * OsString
    qOsSemigroup,
    qOsFirstSemigroup,
    qOsFirstSuccess,
    qOsFirstSuccess2,
    qOsFirstSuccessAllLefts,

    -- * Assertions
    assertBoolean,
    assertEither,
    assertJust,
    assertNonEmpty,
  )
where

import Control.Exception (Exception (displayException))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.String (IsString)
import Development.GitRev.Typed (Errors, QFirst (unQFirst))
import Development.GitRev.Typed qualified as GRT
import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Syntax (Lift)
import System.OsString (OsString, osstr)
import Test.Tasty.HUnit (assertFailure)

type Counter = (Int, Int, Int)

type QResult = Either E String

type QResultOs = Either E OsString

qSemigroup :: Q Counter
qSemigroup = do
  ref <- runIO $ newIORef (0, 0, 0)
  _ <- q1 ref <> q2 ref <> q3 ref
  runIO $ readIORef ref

qOsSemigroup :: Q Counter
qOsSemigroup = do
  ref <- runIO $ newIORef (0, 0, 0)
  _ <- qOs1 ref <> qOs2 ref <> qOs3 ref
  runIO $ readIORef ref

qFirstSemigroup :: Q Counter
qFirstSemigroup = do
  ref <- runIO $ newIORef (0, 0, 0)
  _ <-
    unQFirst $
      GRT.mkQFirst (q1 ref) <> GRT.mkQFirst (q2 ref) <> GRT.mkQFirst (q3 ref)
  runIO $ readIORef ref

qOsFirstSemigroup :: Q Counter
qOsFirstSemigroup = do
  ref <- runIO $ newIORef (0, 0, 0)
  _ <-
    unQFirst $
      GRT.mkQFirst (qOs1 ref) <> GRT.mkQFirst (qOs2 ref) <> GRT.mkQFirst (qOs3 ref)
  runIO $ readIORef ref

qFirstSuccess :: Q Counter
qFirstSuccess = do
  ref <- runIO $ newIORef (0, 0, 0)
  _ <- GRT.firstSuccessQ (q1 ref) [q2 ref, q3 ref]
  runIO $ readIORef ref

qOsFirstSuccess :: Q Counter
qOsFirstSuccess = do
  ref <- runIO $ newIORef (0, 0, 0)
  _ <- GRT.firstSuccessQ (qOs1 ref) [qOs2 ref, qOs3 ref]
  runIO $ readIORef ref

qFirstSuccess2 :: Q Counter
qFirstSuccess2 = do
  ref <- runIO $ newIORef (0, 0, 0)
  _ <- GRT.firstSuccessQ (qFail1 ref) [q2 ref, q3 ref]
  runIO $ readIORef ref

qOsFirstSuccess2 :: Q Counter
qOsFirstSuccess2 = do
  ref <- runIO $ newIORef (0, 0, 0)
  _ <- GRT.firstSuccessQ (qFail1 ref) [qOs2 ref, qOs3 ref]
  runIO $ readIORef ref

qFirstSuccessAllLefts :: Q (Counter, Either (Errors E) String)
qFirstSuccessAllLefts = do
  ref <- runIO $ newIORef (0, 0, 0)
  result <- GRT.firstSuccessQ (qFail1 ref) [qFail2 ref, qFail3 ref]
  (,result) <$> runIO (readIORef ref)

qOsFirstSuccessAllLefts :: Q (Counter, Either (Errors E) OsString)
qOsFirstSuccessAllLefts = do
  ref <- runIO $ newIORef (0, 0, 0)
  result <- GRT.firstSuccessQ (qFail1 ref) [qFail2 ref, qFail3 ref]
  (,result) <$> runIO (readIORef ref)

newtype E = MkE String
  deriving stock (Eq, Lift, Show)
  deriving newtype (IsString)

instance Exception E where
  displayException (MkE s) = s

q1 :: IORef Counter -> Q QResult
q1 ref = do
  runIO $ modifyIORef' ref inc1
  pure $ Right "q1"

q2 :: IORef Counter -> Q QResult
q2 ref = do
  runIO $ modifyIORef' ref inc2
  pure $ Right "q2"

q3 :: IORef Counter -> Q QResult
q3 ref = do
  runIO $ modifyIORef' ref inc3
  pure $ Right "q3"

qOs1 :: IORef Counter -> Q QResultOs
qOs1 ref = do
  runIO $ modifyIORef' ref inc1
  pure $ Right [osstr|q1|]

qOs2 :: IORef Counter -> Q QResultOs
qOs2 ref = do
  runIO $ modifyIORef' ref inc2
  pure $ Right [osstr|q2|]

qOs3 :: IORef Counter -> Q QResultOs
qOs3 ref = do
  runIO $ modifyIORef' ref inc3
  pure $ Right [osstr|q3|]

qFail1 :: IORef Counter -> Q (Either E a)
qFail1 ref = do
  runIO $ modifyIORef' ref inc1
  pure $ Left "qFail1"

qFail2 :: IORef Counter -> Q (Either E a)
qFail2 ref = do
  runIO $ modifyIORef' ref inc2
  pure $ Left "qFail2"

qFail3 :: IORef Counter -> Q (Either E a)
qFail3 ref = do
  runIO $ modifyIORef' ref inc3
  pure $ Left "qFail3"

inc1 :: Counter -> Counter
inc1 (x, y, z) = (x + 1, y, z)

inc2 :: Counter -> Counter
inc2 (x, y, z) = (x, y + 1, z)

inc3 :: Counter -> Counter
inc3 (x, y, z) = (x, y, z + 1)

assertNonEmpty :: (Eq m, Monoid m) => m -> IO ()
assertNonEmpty s
  | s == mempty = assertFailure "Received empty"
  | otherwise = pure ()

assertJust :: Maybe p -> IO ()
assertJust Nothing = assertFailure "Received nothing"
assertJust _ = pure ()

assertBoolean :: Bool -> IO ()
assertBoolean True = pure ()
assertBoolean False = pure ()

assertEither :: Either e a -> IO ()
assertEither (Right _) = pure ()
assertEither (Left _) = pure ()
