module Utils
  ( E (..),
    qSemigroup,
    qFirstSemigroup,
    qFirstSuccess,
    qFirstSuccess2,
    qFirstSuccessAllLefts,
  )
where

import Control.Exception (Exception (displayException))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.String (IsString)
import Development.GitRev.Typed (Exceptions, QFirst (unQFirst))
import Development.GitRev.Typed qualified as GRT
import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Syntax (Lift)

type Counter = (Int, Int, Int)

type QResult = Either E String

qSemigroup :: Q Counter
qSemigroup = do
  ref <- runIO $ newIORef (0, 0, 0)
  _ <- q1 ref <> q2 ref <> q3 ref
  runIO $ readIORef ref

qFirstSemigroup :: Q Counter
qFirstSemigroup = do
  ref <- runIO $ newIORef (0, 0, 0)
  _ <-
    unQFirst $
      GRT.mkQFirst (q1 ref) <> GRT.mkQFirst (q2 ref) <> GRT.mkQFirst (q3 ref)
  runIO $ readIORef ref

qFirstSuccess :: Q Counter
qFirstSuccess = do
  ref <- runIO $ newIORef (0, 0, 0)
  _ <- GRT.firstSuccessQ (q1 ref) [q2 ref, q3 ref]
  runIO $ readIORef ref

qFirstSuccess2 :: Q Counter
qFirstSuccess2 = do
  ref <- runIO $ newIORef (0, 0, 0)
  _ <- GRT.firstSuccessQ (qFail1 ref) [q2 ref, q3 ref]
  runIO $ readIORef ref

qFirstSuccessAllLefts :: Q (Counter, Either (Exceptions E) String)
qFirstSuccessAllLefts = do
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

qFail1 :: IORef Counter -> Q QResult
qFail1 ref = do
  runIO $ modifyIORef' ref inc1
  pure $ Left "qFail1"

qFail2 :: IORef Counter -> Q QResult
qFail2 ref = do
  runIO $ modifyIORef' ref inc2
  pure $ Left "qFail2"

qFail3 :: IORef Counter -> Q QResult
qFail3 ref = do
  runIO $ modifyIORef' ref inc3
  pure $ Left "qFail3"

inc1 :: Counter -> Counter
inc1 (x, y, z) = (x + 1, y, z)

inc2 :: Counter -> Counter
inc2 (x, y, z) = (x, y + 1, z)

inc3 :: Counter -> Counter
inc3 (x, y, z) = (x, y, z + 1)
