module Utils
  ( qSemigroup,
    qFirstSemigroup,
    qFirstRight,
    qFirstRight2,
  )
where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Development.GitRev.Utils (QFirst (MkQFirst, unQFirst))
import Development.GitRev.Utils qualified as GRU
import Language.Haskell.TH (Q, runIO)

type Counter = (Int, Int, Int)

qSemigroup :: Q Counter
qSemigroup = do
  ref <- runIO $ newIORef (0, 0, 0)
  _ <- q1 ref <> q2 ref <> q3 ref
  runIO $ readIORef ref

qFirstSemigroup :: Q Counter
qFirstSemigroup = do
  ref <- runIO $ newIORef (0, 0, 0)
  _ <- unQFirst $ MkQFirst (q1 ref) <> MkQFirst (q2 ref) <> MkQFirst (q3 ref)
  runIO $ readIORef ref

qFirstRight :: Q Counter
qFirstRight = do
  ref <- runIO $ newIORef (0, 0, 0)
  _ <- GRU.firstRight (q1 ref) [q2 ref, q3 ref]
  runIO $ readIORef ref

qFirstRight2 :: Q Counter
qFirstRight2 = do
  ref <- runIO $ newIORef (0, 0, 0)
  _ <- GRU.firstRight (qFail ref) [q2 ref, q3 ref]
  runIO $ readIORef ref

qFail :: IORef Counter -> Q (Either () String)
qFail ref = do
  runIO $ modifyIORef' ref inc1
  pure $ Left ()

q1 :: IORef Counter -> Q (Either () String)
q1 ref = do
  runIO $ modifyIORef' ref inc1
  pure $ Right "q1"

q2 :: IORef Counter -> Q (Either () String)
q2 ref = do
  runIO $ modifyIORef' ref inc2
  pure $ Right "q2"

q3 :: IORef Counter -> Q (Either () String)
q3 ref = do
  runIO $ modifyIORef' ref inc3
  pure $ Right "q3"

inc1 :: Counter -> Counter
inc1 (x, y, z) = (x + 1, y, z)

inc2 :: Counter -> Counter
inc2 (x, y, z) = (x, y + 1, z)

inc3 :: Counter -> Counter
inc3 (x, y, z) = (x, y, z + 1)
