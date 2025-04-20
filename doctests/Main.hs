{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (main) where

import Development.GitRev qualified
import Development.GitRev.Internal.Utils qualified
import System.Environment (getArgs)
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import Test.DocTest qualified as DT

-- NOTE: The doctest-parallel docs say we need to depend on the library, even
-- though it trips an "unused package" warning.

main :: IO ()
main =
  guardOrElse'
    "RUN_DOCTEST"
    ExpectEnvSet
    run
    (putStrLn "*** Doctests disabled. Run with env var RUN_DOCTEST=1 to enable ***")
  where
    run = do
      args <- getArgs
      DT.mainFromCabal "gitrev-internal" args
      DT.mainFromCabal "gitrev" args
