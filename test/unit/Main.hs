module Main (main) where

import System.Environment.Guard (ExpectEnv (ExpectEnvSet))
import System.Environment.Guard qualified as Guard
import Test.Tasty (defaultMain, testGroup)
import Unit.Development.GitRev qualified
import Unit.Development.GitRev.Typed qualified
import Unit.Development.GitRev.Typed.OsString qualified

main :: IO ()
main = do
  Guard.guardOrElse'
    "RUN_UNIT"
    ExpectEnvSet
    runTests
    (putStrLn "*** Tests disabled. Enable with RUN_UNIT=1 ***")
  where
    runTests =
      defaultMain $
        testGroup
          "Unit"
          [ Unit.Development.GitRev.tests,
            Unit.Development.GitRev.Typed.tests,
            Unit.Development.GitRev.Typed.OsString.tests
          ]
