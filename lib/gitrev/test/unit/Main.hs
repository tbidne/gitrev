module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Unit.Development.GitRev qualified
import Unit.Development.GitRev.Typed qualified
import Unit.Development.GitRev.Typed.OsString qualified

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Unit"
      [ Unit.Development.GitRev.tests,
        Unit.Development.GitRev.Typed.tests,
        Unit.Development.GitRev.Typed.OsString.tests
      ]
