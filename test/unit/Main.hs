{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Development.GitRev qualified as GitRev
import Development.GitRev.Typed qualified as GitRev.Typed
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Unit"
      [ gitRevTests,
        gitRevTypedTests
      ]

gitRevTests :: TestTree
gitRevTests =
  testGroup
    "Development.GitRev"
    [ testGitBranch,
      testGitBranchMay,
      testGitBranchEnv,
      testGitBranchMayEnv,
      testGitCommitCount,
      testGitCommitCountMay,
      testGitCommitCountEnv,
      testGitCommitCountMayEnv,
      testGitCommitDate,
      testGitCommitDateMay,
      testGitCommitDateEnv,
      testGitCommitDateMayEnv,
      testGitDescribe,
      testGitDescribeMay,
      testGitDescribeEnv,
      testGitDescribeMayEnv,
      testGitDirty,
      testGitDirtyTracked,
      testGitHash,
      testGitHashMay,
      testGitHashEnv,
      testGitHashMayEnv,
      testGitShortHash,
      testGitShortHashMay,
      testGitShortHashEnv,
      testGitShortHashMayEnv
    ]

testGitBranch :: TestTree
testGitBranch = testCase "gitBranch" $ do
  assertNonEmpty $GitRev.gitBranch

testGitBranchMay :: TestTree
testGitBranchMay = testCase "gitBranchMay" $ do
  assertJust $GitRev.gitBranchMay

testGitBranchEnv :: TestTree
testGitBranchEnv = testCase "gitBranchEnv" $ do
  assertNonEmpty $(GitRev.gitBranchEnv "var")

testGitBranchMayEnv :: TestTree
testGitBranchMayEnv = testCase "gitBranchMayEnv" $ do
  assertJust $(GitRev.gitBranchMayEnv "var")

testGitCommitCount :: TestTree
testGitCommitCount = testCase "gitCommitCount" $ do
  assertNonEmpty $GitRev.gitCommitCount

testGitCommitCountMay :: TestTree
testGitCommitCountMay = testCase "gitCommitCountMay" $ do
  assertJust $GitRev.gitCommitCountMay

testGitCommitCountEnv :: TestTree
testGitCommitCountEnv = testCase "gitCommitCountEnv" $ do
  assertNonEmpty $(GitRev.gitCommitCountEnv "var")

testGitCommitCountMayEnv :: TestTree
testGitCommitCountMayEnv = testCase "gitCommitCountMayEnv" $ do
  assertJust $(GitRev.gitCommitCountMayEnv "var")

testGitCommitDate :: TestTree
testGitCommitDate = testCase "gitCommitDate" $ do
  assertNonEmpty $GitRev.gitCommitDate

testGitCommitDateMay :: TestTree
testGitCommitDateMay = testCase "gitCommitDateMay" $ do
  assertJust $GitRev.gitCommitDateMay

testGitCommitDateEnv :: TestTree
testGitCommitDateEnv = testCase "gitCommitDateEnv" $ do
  assertNonEmpty $(GitRev.gitCommitDateEnv "var")

testGitCommitDateMayEnv :: TestTree
testGitCommitDateMayEnv = testCase "gitCommitDateMayEnv" $ do
  assertJust $(GitRev.gitCommitDateMayEnv "var")

testGitDescribe :: TestTree
testGitDescribe = testCase "gitDescribe" $ do
  assertNonEmpty $GitRev.gitDescribe

testGitDescribeMay :: TestTree
testGitDescribeMay = testCase "gitDescribeMay" $ do
  assertJust $GitRev.gitDescribeMay

testGitDescribeEnv :: TestTree
testGitDescribeEnv = testCase "gitDescribeEnv" $ do
  assertNonEmpty $(GitRev.gitDescribeEnv "var")

testGitDescribeMayEnv :: TestTree
testGitDescribeMayEnv = testCase "gitDescribeMayEnv" $ do
  assertJust $(GitRev.gitDescribeMayEnv "var")

testGitDirty :: TestTree
testGitDirty = testCase "gitDirty" $ do
  assertBoolean $GitRev.gitDirty

testGitDirtyTracked :: TestTree
testGitDirtyTracked = testCase "gitDirtyTracked" $ do
  assertBoolean $GitRev.gitDirtyTracked

testGitHash :: TestTree
testGitHash = testCase "gitHash" $ do
  assertNonEmpty $GitRev.gitHash

testGitHashMay :: TestTree
testGitHashMay = testCase "gitHashMay" $ do
  assertJust $GitRev.gitHashMay

testGitHashEnv :: TestTree
testGitHashEnv = testCase "gitHashEnv" $ do
  assertNonEmpty $(GitRev.gitHashEnv "var")

testGitHashMayEnv :: TestTree
testGitHashMayEnv = testCase "gitHashMayEnv" $ do
  assertJust $(GitRev.gitHashMayEnv "var")

testGitShortHash :: TestTree
testGitShortHash = testCase "gitShortHash" $ do
  assertNonEmpty $GitRev.gitShortHash

testGitShortHashMay :: TestTree
testGitShortHashMay = testCase "gitShortHashMay" $ do
  assertJust $GitRev.gitShortHashMay

testGitShortHashEnv :: TestTree
testGitShortHashEnv = testCase "gitShortHashEnv" $ do
  assertNonEmpty $(GitRev.gitShortHashEnv "var")

testGitShortHashMayEnv :: TestTree
testGitShortHashMayEnv = testCase "gitShortHashMayEnv" $ do
  assertJust $(GitRev.gitShortHashMayEnv "var")

gitRevTypedTests :: TestTree
gitRevTypedTests =
  testGroup
    "Development.GitRev.Typed"
    [ testGitBranchTyped,
      testGitBranchMayTyped,
      testGitBranchEnvTyped,
      testGitBranchMayEnvTyped,
      testGitCommitCountTyped,
      testGitCommitCountMayTyped,
      testGitCommitCountEnvTyped,
      testGitCommitCountMayEnvTyped,
      testGitCommitDateTyped,
      testGitCommitDateMayTyped,
      testGitCommitDateEnvTyped,
      testGitCommitDateMayEnvTyped,
      testGitDescribeTyped,
      testGitDescribeMayTyped,
      testGitDescribeEnvTyped,
      testGitDescribeMayEnvTyped,
      testGitDirtyTyped,
      testGitDirtyTrackedTyped,
      testGitHashTyped,
      testGitHashMayTyped,
      testGitHashEnvTyped,
      testGitHashMayEnvTyped,
      testGitShortHashTyped,
      testGitShortHashMayTyped,
      testGitShortHashEnvTyped,
      testGitShortHashMayEnvTyped
    ]

testGitBranchTyped :: TestTree
testGitBranchTyped = testCase "gitBranch" $ do
  assertNonEmpty $$GitRev.Typed.gitBranch

testGitBranchMayTyped :: TestTree
testGitBranchMayTyped = testCase "gitBranchMay" $ do
  assertJust $$GitRev.Typed.gitBranchMay

testGitBranchEnvTyped :: TestTree
testGitBranchEnvTyped = testCase "gitBranchEnv" $ do
  assertNonEmpty $$(GitRev.Typed.gitBranchEnv "var")

testGitBranchMayEnvTyped :: TestTree
testGitBranchMayEnvTyped = testCase "gitBranchMayEnv" $ do
  assertJust $$(GitRev.Typed.gitBranchMayEnv "var")

testGitCommitCountTyped :: TestTree
testGitCommitCountTyped = testCase "gitCommitCount" $ do
  assertNonEmpty $$GitRev.Typed.gitCommitCount

testGitCommitCountMayTyped :: TestTree
testGitCommitCountMayTyped = testCase "gitCommitCountMay" $ do
  assertJust $$GitRev.Typed.gitCommitCountMay

testGitCommitCountEnvTyped :: TestTree
testGitCommitCountEnvTyped = testCase "gitCommitCountEnv" $ do
  assertNonEmpty $$(GitRev.Typed.gitCommitCountEnv "var")

testGitCommitCountMayEnvTyped :: TestTree
testGitCommitCountMayEnvTyped = testCase "gitCommitCountMayEnv" $ do
  assertJust $$(GitRev.Typed.gitCommitCountMayEnv "var")

testGitCommitDateTyped :: TestTree
testGitCommitDateTyped = testCase "gitCommitDate" $ do
  assertNonEmpty $$GitRev.Typed.gitCommitDate

testGitCommitDateMayTyped :: TestTree
testGitCommitDateMayTyped = testCase "gitCommitDateMay" $ do
  assertJust $$GitRev.Typed.gitCommitDateMay

testGitCommitDateEnvTyped :: TestTree
testGitCommitDateEnvTyped = testCase "gitCommitDateEnv" $ do
  assertNonEmpty $$(GitRev.Typed.gitCommitDateEnv "var")

testGitCommitDateMayEnvTyped :: TestTree
testGitCommitDateMayEnvTyped = testCase "gitCommitDateMayEnv" $ do
  assertJust $$(GitRev.Typed.gitCommitDateMayEnv "var")

testGitDescribeTyped :: TestTree
testGitDescribeTyped = testCase "gitDescribe" $ do
  assertNonEmpty $$GitRev.Typed.gitDescribe

testGitDescribeMayTyped :: TestTree
testGitDescribeMayTyped = testCase "gitDescribeMay" $ do
  assertJust $$GitRev.Typed.gitDescribeMay

testGitDescribeEnvTyped :: TestTree
testGitDescribeEnvTyped = testCase "gitDescribeEnv" $ do
  assertNonEmpty $$(GitRev.Typed.gitDescribeEnv "var")

testGitDescribeMayEnvTyped :: TestTree
testGitDescribeMayEnvTyped = testCase "gitDescribeMayEnv" $ do
  assertJust $$(GitRev.Typed.gitDescribeMayEnv "var")

testGitDirtyTyped :: TestTree
testGitDirtyTyped = testCase "gitDirty" $ do
  assertBoolean $$GitRev.Typed.gitDirty

testGitDirtyTrackedTyped :: TestTree
testGitDirtyTrackedTyped = testCase "gitDirtyTracked" $ do
  assertBoolean $$GitRev.Typed.gitDirtyTracked

testGitHashTyped :: TestTree
testGitHashTyped = testCase "gitHash" $ do
  assertNonEmpty $$GitRev.Typed.gitHash

testGitHashMayTyped :: TestTree
testGitHashMayTyped = testCase "gitHashMay" $ do
  assertJust $$GitRev.Typed.gitHashMay

testGitHashEnvTyped :: TestTree
testGitHashEnvTyped = testCase "gitHashEnv" $ do
  assertNonEmpty $$(GitRev.Typed.gitHashEnv "var")

testGitHashMayEnvTyped :: TestTree
testGitHashMayEnvTyped = testCase "gitHashMayEnv" $ do
  assertJust $$(GitRev.Typed.gitHashMayEnv "var")

testGitShortHashTyped :: TestTree
testGitShortHashTyped = testCase "gitShortHash" $ do
  assertNonEmpty $$GitRev.Typed.gitShortHash

testGitShortHashMayTyped :: TestTree
testGitShortHashMayTyped = testCase "gitShortHashMay" $ do
  assertJust $$GitRev.Typed.gitShortHashMay

testGitShortHashEnvTyped :: TestTree
testGitShortHashEnvTyped = testCase "gitShortHashEnv" $ do
  assertNonEmpty $$(GitRev.Typed.gitShortHashEnv "var")

testGitShortHashMayEnvTyped :: TestTree
testGitShortHashMayEnvTyped = testCase "gitShortHashMayEnv" $ do
  assertJust $$(GitRev.Typed.gitShortHashMayEnv "var")

assertNonEmpty :: String -> IO ()
assertNonEmpty "" = assertFailure "Received empty"
assertNonEmpty _ = pure ()

assertJust :: Maybe String -> IO ()
assertJust Nothing = assertFailure "Received nothing"
assertJust _ = pure ()

assertBoolean :: Bool -> IO ()
assertBoolean True = pure ()
assertBoolean False = pure ()
