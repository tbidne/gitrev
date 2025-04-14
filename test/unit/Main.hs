{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Exception (Exception (displayException))
import Development.GitRev qualified as GitRev
import Development.GitRev.Typed (GitError)
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
      testGitCommitCount,
      testGitCommitDate,
      testGitDescribe,
      testGitDirty,
      testGitDirtyTracked,
      testGitHash,
      testGitShortHash
    ]

testGitBranch :: TestTree
testGitBranch = testCase "gitBranch" $ do
  assertNonEmpty $GitRev.gitBranch

testGitCommitCount :: TestTree
testGitCommitCount = testCase "gitCommitCount" $ do
  assertNonEmpty $GitRev.gitCommitCount

testGitCommitDate :: TestTree
testGitCommitDate = testCase "gitCommitDate" $ do
  assertNonEmpty $GitRev.gitCommitDate

testGitDescribe :: TestTree
testGitDescribe = testCase "gitDescribe" $ do
  assertNonEmpty $GitRev.gitDescribe

testGitDirty :: TestTree
testGitDirty = testCase "gitDirty" $ do
  assertBoolean $GitRev.gitDirty

testGitDirtyTracked :: TestTree
testGitDirtyTracked = testCase "gitDirtyTracked" $ do
  assertBoolean $GitRev.gitDirtyTracked

testGitHash :: TestTree
testGitHash = testCase "gitHash" $ do
  assertNonEmpty $GitRev.gitHash

testGitShortHash :: TestTree
testGitShortHash = testCase "gitShortHash" $ do
  assertNonEmpty $GitRev.gitShortHash

gitRevTypedTests :: TestTree
gitRevTypedTests =
  testGroup
    "Development.GitRev.Typed"
    [ testGitBranchTyped,
      testGitCommitCountTyped,
      testGitCommitDateTyped,
      testGitDescribeTyped,
      testGitDirtyTyped,
      testGitDirtyTrackedTyped,
      testGitHashTyped,
      testGitShortHashTyped,
      testLiftError,
      testEnvFallback,
      testEnvFallbackLiftError
    ]

testGitBranchTyped :: TestTree
testGitBranchTyped = testCase "gitBranch" $ do
  assertNonEmpty $$GitRev.Typed.gitBranch

testGitCommitCountTyped :: TestTree
testGitCommitCountTyped = testCase "gitCommitCount" $ do
  assertNonEmpty $$GitRev.Typed.gitCommitCount

testGitCommitDateTyped :: TestTree
testGitCommitDateTyped = testCase "gitCommitDate" $ do
  assertNonEmpty $$GitRev.Typed.gitCommitDate

testGitDescribeTyped :: TestTree
testGitDescribeTyped = testCase "gitDescribe" $ do
  assertNonEmpty $$GitRev.Typed.gitDescribe

testGitDirtyTyped :: TestTree
testGitDirtyTyped = testCase "gitDirty" $ do
  assertBoolean $$GitRev.Typed.gitDirty

testGitDirtyTrackedTyped :: TestTree
testGitDirtyTrackedTyped = testCase "gitDirtyTracked" $ do
  assertBoolean $$GitRev.Typed.gitDirtyTracked

testGitHashTyped :: TestTree
testGitHashTyped = testCase "gitHash" $ do
  assertNonEmpty $$GitRev.Typed.gitHash

testGitShortHashTyped :: TestTree
testGitShortHashTyped = testCase "gitShortHash" $ do
  assertNonEmpty $$GitRev.Typed.gitShortHash

testLiftError :: TestTree
testLiftError = testCase "Lifts error" $ do
  assertNonEmpty
    $$(GitRev.Typed.qToCode $ GitRev.Typed.liftError GitRev.Typed.gitHashQ)

testEnvFallback :: TestTree
testEnvFallback = testCase "Use environment var fallback" $ do
  assertGitSuccess
    $$( GitRev.Typed.qToCode $
          GitRev.Typed.envFallback "var" GitRev.Typed.gitHashQ
      )

testEnvFallbackLiftError :: TestTree
testEnvFallbackLiftError = testCase "Combines envFallback and liftError" $ do
  assertNonEmpty
    $$( GitRev.Typed.qToCode $
          GitRev.Typed.liftError $
            GitRev.Typed.envFallback "var" GitRev.Typed.gitHashQ
      )

assertNonEmpty :: String -> IO ()
assertNonEmpty "" = assertFailure "Received empty"
assertNonEmpty _ = pure ()

assertJust :: Maybe String -> IO ()
assertJust Nothing = assertFailure "Received nothing"
assertJust _ = pure ()

assertBoolean :: Bool -> IO ()
assertBoolean True = pure ()
assertBoolean False = pure ()

assertGitSuccess :: Either GitError a -> IO ()
assertGitSuccess (Right _) = pure ()
assertGitSuccess (Left e) =
  assertFailure $ "Received error: " ++ displayException e
