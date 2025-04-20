{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Unit.Development.GitRev (tests) where

import Development.GitRev qualified as GR
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Utils qualified

tests :: TestTree
tests =
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
  Utils.assertNonEmpty @String $GR.gitBranch

testGitCommitCount :: TestTree
testGitCommitCount = testCase "gitCommitCount" $ do
  Utils.assertNonEmpty @String $GR.gitCommitCount

testGitCommitDate :: TestTree
testGitCommitDate = testCase "gitCommitDate" $ do
  Utils.assertNonEmpty @String $GR.gitCommitDate

testGitDescribe :: TestTree
testGitDescribe = testCase "gitDescribe" $ do
  Utils.assertNonEmpty @String $GR.gitDescribe

testGitDirty :: TestTree
testGitDirty = testCase "gitDirty" $ do
  Utils.assertBoolean $GR.gitDirty

testGitDirtyTracked :: TestTree
testGitDirtyTracked = testCase "gitDirtyTracked" $ do
  Utils.assertBoolean $GR.gitDirtyTracked

testGitHash :: TestTree
testGitHash = testCase "gitHash" $ do
  Utils.assertNonEmpty @String $GR.gitHash

testGitShortHash :: TestTree
testGitShortHash = testCase "gitShortHash" $ do
  Utils.assertNonEmpty @String $GR.gitShortHash
