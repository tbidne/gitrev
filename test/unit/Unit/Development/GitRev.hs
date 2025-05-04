{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Unit.Development.GitRev (tests) where

import Data.Text qualified as T
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
  (Utils.assertDefaultText . T.pack) $GR.gitBranch

testGitCommitCount :: TestTree
testGitCommitCount = testCase "gitCommitCount" $ do
  (Utils.assertPositiveInt . T.pack) $GR.gitCommitCount

testGitCommitDate :: TestTree
testGitCommitDate = testCase "gitCommitDate" $ do
  (Utils.assertDefaultText . T.pack) $GR.gitCommitDate

testGitDescribe :: TestTree
testGitDescribe = testCase "gitDescribe" $ do
  (Utils.assertDefaultText . T.pack) $GR.gitDescribe

testGitDirty :: TestTree
testGitDirty = testCase "gitDirty" $ do
  Utils.assertBoolean $GR.gitDirty

testGitDirtyTracked :: TestTree
testGitDirtyTracked = testCase "gitDirtyTracked" $ do
  Utils.assertBoolean $GR.gitDirtyTracked

testGitHash :: TestTree
testGitHash = testCase "gitHash" $ do
  (Utils.assertHash . T.pack) $GR.gitHash

testGitShortHash :: TestTree
testGitShortHash = testCase "gitShortHash" $ do
  (Utils.assertShortHash . T.pack) $GR.gitShortHash
