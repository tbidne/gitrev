{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Development.GitRev qualified as GR
import Development.GitRev.Typed qualified as GRT
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@=?))
import Utils qualified

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
  assertNonEmpty $GR.gitBranch

testGitCommitCount :: TestTree
testGitCommitCount = testCase "gitCommitCount" $ do
  assertNonEmpty $GR.gitCommitCount

testGitCommitDate :: TestTree
testGitCommitDate = testCase "gitCommitDate" $ do
  assertNonEmpty $GR.gitCommitDate

testGitDescribe :: TestTree
testGitDescribe = testCase "gitDescribe" $ do
  assertNonEmpty $GR.gitDescribe

testGitDirty :: TestTree
testGitDirty = testCase "gitDirty" $ do
  assertBoolean $GR.gitDirty

testGitDirtyTracked :: TestTree
testGitDirtyTracked = testCase "gitDirtyTracked" $ do
  assertBoolean $GR.gitDirtyTracked

testGitHash :: TestTree
testGitHash = testCase "gitHash" $ do
  assertNonEmpty $GR.gitHash

testGitShortHash :: TestTree
testGitShortHash = testCase "gitShortHash" $ do
  assertNonEmpty $GR.gitShortHash

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
      testHashAndEnvVal,
      testHashAndEnvDir,
      semigroupTests
    ]

testGitBranchTyped :: TestTree
testGitBranchTyped = testCase "gitBranch" $ do
  assertNonEmpty $$GRT.gitBranch

testGitCommitCountTyped :: TestTree
testGitCommitCountTyped = testCase "gitCommitCount" $ do
  assertNonEmpty $$GRT.gitCommitCount

testGitCommitDateTyped :: TestTree
testGitCommitDateTyped = testCase "gitCommitDate" $ do
  assertNonEmpty $$GRT.gitCommitDate

testGitDescribeTyped :: TestTree
testGitDescribeTyped = testCase "gitDescribe" $ do
  assertNonEmpty $$GRT.gitDescribe

testGitDirtyTyped :: TestTree
testGitDirtyTyped = testCase "gitDirty" $ do
  assertBoolean $$GRT.gitDirty

testGitDirtyTrackedTyped :: TestTree
testGitDirtyTrackedTyped = testCase "gitDirtyTracked" $ do
  assertBoolean $$GRT.gitDirtyTracked

testGitHashTyped :: TestTree
testGitHashTyped = testCase "gitHash" $ do
  assertNonEmpty $$GRT.gitHash

testGitShortHashTyped :: TestTree
testGitShortHashTyped = testCase "gitShortHash" $ do
  assertNonEmpty $$GRT.gitShortHash

testLiftError :: TestTree
testLiftError = testCase "Lifts with default string" $ do
  assertNonEmpty
    $$(GRT.qToCode $ GRT.liftDefString GRT.gitHashQ)

testHashAndEnvVal :: TestTree
testHashAndEnvVal = testCase "Composes hash and env val lookup" $ do
  assertGitResult
    $$( GRT.qToCode $
          GRT.liftGitError GRT.gitHashQ
            <> GRT.envValQ "var"
      )

testHashAndEnvDir :: TestTree
testHashAndEnvDir = testCase "Composes hash and env dir lookup" $ do
  assertGitResult
    $$( GRT.qToCode $
          GRT.liftGitError GRT.gitHashQ
            <> GRT.runGitInEnvDirQ "var" GRT.gitHashQ
      )

semigroupTests :: TestTree
semigroupTests =
  testGroup
    "Semigroup"
    [ testSemigroupQNotLazy,
      testSemigroupQFirstLazy,
      testSemigroupQFirstRightLazy,
      testSemigroupQFirstRightLazy2,
      testSemigroupQFirstRightLastLeft
    ]

testSemigroupQNotLazy :: TestTree
testSemigroupQNotLazy = testCase "Q Semigroup is _not_ lazy in the rhs" $ do
  let (num1, num2, num3) = $$(GRT.qToCode Utils.qSemigroup)
  1 @=? num1
  1 @=? num2
  1 @=? num3

testSemigroupQFirstLazy :: TestTree
testSemigroupQFirstLazy = testCase "QFirst Semigroup is lazy in the rhs" $ do
  let (num1, num2, num3) = $$(GRT.qToCode Utils.qFirstSemigroup)
  1 @=? num1
  0 @=? num2
  0 @=? num3

testSemigroupQFirstRightLazy :: TestTree
testSemigroupQFirstRightLazy = testCase "Utils.firstRight is lazy" $ do
  let (num1, num2, num3) = $$(GRT.qToCode Utils.qFirstRight)
  1 @=? num1
  0 @=? num2
  0 @=? num3

testSemigroupQFirstRightLazy2 :: TestTree
testSemigroupQFirstRightLazy2 = testCase "Utils.firstRight is lazy 2" $ do
  let (num1, num2, num3) = $$(GRT.qToCode Utils.qFirstRight2)
  1 @=? num1
  1 @=? num2
  0 @=? num3

testSemigroupQFirstRightLastLeft :: TestTree
testSemigroupQFirstRightLastLeft = testCase desc $ do
  let ((num1, num2, num3), result) = $$(GRT.qToCode Utils.qFirstRightLastLeft)
  1 @=? num1
  1 @=? num2
  1 @=? num3

  Left "qFail3" @=? result
  where
    desc = "Utils.firstRight takes the last when all Lefts"

assertNonEmpty :: String -> IO ()
assertNonEmpty "" = assertFailure "Received empty"
assertNonEmpty _ = pure ()

assertJust :: Maybe String -> IO ()
assertJust Nothing = assertFailure "Received nothing"
assertJust _ = pure ()

assertBoolean :: Bool -> IO ()
assertBoolean True = pure ()
assertBoolean False = pure ()

assertGitResult :: Either e a -> IO ()
assertGitResult (Right _) = pure ()
assertGitResult (Left _) = pure ()
