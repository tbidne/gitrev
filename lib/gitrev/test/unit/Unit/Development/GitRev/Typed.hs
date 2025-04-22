{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Unit.Development.GitRev.Typed (tests) where

import Control.Exception (Exception (displayException))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Development.GitRev.Typed (mkErrors)
import Development.GitRev.Typed qualified as GRT
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@=?))
import Utils qualified

tests :: TestTree
tests =
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
  Utils.assertNonEmpty $$GRT.gitBranch

testGitCommitCountTyped :: TestTree
testGitCommitCountTyped = testCase "gitCommitCount" $ do
  Utils.assertNonEmpty $$GRT.gitCommitCount

testGitCommitDateTyped :: TestTree
testGitCommitDateTyped = testCase "gitCommitDate" $ do
  Utils.assertNonEmpty $$GRT.gitCommitDate

testGitDescribeTyped :: TestTree
testGitDescribeTyped = testCase "gitDescribe" $ do
  Utils.assertNonEmpty $$GRT.gitDescribe

testGitDirtyTyped :: TestTree
testGitDirtyTyped = testCase "gitDirty" $ do
  Utils.assertBoolean $$GRT.gitDirty

testGitDirtyTrackedTyped :: TestTree
testGitDirtyTrackedTyped = testCase "gitDirtyTracked" $ do
  Utils.assertBoolean $$GRT.gitDirtyTracked

testGitHashTyped :: TestTree
testGitHashTyped = testCase "gitHash" $ do
  Utils.assertNonEmpty $$GRT.gitHash

testGitShortHashTyped :: TestTree
testGitShortHashTyped = testCase "gitShortHash" $ do
  Utils.assertNonEmpty $$GRT.gitShortHash

testLiftError :: TestTree
testLiftError = testCase "Lifts with default string" $ do
  Utils.assertNonEmpty
    $$(GRT.qToCode $ GRT.projectStringUnknown GRT.gitHashQ)

testHashAndEnvVal :: TestTree
testHashAndEnvVal = testCase "Composes hash and env val lookup" $ do
  Utils.assertEither
    $$( GRT.qToCode $
          GRT.embedGitError GRT.gitHashQ
            <> GRT.embedEnvLookupError (GRT.envValQ "var")
      )

testHashAndEnvDir :: TestTree
testHashAndEnvDir = testCase "Composes hash and env dir lookup" $ do
  Utils.assertEither
    $$( GRT.qToCode $
          GRT.embedGitError GRT.gitHashQ
            <> GRT.runGitInEnvDirQ "var" GRT.gitHashQ
      )

semigroupTests :: TestTree
semigroupTests =
  testGroup
    "Semigroup"
    [ testSemigroupQNotLazy,
      testSemigroupQFirstLazy,
      testSemigroupQFirstSuccessLazy,
      testSemigroupQFirstSuccessLazy2,
      testSemigroupQFirstSuccessAllLefts
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

testSemigroupQFirstSuccessLazy :: TestTree
testSemigroupQFirstSuccessLazy = testCase "Utils.firstSuccessQ is lazy" $ do
  let (num1, num2, num3) = $$(GRT.qToCode Utils.qFirstSuccess)
  1 @=? num1
  0 @=? num2
  0 @=? num3

testSemigroupQFirstSuccessLazy2 :: TestTree
testSemigroupQFirstSuccessLazy2 = testCase "Utils.firstSuccessQ is lazy 2" $ do
  let (num1, num2, num3) = $$(GRT.qToCode Utils.qFirstSuccess2)
  1 @=? num1
  1 @=? num2
  0 @=? num3

testSemigroupQFirstSuccessAllLefts :: TestTree
testSemigroupQFirstSuccessAllLefts = testCase desc $ do
  let ((num1, num2, num3), eResult) = $$(GRT.qToCode Utils.qFirstSuccessAllLefts)
  1 @=? num1
  1 @=? num2
  1 @=? num3

  case eResult of
    Right x -> assertFailure $ "Received Right: " ++ x
    Left result -> do
      expected @=? result
      expectedStr @=? (displayException result)
  where
    desc = "Utils.firstSuccessQ takes all Lefts"

    expected = mkErrors @Utils.E ("qFail1" :| ["qFail2", "qFail3"])

    expectedStr =
      mconcat
        [ "Exception(s):",
          "\n1. qFail1",
          "\n2. qFail2",
          "\n3. qFail3"
        ]
