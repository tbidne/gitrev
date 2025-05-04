{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Unit.Development.GitRev.Typed.OsString (tests) where

import Control.Exception (Exception (displayException))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Text qualified as T
import Development.GitRev.Typed.OsString (mkErrors)
import Development.GitRev.Typed.OsString qualified as GRT
import Language.Haskell.TH (runIO)
import System.Environment qualified as Env
import System.OsString (OsString, decodeUtf, osstr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@=?))
import Utils qualified

tests :: TestTree
tests =
  testGroup
    "Development.GitRev.Typed.OsString"
    [ gitTests,
      envTests,
      combinatorTests,
      semigroupTests
    ]

gitTests :: TestTree
gitTests =
  testGroup
    "Git"
    [ testGitBranch,
      testGitCommitCount,
      testGitCommitDate,
      testGitDescribe,
      testGitDiff,
      testGitDirty,
      testGitDirtyTracked,
      testGitHash,
      testGitShortHash,
      testGitTree
    ]

testGitBranch :: TestTree
testGitBranch = testCase "gitBranch" $ do
  (Utils.assertDefaultText . toText) $$GRT.gitBranch

testGitCommitCount :: TestTree
testGitCommitCount = testCase "gitCommitCount" $ do
  (Utils.assertPositiveInt . toText) $$GRT.gitCommitCount

testGitCommitDate :: TestTree
testGitCommitDate = testCase "gitCommitDate" $ do
  (Utils.assertDefaultText . toText) $$GRT.gitCommitDate

testGitDescribe :: TestTree
testGitDescribe = testCase "gitDescribe" $ do
  (Utils.assertDefaultText . toText) $$GRT.gitDescribe

testGitDiff :: TestTree
testGitDiff = testCase "gitDiff" $ do
  (Utils.assertText . toText) $$GRT.gitDiff

testGitDirty :: TestTree
testGitDirty = testCase "gitDirty" $ do
  Utils.assertBoolean $$GRT.gitDirty

testGitDirtyTracked :: TestTree
testGitDirtyTracked = testCase "gitDirtyTracked" $ do
  Utils.assertBoolean $$GRT.gitDirtyTracked

testGitHash :: TestTree
testGitHash = testCase "gitHash" $ do
  (Utils.assertHash . toText) $$GRT.gitHash

testGitShortHash :: TestTree
testGitShortHash = testCase "gitShortHash" $ do
  (Utils.assertShortHash . toText) $$GRT.gitShortHash

testGitTree :: TestTree
testGitTree = testCase "gitTree" $ do
  (Utils.assertHash . toText) $$GRT.gitTree

envTests :: TestTree
envTests =
  testGroup
    "Environment"
    [ testEnvLookupFailure,
      testRunGitInEnvBadDir
    ]

testEnvLookupFailure :: TestTree
testEnvLookupFailure = testCase desc $ do
  let result = $$(GRT.qToCode $ GRT.envValQ [osstr|SOME_BAD_VAR|])
  case result of
    Right x -> assertFailure $ "Unexpected success: " ++ show x
    Left err -> expected @=? displayException err
  where
    desc = "Environment lookup should fail"
    expected =
      mconcat
        [ "Environment error with env variable 'SOME_BAD_VAR', value ",
          "<none>: No such var found."
        ]

testRunGitInEnvBadDir :: TestTree
testRunGitInEnvBadDir = testCase desc $ do
  let result =
        $$( GRT.qToCode $ do
              runIO $ Env.setEnv "SOME_GOOD_VAR" "some/bad/dir"
              GRT.runGitInEnvDirQ [osstr|SOME_GOOD_VAR|] GRT.gitHashQ
          )
  case result of
    Right x -> assertFailure $ "Unexpected success: " ++ show x
    Left ex -> do
      let exStr = displayException ex
          exTxt = T.pack exStr
      assertBool
        ("Unexpected prefix: " ++ exStr)
        (expected `T.isPrefixOf` exTxt)
  where
    desc = "Run in environment dir should fail"
    expected =
      mconcat
        [ "Environment error with env variable 'SOME_GOOD_VAR', value ",
          "'some/bad/dir': Could not set directory:"
        ]

combinatorTests :: TestTree
combinatorTests =
  testGroup
    "Combinations"
    [ testLiftError,
      testHashAndEnvVal,
      testHashAndEnvDir
    ]

testLiftError :: TestTree
testLiftError = testCase "Lifts with default string" $ do
  (Utils.assertHash . toText)
    $$(GRT.qToCode $ GRT.projectStringUnknown GRT.gitHashQ)

testHashAndEnvVal :: TestTree
testHashAndEnvVal = testCase "Composes hash and env val lookup" $ do
  (Utils.assertHash . toText)
    $$( GRT.qToCode $
          GRT.projectStringUnknown $
            GRT.embedGitError GRT.gitHashQ
              <> GRT.embedEnvError (GRT.envValQ [osstr|var|])
      )

testHashAndEnvDir :: TestTree
testHashAndEnvDir = testCase "Composes hash and env dir lookup" $ do
  (Utils.assertHash . toText)
    $$( GRT.qToCode $
          GRT.projectStringUnknown $
            GRT.embedGitError GRT.gitHashQ
              <> GRT.runGitInEnvDirQ [osstr|var|] GRT.gitHashQ
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
  let (num1, num2, num3) = $$(GRT.qToCode Utils.qOsSemigroup)
  1 @=? num1
  1 @=? num2
  1 @=? num3

testSemigroupQFirstLazy :: TestTree
testSemigroupQFirstLazy = testCase "QFirst Semigroup is lazy in the rhs" $ do
  let (num1, num2, num3) = $$(GRT.qToCode Utils.qOsFirstSemigroup)
  1 @=? num1
  0 @=? num2
  0 @=? num3

testSemigroupQFirstSuccessLazy :: TestTree
testSemigroupQFirstSuccessLazy = testCase "Utils.firstSuccessQ is lazy" $ do
  let (num1, num2, num3) = $$(GRT.qToCode Utils.qOsFirstSuccess)
  1 @=? num1
  0 @=? num2
  0 @=? num3

testSemigroupQFirstSuccessLazy2 :: TestTree
testSemigroupQFirstSuccessLazy2 = testCase "Utils.firstSuccessQ is lazy 2" $ do
  let (num1, num2, num3) = $$(GRT.qToCode Utils.qOsFirstSuccess2)
  1 @=? num1
  1 @=? num2
  0 @=? num3

testSemigroupQFirstSuccessAllLefts :: TestTree
testSemigroupQFirstSuccessAllLefts = testCase desc $ do
  let ((num1, num2, num3), eResult) = $$(GRT.qToCode Utils.qOsFirstSuccessAllLefts)
  1 @=? num1
  1 @=? num2
  1 @=? num3

  case eResult of
    Right x -> assertFailure $ "Received Right: " ++ show x
    Left result -> do
      expected @=? result
      expectedStr @=? displayException result
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

toText :: OsString -> Text
toText = T.pack . unsafeDecodeUtf

unsafeDecodeUtf :: OsString -> String
unsafeDecodeUtf os = case decodeUtf os of
  Left ex -> error $ displayException ex
  Right s -> s
