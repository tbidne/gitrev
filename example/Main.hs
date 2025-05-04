{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Exception (Exception (displayException))
import Control.Monad ((>=>))
import Data.List qualified as L
import Data.Version (showVersion)
import Paths_example qualified as Paths
import System.Environment qualified as Env
import System.Info qualified as Info
import System.OsString (OsString, decodeUtf)
import System.OsString qualified as OsString
import TH qualified
import TH.OsString qualified

main :: IO ()
main =
  Env.getArgs >>= \case
    ["hash_legacy"] -> putStrLn $TH.hashLegacy
    ["hash_env_val"] -> putStrLn $$TH.hashEnvVal
    ["hash_env_dir"] -> putStrLn $$TH.hashEnvDir
    ["hash_env_val_dir"] -> putStrLn $$TH.hashEnvValDir
    ["complex"] -> putStrLn complexData
    ["hash_legacy_os"] -> putStrLn $TH.OsString.hashLegacy
    ["hash_env_val_os"] -> printOsString $$TH.OsString.hashEnvVal
    ["hash_env_dir_os"] -> printOsString $$TH.OsString.hashEnvDir
    ["hash_env_val_dir_os"] -> printOsString $$TH.OsString.hashEnvValDir
    ["complex_os"] -> putStrLn complexDataOs
    other -> error $ "Unexpected args: " ++ show other
  where
    printOsString = OsString.decodeUtf >=> putStrLn

complexData :: String
complexData =
  L.intercalate
    "\n"
    [ "Example: " <> showVersion Paths.version,
      " - Git revision: " <> h,
      " - Commit date:  " <> d,
      " - GHC version:  " <> showVersion Info.compilerVersion
    ]
  where
    (d, h, _) = $$TH.gitComplexData

complexDataOs :: String
complexDataOs =
  L.intercalate
    "\n"
    [ "Example: " <> showVersion Paths.version,
      " - Git revision: " <> unsafeDecodeUtf h,
      " - Commit date:  " <> unsafeDecodeUtf d,
      " - GHC version:  " <> showVersion Info.compilerVersion
    ]
  where
    (d, h, _) = $$TH.OsString.gitComplexData

unsafeDecodeUtf :: OsString -> String
unsafeDecodeUtf os = case decodeUtf os of
  Left ex -> error $ displayException ex
  Right s -> s
