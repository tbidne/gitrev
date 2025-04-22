{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad ((>=>))
import System.Environment qualified as Env
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
    ["hash_legacy_os"] -> putStrLn $TH.OsString.hashLegacy
    ["hash_env_val_os"] -> printOsString $$TH.OsString.hashEnvVal
    ["hash_env_dir_os"] -> printOsString $$TH.OsString.hashEnvDir
    ["hash_env_val_dir_os"] -> printOsString $$TH.OsString.hashEnvValDir
    other -> error $ "Unexpected args: " ++ show other
  where
    printOsString = OsString.decodeUtf >=> putStrLn
