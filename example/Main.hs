{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import System.Environment qualified as Env
import TH qualified

main :: IO ()
main =
  Env.getArgs >>= \case
    ["hash_legacy"] -> putStrLn $TH.hashLegacy
    ["hash_env_val"] -> putStrLn $$TH.hashEnvVal
    ["hash_env_dir"] -> putStrLn $$TH.hashEnvDir
    ["hash_env_val_dir"] -> putStrLn $$TH.hashEnvValDir
    other -> error $ "Unexpected args: " ++ show other
