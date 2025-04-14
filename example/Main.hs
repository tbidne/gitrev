{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import System.Environment qualified as Env
import TH qualified

main :: IO ()
main =
  Env.getArgs >>= \case
    ["hash_legacy"] -> putStrLn $TH.hashLegacy
    ["hash_typed"] -> putStrLn $$TH.hashTyped
    other -> error $ "Unexpected args: " ++ show other
