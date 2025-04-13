{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import Development.GitRev
import Prelude (String, concat, error, otherwise)

panic :: String -> a
panic msg = error panicMsg
  where
    panicMsg =
      concat
        [ "[panic ",
          $(gitBranch),
          "@",
          $(gitHash),
          " (",
          $(gitCommitDate),
          ")",
          " (",
          $(gitCommitCount),
          " commits in HEAD)",
          dirty,
          "] ",
          msg
        ]
    dirty
      | $(gitDirty) = " (uncommitted files present)"
      | otherwise = ""

main = panic "oh no!"
