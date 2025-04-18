# Revision history for gitrev-typed

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

Note that the first version, 0.1, is relative to its parent from which it was
forked: https://github.com/acfoltzer/gitrev

## [0.1]
### Changed
* Updated GHC support to `>= 9 && < 9.13`.

### Added
* Added `Development.GitRev.Typed` for typed TH interface. Add supporting
  modules: `Development.GitRev.Utils.Git`,
  `Development.GitRev.Utils.LookupEnv`, `Development.GitRev.Utils`.
* Added variants that return typed errors (`Either`), rather than just a
  default string "UNKNOWN".
* Added support for short hashes.
* Added support for obtaining data via environment variables for e.g.
  "out-of-tree" builds.
* Added git diff.
* Added git tree.

### Fixed
* Possibly fixed locale bug by switching from prelude
  `readFile :: FilePath -> IO String` to
  `file-io.readFile' :: OsPath -> IO ByteString` and decoding to UTF-8.

