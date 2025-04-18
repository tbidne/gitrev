<div align="center">

# Gitrev

## Embedding git metadata in haskell projects

[![Hackage](https://img.shields.io/hackage/v/gitrev)](https://hackage.haskell.org/package/gitrev)
![haskell](https://img.shields.io/static/v1?label=&message=9.0%20-%209.12&logo=haskell&logoColor=655889&labelColor=2f353e&color=655889)
[![ci](http://img.shields.io/github/actions/workflow/status/tbidne/gitrev/ci.yaml?branch=main)](https://github.com/tbidne/gitrev/actions/workflows/ci.yaml)
[![BSD-3-Clause](https://img.shields.io/github/license/tbidne/gitrev?color=blue)](https://opensource.org/licenses/BSD-3-Clause)

</div>

---

# Description

Some handy Template Haskell splices for including the current git hash and branch in the code of your project. Useful for including in panic messages, `--version` output, or diagnostic info for more informative bug reports.

There are two main interfaces:

## 1. `Development.GitRev`

This module provides untyped splices e.g.

```haskell
-- Definition in Development.GitRev
gitHash :: ExpQ
```

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Development.GitRev qualified as GR

-- Returns a hash like "e67e943dd03744d3f93c21f84e127744e6a04543" or
-- "UNKNOWN", if something goes wrong.
projectHash :: String
projectHash = $(GR.gitHash)
```

## 2. `Development.GitRev.Typed`

This module -- on the other hand -- provides typed splices e.g.

```haskell
-- Definition in Development.GitRev.Typed
gitHash :: Code Q String
```

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Development.GitRev.Typed qualified as GRT

-- Returns a hash like "e67e943dd03744d3f93c21f84e127744e6a04543" or
-- "UNKNOWN", if something goes wrong.
projectHash :: String
projectHash = $$(GRT.gitHash)
```

We also provide combinators for defining custom behavior. For instance, we can instead define a variant that fails at compile-time instead of returning the string `UNKNOWN`.

```haskell
-- gitHashQ :: Q (Either GitError String)
-- projectError :: Q (Either GitError String) -> Q String
-- qToCode :: Q a -> Code Q a
projectHashOrDie :: String
projectHashOrDie = $$(GRT.qToCode $ GRT.projectError GRT.gitHashQ)
```

### Out-of-tree builds

Furthermore, we have workarounds for "out-of-tree" builds:

```haskell
projectHashEnv :: Code Q String
projectHashEnv = toCode gitHash
  where
    toCode :: Q (Either (Exceptions GitOrLookupEnvError) String) -> Code Q String
    toCode = GRT.qToCode . GRT.projectError

    gitHash :: Q (Either (Exceptions GitOrLookupEnvError) String)
    gitHash =
      -- Tries, in order:
      --
      -- 1. Retrieving the git hash, as normal.
      -- 2. Looking up environment variable EXAMPLE_HASH, returning the
      --    value if it exists.
      -- 3. Running the git action under the directory pointed to by the
      --    environment variable EXAMPLE_HOME, if it exists.
      GRT.firstSuccessQ
        (GRT.embedGitError GRT.gitHashQ)
        [ GRT.embedLookupEnvError $ GRT.envValQ "EXAMPLE_HASH",
          GRT.runGitInEnvDirQ "EXAMPLE_HOME" GRT.gitHashQ
        ]
```

For example, `projectHashEnv` will work for `cabal install` if we include the
environment variable:

```sh
$ export EXAMPLE_HOME=$(pwd); cabal install example
```

This function will also work with nix flakes:

```nix
# flake.nix
let
  compiler = pkgs.haskell.packages."ghc9101";
in
{
  # Using nixpkgs haskell infra i.e. developPackage.
  packages.default = compiler.developPackage {
    name = "example";
    root = ./.;
    returnShellEnv = false;
    modifier =
      drv:
      let
        drv' = pkgs.haskell.lib.addBuildTools drv [
          compiler.cabal-install
          compiler.ghc
          pkgs.git
          pkgs.zlib
        ];
      in
      drv'.overrideAttrs (oldAttrs: {
        EXAMPLE_HASH = "${self.rev or self.dirtyRev}";
      });
  };
};
```

See `example` in the `flake.nix` for a full nix example, and `Development.GitRev.Typed` for full documentation.

# Addendum

Most of the complication is due to the various places the current git hash might be stored:

1. Detached HEAD: the hash is in `.git/HEAD`
2. On a branch or tag: the hash is in a file pointed to by `.git/HEAD`
in a location like `.git/refs/heads`
3. On a branch or tag but in a repository with packed refs: the hash
is in `.git/packed-refs`
4. In any of the above situations, if the current repo is checked out
as a submodule, follow the reference to its `.git` directory first

These files are added as dependencies to modules that use `GitRev`, and so the module should be rebuilt automatically whenever these files change.

If you run into further scenarios that cause problems, let me know!
