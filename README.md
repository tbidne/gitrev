<div align="center">

# Gitrev-typed

## Embedding git metadata in haskell projects

[![Hackage](https://img.shields.io/hackage/v/gitrev-typed)](https://hackage.haskell.org/package/gitrev-typed)
![haskell](https://img.shields.io/static/v1?label=&message=9.0%20-%209.12&logo=haskell&logoColor=655889&labelColor=2f353e&color=655889)
[![ci](http://img.shields.io/github/actions/workflow/status/tbidne/gitrev-typed/ci.yaml?branch=main)](https://github.com/tbidne/gitrev-typed/actions/workflows/ci.yaml)
[![BSD-3-Clause](https://img.shields.io/github/license/tbidne/gitrev-typed?color=blue)](https://opensource.org/licenses/BSD-3-Clause)

</div>

---


# Index
- [Description](#description)
  - [1. Development.GitRev](#1-developmentgitrev)
  - [2. Development.GitRev.Typed](#2-developmentgitrevtyped)
    - [Out-of-tree builds](#out-of-tree-builds)
  - [3. Development.GitRev.Typed.OsString](#3-developmentgitrevtypedosstring)
- [Library Comparisons](#library-comparisons)
  - [Gitrev](#gitrev)
  - [Githash](#githash)

# Description

This is a fork of the popular [`gitrev`](https://hackage.haskell.org/package/gitrev) package, offering Template Haskell splices for git revision information.

There are three interfaces:

## 1. `Development.GitRev`

This module provides the same interface as `gitrev`'s `Development.GitRev` i.e. untyped splices:

```haskell
-- Definition in Development.GitRev
gitHash :: ExpQ
```

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Development.GitRev qualified as GR

-- Returns a hash like "e67e943dd03744d3f93c21f84e127744e6a04543" or
-- "UNKNOWN", if something goes wrong.
myHash :: String
myHash = $(GR.gitHash)
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
myHash :: String
myHash = $$(GRT.gitHash)
```

We also provide combinators for defining custom behavior. For instance, we can instead define a variant that fails at compile-time instead of returning the string `UNKNOWN`.

```haskell
-- gitHashQ :: Q (Either GitError String)
-- projectError :: Q (Either e String) -> Q String
-- qToCode :: Q a -> Code Q a
myHashOrDie :: String
myHashOrDie = $$(GRT.qToCode $ GRT.projectError GRT.gitHashQ)
```

### Out-of-tree builds

Furthermore, we have workarounds for "out-of-tree" builds:

```haskell
{-# LANGUAGE OverloadedLists #-}

myHashEnv :: Code Q String
myHashEnv = toCode gitHash
  where
    toCode :: Q (Either (Exceptions GitRevError) String) -> Code Q String
    toCode = GRT.qToCode . GRT.projectError

    gitHash :: Q (Either (Exceptions GitRevError) String)
    gitHash =
      -- Tries, in order:
      --
      -- 1. Retrieving the git hash, as normal.
      -- 2. Running the git action under the directory pointed to by the
      --    environment variable EXAMPLE_HOME, if it exists.
      -- 3. Looking up environment variable EXAMPLE_HASH, returning the
      --    value if it exists.
      GRT.firstSuccessQ
        [ GRT.embedGitError GRT.gitHashQ,
          GRT.runGitInEnvDirQ "EXAMPLE_HOME" GRT.gitHashQ
          GRT.embedEnvError $ GRT.envValQ "EXAMPLE_HASH",
        ]
```

For example, `myHashEnv` will work for `cabal install` if we include the
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

See the `example` in the [repo](https://github.com/tbidne/gitrev-typed/) for example usage, and `Development.GitRev.Typed` for full documentation.

## 3. `Development.GitRev.Typed.OsString`

`Development.GitRev.Typed` for `OsString` rather than `String`.

# Library Comparisons

## Overview

- 🌕: Supported.
- 🌓: Partial support.
- 🌑: Not supported.

| 👇 Feature / Library 👉   | `gitrev-typed` | `gitrev` | `githash` |
|---------------------------|----------------|----------|-----------|
| Maintained                | 🌕             | 🌑       | 🌓        |
| Untyped TH                | 🌕             | 🌕       | 🌑        |
| Typed TH                  | 🌕             | 🌑       | 🌕        |
| Custom git queries        | 🌕             | 🌑       | 🌑        |
| "Out-of-tree" workarounds | 🌕             | 🌑       | 🌑        |
| `OsString` support        | 🌕             | 🌑       | 🌑        |

## Gitrev

- As stated, this is a fork of `gitrev`. While `gitrev` has been admirably stable for many years, it was officially declared unmaintained in January 2024, and the [repo](https://github.com/acfoltzer/gitrev/) was archived.

- Not only is `gitrev-typed` currently maintained, it offers the new, typed interface. Additionally, `gitrev-typed` resolves a number of open `gitrev` issues (see the [changelog](./CHANGELOG.md)).

## Githash

- [`Githash`](https://hackage.haskell.org/package/githash) is another `gitrev` fork, though it takes a different approach. Rather than offering multiple, individual git queries, `githash` returns a single one with all of the (pre-defined) git data at once. The user can then take whatever subset they wish.

  This provides nice ergonomics when you want to run multiple queries at once, though it is arguably wasteful when you just want one or two. It also makes it harder to fit custom user queries into the API.

- Both `githash` and `gitrev-typed` have a typed TH interface (unlike `gitrev`).

- `githash` does not support custom git queries, workarounds for "out-of-tree" builds, or `OsString`.
