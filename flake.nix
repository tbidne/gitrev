{
  description = "Compiling git revisions into haskell projects";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.nix-hs-utils.url = "github:tbidne/nix-hs-utils";
  outputs =
    inputs@{
      flake-parts,
      nixpkgs,
      nix-hs-utils,
      self,
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem =
        { pkgs, ... }:
        let
          hlib = pkgs.haskell.lib;
          ghc-version = "ghc9101";
          compiler = pkgs.haskell.packages."${ghc-version}";
          compilerPkgs = {
            inherit compiler pkgs;
          };

          mkPkg =
            returnShellEnv:
            nix-hs-utils.mkHaskellPkg {
              inherit compiler pkgs returnShellEnv;
              name = "gitrev";
              root = ./.;

              devTools = [
                (hlib.dontCheck compiler.cabal-fmt)
                (hlib.dontCheck compiler.haskell-language-server)
                pkgs.nixfmt-rfc-style
              ];
            };
        in
        {
          packages = {
            # nix expression for gitrev
            default = mkPkg false;

            # self-contained example of building a haskell package via
            # nixpkgs infra i.e. developPackage.
            example = compiler.developPackage {
              name = "example";
              root = ./example;
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
                  # Set the hash so that example/TH.hs works as expected.
                  EXAMPLE_HASH = "${self.rev or self.dirtyRev}";
                });
              source-overrides = {
                # depends on gitrev here...
                gitrev = ./.;
              };
            };
          };
          devShells = {
            default = mkPkg true;
          };

          apps = {
            format = nix-hs-utils.format compilerPkgs;
          };
        };
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
