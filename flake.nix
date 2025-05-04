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
              name = "gitrev-typed";
              root = ./.;

              devTools = [
                (hlib.dontCheck compiler.cabal-fmt)
                (hlib.dontCheck compiler.haskell-language-server)
                pkgs.nixfmt-rfc-style
              ];
            };

          stack-wrapped = pkgs.symlinkJoin {
            name = "stack";
            paths = [ pkgs.stack ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/stack --add-flags "--no-nix --system-ghc"
            '';
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
                  # Set the hashes so that example/TH.hs works as expected.
                  EXAMPLE_HASH = "${self.rev or self.dirtyRev}";
                  EXAMPLE_MODIFIED = "${builtins.toString self.lastModified}";
                  EXAMPLE_SHORT_HASH = "${self.shortRev or self.dirtyShortRev}";
                });
              source-overrides = {
                # depends on gitrev-typed here...
                gitrev-typed = ./.;
              };
            };
          };
          devShells = {
            default = mkPkg true;

            stack = pkgs.mkShell {
              buildInputs = [
                compiler.ghc
                pkgs.zlib
                stack-wrapped
              ];
            };
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
