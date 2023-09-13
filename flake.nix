{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};

          # We'll stick with the "default" version of GHC in nixpkgs to benefit
          # from the binary cache:
          #   https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/haskell.section.md#available-packages-haskell-available-packages
          ghcVer = "ghc946";

          haskPkgs = pkgs.haskell.packages."${ghcVer}";
      in {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            cabal-install
            haskell.compiler."${ghcVer}"
            haskPkgs.haskell-language-server
            hlint
            haskPkgs.hspec-golden
            stylish-haskell

            # For typechecking golden output
            nodejs
            yarn
          ];
        };
      });
}
