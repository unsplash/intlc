{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            git

            haskell.compiler.ghc923
            haskell.packages.ghc923.cabal-install
            haskell.packages.ghc923.hspec-golden

            # For typechecking golden output
            nodejs
            yarn
          ];
        };
      });
}
