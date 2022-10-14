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

            # We use mainline Haskell packages rather than specifying a custom
            # GHC version to ensure everything we want is cached in Hydra.
            ghc
            cabal-install
            haskellPackages.hspec-golden

            # For typechecking golden output
            nodejs
            yarn
          ];
        };
      });
}
