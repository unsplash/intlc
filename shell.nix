{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    haskell.compiler.ghc8107
    haskell.packages.ghc8107.cabal-install
    haskell.packages.ghc8107.hspec-golden

    # For typechecking golden output
    nodejs
    yarn
  ];
}
