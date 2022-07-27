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

            # HLS 1.7.0.0 via ghcup doesn't supply a 9.2.3 binary.
            #
            # HLS via Nix is currently unavailable on ARM/M1 on 9.2.3:
            #   https://github.com/NixOS/nixpkgs/issues/140774#issuecomment-1186546139
            #
            # Thus we've pinned nixpkgs to the last commit before 9.2.2 was bumped up to 9.2.3.
            haskell.compiler.ghc922
            haskell.packages.ghc922.cabal-install
            haskell.packages.ghc922.haskell-language-server
            haskell.packages.ghc922.hspec-golden

            # For typechecking golden output
            nodejs
            yarn
          ];
        };
      });
}
