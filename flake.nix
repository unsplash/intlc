{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};

          # Ensure this is available for both GHC and HLS in ghcup for non-Nix
          # contributors.
          ghcVer = "924";

          # HLS in nixpkgs is marked as broken on aarch64-darwin via LLVM 7,
          # see:
          #   https://github.com/NixOS/nixpkgs/blob/a410420844fe1ad6415cf9586308fe7538cc7584/pkgs/development/compilers/llvm/7/compiler-rt/default.nix#L108
          #
          # See also in unsplash/intlc: #162, #167
          #
          # GHC and HLS need to be sourced from the same place to ensure that
          # HLS was compiled against that same version of GHC, else we'll see
          # "ABIs don't match".
          hask = if system == flake-utils.lib.system.aarch64-darwin
            then [ ]
            else with pkgs; [
              haskell.compiler."ghc${ghcVer}"
              (haskell-language-server.override { supportedGhcVersions = [ ghcVer ];})
            ];
      in {
        # Ensure that everything here has a binary cached in Hydra on all
        # supported architectures; avoid `haskell.packages.ghcXXX.package`.
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            git

            cabal-install
            haskellPackages.hspec-golden

            # For typechecking golden output
            nodejs
            yarn
          ] ++ hask;
        };
      });
}
