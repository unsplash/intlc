# There are two major considerations when picking a version of GHC for this
# project.
#
# Firstly, we'd like the version we choose to have tooling available in Nix's
# binary cache, without contributors having to mess around with additional
# caches. This limits us to the "default" version of GHC in nixpkgs.
#
# With this in mind, we could simply use the `ghc` and `haskellPackages`
# package and package set, however the following has been written with
# flexibility in mind. Note that using `haskellPackages` implicitly brings the
# default version of GHC into scope as it's merely an alias for the default
# package set [1].
#
# Secondly, HLS is currently marked as broken on aarch64-darwin [2], which
# impacts Apple silicon contributors. With this in mind we're also keen to
# ensure that our chosen version of GHC is supported by HLS as distributed via
# ghcup.
#
# This means we're effectively stuck with whichever version of GHC is both the
# default for nixpkgs at some point in time - in the past is okay, as nixpkgs
# is pinned and the cache is long-lived - and which has the requisite support
# in ghcup as well. At time of writing that version is 9.2.4.
#
# Further complicating matters, GHC and HLS need to be sourced from the same
# place to ensure that HLS was compiled against that same version of GHC, lest
# we see "ABIs don't match" errors. Therefore aarch64-darwin contributors must
# not only source HLS externally but also GHC.
#
# But that's not all! A subset of Apple silicon contributors have reported being
# unable to run compiled 9.2.4 code. 9.2.5 fixes it. These users are encouraged
# to use 9.2.5 for the timebeing irrespective of the configuration here.
#
# In short, don't upgrade GHC until it's the default in nixpkgs, and available
# in ghcup and supported by HLS there as well.
#
# [1]: https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/haskell.section.md#available-packages-haskell-available-packages
# [2]: https://github.com/NixOS/nixpkgs/blob/a410420844fe1ad6415cf9586308fe7538cc7584/pkgs/development/compilers/llvm/7/compiler-rt/default.nix#L108
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};

          ghcVer = "924";

          hask = if system == flake-utils.lib.system.aarch64-darwin
            then [ ]
            else with pkgs; [
              haskell.compiler."ghc${ghcVer}"
              (haskell-language-server.override { supportedGhcVersions = [ ghcVer ];})
            ];

          haskPkgs = pkgs.haskell.packages."ghc${ghcVer}";
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            git

            cabal-install
            hlint
            haskPkgs.hspec-golden
            stylish-haskell

            # For typechecking golden output
            nodejs
            yarn
          ] ++ hask;
        };
      });
}
