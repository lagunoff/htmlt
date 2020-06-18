# Same arguments as for reflex-platform/default.nix
# https://github.com/reflex-frp/reflex-platform/blob/develop/default.nix
{}:
let
  reflex-platform = import (builtins.fetchGit {
    url = "git@github.com:reflex-frp/reflex-platform.git";
    rev = "846964a0895e819946c1a415202aee414d27cfa3";
  }) { config.allowBroken = true; };

in reflex-platform.project({ pkgs, ... }:
  let
    customOverrides = haskellLib: self: super: with haskellLib; {
    };

    dontCheckOverrides = _: super: {
      mkDerivation = args: super.mkDerivation (args // {
        doCheck = false;
        doHaddock = false;
        doHoogle = false;
      });
    };
  in {
    useWarp = true;
    withHoogle = false;
    name = "massaraksh";

    packages = {
      massaraksh = ./.;
    };

    shells = {
      ghc = ["massaraksh"];
      ghcjs = ["massaraksh"];
      wasm = ["massaraksh"];
    };

    shellToolOverrides = ghc: super: {
      inherit (pkgs) pkgconfig zlib;
      ghc-mod = null;
      haskell-ide-engine = null;
      cabal-cargs = pkgs.haskell.lib.dontCheck pkgs.haskellPackages.cabal-cargs;
    };

    overrides = with pkgs.haskell.lib; pkgs.lib.composeExtensions
      (customOverrides pkgs.haskell.lib)
      dontCheckOverrides;
  }
)
