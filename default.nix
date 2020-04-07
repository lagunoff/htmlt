# Same arguments as for reflex-platform/default.nix
# https://github.com/reflex-frp/reflex-platform/blob/develop/default.nix
{}:
let
  reflex-platform = import (builtins.fetchTarball {
    url = "https://github.com/reflex-frp/reflex-platform/archive/1f04194d051c3a4fd54aa30d4c46a5659261a620.tar.gz";
  }) {};

in reflex-platform.project({ pkgs, ... }:
  let
    customOverrides = hlib: self: super: with hlib; {
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
    };

    shellToolOverrides = ghc: super: {
      inherit (pkgs) pkgconfig zlib;
      ghc-mod = null;
      haskell-ide-engine = null;
    };

    overrides = pkgs.lib.composeExtensions
      (customOverrides pkgs.haskell.lib)
      dontCheckOverrides;
  }
)
