# Same arguments as for reflex-platform/default.nix
# https://github.com/reflex-frp/reflex-platform/blob/develop/default.nix
{}:
let
  reflex-platform = import (builtins.fetchTarball {
    url = "https://github.com/reflex-frp/reflex-platform/archive/846964a0895e819946c1a415202aee414d27cfa3.tar.gz";
  }) { config.allowBroken = true; };

in reflex-platform.project({ pkgs, ... }:{
  useWarp = true;
  withHoogle = true;
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
})
