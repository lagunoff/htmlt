# Same arguments as for reflex-platform/default.nix
# https://github.com/reflex-frp/reflex-platform/blob/develop/default.nix
reflex-platform-args:
let
  nixpkgsFunc = reflex-platform-args.nixpkgsFunc or import (builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs-channels/archive/9d55c1430af72ace3a479d5e0a90451108e774b4.tar.gz";
  });

  reflex-platform = import (builtins.fetchTarball {
    url = "https://github.com/reflex-frp/reflex-platform/archive/1f04194d051c3a4fd54aa30d4c46a5659261a620.tar.gz";
  }) reflex-platform-args;

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
      massaraksh-examples = ./examples;
    };

    shells = {
      ghc = ["massaraksh" "massaraksh-examples"];
      ghcjs = ["massaraksh" "massaraksh-examples"];
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
