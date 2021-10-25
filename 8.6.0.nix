{
  nixpkgs ? builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/20.09.tar.gz";
  }
}:
let
  inherit (pkgs) lib haskell;
  pkgs = import nixpkgs {};
  reflexPlatform = import reflexPlatformSrc {};
  cure = p: haskell.lib.dontCheck (haskell.lib.doJailbreak p);

  reflexPlatformSrc = builtins.fetchGit {
    url = "https://github.com/reflex-frp/reflex-platform.git";
    rev = "f019863c21ee85498e6a6e0072e617b2462b70ed";
  };

  ghcjsBaseDummySrc = builtins.fetchGit {
    url = "git@github.com:lagunoff/ghcjs-base-dummy.git";
    rev = "ec549b970fc7d0f8031e8f2fc943dac89e443f69";
  };

  extraPackages = super: {
    gauge = builtins.fetchGit {
      url = "https://github.com/vincenthz/hs-gauge.git";
      rev = "3069be4752a4c0f2ac310e66b44d7929763da19c";
    };
  } // lib.optionalAttrs (!(super.ghc.isGhcjs or false)) {
    ghcjs-base = ghcjsBaseDummySrc;
  };

  overrides = self: super: {
    htmlt = self.callCabal2nixWithOptions "htmlt" ./. "-fexamples -fbenchmarks" {};
    gauge = cure super.gauge;
  } // lib.optionalAttrs (!(super.ghc.isGhcjs or false)) {
    ghcjs-base = cure super.ghcjs-base;
  };

  mkPackages = self: super: super.override {
    overrides = lib.composeExtensions (self: super:
      lib.mapAttrs (k: v: self.callCabal2nix k v {}) (extraPackages super)
    ) overrides;
  } // {
    shell = pkgs.mkShell {
      inputsFrom = [self.htmlt.env];
    };
  };
in rec {
  ghcjs = mkPackages ghcjs reflexPlatform.ghcjs;
  ghc = mkPackages ghc pkgs.haskellPackages;
}
