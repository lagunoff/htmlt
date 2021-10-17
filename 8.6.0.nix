{
  nixpkgs ? builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/20.09.tar.gz";
  }
}:
let
  inherit (pkgs) lib haskell;
  pkgs = import nixpkgs {};
  reflexPlatform = import reflexPlatformSrc {};

  reflexPlatformSrc = builtins.fetchGit {
    url = "https://github.com/reflex-frp/reflex-platform.git";
    rev = "f019863c21ee85498e6a6e0072e617b2462b70ed";
  };

  extraPackages = super: {
    gauge = builtins.fetchGit {
      url = "https://github.com/vincenthz/hs-gauge.git";
      rev = "3069be4752a4c0f2ac310e66b44d7929763da19c";
    };
  };

  overrides = self: super: {
    htmlt = self.callCabal2nixWithOptions "htmlt" ./. "-fexamples" {};
  };

  ghcjsPackages = reflexPlatform.ghcjs.override {
    overrides = lib.composeExtensions (self: super:
      lib.mapAttrs (k: v: self.callCabal2nix k v {}) (extraPackages super)
    ) overrides;
  };
in
  ghcjsPackages // {
    shell = pkgs.mkShell {
      inputsFrom = [ghcjsPackages.htmlt.env];
    };
  }
