{
  nixpkgs ? builtins.fetchGit {
    rev = "7b47000f84ccc4061c053c1cc92bc172a6b814da";
    url = "https://github.com/NixOS/nixpkgs.git";
  }
}:
let
  inherit (pkgs) lib haskell;
  pkgs = import nixpkgs {};
  cure = x: haskell.lib.dontCheck (haskell.lib.doJailbreak x);

  vectorSrc = builtins.fetchGit {
    url = "https://github.com/haskell/vector.git";
    rev = "b592c22e713aa8a901d9e4387ee4522632bd7d10";
  };

  foundationSrc = builtins.fetchGit {
    url = "https://github.com/haskell-foundation/foundation.git";
    rev = "31d978d94d1493a53565e1050450c434e8808bd6";
  };

  extraPackages = super: {
    gauge = builtins.fetchGit {
      url = "https://github.com/vincenthz/hs-gauge.git";
      rev = "3069be4752a4c0f2ac310e66b44d7929763da19c";
    };
    vector = vectorSrc + "/vector";
    vector-stream = vectorSrc + "/vector-stream";
    foundation = foundationSrc + "/foundation";
    basement = foundationSrc + "/basement";
  };

  overrides = self: super: {
    htmlt = self.callCabal2nixWithOptions "htmlt" ./. "-fexamples" {};
    ghcjs-base = cure super.ghcjs-base;
    aeson = cure super.aeson;
    generic-lens-core = cure super.generic-lens-core;
    generic-lens = cure super.generic-lens;
    vector = cure super.vector;
    vector-streaming = cure super.vector-streaming;
    foundation = cure super.foundation;
    basement = cure super.basement;
    gauge = cure super.gauge;
    http-api-data = cure super.http-api-data;
    network-uri = cure super.network-uri;
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
  ghcjs = mkPackages ghcjs pkgs.haskell.packages.ghcjs810;
  ghc = mkPackages ghc pkgs.haskellPackages;
}
