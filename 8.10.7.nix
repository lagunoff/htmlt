{
  nixpkgs ? builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/21.11.tar.gz";
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

  extraPackages = super: {
    vector = vectorSrc + "/vector";
    vector-stream = vectorSrc + "/vector-stream";
  };

  overrides = self: super: {
    htmlt = self.callCabal2nixWithOptions "htmlt" ./. "-fexamples -fbenchmarks" {};
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
  };

in rec {
  ghcjs = mkPackages ghcjs pkgs.haskell.packages.ghcjs810;
  ghc = mkPackages ghc pkgs.haskellPackages;
  shell.ghcjs = pkgs.mkShell { inputsFrom = [ghcjs.htmlt.env]; };
  shell.ghc = pkgs.mkShell { inputsFrom = [ghc.htmlt.env]; };
}
