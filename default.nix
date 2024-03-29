{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/f4429fde23e1fb20ee27f264e74c28c619d2cebb.tar.gz";
}) {}
}:

let
  inherit (pkgs.haskell.lib) doJailbreak;

  haskellPackages = pkgs.haskell.packages.ghcHEAD.override {
    overrides = self: super: {
      mkDerivation = args: super.mkDerivation ({
        doCheck = false;
        doBenchmark = false;
        doHoogle = true;
        doHaddock = true;
        enableLibraryProfiling = false;
        enableExecutableProfiling = false;
      } // args);

      htmlt = self.callCabal2nix "htmlt" ./. {};


      th-compat = self.callCabal2nix "th-compat" ../fullstack-app/packages/th-compat {};
      aeson = doJailbreak (self.callCabal2nix "aeson" ../fullstack-app/packages/aeson {});
      bifunctors = doJailbreak super.bifunctors;
      free = doJailbreak super.free;
      generic-deriving = doJailbreak super.generic-deriving;
      invariant = doJailbreak super.invariant;
      lens = doJailbreak super.lens;
      semigroupoids = doJailbreak super.semigroupoids;
      tagged = self.callCabal2nix "tagged" ../fullstack-app/packages/tagged {};
      text-short = self.callCabal2nix "text-short" ../fullstack-app/packages/text-short {};
      th-abstraction = self.callCabal2nix "th-abstraction" ../fullstack-app/packages/th-abstraction {};
      th-expand-syns = doJailbreak super.th-expand-syns;
      th-lift = doJailbreak super.th-lift;
      unordered-containers = self.callCabal2nix "unordered-containers" ../fullstack-app/packages/unordered-containers {};
      vector = self.callCabal2nix "vector" ../fullstack-app/packages/vector/vector {};
      vector-stream = self.callCabal2nix "vector-stream" ../fullstack-app/packages/vector/vector-stream {};
      websockets = doJailbreak super.websockets;
      wuss = doJailbreak super.wuss;
    };
  };

  result = {
    pkgs = haskellPackages;
    shell = pkgs.mkShell {
      inputsFrom = [haskellPackages.htmlt.env];
    };
  };
in
  result
