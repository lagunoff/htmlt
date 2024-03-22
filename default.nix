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


      # th-compat = self.callCabal2nix "th-compat" ./packages/th-compat {};
      tagged = self.callCabal2nix "tagged" ../fullstack-app/packages/tagged {};
      th-abstraction = self.callCabal2nix "th-abstraction" ../fullstack-app/packages/th-abstraction {};
      vector = self.callCabal2nix "vector" ../fullstack-app/packages/vector/vector {};
      vector-stream = self.callCabal2nix "vector-stream" ../fullstack-app/packages/vector/vector-stream {};
      unordered-containers = self.callCabal2nix "unordered-containers" ../fullstack-app/packages/unordered-containers {};
      # text-short = self.callCabal2nix "text-short" ./packages/text-short {};
      generic-deriving = doJailbreak super.generic-deriving;
      bifunctors = doJailbreak super.bifunctors;
      semigroupoids = doJailbreak super.semigroupoids;
      th-lift = doJailbreak super.th-lift;
      th-expand-syns = doJailbreak super.th-expand-syns;
      invariant = doJailbreak super.invariant;
      free = doJailbreak super.free;
      th-compat = doJailbreak super.th-compat;
      lens = doJailbreak super.lens;
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
