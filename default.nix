{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/24.05.tar.gz";
}) {}
}:

let
  inherit (pkgs.haskell.lib) doJailbreak;

  haskellPackages = pkgs.haskell.packages.ghcHEAD.override {
    overrides = self: super: {
      mkDerivation = args: super.mkDerivation ({
        doCheck = false;
        doBenchmark = false;
        doHoogle = false;
        doHaddock = false;
        jailbreak = true;
        enableLibraryProfiling = false;
        enableExecutableProfiling = false;
      } // args);

      htmlt = self.callCabal2nix "htmlt" ./. {};

      th-compat = self.callCabal2nix "th-compat" (builtins.fetchGit {
        url = "https://github.com/haskell-compat/th-compat.git";
        rev = "eec1edb9e09eba14e7ba17be29a9674431c63c05";
      }) {};

      th-abstraction = self.callCabal2nix "th-abstraction" (builtins.fetchGit {
        url = "https://github.com/glguy/th-abstraction.git";
        rev = "1fb59d1651ac5462f44100c62e5806665d39b6c6";
      }) {};

      aeson = doJailbreak (self.callCabal2nix "aeson" (builtins.fetchGit {
        url = "https://github.com/haskell/aeson.git";
        rev = "0834ba7b081297d44c706914c92c31d264aef816";
      }) {});
    };
  };

  result = {
    pkgs = haskellPackages;
    shell = pkgs.mkShell {
      inputsFrom = [haskellPackages.htmlt.env];
      # fails to build, you have to have cabal installed on your system
      # buildDepends = [haskellPackages.cabal-install];
    };
    javascript = pkgs.mkShell {
      buildInputs = [
        pkgs.pkgsCross.ghcjs.buildPackages.haskell.compiler.ghcHEAD
      ];
    };
  };
in
  result
