{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/3e2237222b33d8b5754cc1cc3ee155cadd76770d.tar.gz";
}) {}
}: {
  shell = {
    javascript = pkgs.mkShell {
      buildInputs = [
        pkgs.pkgsCross.ghcjs.buildPackages.haskell.compiler.ghcHEAD
        pkgs.haskellPackages.cabal-install
      ];
    };
    x86_64 = pkgs.mkShell {
      buildInputs = [
        pkgs.haskell.compiler.ghcHEAD
      ];
    };
  };
}
