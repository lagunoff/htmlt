with
  import (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/2cba90f281c42ab37c27e5cf5dcebc362654efb5.tar.gz) { };
  pkgs.mkShell {
    buildInputs = [pkgsCross.ghcjs.haskell.packages.ghcHEAD.ghc];
  }
