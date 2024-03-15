{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/f4429fde23e1fb20ee27f264e74c28c619d2cebb.tar.gz";
}) {}
}: {
  shell = {
    x86_64 = pkgs.mkShell {
      buildInputs = [
        pkgs.haskell.compiler.ghcHEAD
      ];
    };
  };
}
