(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    massaraksh = ./.;
  };

  shells = {
    ghcjs = ["massaraksh"];
  };
})
