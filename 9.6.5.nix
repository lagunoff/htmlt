let
  sources = {
    nixpkgs = builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/24.05.tar.gz";
    };
    ghc-wasm-meta = "gitlab:ghc/ghc-wasm-meta?host=gitlab.haskell.org&ref=455a759195e71c572e73b56d868e544176d32897";
  };

  pkgs = import sources.nixpkgs {};

  haskellPackages = pkgs.haskellPackages.override {
    overrides = final: prev: {
      clickable = final.callCabal2nix "clickable" ./. {};
    };
  };

  ghc-wasm-meta = builtins.getFlake sources.ghc-wasm-meta;
in
  pkgs.mkShell {
    inputsFrom = [
      haskellPackages.clickable.env
    ];
    nativeBuildInputs = [
      ghc-wasm-meta.packages.x86_64-linux.default
    ];
  }
