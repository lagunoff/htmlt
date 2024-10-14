let
  sources = {
    nixpkgs = builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/24.05.tar.gz";
    };
    ghc-wasm-meta = "gitlab:ghc/ghc-wasm-meta?host=gitlab.haskell.org&ref=108e4693cd147777e8d93683e58c8a5e1da74c96";
  };

  pkgs = import sources.nixpkgs {};

  overrides = final: prev: {
    mkDerivation = args: prev.mkDerivation ({
      doCheck = false;
      doBenchmark = false;
      doHoogle = true;
      doHaddock = true;
      jailbreak = true;
      enableLibraryProfiling = false;
      enableExecutableProfiling = false;
    } // args);

    clickable = final.callCabal2nix "clickable" ./. {};
  };

  haskellPackages = pkgs.haskell.packages.ghc9101.override {
    overrides = pkgs.lib.composeExtensions
      (import ../overrides/nixpkgs-24.05-ghc9101) overrides;
  };

  ghc-wasm-meta = builtins.getFlake sources.ghc-wasm-meta;
in
  pkgs.mkShell {
    inputsFrom = [
      haskellPackages.clickable.env
    ];
    nativeBuildInputs = [
      ghc-wasm-meta.packages.x86_64-linux.default
      haskellPackages.haskell-language-server-exe
      (haskellPackages.hoogleWithPackages (p: p.clickable.buildInputs))
    ];
  }
