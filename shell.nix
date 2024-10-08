let
  sources = {
    nixpkgs = builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/24.05.tar.gz";
    };
    http-client = builtins.fetchGit {
      url = "https://github.com/lagunoff/http-client.git";
      rev = "18388fb8f2a13290279a9e69a1ca2b4719e58cca";
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

    clickable = final.callCabal2nix "clickable" ./ {};
    semirings = final.callCabal2nix "semirings" ../fullstack-hs/.overrides/semirings {};

    th-compat = final.callCabal2nix "th-compat" (builtins.fetchGit {
      url = "https://github.com/haskell-compat/th-compat.git";
      rev = "eec1edb9e09eba14e7ba17be29a9674431c63c05";
    }) {};

    th-abstraction = final.callCabal2nix "th-abstraction" (builtins.fetchGit {
      url = "https://github.com/glguy/th-abstraction.git";
      rev = "1fb59d1651ac5462f44100c62e5806665d39b6c6";
    }) {};

    crypton-connection = final.callCabal2nix "crypton-connection" (builtins.fetchGit {
      url = "https://github.com/kazu-yamamoto/crypton-connection.git";
      rev = "bab16ae4d0fa417ba315fb3d26f55418ce91fcb6";
    }) {};

    lens = final.callCabal2nix "lens" (builtins.fetchGit {
      url = "https://github.com/ekmett/lens.git";
      rev = "c1f47431d66f114262acb4023a0d79e979af5636";
    }) {};

    ghc-exactprint = final.callCabal2nix "ghc-exactprint" (builtins.fetchGit {
      url = "https://github.com/alanz/ghc-exactprint.git";
      rev = "630c4e8f36d0decb765644041a273f6ba8888d57";
    }) {};

    hashable = final.callCabal2nix "hashable" ../fullstack-hs/.overrides/hashable {};

    semigroupoids = final.callCabal2nix "semigroupoids" ../fullstack-hs/.overrides/semigroupoids {};
    extensions = final.callCabal2nix "extensions" ../fullstack-hs/.overrides/extensions {};
    stan = final.callCabal2nix "stan" ../fullstack-hs/.overrides/stan {};
    apply-refact = final.callCabal2nix "apply-refact" ../fullstack-hs/.overrides/apply-refact {};
    retrie = final.callCabal2nix "retrie" ../fullstack-hs/.overrides/retrie {};
    aeson-pretty = final.callCabal2nix "aeson-pretty" ../fullstack-hs/.overrides/aeson-pretty {};

    ghcide = final.callCabal2nix "ghcide" ../fullstack-hs/.overrides/haskell-language-server/ghcide {};
    haskell-language-server = final.callCabal2nix "haskell-language-server" ../fullstack-hs/.overrides/haskell-language-server {};
    haskell-language-server-exe = final.callCabal2nix "haskell-language-server-exe" ../fullstack-hs/.overrides/haskell-language-server/exe {};
    hie-compat = final.callCabal2nix "hie-compat" ../fullstack-hs/.overrides/haskell-language-server/hie-compat {};
    hls-graph = final.callCabal2nix "hls-graph" ../fullstack-hs/.overrides/haskell-language-server/hls-graph {};
    hls-plugin-api = final.callCabal2nix "hls-plugin-api" ../fullstack-hs/.overrides/haskell-language-server/hls-plugin-api {};
    hls-test-utils = final.callCabal2nix "hls-test-utils" ../fullstack-hs/.overrides/haskell-language-server/hls-test-utils {};
    shake-bench = final.callCabal2nix "shake-bench" ../fullstack-hs/.overrides/haskell-language-server/shake-bench {};

    cabal-add = final.callCabal2nix "cabal-add" ../fullstack-hs/.overrides/cabal-add {};
    ghc-lib-parser = final.callCabal2nix "ghc-lib-parser" ../fullstack-hs/.overrides/ghc-lib-parser-9.10.1.20240511 {};

    aeson = final.callCabal2nix "aeson" ../fullstack-hs/.overrides/aeson {};
    attoparsec-aeson = final.callCabal2nix "attoparsec-aeson" ../fullstack-hs/.overrides/aeson/attoparsec-aeson {};
    lsp = final.callCabal2nix "lsp" ../fullstack-hs/.overrides/lsp/lsp {};
    lsp-types = final.callCabal2nix "lsp-types" ../fullstack-hs/.overrides/lsp/lsp-types {};
    lsp-test = final.callCabal2nix "lsp-test" ../fullstack-hs/.overrides/lsp/lsp-test {};
  };

  haskellPackages = pkgs.haskell.packages.ghc9101.override {
    inherit overrides;
  };

  ghc-wasm-meta = builtins.getFlake sources.ghc-wasm-meta;
in
  (pkgs.mkShell {
    inputsFrom = [haskellPackages.clickable.env];
    nativeBuildInputs = [
      ghc-wasm-meta.packages.x86_64-linux.default
      haskellPackages.haskell-language-server-exe
      (haskellPackages.hoogleWithPackages (p: p.clickable.buildInputs))
    ];
  }) // {
    inherit haskellPackages;
  }
