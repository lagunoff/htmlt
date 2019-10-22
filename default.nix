(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    massaraksh = ./.;
    massaraksh-examples = ./examples;
  };

  withHoogle = false;

  shells = {
    ghcjs = ["massaraksh" "massaraksh-examples"];
    ghc = ["massaraksh" "massaraksh-examples"];
  };
  
  overrides = self: super:
    let nixpkgs = import ./reflex-platform/nixpkgs {}; in
    with nixpkgs.haskell.lib; {
      hpack = dontCheck pkgs.haskellPackages.hpack;
      doctest = nixpkgs.lib.warn "ignoring dependency on doctest" null;
      
      polysemy = dontCheck (self.callHackage "polysemy" "1.2.1.0" {});
      bifunctors = self.callHackage "bifunctors" "5.5.5" {};
      type-errors = dontCheck (self.callHackage "type-errors" "0.2.0.0" {});
      th-abstraction = dontCheck (self.callHackage "th-abstraction" "0.3.1.0" {});
      first-class-families = self.callHackage "first-class-families" "0.5.0.0" {};
      th-lift = self.callHackage "th-lift" "0.8.0.1" {};
      unliftio = dontCheck super.unliftio;
      time-compat = doJailbreak (dontCheck (self.callHackage "time-compat" "1.9.2.2" {}));
      Glob = dontCheck super.Glob;
      generic-deriving = dontCheck (self.callHackage "generic-deriving" "1.13" {});
      invariant = dontCheck (self.callHackage "invariant" "0.5.3" {});
      lens = dontCheck (doJailbreak super.lens);

      type-errors-pretty = dontCheck (self.callCabal2nix "type-errors-pretty" (pkgs.fetchFromGitHub {
        owner = "chshersh";
        repo = "type-errors-pretty";
        rev = "135523afc869aa8fe615389f1dcd398a81df66c3";
        sha256 = "09k0y2n6gf5hp9ywimr4gdn1z78bhzxlmis59rk87g02nd76fbhw";
      }) {});

      aeson = dontCheck (self.callCabal2nix "aeson" (pkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "aeson";
        rev = "d6288c431a477f9a6e93aa80454a9e1712127548";
        sha256 = "102hj9b42z1h9p634g9226nvs756djwadrkz9yrb15na671f2xf4";
      }) {});
    };
})
