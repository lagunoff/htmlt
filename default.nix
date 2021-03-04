{ nixpkgs ? <nixpkgs>
, isGhcjs ? false }:
let
  pkgs = import nixpkgs {};
  combine = lib.foldr lib.composeExtensions (_: _: {});
  cure = with pkgs.haskell.lib; x: dontCheck (doJailbreak x);
  reflexPlatform = import reflexPlatformSrc {};
  inherit (pkgs) lib fetchgit;

  reflexPlatformSrc = fetchgit {
    url = "https://github.com/reflex-frp/reflex-platform.git";
    rev = "f019863c21ee85498e6a6e0072e617b2462b70ed";
    sha256 = "146xfjqdwd55s9jg1ggi6akcxxxd5c0pvc4bpjx3whwiikpcv8y4";
  };

  localPackages = {
    htmlt = ./.;
    htmlt-examples = ./examples;
  };

  overrides = super: {
    cabal-cargs = cure;
  };

  extensions = [
    (self: super:
      lib.mapAttrs (k: v: self.callCabal2nix k v {}) localPackages
    )
    (self: super:
      lib.mapAttrs (k: v: v super.${k}) (overrides super)
    )
  ];

  haskellBase = if isGhcjs
    then reflexPlatform.ghcjs
    else pkgs.haskellPackages;

  nativeHaskellPackages = pkgs.haskellPackages.override {
    overrides = combine extensions;
  };

  haskellPackages = haskellBase.override {
    overrides = combine extensions;
  };
in
  haskellPackages // {
    shell = with haskellPackages; pkgs.mkShell {
      inputsFrom = [htmlt.env];
      buildInputs = lib.optionals (!isGhcjs) [hoogle];
      nativeBuildInputs = with nativeHaskellPackages; [cabal-cargs];
    };
  }
