let
  fetchFromGitHub = { owner, repo, rev, sha256 }:
    if (builtins ? "fetchTarball")
    then builtins.fetchTarball {
      inherit sha256;
      url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
    }
    else (import <nixpkgs> {}).fetchFromGitHub { inherit owner repo rev sha256; };
  config = { allowBroken = true; };
in {
  nixpkgsFunc ? import (fetchFromGitHub {
    owner = "nixos";
    repo = "nixpkgs-channels";
    rev = "9d55c1430af72ace3a479d5e0a90451108e774b4";
    sha256 = "05625fwgsa15i2jlsf2ymv3jx68362nf3zqbpnrwq6d3sn89liny";
  }),
  reflex-platform ? import (fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    rev = "aa8a9d1ac3d41ad51fbe04e575d4350da65cf3db";
    sha256 = "0hffy5rcfy6ay06ww3cxykqzp024jfcdg6w1yrmnf0h80sjfgdcm";
  }) { nixpkgsFunc = c: nixpkgsFunc (c // { inherit config; }); }
}:
reflex-platform.project({ pkgs, ... }:
  let
    customOverrides = hlib: self: super: with hlib; {
    };

    dontCheckOverrides = _: super: {
      mkDerivation = args: super.mkDerivation (args // {
        doCheck = false;
        doHaddock = false;
      });
    };
  in {
    useWarp = true;
    withHoogle = false;
    name = "massaraksh";

    packages = {
      massaraksh = ./.;
      massaraksh-examples = ./examples;
    };

    shells = {
      ghc = ["massaraksh" "massaraksh-examples"];
      ghcjs = ["massaraksh" "massaraksh-examples"];
    };

    shellToolOverrides = ghc: super: {
      inherit (pkgs) pkgconfig zlib;
    };
  }
)
