{
  description = "hs-jev — Haskell client for TypeSafe's System One (Jev) API";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        hs-jev = pkgs.haskellPackages.callCabal2nix "hs-jev" ./. {};
        devShell = hs-jev.env.overrideAttrs (old: {
          nativeBuildInputs = (old.nativeBuildInputs or [])
            ++ [ pkgs.cabal-install pkgs.haskell-language-server ];
        });
      in {
        packages = { inherit hs-jev; default = hs-jev; };
        devShells = { dev = devShell; default = devShell; };
      });
}
