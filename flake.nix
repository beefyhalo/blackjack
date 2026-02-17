{
  description = "blackjack";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hsPkgs = pkgs.haskell.packages.ghc966;
        blackjack = hsPkgs.callCabal2nix "blackjack" ./. { };
      in
      {
        packages.default = blackjack;
        packages.blackjack = blackjack;

        apps.default = {
          type = "app";
          program = "${blackjack}/bin/blackjack";
        };
        apps.blackjack-webapp = {
          type = "app";
          program = "${blackjack}/bin/blackjack-webapp";
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            hsPkgs.ghc
            hsPkgs.cabal-install
            hsPkgs.haskell-language-server
            hsPkgs.hlint
            pkgs.stack
            pkgs.zlib
          ];
        };
      }
    );
}
