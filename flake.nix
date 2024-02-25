{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/release-23.11";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in rec {
        packages.nates-mazes = pkgs.stdenv.mkDerivation {
          name = "nates-mazes";
          src = ./.;

          buildInputs = [ pkgs.elmPackages.elm ];

          buildPhase = pkgs.elmPackages.fetchElmDeps {
            elmPackages = import ./nix/elm-srcs.nix;
            elmVersion = "0.19.1";
            registryDat = ./nix/registry.dat;
          };

          installPhase = ''
            mkdir -p $out/share/nates-mazes
            elm make src/Main.elm --output $out/share/nates-mazes/index.html
          '';
        };
        defaultPackage = packages.nates-mazes;

        overlay = final: prev: { nates-mazes = packages.nates-mazes; };

        devShell = pkgs.mkShell {
          packages = [
            pkgs.elm2nix
            pkgs.elmPackages.elm
            pkgs.elmPackages.elm-format
            pkgs.elmPackages.elm-live
            pkgs.elmPackages.elm-test
            pkgs.elmPackages.elm-json
          ];
        };
      });
}
