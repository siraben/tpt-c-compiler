{
  description = "Haskell rewrite workspace for the TPT C compiler";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
          haskellPackage =
            pkgs.haskellPackages.callCabal2nix "tpt-c-compiler-hs" ./. { };
        in
        {
          default = haskellPackage;
        });

      apps = forAllSystems (system: {
        default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/tptcc-hs";
        };
      });

      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          default = pkgs.mkShell {
            packages = [
              pkgs.cabal-install
              pkgs.ghc
              pkgs.lua5_4
            ];
          };
        });
    };
}
