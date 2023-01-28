{
  description = "servant-serialization";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-22.05;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      src = pkgs.nix-gitignore.gitignoreSource [ "*.nix" "result" "build-env" "*.cabal" "dist/" ] ./.;
      drv = pkgs.haskellPackages.callCabal2nix "servant-serialization" src { };
    in
    {
      packages.default = drv; # Can reference with: self.packages.${system}.default;
      packages.sdistTarball = pkgs.haskell.lib.sdistTarball drv;
      packages.docsTarball = pkgs.haskell.lib.documentationTarball drv;
      devShells.default = drv.env;
      devShells.hoogle = drv.envFunc { withHoogle = true; };
    });
}
