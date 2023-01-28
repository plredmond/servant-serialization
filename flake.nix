{
  description = "servant-serialization";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-22.05;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system: {
    devShell = self.defaultPackage.${system}.envFunc { withHoogle = true; };
    defaultPackage =
      let
        pkgs = import nixpkgs { inherit system; };
        src = pkgs.nix-gitignore.gitignoreSource [ "*.nix" "result" "build-env" "*.cabal" "dist/" ] ./.;
        drv = pkgs.haskellPackages.callCabal2nix "servant-serialization" src { };
      in
      drv;
  });
}
