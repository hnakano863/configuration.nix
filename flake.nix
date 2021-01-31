{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.09";
    home-manager.url = "github:nix-community/home-manager/release-20.09";
    rycee.url = "gitlab:rycee/nur-expressions/master";
    rycee.flake = false;
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs =
    { self
    , nixpkgs
    , home-manager
    , rycee
    , emacs-overlay
    }:
    let
      lock = builtins.fromJSON (builtins.readFile ./flake.lock);
      ryceeNurExpressions = import (fetchTarball {
        url = "https://gitlab.com/rycee/nur-expressions/-/archive/${lock.nodes.rycee.locked.rev}/nur-expressions-${lock.nodes.rycee.locked.rev}.tar.gz";
        sha256 = lock.nodes.rycee.locked.narHash;
      });
    in
    {
      nixosConfigurations.bravo = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";

        modules = [

          nixpkgs.nixosModules.notDetected
          home-manager.nixosModules.home-manager

          ./configuration.nix
          ./hardware.nix
          ./users.nix
          ./guix.nix

          { system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev; }
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.hnakano = { config, pkgs ? pkgs, lib, ... }: {
              imports = [
                (ryceeNurExpressions { inherit pkgs; }).hmModules.emacs-init
                ./home/hnakano/home.nix
              ];
            };
          }
          {
            nix.registry.nixpkgs.flake = nixpkgs;
            nixpkgs.overlays = [
              (import ./overlays)
              emacs-overlay.overlay
            ];
          }

        ];
      };
    };
}
