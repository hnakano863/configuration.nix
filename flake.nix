{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
    home-manager.url = "github:nix-community/home-manager/release-21.05";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    eijiro.url = "path:/home/hnakano/repos/eijiro.nix";
    hnakano863.url = "github:hnakano863/nixos-overlay";
  };

  outputs =
    { self
    , nixpkgs
    , home-manager
    , emacs-overlay
    , eijiro
    , hnakano863
    }:
    let
      lock = builtins.fromJSON (builtins.readFile ./flake.lock);
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
              imports = [ ./home/hnakano/home.nix  ];
            };
          }
          {
            nix.registry.nixpkgs.flake = nixpkgs;
            nixpkgs.overlays = [
              hnakano863.overlay
              emacs-overlay.overlay
              eijiro.overlay
            ];
          }

        ];
      };
    };
}
