{
  inputs = {
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.09";
    home-manager.url = "github:nix-community/home-manager/release-20.09";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs =
    { self
    , nixos-unstable
    , nixpkgs
    , home-manager
    , emacs-overlay
    }:
    let
      lock = builtins.fromJSON (builtins.readFile ./flake.lock);
      pkgs-unstable = import nixos-unstable {
        system = "x86_64-linux";
        overlays = [ (import ./overlays) ];
        config.allowUnfree = true;
      };
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
                ./home/hnakano/home.nix
              ];
              home.packages = [ pkgs-unstable.vivaldi ];
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
