{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
    home-manager.url = "github:nix-community/home-manager/release-21.05";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    eijiro.url = "path:/home/hnakano/ghq/github.com/hnakano/eijiro.nix";
    hnakano863.url = "github:hnakano863/nixos-overlay";
    nixos-wsl.url = "github:hnakano863/NixOS-WSL/develop";
  };

  outputs =
    { self
    , nixpkgs
    , home-manager
    , emacs-overlay
    , eijiro
    , hnakano863
    , nixos-wsl
    }:
    let
      lock = builtins.fromJSON (builtins.readFile ./flake.lock);

      commonModules = [

        home-manager.nixosModules.home-manager
        ./configuration/common.nix
        ./users.nix

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

    in {
      nixosConfigurations.bravo = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";

        modules = [
          nixpkgs.nixosModules.notDetected
          ./configuration/linux.nix
          ./hardware.nix
          ./guix.nix
        ] ++ commonModules;
      };

      nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";

        modules = [
          nixos-wsl.nixosModule
          ./configuration/wsl2.nix

          { wsl2.enable = true; wsl2.defaultUser = "hnakano"; }
        ] ++ commonModules;
      };
    };
}
