{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
    home-manager.url = "github:nix-community/home-manager/release-21.11";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    eijiro.url = "path:/home/hnakano/ghq/github.com/hnakano/eijiro.nix";
    hnakano863.url = "github:hnakano863/nixos-overlay";
    nixos-wsl2.url = "github:hnakano863/NixOS-WSL";
    nix-ld = {
      url = "github:Mic92/nix-ld";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-alien = {
      url = "github:thiagokokada/nix-alien";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self
    , nixpkgs
    , home-manager
    , emacs-overlay
    , eijiro
    , hnakano863
    , nixos-wsl2
    , nix-ld
    , nix-alien
    }:
    let
      lock = builtins.fromJSON (builtins.readFile ./flake.lock);

      commonModules = [

        nix-ld.nixosModules.nix-ld
        home-manager.nixosModules.home-manager
        ./configuration/common.nix
        ./users.nix

        { system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev; }

        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
        }

        {
          nix.registry.nixpkgs.flake = nixpkgs;
          nixpkgs.overlays = [
            hnakano863.overlay
            emacs-overlay.overlay
            eijiro.overlay
            nix-alien.overlay
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
          {
            home-manager.users.hnakano = { config, pkgs ? pkgs, lib, ... }: {
              imports = [ ./home/hnakano/common.nix ./home/hnakano/linux.nix ];
            };
          }
        ] ++ commonModules;
      };

      nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";

        modules = [
          nixos-wsl2.nixosModule
          ./configuration/wsl2.nix

          { wsl2.enable = true; wsl2.defaultUser = "hnakano"; }
          {
            home-manager.users.hnakano = { config, pkgs ? pkgs, lib, ... }: {
              imports = [ ./home/hnakano/common.nix ./home/hnakano/wsl2.nix ];
            };
          }
        ] ++ commonModules;
      };
    };
}
