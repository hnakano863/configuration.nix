{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    home-manager.url = "github:nix-community/home-manager/release-24.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    nixos-wsl.url = "github:nix-community/NixOS-WSL";
    nixos-wsl.inputs.nixpkgs.follows = "nixpkgs";

    eijiro.url = "path:/home/hnakano/ghq/github.com/hnakano/eijiro.nix";
    eijiro.inputs.nixpkgs.follows = "nixpkgs";

    vscode-server.url = "github:nix-community/nixos-vscode-server";

  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-unstable
    , home-manager
    , emacs-overlay
    , nixos-wsl
    , eijiro
    , vscode-server
    }:

    let
      overlays = { config, pkgs, ... }:
        let
          unstable-overlay = final: prev: {
            unstable = import nixpkgs-unstable {
              inherit (config.nixpkgs) system config;
            };
          };
        in {
          nixpkgs.overlays = [
            emacs-overlay.overlay
            eijiro.overlay
            unstable-overlay
            (import ./overlays)
          ];
        };
    in {
      nixosConfigurations.bravo = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit self nixpkgs nixpkgs-unstable; };
        modules = [
          home-manager.nixosModules.home-manager
          overlays
          nixpkgs.nixosModules.notDetected
          ./configuration/linux.nix
        ];
      };

      nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit self nixpkgs nixpkgs-unstable; };
        modules = [
          home-manager.nixosModules.home-manager
          overlays
          nixos-wsl.nixosModules.wsl
          vscode-server.nixosModules.default
          ./configuration/wsl2.nix
        ];
      };

      devShells.x86_64-linux.default = let
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
      in import ./shell.nix { inherit pkgs; };

    };
}
