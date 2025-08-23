{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    nixos-wsl.url = "github:nix-community/NixOS-WSL";
    nixos-wsl.inputs.nixpkgs.follows = "nixpkgs";

    eijiro.url = "path:/home/hnakano/ghq/github.com/hnakano/eijiro.nix";
    eijiro.inputs.nixpkgs.follows = "nixpkgs";

    vscode-server.url = "github:nix-community/nixos-vscode-server";

    julia-registry.url = "github:codedownio/General";
    julia-registry.flake = false;

    emacs-lean4-mode-src.url = "github:leanprover-community/lean4-mode";
    emacs-lean4-mode-src.flake = false;

    skktools-unstable-src.url = "github:skk-dev/skktools";
    skktools-unstable-src.flake = false;

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
    , julia-registry
    , emacs-lean4-mode-src
    , skktools-unstable-src
    }:

    let

      overlays-module = { config, pkgs, ... }:
        let
          flake-input-overlay = final: prev: {

            unstable = import nixpkgs-unstable {
              inherit (config.nixpkgs) system config;
            };

            inherit julia-registry emacs-lean4-mode-src skktools-unstable-src;

          };
        in {
          nixpkgs.overlays = [
            emacs-overlay.overlay
            eijiro.overlay
            flake-input-overlay
            (import ./overlays)
          ];
        };

      flake-input-config = { config, pkgs, lib, ... }: {

        system.configurationRevision = lib.mkIf (self ? rev) self.rev;

        nix.registry.nixpkgs.flake = nixpkgs;

        nix.nixPath = [
          "nixpkgs=${nixpkgs}"
          "nixpkgs-unstable=${nixpkgs-unstable}"
        ];

      };

    in {
      nixosConfigurations.bravo = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          home-manager.nixosModules.home-manager
          overlays-module
          nixpkgs.nixosModules.notDetected
          flake-input-config
          ./configuration/linux.nix
        ];
      };

      nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          home-manager.nixosModules.home-manager
          overlays-module
          nixos-wsl.nixosModules.wsl
          vscode-server.nixosModules.default
          flake-input-config
          ./configuration/wsl2.nix
        ];
      };

      devShells.x86_64-linux.default = let
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
      in import ./shell.nix { inherit pkgs; };

    };
}
