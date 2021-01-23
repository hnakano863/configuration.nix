{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.09";
    home-manager.url = "github:nix-community/home-manager/release-20.09";
    nur.url = "github:nix-community/NUR";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs =
    { self
    , nixpkgs
    , home-manager
    , nur
    , emacs-overlay
    }: {
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
            home-manager.users.hnakano = import ./home/hnakano/home.nix;
          }
          {
            nixpkgs.overlays = [
              (import ./overlays)
              # nur.overlay
              emacs-overlay.overlay
            ];
          }

        ];
      };
    };
}
