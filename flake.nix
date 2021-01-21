{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.09";

  outputs = { self, nixpkgs }: {

    nixosConfigurations.bravo = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";

      modules = [
        ./configuration.nix
        ./hardware-configuration.nix
        ./user-configuration.nix
        ./systemd-configuration.nix
        nixpkgs.nixosModules.notDetected
        ({ pkgs, ... }: {
          # Let 'nixos-version --json' know about the Git revision of this flake.
          system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;
          nixpkgs.overlays = [
            (import ./overlays/fonts)
            (import ./overlays/polybar)
          ];
        })
      ];
    };
  };
}
