{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";

    home-manager.url = "github:nix-community/home-manager/release-21.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    nixos-wsl.url = "github:nix-community/NixOS-WSL";
    nixos-wsl.inputs.nixpkgs.follows = "nixpkgs";

    nix-ld.url = "github:Mic92/nix-ld";
    nix-ld.inputs.nixpkgs.follows = "nixpkgs";

    nix-alien.url = "github:thiagokokada/nix-alien";
    nix-alien.inputs.nixpkgs.follows = "nixpkgs";

    hnakano863.url = "github:hnakano863/nixos-overlay";

    eijiro.url = "path:/home/hnakano/ghq/github.com/hnakano/eijiro.nix";
    eijiro.inputs.nixpkgs.follows = "nixpkgs";

  };

  outputs =
    { self
    , nixpkgs
    , home-manager
    , emacs-overlay
    , nixos-wsl
    , nix-ld
    , nix-alien
    , hnakano863
    , eijiro
    } @ attrs:
    let

      commonModules = [
        home-manager.nixosModules.home-manager
        nix-ld.nixosModules.nix-ld
        ./configuration/common.nix
        ./users.nix
      ];

    in {

      nixosConfigurations.bravo = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = attrs;
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
          nixos-wsl.nixosModules.wsl
          ./configuration/wsl2.nix
        ] ++ commonModules;
      };

    };
}
