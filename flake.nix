{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    home-manager.url = "github:nix-community/home-manager/release-23.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    nixos-wsl.url = "github:nix-community/NixOS-WSL";
    nixos-wsl.inputs.nixpkgs.follows = "nixpkgs";

    nix-direnv.url = "github:nix-community/nix-direnv";
    nix-direnv.inputs.nixpkgs.follows = "nixpkgs-unstable";

    nix-ld.url = "github:Mic92/nix-ld";
    nix-ld.inputs.nixpkgs.follows = "nixpkgs";

    nix-alien.url = "github:thiagokokada/nix-alien";
    nix-alien.inputs.nixpkgs.follows = "nixpkgs";

    hnakano863.url = "github:hnakano863/nixos-overlay";

    eijiro.url = "path:/home/hnakano/ghq/github.com/hnakano/eijiro.nix";
    eijiro.inputs.nixpkgs.follows = "nixpkgs";

    lean4.url = "github:leanprover/lean4";
    lean4.inputs.lean4-mode = {
      url = "github:leanprover/lean4-mode";
      flake = false;
    };

  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-unstable
    , home-manager
    , emacs-overlay
    , nixos-wsl
    , nix-direnv
    , nix-ld
    , nix-alien
    , hnakano863
    , eijiro
    , lean4
    } @ attrs:

    {
      nixosConfigurations.bravo = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = attrs;
        modules = [ ./configuration/linux.nix ];
      };

      nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = attrs;
        modules = [ ./configuration/wsl2.nix ];
      };

    };
}
