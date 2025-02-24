{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    home-manager.url = "github:nix-community/home-manager/release-24.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    nixos-wsl.url = "github:nix-community/NixOS-WSL";
    nixos-wsl.inputs.nixpkgs.follows = "nixpkgs";

    nix-ld.url = "github:Mic92/nix-ld";
    nix-ld.inputs.nixpkgs.follows = "nixpkgs";

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
    , nix-ld
    , eijiro
    , vscode-server
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

      devShells.x86_64-linux.default = let
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
      in import ./shell.nix { inherit pkgs; };

    };
}
