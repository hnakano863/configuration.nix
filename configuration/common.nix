# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... } @ attrs:

{
  system.configurationRevision = with attrs; lib.mkIf (self ? rev) self.rev;

  imports = with attrs; [
    home-manager.nixosModules.home-manager
    ./users.nix
  ];

  # use nix unstable and enable nix flake
  nix.package = pkgs.nixFlakes;
  nix.nixPath = [
    "nixpkgs=${attrs.nixpkgs}"
    "nixpkgs-unstable=${attrs.nixpkgs-unstable}"
  ];

  # nix registry config
  nix.registry = {
    nixpkgs.flake = attrs.nixpkgs;
  };

  nixpkgs.config = {
    allowUnfree = true;
    allowBroken = true;
  };

  nixpkgs.overlays = with attrs; [
    emacs-overlay.overlay
    nix-alien.overlays.default
    # eijiro.overlay
    nix-direnv.overlays.default
  ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    git wget vim gnupg mkpasswd psmisc file
    feh docker-compose
  ];

  fonts.enableDefaultPackages = true;
  fonts.enableGhostscriptFonts = true;
  fonts.fontDir.enable = true;
  fonts.packages = with pkgs; [
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    source-han-code-jp
    hackgen-font
    unifont
    siji
    nerdfonts
    material-icons
  ];

  environment.variables = {
    LIBRARY_PATH = with pkgs; builtins.concatStringsSep ":" [
      "${lib.getLib stdenv.cc.cc}/lib"
      "${lib.getLib pkgs.glibc}/lib"
      "${lib.getLib libgccjit}/lib/gcc/x86_64-unknown-linux-gnu/${libgccjit.version}"
    ];
  };

  # enable nix-ld
  programs.nix-ld.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;

  programs.gnupg.agent.enable = true;
  programs.gnupg.agent.pinentryPackage = pkgs.pinentry-tty;

  services.mpd.enable = false;

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # dictd service
  services.dictd = {
    enable = true;
    DBs = with pkgs.dictdDBs; [ wiktionary wordnet eng2jpn ]; # eijiro ];
  };

  services.emacs.enable = true;
  services.emacs.package = config.home-manager.users.hnakano.programs.emacs.finalPackage;

  # enable docker
  virtualisation.docker.enable = true;

  # home-manager configuration
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.hnakano = { config, pkgs, lib, ... }: {
      imports = [ ../home/hnakano/common.nix ];
    };
    extraSpecialArgs = let
      sys = config.nixpkgs.localSystem.system;
    in {
      pkgs-unstable = attrs.nixpkgs-unstable.legacyPackages."${sys}";
    };
  };
}
