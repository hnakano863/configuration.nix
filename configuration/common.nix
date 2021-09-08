# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  # use nix unstable and enable nix flake
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';
  };

  nixpkgs.config = {
    allowUnfree = true;
    allowBroken = true;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    git wget vim gnupg mkpasswd psmisc file
    feh docker-compose
  ];

  fonts.enableDefaultFonts = true;
  fonts.fontDir.enable = true;
  fonts.fonts = with pkgs; [
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    source-han-code-jp
    cica
    unifont
    siji
    nerdfonts
    feather-icon-font
    material-icons
  ];

  environment.variables = {
    LIBRARY_PATH = with pkgs; builtins.concatStringsSep ":" [
      "${lib.getLib stdenv.cc.cc}/lib"
      "${lib.getLib stdenv.glibc}/lib"
      "${lib.getLib libgccjit}/lib/gcc/x86_64-unknown-linux-gnu/9.3.0"
    ];
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  #   pinentryFlavor = "gnome3";
  # };

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
    DBs = with pkgs.dictdDBs; [ wiktionary wordnet eng2jpn eijiro ];
  };

  services.emacs.enable = true;
  services.emacs.package = config.home-manager.users.hnakano.programs.emacs.finalPackage;

  # enable docker
  virtualisation.docker.enable = true;
}
