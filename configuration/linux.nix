# configuration fraction specific to linux.

{ config, pkgs, lib, ... } @ attrs:

{
  imports = [
    attrs.nixpkgs.nixosModules.notDetected
    ./common.nix
    ./hardware.nix
    ./guix.nix
  ];

  nix.extraOptions = ''
    experimental-features = nix-command flakes
    keep-outputs = true
    keep-derivations = true
  '';

  boot.isContainer = false;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "bravo"; # Define your hostname.
  networking.networkmanager.enable = true;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp3s0.useDHCP = true;
  networking.interfaces.wlp2s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "ja_JP.UTF-8";
  i18n.inputMethod.enabled = "fcitx5";
  i18n.inputMethod.fcitx5.addons = with pkgs; [ fcitx5-mozc ];
  console = {
    font = "Lat2-Terminus16";
    useXkbConfig = true;
  };

  # Set your time zone.
  time.timeZone = "Asia/Tokyo";

  # List services that you want to enable:
  services.picom = {
    enable = true;
    fade = true;
    inactiveOpacity = 0.8;
    fadeDelta = 6;
    settings.corner-radius = 10;
  };

  services.logind = {
    lidSwitch = "suspend-then-hibernate";
    lidSwitchExternalPower = "lock";
    extraConfig = ''
      HandlePowerKey=ignore
    '';
  };

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  #Enable backlight
  programs.light.enable = true;

  # Enable power management
  powerManagement.enable = true;

  # Enable dconf for gnome
  programs.dconf.enable = false;

  services.gnome.gnome-keyring.enable = true;

  services.smartd.notifications.x11.enable = true;

  services.xserver = {
    # for configuration
    autorun = true;

    # Enable the X11 windowing system.
    enable = true;
    layout = "jp";
    xkbOptions = "ctrl:swapcaps";

    # Enable touchpad support.
    libinput.enable = true;

    # desktop environment
    windowManager.i3 = {
      enable = true;
      extraPackages = with pkgs; [
        networkmanagerapplet
        i3lock
        pavucontrol
        alacritty
      ];
    };

    windowManager.xmonad = {
      enable = true;
      extraPackages = ps: with ps; [ xmonad-contrib ];
    };

    desktopManager.gnome.enable = false;

    displayManager = {
      defaultSession = "none+xmonad";
      lightdm.enable = true;
      gdm.enable = false;
      gdm.wayland = false;
    };
  };

  # Enable opengl
  hardware.opengl.enable = true;

  # home-manager configuration
  home-manager.users.hnakano = { config, pkgs, lib, ... }: {
    imports = [ ../home/hnakano/linux.nix ];
  };

  home-manager.extraSpecialArgs = let
    ps = attrs.nixpkgs-unstable.legacyPackages;
    sys = config.nixpkgs.localSystem.system;
  in {
    pkgs-unstable = ps."${sys}";
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?

}
