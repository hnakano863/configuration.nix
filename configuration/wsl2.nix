# configuration fraction specific to wsl2.
{ config, pkgs, lib, ... } @ attrs:

{
  imports = [
    ./common.nix
    attrs.nixos-wsl.nixosModules.wsl
  ];

  # wsl
  wsl = {
    enable = true;
    defaultUser = "hnakano";
    automountPath = "/mnt";
    interop = { register = false; includePath = false; };
    docker-native = { enable = true; addToDockerGroup = true; };
  };

  # nix
  nix.extraOptions = ''
    experimental-features = nix-command flakes
    keep-outputs = false
    keep-derivations = false
  '';

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  i18n.defaultLocale = "ja_JP.UTF-8";
  time.timeZone = "Asia/Tokyo";

  services.xserver.enable = true;
  services.xserver.autorun = false;

  # home-manager configuration
  home-manager.users.hnakano = { config, pkgs, lib, ... }: {
    imports = [ ../home/hnakano/wsl2.nix ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?

}
