# configuration fraction specific to wsl2.
{ config, pkgs, lib, ... }:

{
  imports = [ ./common.nix ];

  # wsl
  wsl = {
    enable = true;
    defaultUser = "hnakano";
    wslConf.automount.root = "/mnt";
    interop = { register = true; includePath = false; };
  };

  # extra settings for WSL2
  nix.settings = {
    keep-outputs = false;
    keep-derivations = false;
  };

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  i18n.defaultLocale = "ja_JP.UTF-8";
  time.timeZone = "Asia/Tokyo";

  services.xserver.enable = true;
  services.xserver.autorun = true;

  # vscode
  services.vscode-server.enable = true;

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
