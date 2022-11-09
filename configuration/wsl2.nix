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
    interop = { register = false; includePath = false; };
  };

  # nix
  nix.extraOptions = ''
    experimental-features = nix-command flakes
    keep-outputs = false
    keep-derivations = false
  '';
  nix.autoOptimiseStore = true;
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 60d";
  };

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

}
