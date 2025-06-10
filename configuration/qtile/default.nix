{ config, pkgs, lib, ... }:

{
  services.xserver.windowManager.qtile = {
    enable = true;
    configFile = ./config.py;
  };

  # Additional packages for qtile
  environment.systemPackages = with pkgs; [
    dex
  ];
}
