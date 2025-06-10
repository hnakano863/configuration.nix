{ config, pkgs, lib, ... }:

{
  services.xserver.windowManager.qtile = {
    enable = true;
  };

  # Additional packages for qtile
  environment.systemPackages = with pkgs; [
    dex
  ];
}
