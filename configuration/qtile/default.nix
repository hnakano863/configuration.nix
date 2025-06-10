{ config, pkgs, lib, ... }:

{
  services.xserver.windowManager.qtile = {
    enable = true;
  };

  # Additional packages for qtile
  environment.systemPackages = with pkgs; [
    dex
  ];

  # config.py
  home-manager.users.hnakano = { config, pkgs, lib, ... }: {
    home.file.".config/qtile/config.py".source = ./config.py;
  };

}
