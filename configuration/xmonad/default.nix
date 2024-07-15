# xmonad and xmobar settings
{ config, pkgs, lib, ... }:

{
  windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = builtins.readFile ./xmonad.hs;
  };

  # xmobar configuration
  home-manager.users.hnakano = { config, pkgs, lib, ... }: {

    home.packages = with pkgs; [
      haskellPackages.xmobar
      xmonad-log
    ];

    home.file.".xmobarrc".source = ./xmobarrc;
  };
}
