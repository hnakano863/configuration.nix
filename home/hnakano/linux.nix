# configuraion fraction specific to linux home.
{ config, pkgs, lib, ... }:
{
  imports = [
    ./common.nix
    ./alacritty.nix
    ./language/linux
  ];

  home.packages = with pkgs; [
    firefox-bin
    gimp
    vlc
    dmenu
    rofi-power-menu
    xmessage
    htop
    trayer
    anki
    gnome-tweaks
    brightnessctl
  ];

  programs.bash = {
    # シェルの起動時スクリプトは共通化しない
    initExtra = lib.mkAfter ''
      if [ $SHLVL -eq 1 ]; then
        exec fish
      fi
    '';
  };

  programs.emacs.extraPackages = epkgs: with epkgs; [
    my-early-init
    my-init-linux
  ];

  programs.rofi = {
    enable = true;
    extraConfig.modi = "drun,window,power-menu:rofi-power-menu";
    theme = "gruvbox-dark";
    terminal = "${config.programs.alacritty.package}/bin/alacritty";
  };

  programs.autorandr.enable = true;

  services.dropbox.enable = true;

  programs.password-store.settings = {
    PASSWORD_STORE_DIR = "${config.home.homeDirectory}/.local/share/password-store";
  };

}
