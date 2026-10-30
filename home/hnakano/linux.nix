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
    thunderbird
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

  home.sessionVariables = {
    # GUIX_LOCPATH = "${config.home.homeDirectory}/.guix-profile/lib/locale";
    # GUIX_PROFILE = "${config.home.homeDirectory}/.config/guix/current";
    # GUIX_EXTRA_PROFILES = "${config.home.homeDirectory}/.guix-extra-profiles";
  };

  programs.bash = {
    # profileExtra = lib.mkAfter ''
    #   source "$GUIX_PROFILE/etc/profile"
    # '';
    # シェルの起動時スクリプトは共通化しない
    initExtra = lib.mkAfter ''
      if [ $SHLVL -eq 1 ]; then
        exec fish
      fi
    '';
  };

  programs.fish.functions = {
    lsprof.body = "ls $GUIX_EXTRA_PROFILES";
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
