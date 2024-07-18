# configuraion fraction specific to linux home.
{ config, pkgs, lib, pkgs-unstable, ... }:
{
  imports = [
    ./skk
    ./alacritty.nix
  ];

  home.packages = with pkgs; [
    firefox-bin
    thunderbird
    gimp
    vlc
    myVivaldi
    brave
    dmenu
    rofi-power-menu
    xorg.xmessage
    htop
    trayer
    nyxt
    glxinfo
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
      eval "$(${pkgs.direnv}/bin/direnv hook bash)"
      exec fish
    '';
  };

  programs.fish.functions = {
    lsprof.body = "ls $GUIX_EXTRA_PROFILES";
  };

  programs.git = {
    extraConfig.safe.directory = "/home/hnakano/repos/configuration.nix";
  };

  programs.rofi = {
    enable = true;
    extraConfig.modi = "drun,window,power-menu:rofi-power-menu";
    theme = "gruvbox-dark";
    terminal = "${config.programs.alacritty.package}/bin/alacritty";
  };

  programs.autorandr.enable = true;

  services.dropbox.enable = true;

  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "text/html" = "vivaldi-stable.desktop";
      "x-scheme-handler/http" = "vivaldi-stable.desktop";
      "x-scheme-handler/https" = "vivaldi-stable.desktop";
      "x-scheme-handler/about" = "vivaldi-stable.desktop";
      "x-scheme-handler/unknown" = "vivaldi-stable.desktop";
    };
  };

}
