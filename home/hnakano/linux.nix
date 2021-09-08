# configuraion fraction specific to linux home.
{ config, pkgs, lib, ... }:
{
  imports = [
    ./polybar
    ./gnome
  ];

  home.packages = with pkgs; [
    alacritty
    anki
    firefox-bin
    thunderbird
    gimp
    vlc
    palemoon
    vivaldi
  ];

  home.sessionVariables = {
    GUIX_LOCPATH = "$HOME/.guix-profile/lib/locale";
  };

  programs.bash.initExtra = ''
    if [[ $SHLVL -eq 1 ]]; then
      exec fish
    fi
  '';

  programs.fish.functions = {
    lsprof.body = ''
      function lsprof;
          ls $GUIX_EXTRA_PROFILES
      end
    '';
  };

  programs.fish.interactiveShellInit = ''
    set -gx GUIX_DEFAULT_PROFILE "$HOME/.guix-profile"
    set -gx GUIX_EXTRA_PROFILES "$HOME/.guix-extra-profiles"
    set -gx GUIX_PROFILE $GUIX_DEFAULT_PROFILE
    bass source "$GUIX_PROFILE/etc/profile"
  '';

  programs.git.userName = "hnakano863";
  programs.git.userEmail = "notchi863@gmail.com";

  programs.rofi = {
    enable = false;
    extraConfig.modi = "drun,window";
    theme = "Pop-Dark";
    terminal = "${pkgs.alacritty}/bin/alacritty";
  };

}
