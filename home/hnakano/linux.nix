# configuraion fraction specific to linux home.
{ config, pkgs, lib, ... }:
{
  imports = [
    ./polybar
    ./gnome
    ./skk
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

  programs.bash.profileExtra = with pkgs; ''
    ${yaskkserv2}/bin/yaskkserv2 --config-filename ${config.home.homeDirectory}/.config/yaskkserv2/yaskkserv2.conf
  '';

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

  programs.git = {
    package = pkgs.git.override { withLibsecret = true; };
    userName = "hnakano863";
    userEmail = "notchi863@gmail.com";
    extraConfig.credential.helper =
      "${config.programs.git.package}/bin/git-credential-libsecret";
  };

  programs.rofi = {
    enable = true;
    extraConfig.modi = "drun,window";
    theme = "Pop-Dark";
    terminal = "${pkgs.alacritty}/bin/alacritty";
  };

  services.dropbox.enable = true;

}
