# configuraion fraction specific to linux home.
{ config, pkgs, lib, ... }:
{
  imports = [
    ./polybar
    #./gnome
    ./skk
    ./alacritty.nix
  ];

  home.packages = with pkgs; [
    anki
    firefox-bin
    thunderbird
    gimp
    vlc
    palemoon
    vivaldi
    dmenu
  ];

  home.sessionVariables = {
    GUIX_LOCPATH = "$HOME/.guix-profile/lib/locale";
  };

  programs.bash.initExtra = ''
    if [[ -z "$IN_NIX_SHELL" || $SHLVL = 1 ]]; then
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
    terminal = "${config.programs.alacritty.package}/bin/alacritty";
  };

  services.dropbox.enable = true;

}
