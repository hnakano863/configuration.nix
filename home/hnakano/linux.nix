# configuraion fraction specific to linux home.
{ config, pkgs, lib, pkgs-unstable, ... }:
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
    brave
    dmenu
    haskellPackages.xmobar
    xorg.xmessage
    xmonad-log
    htop
    trayer
    pkgs-unstable.nyxt
    glxinfo
  ];

  home.sessionVariables = {
    GUIX_LOCPATH = "$HOME/.guix-profile/lib/locale";
  };

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

    set -gx GPG_TTY "$(tty)"
  '';

  programs.git = {
    userName = "hnakano863";
    userEmail = "notchi863@gmail.com";
    extraConfig = {
      credential.helper =
        "${pkgs.pass-git-helper.out}/bin/pass-git-helper";
      init.defaultBranch = "main";
      safe.directory = "/home/hnakano/repos/configuration.nix";
    };
  };

  home.file.".config/pass-git-helper/git-pass-mapping.ini".text = ''
  [github.com*]
  target=dev/github.com
  '';

  programs.rofi = {
    enable = true;
    extraConfig.modi = "drun,window";
    theme = "Pop-Dark";
    terminal = "${config.programs.alacritty.package}/bin/alacritty";
  };

  programs.password-store.enable = true;

  services.dropbox.enable = true;

}
