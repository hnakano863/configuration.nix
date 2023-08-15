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
    vivaldi
    brave
    dmenu
    haskellPackages.xmobar
    xorg.xmessage
    xmonad-log
    htop
    trayer
    nyxt
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
    set -gx GUIX_PROFILE "$HOME/.config/guix/current"
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
    theme = "gruvbox-dark";
    terminal = "${config.programs.alacritty.package}/bin/alacritty";
  };

  programs.password-store.enable = true;

  services.dropbox.enable = true;

}
