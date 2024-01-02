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
    GUIX_LOCPATH = "${config.home.homeDirectory}/.guix-profile/lib/locale";
    GUIX_PROFILE = "${config.home.homeDirectory}/.config/guix/current";
    GUIX_EXTRA_PROFILES = "${config.home.homeDirectory}/.guix-extra-profiles";
  };

  programs.bash = {
    profileExtra = ''
      source "$GUIX_PROFILE/etc/profile"
    '';
  };

  programs.fish.functions = {
    lsprof.body = "ls $GUIX_EXTRA_PROFILES";
  };

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
