# configuraion fraction specific to wsl2 home.
{ config, pkgs, lib, pkgs-unstable, ... }:

{

  home.packages = with pkgs; [
    gopass
    git-credential-gopass
    awscli2
    pkgs-unstable.google-cloud-sdk
  ];

  programs.git = {
    userName = "hnakano863";
    userEmail = "notchi863@gmail.com";
    extraConfig.credential.helper = "gopass";
  };

  programs.fish.functions = {
    ec.body = "emacsclient -c";
  };

  programs.fish.interactiveShellInit = ''
    set -gx DISPLAY (cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0
    set -gx GPG_TTY (tty)
  '';
}
