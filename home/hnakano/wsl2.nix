# configuraion fraction specific to wsl2 home.
{ config, pkgs, lib, ... }:

{

  home.packages = with pkgs; [
    gopass
    git-credential-gopass
    awscli2
  ];

  programs.git = {
    userName = "hnakano";
    userEmail = "hnakano@tlv.co.jp";
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
