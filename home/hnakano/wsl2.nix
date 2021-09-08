# configuraion fraction specific to wsl2 home.
{ config, pkgs, lib, ... }:

{

  home.packages = with pkgs; [
    gopass
    git-credential-gopass
  ];

  programs.git = {
    userName = "hnakano";
    userEmail = "hnakano@tlv.co.jp";
    extraConfig.credential.helper = "gopass";
  };

  programs.bash.initExtra = ''
    export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0
    export GPG_TTY=$(tty)

    if [[ $SHLVL -eq 1 ]]; then
      exec fish
    fi
  '';

  programs.fish.functions = {
    ec.body = "emacsclient -c";
  };

}
