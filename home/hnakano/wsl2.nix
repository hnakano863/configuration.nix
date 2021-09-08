# configuraion fraction specific to wsl2 home.
{ config, pkgs, lib, ... }:

{

  programs.git.userName = "hnakano";
  programs.git.userEmail = "hnakano@tlv.co.jp";

  programs.bash.initExtra = ''
    export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0

    if [[ $SHLVL -eq 1 ]]; then
      exec fish
    fi
  '';

  programs.fish.functions = {
    ec.body = "emacsclient -c";
  };

}
