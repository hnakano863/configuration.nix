# configuraion fraction specific to wsl2 home.
{ config, pkgs, lib, ... }:

{

  programs.git.userName = "hnakano";
  programs.git.userEmail = "hnakano@tlv.co.jp";

  programs.bash.profileExtra = ''
    export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0
  '';

}
