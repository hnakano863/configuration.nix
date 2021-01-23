{ config, pkgs, lib, ... }:
{
  programs.emacs.init.usePackage = {
    restclient.enable = true;
    restclient.command = [ "restclient-mode" ];

    ob-restclient.enable = true;
    ob-restclient.defer = true;
  };
}
