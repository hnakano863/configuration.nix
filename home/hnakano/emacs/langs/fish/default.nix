{ config, pkgs, lib, ... }:
{
  programs.emacs.init.usePackage = {
    fish-mode.enable = true;
    fish-mode.mode = [ ''"\\.fish\\'"'' ];
  };
}
