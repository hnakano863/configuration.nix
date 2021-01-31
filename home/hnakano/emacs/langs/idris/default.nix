{ config, pkgs, lib, ... }:
{
  programs.emacs.init.usePackage = {
    idris-mode.enable = true;
    idris-mode.mode = [ ''"\\.idr\\'"'' ];
  };
}
