{ config, pkgs, lib, ... }:
{
  programs.emacs.init.usePackage = {
    python-mode = {
      enable = true;
      mode = [ ''"\\.py\\'"'' ];
      extraConfig = ''
        :custom
        (python-guess-indent nil)
      '';
    };
  };
}
