{ config, pkgs, lib, ... }:
with pkgs;
{
  programs.emacs.init.usePackage = {
    gnuplot = {
      enable = true;
      extraConfig = ''
        :custom
        (gnuplot-program "${gnuplot}/bin/gnuplot")
      '';
      mode = [ ''("\\.gp\\'" . gnuplot-mode)'' ];
    };

    org-babel-gnuplot = {
      enable = true;
      defer = true;
    };
  };
}
