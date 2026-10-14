{ config, pkgs, ... }:
with pkgs;
{
  programs.emacs.enable = true;
  programs.emacs.overrides = import ./overrides { inherit pkgs; };
  programs.emacs.package = unstable.emacs;

  home.file = {
    ".emacs.d/early-init.el".text = ''
      ;;; early-init.el -*- lexical-binding: t; -*-
      (require 'my-early-init)
      (provide 'early-init)
    '';

    ".emacs.d/init.el".text = ''
      ;;; init.el -*- lexical-binding: t; -*-
      (require 'my-init)
      (provide 'init)
    '';
  };

}
