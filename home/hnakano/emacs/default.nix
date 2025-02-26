{ config, pkgs, ... }:
with pkgs;
{
  programs.emacs.enable = true;
  programs.emacs.overrides = import ./overrides { inherit pkgs; };
  programs.emacs.package = emacs29;

  programs.emacs.extraPackages = epkgs: with epkgs; [
    my-early-init
    my-init-common
  ];

  home.file = {
    ".emacs.d/early-init.el".text = ''
      (require 'my-early-init)
      (provide 'early-init)
    '';

    ".emacs.d/init.el".text = ''
      (require 'my-init-common)
      (provide 'init)
    '';
  };

}
