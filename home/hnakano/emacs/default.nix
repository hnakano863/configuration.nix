{ config, pkgs, lib, ... }:
with pkgs;
with lib;
let
  readFile = builtins.readFile;
in
{
  imports = [
    ./packages.nix
    ./langs
    ./org
  ];

  programs.emacs.enable = true;
  programs.emacs.overrides = import ./overrides { inherit pkgs; };
  # programs.emacs.package = emacsGcc;

  programs.emacs.init = {
    enable = true;
    startupTimer = true;

    recommendedGcSettings = true;

    earlyInit = ''
      (push '(menu-bar-lines . 0) default-frame-alist)
      (push '(tool-bar-lines . 0) default-frame-alist)
      (push '(vertical-scroll-bars . nil) default-frame-alist)
      (push '(font . "Cica-14") default-frame-alist)
      (push '(fullscreen . maximized) initial-frame-alist)
    '';

    prelude = readFile ./prelude.el;

    postlude = ''
      (load "${./hydrae.el}")
      (load "${./keybinds.el}")
    '';
  };
}
