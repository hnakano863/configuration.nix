{ config, pkgs, lib, ... }:
with pkgs;
with lib;
let
  init-el = runCommand "init.el" {
    inherit python3 skk-dicts rnix-lsp gnuplot idris;
    inherit julia-bin jupyterCmdFHS sqlite graphviz;
  } ''substituteAll "${./init.el}" $out '';
in
{
  imports = [ ./extraPackages ];

  programs.emacs.enable = true;
  programs.emacs.overrides = import ./overrides { inherit pkgs; };
  # programs.emacs.package = emacsGcc;

  home.file.".emacs.d/early-init.el".source = ./early-init.el;
  home.file.".emacs.d/init.el".source = init-el;
}
