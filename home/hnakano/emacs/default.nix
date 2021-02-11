{ config, pkgs, lib, ... }:
with pkgs;
with lib;
let
  init-el = runCommand "my-init.el" {
    inherit python3 gnuplot idris jupyterCmdFHS sqlite graphviz;
    rnixlsp = rnix-lsp;
    juliabin = julia-bin;
    skkdicts = skk-dicts;
  } ''substituteAll "${./my-init.el}" $out '';
in
{
  imports = [
    ./compileInit.nix
    ./extraPackages.nix
  ];

  programs.emacs.enable = true;
  programs.emacs.overrides = import ./overrides { inherit pkgs; };
  # programs.emacs.package = emacsGcc;

  programs.emacs.compileInit = {
    enable = true;
    initFile = init-el;
    earlyInitFile = ./my-early-init.el;
  };
}
