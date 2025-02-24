{ config, pkgs, lib, pkgs-unstable, ... }:
with pkgs;
with lib;
let
  init-el = runCommand "my-init.el" {
    inherit gnuplot julia;
    skkdicts = skkDictionariesUtf8Cdb.combined;
  } ''substituteAll "${./my-init.el}" $out '';
in
{
  imports = [
    ./compileInit.nix
    ./extraPackages.nix
    ./orgProtocol.nix
  ];

  programs.emacs.enable = true;
  programs.emacs.overrides = import ./overrides { inherit pkgs; };
  programs.emacs.package = emacs29;

  programs.emacs.compileInit = {
    enable = true;
    initFile = init-el;
    earlyInitFile = ./my-early-init.el;
  };

  programs.emacs.orgProtocol.enable = true;
}
