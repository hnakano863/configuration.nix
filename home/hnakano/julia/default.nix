{ config, pkgs, lib, ... }:
with pkgs;
with lib;
let
  pyCallEnv = python3.withPackages ( ps: with ps; [ matplotlib ]);
  startup-jl = runCommand "startup.jl" {
    inherit jupyterCommand pyCallEnv;
    inherit (pyCallEnv) sitePackages;
  } ''substituteAll "${./startup.jl.in}" $out '';
in
{
  home.packages = [
    julia-bin
  ];

  home.file.".julia/config/startup.jl".source = startup-jl;
  home.file.".julia/environments/v1.6".source = ./environments/v1.6;
}
