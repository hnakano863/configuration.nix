{ config, pkgs, lib, pkgs-unstable, ... }:
with pkgs;
with lib;
let
  startup-jl = runCommand "startup.jl" {
    inherit jupyterCommand;
  } ''substituteAll "${./startup.jl.in}" $out '';
in
{
  home.packages = [
    pkgs-unstable.julia
  ];

  home.file.".julia/config/startup.jl".source = startup-jl;
  home.file.".julia/environments/v1.9".source = ./environments/v1.9;
}
