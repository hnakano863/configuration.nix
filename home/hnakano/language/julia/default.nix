{ config, pkgs, lib, ... }:
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
  home.file.".julia/environments/v1.8".source = ./environments/v1.8;
}
