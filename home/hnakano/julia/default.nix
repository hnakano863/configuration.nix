{ config, pkgs, lib, ... }:
with pkgs;
with lib;
let
  pyCallEnv = python3.withPackages ( ps: with ps; [ matplotlib ]);
  startup-jl = runCommand "startup.jl" {
    inherit jupyterCmdFHS pyCallEnv;
  } ''substituteAll "${./startup.jl.in}" $out '';
in
{
  home.packages = [
    julia-bin
  ];

  home.file.".julia/config/startup.jl".source = startup-jl;
}
