{ config, pkgs, lib, ... }:

with pkgs;
with lib;

let

  startup-jl = runCommand "startup.jl" {
    inherit jupyterCommand;
  } ''substituteAll "${./startup.jl.in}" $out '';

  myJulia = julia.withPackages [
    "BenchmarkTools"
    "DrWatson"
    "IJulia"
    "LanguageServer"
    "OhMyREPL"
    # "Pluto"
    "Revise"
    "JLD2" # required for DrWatson
  ];

in
{
  home.packages = [
    myJulia
  ];

  home.file.".julia/config/startup.jl".source = startup-jl;
}
