{ config, pkgs, lib, ... }:

with pkgs lib;

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
    "Pluto"
    "Revise"
  ];

in
{
  home.packages = [
    myJulia
  ];

  home.file.".julia/config/startup.jl".source = startup-jl;
}
