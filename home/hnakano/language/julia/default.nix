{ config, pkgs, lib, ... }:

with pkgs;
with lib;

let

  startup-jl = runCommand "startup.jl" {
    inherit jupyterCommand;
  } ''substituteAll "${./startup.jl.in}" $out '';

  myJulia = (julia.withPackages.override {
    augmentedRegistry = fetchFromGitHub {
      owner = "CodeDownIO";
      repo = "General";
      rev = "8931e0b2c4359c8764c9911ef82ca1e21f9e21e1";
      hash = "sha256-rTkDLAiiQj2LDyndi+aPNCPvZBeuAsjsU8tGKh1A8/8=";
    };
  }) [
    "BenchmarkTools"
    "DrWatson"
    "IJulia"
    "LanguageServer"
    "OhMyREPL"
    "Pluto"
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
