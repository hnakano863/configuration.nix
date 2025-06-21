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
      rev = "3002053bf0129717cf37c5e973eb50a0983c8af9";
      hash = "sha256-/e32RKNOrRuJKzOvxI/F13WHFBlomN70k2HZ4i6f29E=";
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
