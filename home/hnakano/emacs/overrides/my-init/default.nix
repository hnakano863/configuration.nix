{ trivialBuild
, runCommand
, gnuplot
, julia
, skkdicts
, epkgs
, my-init-common
}:

let
  init-el = runCommand "my-init.el" {
    inherit gnuplot julia skkdicts;
  } ''substituteAll "${./my-init.el}" $out'';

  deps = import ./deps.nix { inherit epkgs; };
in

trivialBuild {
   pname = "my-init";
   version = "2025-02-23";
   src = init-el;
   packageRequires = deps ++ [ my-init-common ];
   preferLocalBuild = true;
   allowSubstitute = false;
}
