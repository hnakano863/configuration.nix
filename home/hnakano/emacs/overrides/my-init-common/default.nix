{ trivialBuild
, runCommand
, skkdicts
, epkgs
}:

let
  src = runCommand "my-init-common.el" {
    inherit skkdicts;
  } ''substituteAll "${./my-init-common.el}" $out'';

  deps = import ./deps.nix { inherit epkgs; };
in

trivialBuild {
   inherit src;
   pname = "my-init-common";
   version = "2025-02-23";
   packageRequires = deps;
   preferLocalBuild = true;
   allowSubstitute = false;
}
