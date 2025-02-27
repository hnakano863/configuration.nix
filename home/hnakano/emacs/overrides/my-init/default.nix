{ trivialBuild
, runCommand
, julia
, epkgs
, my-init-common
}:

let
  src = runCommand "my-init.el" {
    inherit julia;
  } ''substituteAll "${./my-init.el}" $out'';

  deps = import ./deps.nix { inherit epkgs; };
in

trivialBuild {
  inherit src;
  pname = "my-init";
  version = "2025-02-28";
  packageRequires = deps ++ [ my-init-common ];
  preferLocalBuild = true;
  allowSubstitute = false;
}
