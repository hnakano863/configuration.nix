{ trivialBuild
, epkgs
}:

let deps = import ./deps.nix { inherit epkgs; }; in

trivialBuild {
   pname = "my-init-common";
   version = "2025-02-23";
   src = ./my-init-common.el;
   packageRequires = deps;
   preferLocalBuild = true;
   allowSubstitute = false;
}
