{ trivialBuild
, my-init-common
, epkgs
}:

let
  deps = import ./deps.nix { inherit epkgs; };
in

trivialBuild {
  src = ./my-init.el;
  pname = "my-init-linux";
  version = "2025-02-28";
  packageRequires = deps ++ [ my-init-common ];
  preferLocalBuild = true;
  allowSubstitute = false;
}
