{ trivialBuild
, benchmark-init
}:

trivialBuild {
  pname = "my-init";
  version = "2025-02-23";
  src = ./my-early-init.el;

  packageRequires = [
    benchmark-init
  ];

  preferLocalBuild = true;
  allowSubstitute = false;
}
