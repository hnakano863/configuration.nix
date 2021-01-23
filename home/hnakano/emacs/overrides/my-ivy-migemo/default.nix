{ trivialBuild, migemo, ivy }:
trivialBuild {
  pname = "my-ivy-migemo";
  version = "unstable";
  src = ./my-ivy-migemo.el;
  packageRequires = [ migemo ivy ];
}
