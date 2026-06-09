{ buildNpmPackage
, fetchFromGitHub
, lib
}:

buildNpmPackage (finalAttrs: {
  pname = "@looker/look-at-me-sideways";
  version = "5.0.0";

  src = fetchFromGitHub {
    owner = "looker-open-source";
    repo = "look-at-me-sideways";
    rev = "a35cb309d2ac603d3ad62e556c36725c31c2f907";
    hash = lib.fakeHash;
  };

  npmDepsHash = lib.fakeHash;
})
