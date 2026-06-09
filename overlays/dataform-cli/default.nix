{ buildNpmPackage
, fetchFromGitHub
, lib
}:

buildNpmPackage (finalAttrs: {
  pname = "@dataform/cli";
  version = "3.0.59";

  src = fetchFromGitHub {
    owner = "dataform-co";
    repo = "dataform";
    tag = "${finalAttrs.version}";
    hash = lib.fakeHash;
  };

  npmDepsHash = lib.fakeHash;
})
