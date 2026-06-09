{ buildNpmPackage
, fetchFromGitHub
, lib
}:

buildNpmPackage (finalAttrs: {
  pname = "lookml-parser";
  version = "7.1.0";

  src = fetchFromGitHub {
    owner = "fabio-looker";
    repo = "node-lookml-parser";
    rev = "b7d5afa02a704f6a02d9a0e668223dabf1404e70";
    hash = lib.fakeHash;
  };

  npmDepsHash = lib.fakeHash;
})
