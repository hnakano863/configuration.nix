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
    rev = "6a9262552f416c3205a7c5fddc63516108fd702f";
    hash = "sha256-V5RexJ19QvPFCuiiKOryuoKLlvOKI0hhte36rtBYr2E=";
  };

  npmDepsHash = "sha256-csbr6a5+hknwcPrfsa71rVa77vcNOAn/1hScFb7h1Mo=";

  dontNpmBuild = true;

  meta = with lib; {
    description = "Look at me sideways";
    mainProgram = "lams";
    license = licenses.mit;
  };
})
