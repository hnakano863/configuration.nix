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
    rev = "716727d0f002cc8568308e3b1452a4cb3fb39322";
    hash = "sha256-vM1/KYR8RpTmPfBIX/SVwpqF9hYMGKBD35JeJX6a4z0=";
  };

  npmDepsHash = "sha256-csbr6a5+hknwcPrfsa71rVa77vcNOAn/1hScFb7h1Mo=";

  dontNpmBuild = true;

  meta = with lib; {
    description = "Look at me sideways";
    mainProgram = "lams";
    license = licenses.mit;
  };
})
