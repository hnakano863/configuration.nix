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
    hash = "sha256-gzAfccj6aot2EbCmXdPKplhV/lDKeH7MaiKbzSkxzDI=";
  };

  npmDepsHash = "sha256-csbr6a5+hknwcPrfsa71rVa77vcNOAn/1hScFb7h1Mo=";

  dontNpmBuild = true;

  meta = with lib; {
    description = "Look at me sideways";
    mainProgram = "lams";
    license = licenses.mit;
  };
})
