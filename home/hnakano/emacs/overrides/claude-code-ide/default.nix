{ trivialBuild
, fetchFromGitHub
}:

trivialBuild {
  src = fetchFromGitHub {
    owner = "manzaltu";
    repo = "claude-code-ide.el";
    rev = "a9485f766ea69f6cb3a3f08dea20d44fd6596673";
    hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  };
  pname = "claude-code-ide";
  version = "2026-06-02";
  preferLocalBuild = true;
  allowSubstitute = false;
}
