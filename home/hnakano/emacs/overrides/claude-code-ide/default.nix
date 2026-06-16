{ trivialBuild
, fetchFromGitHub
, websocket
}:

trivialBuild {
  src = fetchFromGitHub {
    owner = "manzaltu";
    repo = "claude-code-ide.el";
    rev = "a9485f766ea69f6cb3a3f08dea20d44fd6596673";
    hash = "sha256-6kaTPI2CCsdxxiCpZ7qqciv/HJCQRsJ8084+SqW8Idc=";
  };
  pname = "claude-code-ide";
  version = "2026-06-02";
  packageRequires = [ websocket ];
  preferLocalBuild = true;
  allowSubstitute = false;
}
