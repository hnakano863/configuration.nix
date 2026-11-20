{ trivialBuild
, fetchFromGitHub
, websocket
}:

trivialBuild {
  src = fetchFromGitHub {
    owner = "manzaltu";
    repo = "claude-code-ide.el";
    rev = "1f583c36b332c1ae05c808a1ee504965dd9e213b";
    hash = "sha256-nsUXjTIDRub6ZZEQoZZjAGHfg/nX3HfZmD5xTOht6a0=";
  };
  pname = "claude-code-ide";
  version = "2026-11-19";
  packageRequires = [ websocket ];
  preferLocalBuild = true;
  allowSubstitute = false;
}
