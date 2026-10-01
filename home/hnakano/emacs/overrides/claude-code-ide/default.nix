{ trivialBuild
, fetchFromGitHub
, websocket
}:

trivialBuild {
  src = fetchFromGitHub {
    owner = "manzaltu";
    repo = "claude-code-ide.el";
    rev = "50a3d55262805d7207889ed429ff30da96fbf68b";
    hash = "sha256-u+87PjLh0Mc7C8nvDG758rdgpmjED8G/hO+FE1tj7DU=";
  };
  pname = "claude-code-ide";
  version = "2026-09-14";
  packageRequires = [ websocket ];
  preferLocalBuild = true;
  allowSubstitute = false;
}
