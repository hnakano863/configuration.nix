{ trivialBuild
, fetchFromGitHub
, websocket
}:

trivialBuild {
  src = fetchFromGitHub {
    owner = "manzaltu";
    repo = "claude-code-ide.el";
    rev = "3e742ae7da47ef61091cc45d0f9e11172c376431";
    hash = "sha256-Rp5AeVdI+oV5FFDAaGk9a3WUX1PU4TlfGRtBv7kHaTw=";
  };
  pname = "claude-code-ide";
  version = "2026-11-20";
  packageRequires = [ websocket ];
  preferLocalBuild = true;
  allowSubstitute = false;
}
