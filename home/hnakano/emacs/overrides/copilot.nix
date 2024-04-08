{ trivialBuild
, fetchFromGitHub
, writeText
, dash
, editorconfig
, f
, jsonrpc
, s
}:

trivialBuild rec {
  pname = "copilot";
  version = "0.0.1";
  src = fetchFromGitHub {
    owner = "copilot-emacs";
    repo = "copilot.el";
    rev = "5f30a2b667df03c120ba31ce3af933255c8a558b";
    hash = "sha256-28pTCggQyVIn5pA260VokYjH5kaypBrG3FDphfzXJcU=";
  };
  propagatedUserEnvPkgs = [ dash editorconfig f jsonrpc s ];
  buildInputs = propagatedUserEnvPkgs;
}
