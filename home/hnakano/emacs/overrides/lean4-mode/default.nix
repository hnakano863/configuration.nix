{ trivialBuild
, fetchFromGitHub
, dash
, f
, flycheck
, lsp-mode
, magit-section
, s
}:

trivialBuild {

  pname = "lean4-mode";
  version = "unstable";

  src = fetchFromGitHub {

    owner = "leanprover";
    repo = "lean4-mode";
    rev = "d1c936409ade7d93e67107243cbc0aa55cda7fd5";
    sha256 = "tD5Ysa24fMIS6ipFc50OjabZEUge4riSb7p4BR05ReQ=";

  };

  packageRequires = [ dash f flycheck lsp-mode magit-section s ];

}
