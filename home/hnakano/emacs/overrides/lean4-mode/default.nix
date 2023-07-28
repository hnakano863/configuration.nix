{ trivialBuild
, fetchFromGitHub
, dash
, f
, flycheck
, lsp-mode
, magit-section
, s
}:

trivialBuild rec {

  pname = "lean4-mode";
  version = "unstable";

  src = fetchFromGitHub {

    owner = "leanprover";
    repo = "lean4-mode";
    rev = "d1c936409ade7d93e67107243cbc0aa55cda7fd5";
    sha256 = "tD5Ysa24fMIS6ipFc50OjabZEUge4riSb7p4BR05ReQ=";

  };

  packageRequires = [ dash f flycheck lsp-mode magit-section s ];

  installPhase = ''
    LISPDIR=$out/share/emacs/site-lisp/lean4-mode
    install -d $LISPDIR/data
    install *.el *.elc $LISPDIR
    install data/* $LISPDIR/data
    emacs --batch -l package --eval "(package-generate-autoloads \"${pname}\" \"$LISPDIR\")"
  '';

}
