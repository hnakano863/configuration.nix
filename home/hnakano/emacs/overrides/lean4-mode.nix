{ melpaBuild
, fetchFromGitHub
, writeText
, dash
, f
, flycheck
, magit-section
, lsp-mode
, s
}:

# ref: https://github.com/leanprover/lean4/blob/8475ec7e362f3f80809973fcdda410388f72b42b/nix/packages.nix#L48-L60
melpaBuild {
  pname = "lean4-mode";
  version = "1";
  commit = "1";
  src = fetchFromGitHub {
    owner = "leanprover";
    repo = "lean4-mode";
    rev = "d1c936409ade7d93e67107243cbc0aa55cda7fd5";
    hash = "sha256-tD5Ysa24fMIS6ipFc50OjabZEUge4riSb7p4BR05ReQ=";
  };
  packageRequires = [ dash f flycheck magit-section lsp-mode s ];
  recipe = writeText "recipe" ''
    (lean4-mode
     :repo "leanprover/lean4-mode"
     :fetcher github
     :files ("*el" "data"))
  '';
}
