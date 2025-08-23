{ melpaBuild
, writeText
, dash
, flycheck
, magit-section
, lsp-mode
, emacs-lean4-mode-src
}:

# ref: https://github.com/leanprover/lean4/blob/8475ec7e362f3f80809973fcdda410388f72b42b/nix/packages.nix#L48-L60
melpaBuild {
  pname = "lean4-mode";
  version = "1";
  commit = "1";
  src = emacs-lean4-mode-src;
  packageRequires = [ dash flycheck magit-section lsp-mode ];
  recipe = writeText "recipe" ''
    (lean4-mode
     :repo "leanprover/lean4-mode"
     :fetcher github
     :files ("*el" "data"))
  '';
}
