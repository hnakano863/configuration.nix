{ pkgs }:
self: super: {
  evil = self.melpaPackages.evil;
  doom-modeline = self.melpaStablePackages.doom-modeline;
  ddskk = pkgs.callPackage ./ddskk {};
  gnuplot = super.gnuplot.overrideAttrs (old: {
    nativeBuildInputs = [ ];
  });

  evil-org = super.evil-org.overrideAttrs (old: {
    postPatch = (old.postPacth or "") + ''
      substituteInPlace evil-org-agenda.el --replace "(require 'org)" ""
    '';
  });

  geiser = super.geiser.overrideAttrs (old: {
    src = pkgs.fetchFromGitHub {
      owner = "emacsmirror";
      repo = "geiser";
      rev = "2b45bd368b4acbcef53c3c761725241fb6846102";
      sha256 = "E2jkqVDHZrhLy1oQjSEi/xQbIXnRNqsKPAANW6YIiDU=";
    };
  });

  python-mode = super.python-mode.overrideAttrs (old: {
    src = pkgs.fetchFromGitHub {
      owner = "emacsmirror";
      repo = "python-mode";
      rev = "710ffadeb43136d400de0a4c9e4a94c8b7ff36f0";
      sha256 = "AHPG3PS9UYeEmhd8km9SRqyU0u2y0R79qT5tfqlF1e8=";
    };
  });

  undo-fu = super.undo-fu.overrideAttrs (old: {
    src = pkgs.fetchFromGitHub {
      owner = "emacsmirror";
      repo = "undo-fu";
      rev = "e0ad06b5ef2ac2733dad2ad48e3957b5c36edfa5";
      sha256 = "Kt4xgHLk28wFNzMze7x2Vv8AI2z+IgIpbl2YmOEjPXY=";
    };
  });

  undo-fu-session = super.undo-fu-session.overrideAttrs (old: {
    src = pkgs.fetchFromGitHub {
      owner = "emacsmirror";
      repo = "undo-fu-session";
      rev = "243d93b4c7c1224e7067cd323f64d23dfdfe7c0e";
      sha256 = "dZt/3E0ZX8c1RsiLbilmDCbIVow1lagEPhsokOM0vb0=";
    };
  });

  ox-zenn = self.callPackage ./ox-zenn {};

  # https://github.com/nix-community/emacs-overlay/issues/229
  transient = super.transient.overrideAttrs (attrs : {
    buildInputs =
      (attrs.buildInputs or []) ++
      (with pkgs; [gnumake texinfo texi2html texi2mdoc texlive.combined.scheme-medium]);

    preBuild = (attrs.preBuild or "") + ''
      make all
      mv lisp/* ./
    '';

  });

  with-editor = super.with-editor.overrideAttrs (attrs : {
    buildInputs =
      (attrs.buildInputs or []) ++
      (with pkgs; [gnumake texinfo texi2html texi2mdoc texlive.combined.scheme-medium]);

    preBuild = (attrs.preBuild or "") + ''
      make all
      mv lisp/* ./
    '';
  });

  # https://github.com/nix-community/emacs-overlay/issues/298
  # this patch is already applied to nixos-unstable branch
  # remove when nixos stable branch is updated
  emacsql = super.emacsql.overrideAttrs (attrs : {
    buildInputs = attrs.buildInputs ++ [ pkgs.sqlite ];

    postBuild = ''
      cd source/sqlite
      make
      cd -
    '';

    postInstall = (attrs.postInstall or "") + "\n" + ''
      install -m=755 -D source/sqlite/emacsql-sqlite \
        $out/share/emacs/site-lisp/elpa/emacsql-${attrs.version}/sqlite/emacsql-sqlite
    '';
  });

}
