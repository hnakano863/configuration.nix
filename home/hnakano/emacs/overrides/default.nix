{ pkgs }:
self: super: {
  my-ivy-migemo = self.callPackage ./my-ivy-migemo {};
  initchart = self.callPackage ./initchart {};
  evil = self.melpaPackages.evil;
  ddskk = pkgs.callPackage ./ddskk {};
  gnuplot = super.gnuplot.overrideAttrs (old: {
    nativeBuildInputs = [ ];
  });
  use-package = self.melpaStablePackages.use-package;

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
}
