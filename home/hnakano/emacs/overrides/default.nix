{ pkgs }:
self: super: {
  evil = self.melpaPackages.evil;
  ddskk = pkgs.callPackage ./ddskk {};
  # waiting github.com/Alexander-Miller/treemacs/issues/1047 to be fixed
  treemacs = self.melpaStablePackages.treemacs;
  gnuplot = super.gnuplot.overrideAttrs (old: {
    nativeBuildInputs = [ ];
  });

  evil-org = super.evil-org.overrideAttrs (old: {
    postPatch = (old.postPacth or "") + ''
      substituteInPlace evil-org-agenda.el --replace "(require 'org)" ""
    '';
  });

}
