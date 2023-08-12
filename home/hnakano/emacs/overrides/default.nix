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

}
