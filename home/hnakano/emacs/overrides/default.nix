{ pkgs, ... }:
self: super: {
  evil = self.melpaPackages.evil;

  treemacs = self.melpaBuild {
    inherit (super.treemacs) pname ename commit version src recipe meta;
    packageRequires = super.treemacs.propagatedBuildInputs ++ [ self.doom-modeline ];
  };

  evil-org = super.evil-org.overrideAttrs (old: {
    postPatch = (old.postPacth or "") + ''
      substituteInPlace evil-org-agenda.el --replace "(require 'org)" ""
    '';
  });

  lean4-mode = self.callPackage ./lean4-mode.nix { inherit (pkgs) fetchFromGitHub writeText; };

  copilot = self.callPackage ./copilot.nix { inherit (pkgs) fetchFromGitHub writeText; };

  my-early-init = self.callPackage ./my-early-init { };

  my-init = self.callPackage ./my-init {
    inherit (pkgs) runCommand gnuplot julia;
    skkdicts = pkgs.skkDictionariesUtf8Cdb.combined;
    epkgs = self;
  };

}
