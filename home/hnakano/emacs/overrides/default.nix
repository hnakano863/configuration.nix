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
  my-init-common = self.callPackage ./my-init-common {
    inherit (pkgs) runCommand;
    skkdicts = pkgs.skkDictionariesUtf8Cdb.combined;
    epkgs = self;
  };
  my-init-linux = self.callPackage ./my-init-linux { epkgs = self; };
  my-init-wsl = self.callPackage ./my-init-wsl { epkgs = self; };

  dataform-mode = self.callPackage ./dataform-mode { };
  lookml-mode = self.callPackage ./lookml-mode { };

}
