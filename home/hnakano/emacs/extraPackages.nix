{ pkgs, ... }:
{
  programs.emacs.compileInit.earlyInitPackages =
    epkgs: with epkgs; [
      initchart
    ];

  programs.emacs.compileInit.initPackages =
    epkgs: with epkgs; [
      all-the-icons
      company
      company-nixos-options
      consult
      ddskk
      ddskk-posframe
      doom-modeline
      doom-themes
      ebib
      ein
      envrc
      evil
      evil-collection
      evil-org
      fish-mode
      flycheck
      geiser
      general
      git-gutter
      gnuplot
      hydra
      idris-mode
      initchart
      julia-mode
      jupyter
      leaf
      leaf-convert
      lsp-mode
      lsp-ui
      macrostep
      magit
      nix-mode
      nyan-mode
      ob-restclient
      org-bullets
      org-journal
      org-plus-contrib
      org-pomodoro
      org-projectile
      org-roam
      pdf-tools
      posframe
      projectile
      python-mode
      restart-emacs
      restclient
      rust-mode
      selectrum
      selectrum-prescient
      shackle
      smartparens
      treemacs
      treemacs-evil
      treemacs-magit
      treemacs-projectile
      undo-fu
      undo-fu-session
      undo-tree
      vterm
      vterm-toggle
      which-key
      winum
      yasnippet
    ];
}
