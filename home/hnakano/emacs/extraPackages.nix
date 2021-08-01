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
      dictionary
      docker
      dockerfile-mode
      docker-compose-mode
      doom-modeline
      doom-themes
      ebib
      ein
      elm-mode
      envrc
      ess
      evil
      evil-collection
      evil-org
      fish-mode
      flycheck
      geiser
      general
      git-gutter
      gnuplot
      helpful
      hydra
      idris-mode
      imenu-list
      initchart
      julia-mode
      julia-repl
      jupyter
      leaf
      leaf-convert
      lsp-julia
      lsp-mode
      lsp-pyright
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
      pandoc
      pdf-tools
      posframe
      projectile
      rainbow-delimiters
      restart-emacs
      restclient
      rust-mode
      selectrum
      selectrum-prescient
      shackle
      smartparens
      toml-mode
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
      yaml-mode
      yasnippet
    ];
}
