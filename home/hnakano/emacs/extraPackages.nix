{ pkgs, ... }:
{
  programs.emacs.compileInit.earlyInitPackages =
    epkgs: with epkgs; [
      initchart
    ];

  programs.emacs.compileInit.initPackages =
    epkgs: with epkgs; [
      all-the-icons
      anki-editor
      bibtex-actions
      company
      company-nixos-options
      consult
      consult-flycheck
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
      marginalia
      nix-mode
      nyan-mode
      ob-restclient
      orderless
      org-bullets
      org-journal
      org-plus-contrib
      org-pomodoro
      org-projectile
      org-ref
      org-roam
      pandoc
      pdf-tools
      posframe
      projectile
      rainbow-delimiters
      restart-emacs
      restclient
      ripgrep
      rust-mode
      shackle
      slime
      slime-company
      smartparens
      symon
      toml-mode
      treemacs
      treemacs-evil
      treemacs-magit
      treemacs-projectile
      undo-fu
      undo-fu-session
      undo-tree
      uuidgen
      vertico
      vterm
      vterm-toggle
      which-key
      winum
      yaml-mode
      yasnippet
    ];
}
