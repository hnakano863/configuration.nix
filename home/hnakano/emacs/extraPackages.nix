{ pkgs, ... }:
{
  programs.emacs.compileInit.earlyInitPackages =
    epkgs: with epkgs; [
      benchmark-init
    ];

  programs.emacs.compileInit.initPackages =
    epkgs: with epkgs; [
      all-the-icons
      anki-editor
      citar
      company
      company-coq
      company-nixos-options
      consult
      consult-flycheck
      csv-mode
      ddskk
      ddskk-posframe
      dictionary
      docker
      dockerfile-mode
      docker-compose-mode
      doom-modeline
      doom-themes
      ebib
      elfeed
      elfeed-org
      elm-mode
      envrc
      ess
      evil
      evil-collection
      evil-org
      fish-mode
      flycheck
      fsharp-mode
      geiser
      general
      git-gutter
      gnuplot
      haskell-mode
      helpful
      hydra
      idris-mode
      imenu-list
      initchart
      inheritenv
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
      org-contrib
      org-pomodoro
      org-projectile
      org-ref
      org-roam
      ox-pandoc
      ox-zenn
      pandoc
      pdf-tools
      plantuml-mode
      posframe
      projectile
      proof-general
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
