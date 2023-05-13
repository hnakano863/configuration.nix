{ pkgs, ... }:
{
  programs.emacs.compileInit.earlyInitPackages =
    epkgs: with epkgs; [
      benchmark-init
    ];

  programs.emacs.compileInit.initPackages =
    epkgs: with epkgs; [
      all-the-icons
      company
      company-coq
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
      eglot
      eglot-jl
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
      inheritenv
      julia-mode
      julia-repl
      jupyter
      leaf
      leaf-convert
      macrostep
      magit
      marginalia
      nerd-icons
      nix-mode
      nyan-mode
      ob-restclient
      orderless
      org-bullets
      org-contrib
      org-journal
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
