{ config, pkgs, lib, ... }:
with pkgs;
{
  programs.emacs.init.usePackage = {

    macrostep.enable = true;
    general.enable = true;
    hydra.enable = true;
    initchart.enable = true;

    restart-emacs.enable = true;
    restart-emacs.command = [ "restart-emacs" ];

    git-gutter.enable = true;
    git-gutter.config = "(global-git-gutter-mode t)";

    undo-fu.enable = true;
    undo-fu-session.enable = true;
    undo-fu-session.config = "(global-undo-fu-session-mode 1)";

    undo-tree.enable = true;
    undo-tree.command = [ "undo-tree-visualize" ];

    winum.enable = true;
    winum.config = "(winum-mode)";

    which-key.enable = true;
    which-key.config = ''
      (which-key-setup-side-window-bottom)
      (which-key-mode)
    '';

    evil = {
      enable = true;
      config = ''
        (evil-mode 1)
      '';
      extraConfig = ''
        :custom
        (evil-want-keybinding nil)
        (evil-want-integration t)
        (evil-want-C-u-scroll t)
        (evil-undo-system 'undo-fu)
      '';
    };

    selectrum = {
      enable = true;
      config = ''
        (selectrum-mode +1)
      '';
    };

    selectrum-prescient = {
      enable = true;
      config = ''
        (selectrum-prescient-mode +1)
        (prescient-persist-mode +1)
      '';
    };

    consult = {
      enable = true;
      command = [
        "consult-buffer"
      ];
      extraConfig = ''
        :general
        ("C-s" 'consult-line)
        ("C-h a" 'consult-apropos)
      '';
    };

    consult-selectrum = {
      enable = true;
      hook = [ "(consult-mode . (lambda () (require 'consult-selectrum)))" ];
    };

    projectile = {
      enable = true;
      extraPackages = [
        git
        fd
        ripgrep
      ];
      hook = [ "(selectrum-mode . projectile-mode)" ];
      extraConfig = ''
        :custom
        (projectile-indexing-method 'alien)
        (projectile-sort-order 'recentf-active)
        (projectile-enable-caching t)
        (projectile-project-search-path '("~/repos/" "~/.config/" "~/experiments/"))
      '';
    };

    magit = {
      enable = true;
      command = [ "magit-status" ];
      config = "(require 'evil-magit)";
    };

    evil-magit = {
      enable = true;
      defer = true;
      extraConfig = ''
        :custom
        (evil-magit-state 'normal)
        (evil-magit-use-y-for-yank t)
      '';
    };

    treemacs = {
      enable = true;
      command = [
        "treemacs"
        "treemacs-select-window"
      ];
      config = ''
        (treemacs-git-mode 'deferred)
        (require 'treemacs-evil)
        (require 'treemacs-magit)
        (require 'treemacs-projectile)
      '';
      extraConfig = ''
        :custom
        (treemacs-width 30)
        (treemacs-python-executable "${python3}/bin/python")
      '';
    };

    treemacs-evil.enable = true;
    treemacs-evil.defer = true;

    treemacs-projectile.enable = true;
    treemacs-projectile.defer = true;

    treemacs-magit.enable = true;
    treemacs-magit.defer = true;

    skk = {
      package = "ddskk";
      defer = true;
      enable = true;
      extraConfig = ''
        :custom
        (skk-jisyo-code 'utf-8-unix)
        (skk-large-jisyo "${skk-dicts}/share/SKK-JISYO.L")
        (default-input-method "japanese-skk")
      '';
    };

    posframe.enable = true;
    posframe.defer = true;
    ddskk-posframe.enable = true;
    ddskk-posframe.hook = [ "(skk-mode . ddskk-posframe-mode)" ];

    all-the-icons.enable = true;
    all-the-icons.extraPackages = [ emacs-all-the-icons-fonts ];

    doom-themes = {
      enable = true;
      config = ''
        (load-theme 'doom-opera t)
        (require 'doom-themes-ext-treemacs)
        (doom-themes-treemacs-config)
      '';
      extraConfig = ''
        :custom
        (doom-themes-treemacs-theme "doom-colors")
      '';
    };

    doom-modeline = {
      enable = true;
      config = ''
        (doom-modeline-def-modeline 'my/main
          '(bar window-number parrot matches buffer-info " " buffer-position)
          '(misc-info process checker repl lsp vcs indent-info buffer-encoding "   "))

        (defun doom-modeline-set-my/main-modeline ()
          (doom-modeline-set-modeline 'my/main t))

        (add-hook 'doom-modeline-mode-hook 'doom-modeline-set-my/main-modeline)

        (doom-modeline-mode 1)

        (defun my/configure-face-attributes ()
          (progn
            (set-face-attribute 'mode-line nil :family "Cica" :height 120)
            (set-face-attribute 'mode-line-inactive nil :family "Cica" :height 120)))

        (add-hook 'after-init-hook 'my/configure-face-attributes)
      '';
      extraConfig = ''
        :custom
        (all-the-icons-scale-factor 1.1)
        (doom-modeline-height 1)
        (doom-modeline-bar-width 3)
        (doom-modeline-buffer-file-name-style 'truncate-with-project)
      '';
    };

    parrot = {
      enable = true;
      config = "(parrot-mode 1)";
      extraConfig = ''
        :custom
        (parrot-num-rotations 10)
      '';
    };

    nyan-mode.enable = true;
    nyan-mode.config = "(nyan-mode 1)";

    hide-mode-line = {
      enable = false;
      hook = [
        "(help-mode . hide-mode-line-mode)"
        "(vterm-mode . hide-mode-line-mode)"
      ];
    };

    mode-line-bell.enable = true;
    mode-line-bell.config = "(mode-line-bell-mode 1)";

    shackle = {
      enable = true;
      config = "(shackle-mode 1)";
      extraConfig = ''
        :custom
        (shackle-rules '(("*Help*" :align below :size 0.42 :select t :popup t)
                         (magit-status-mode :align right :select t :popup t)
                         (vterm-mode :align below :size 0.35 :select t :popup t)))
      '';
    };

    vterm.enable = true;
    vterm.defer = true;
    vterm.hook = [ "(vterm-mode . (lambda () (setq-local global-hl-line-mode nil)))" ];

    vterm-toggle = {
      enable = true;
      after = [ "evil" ];
      command = [ "vterm-toggle" ];
      config = "(evil-set-initial-state 'vterm-mode 'insert)";
      extraConfig = ''
        :custom
        (vterm-toggle-cd-auto-create-buffer t)
      '';
    };

    pdf-tools = {
      enable = true;
      mode = [ ''("\\.pdf\\'" . pdf-view-mode)'' ];
    };

    evil-collection = {
      enable = true;
      hook = [ "(pdf-view-mode . evil-collection-pdf-setup)" ];
    };

    ebib.enable = true;
    ebib.command = [ "ebib" ];
  };
}
