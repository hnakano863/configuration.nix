{ config, pkgs, lib, ... }:
with lib;
with pkgs;
{
  programs.emacs.init.usePackage = {
    org = {
      enable = true;
      package = "org-plus-contrib";
      mode = [ ''("\\.org\\'" . org-mode)'' ];
      init = readFile ./org-init.el;
      config = readFile ./org-config.el;
    };

    org-bullets = {
      enable = true;
      hook = [ "(org-mode . org-bullets-mode)" ];
      extraConfig = ''
        :custom
        (org-bullets-bullet-list '("✿" "◉" "✸" "○"))
      '';
    };

    org-journal = {
      enable = true;
      command = [ "org-journal-new-entry" ];
      extraConfig = ''
        :custom
        (org-journal-file-type 'weekly)
        (org-journal-file-format "%Y-%m-%d")
        (org-journal-dir (concat "~/Org/journal/" (format-time-string "%Y" (current-time))))
      '';
    };

    org-pomodoro = {
      enable = true;
      command = [ "org-pomodoro" ];
      extraConfig = ''
        :custom
        (org-pomodoro-format "🍅~%s")
        (org-pomodoro-length 20)
        (org-pomodoro-play-sounds nil)
      '';
      hook = [
        "(org-pomodoro-started . parrot-start-animation)"
        "(org-pomodoro-finished . parrot-start-animation)"
        "(org-pomodoro-break-finished . parrot-start-animation)"
      ];
    };

    org-projectile = {
      enable = true;
      command = [ "org-projectile-project-todo-completing-read" ];
      config = ''
        (org-projectile-per-project)
        (setq org-agenda-files
              (append org-agenda-files (org-projectile-todo-files)))
      '';
      extraConfig = ''
        :custom
        (org-projectile-per-project-filepath "todos.org")
      '';
    };

    evil-org = {
      enable = true;
      hook = [
        "(org-mode . evil-org-mode)"
        "(evil-org-mode . evil-org-set-key-theme)"
        "(org-agenda-mode . (lambda () (require 'evil-org-agenda) (evil-org-agenda-set-keys)))"
      ];
    };

    org-eldoc.enable = true;
    org-eldoc.hook = [ "(org-mode . org-eldoc-load)" ];

    smartparens.hook = [ "(org-mode . smartparens-mode)" ];
    smartparens.config = mkAfter ''
      (sp-local-pair 'org-mode "\\[" "\\]")
    '';

    org-roam = {
      enable = true;
      command = [
        "org-roam-find-file"
        "org-roam-mode"
      ];
      extraConfig = ''
        :custom
        (org-roam-directory "~/Org/roam/")
        (emacsql-sqlite3-executable "${pkgs.sqlite}/bin/sqlite3")
        (org-roam-graph-executable "${pkgs.graphviz}/bin/dot")
        (org-roam-graph-extra-config '(("layout" . "neato")
				                               ("overlap" . "false")
				                               ("splines" . "true")))
      '';
      hook = [
        "(org-mode . (lambda () (org-roam-mode +1)))"
      ];
    };
  };
}
