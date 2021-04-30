;;; my-init.el --- My init.el -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Hiroshi Nakano

;; Author: Hiroshi Nakano <notchi863@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My init file.

;;; Code:

(eval-and-compile
  (package-initialize)
  (require 'leaf))
(eval-when-compile
  (require 'smartparens)
  (require 'org)
  (general-create-definer my/bind
    :states '(motion normal)
    :keymaps 'override))

(make-variable-buffer-local 'global-hl-line-mode)
(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :preface
  (defun print-startup-stats ()
    "Prints some basic startup statistics."
    (let ((elapsed (float-time (time-subtract after-init-time before-init-time))))
      (message "Startup took %.2fs with %d GCs" elapsed gcs-done)))
  :custom
  `(custom-file . ,(locate-user-emacs-file "custom.el"))
  (auto-save-list-file-prefix . nil)
  `(read-process-output-max . ,(* 3 1024 1024))
  (backup-directory-alist . '((".*" . "/tmp/")))
  (auto-save-file-name-transforms . '((".*" "/tmp/" t)))
  :global-minor-mode global-hl-line-mode recentf-mode
  :hook
  (before-save-hook . delete-trailing-whitespace)
  (emacs-startup-hook . print-startup-stats)
  :init
  (blink-cursor-mode -1)
  (add-hook 'term-mode-hook  #'(lambda () (setq-local global-hl-line-mode nil))))

(leaf git-gutter
  :global-minor-mode global-git-gutter-mode)

(leaf undo-fu-session
  :global-minor-mode global-undo-fu-session-mode)

(leaf winum
  :global-minor-mode t)

(leaf which-key
  :global-minor-mode t
  :config
  (which-key-setup-side-window-bottom))

(leaf evil
  :global-minor-mode t
  :custom
  (evil-want-keybinding . nil)
  (evil-want-integration . t)
  (evil-want-C-u-scroll . t)
  (evil-undo-system . 'undo-fu)
  :defun evil-set-initial-state
  :config
  (evil-set-initial-state 'vterm-mode 'insert)
  (leaf evil-collection
    :after evil
    :custom (evil-collection-magit-state . 'normal)
    :hook
    (pdf-view-mode-hook . evil-collection-pdf-setup)
    (magit-mode-hook . evil-collection-magit-setup)))

(leaf completion-framework
  :doc "Settings for completion framework."
  :tag "completion"
  :config
  (leaf selectrum
    :global-minor-mode t)
  (leaf selectrum-prescient
    :after selectrum
    :global-minor-mode
    selectrum-prescient-mode
    prescient-persist-mode)
  (leaf consult
    :bind (("C-s" . consult-line)
	   ("C-h a" . consult-apropos)))
  (leaf consult-selectrum
    :after consult selectrum
    :require t))

(leaf projectile
  :global-minor-mode t
  :custom
  (projectile-indexing-method . 'alien)
  (projectile-sort-order . 'recentf-active)
  (projectile-enable-caching . t)
  (projectile-project-search-path . '("~/repos/" "~/.config/" "~/experiments/")))

(leaf treemacs
  :doc "treemacs and its extentions"
  :defun treemacs-git-mode
  :custom
  (treemacs-width . 30)
  (treemacs-python-executable . "@python3@/bin/python")
  :hook (treemacs-mode-hook . (lambda () (treemacs-git-mode 'deferred)))
  :config
  (leaf treemacs-evil
    :after treemacs
    :require t)
  (leaf treemacs-magit
    :after treemacs
    :require t)
  (leaf treemacs-projectile
    :after treemacs
    :require t))

(leaf skk
  :custom
  (skk-jisyo-code . 'utf-8-unix)
  (skk-large-jisyo . "@skkdicts@/share/SKK-JISYO.L")
  (default-input-method . "japanese-skk")
  (skk-use-color-cursor . nil)
  :config
  (leaf ddskk-posframe
    :hook skk-mode-hook))

(leaf doom
  :doc "doom-relative setups"
  :tag "doom"
  :config
  (leaf doom-themes
    :config
    (load-theme 'doom-opera t)
    (leaf doom-themes-ext-treemacs
      :custom
      (doom-themes-treemacs-theme . "doom-colors")
      :config
      (doom-themes-treemacs-config)))
  (leaf nyan-mode
    :global-minor-mode t)
  (leaf doom-modeline
    :global-minor-mode t
    :defun
    (doom-modeline-def-modeline doom-modeline-set-modeline)
    :custom
    (all-the-icons-scale-factor . 1.1)
    (doom-modeline-height . 1)
    (doom-modeline-bar-width . 3)
    (doom-modeline-buffer-file-name-style . 'truncate-with-project)
    :config
    (doom-modeline-def-modeline 'my/main
      '(bar window-number modals buffer-info " " buffer-position)
      '(misc-info process checker repl lsp vcs indent-info buffer-encoding "   "))
    (doom-modeline-set-modeline 'my/main t)
    (set-face-attribute 'mode-line nil :family "Cica" :height 120)
    (set-face-attribute 'mode-line-inactive nil :family "Cica" :height 120)))

(leaf shackle
  :global-minor-mode t
  :custom
  (shackle-rules . '(("*Help*" :align below :size 0.42 :select t :popup t)
                     (magit-status-mode :align right :select t :popup t)
                     (vterm-mode :align below :size 0.35 :select t :popup t))))

(leaf vterm
  :hook
  (vterm-mode-hook . (lambda () (setq-local global-hl-line-mode nil)))
  :config
  (leaf vterm-toggle
    :after evil
    :custom (vterm-toggle-cd-auto-create-buffer . t)))

(leaf pdf-tools
 :mode ("\\.pdf\\'" . pdf-view-mode))

(leaf dictionary
  :custom
  (dictionary-server . "localhost")
  (dictionary-default-dictionary . "dictd-db-eijiro")
  (dictionary-default-strategy . "re")
  (dictionary-default-popup-strategy . "re"))

(leaf ide
  :doc "provide ide-like features"
  :tag "ide" "company" "flycheck" "lsp"
  :config
  (leaf flycheck :hook (emacs-lisp-mode-hook . flycheck-mode))
  (leaf envrc :global-minor-mode envrc-global-mode)
  (leaf company
    :hook
    (emacs-lisp-mode-hook. company-mode)
    :bind
    (:company-active-map
     ("C-n" . company-select-next)
     ("C-p" . company-select-previous))
    :config
    (leaf company-nixos-options
      :after company
      :defvar company-backends
      :config
      (add-to-list 'company-backends 'company-nixos-options)))
  (leaf smartparens
    :hook
    ((emacs-lisp-mode-hook org-mode-hook nix-mode-hook) . smartparens-mode)
    :config
    (leaf smartparens-config
      :defun
      (sp-local-pair sp-with-modes)
      :after smartparens
      :require t
      :config
      (sp-local-pair 'org-mode "\\[" "\\]")
      (sp-with-modes 'nix-mode
	(sp-local-pair "[ " " ]")
	(sp-local-pair "{ " " }")
	(sp-local-pair "( " " )"))))
  (leaf language-server
    :doc "lsp-mode and its extentions"
    :tag "lsp"
    :config
    (leaf lsp-mode
      :hook
      (lsp-mode-hook . lsp-enable-which-key-integration)
      (nix-mode-hook . lsp-deferred)
      (julia-mode-hook . lsp-deferred)
      :custom
      (lsp-keymap-prefix . "C-c C-l")
      (lsp-auto-configure . t)
      (lsp-nix-server-path . "@rnixlsp@/bin/rnix-lsp")
      (lsp-enable-folding . t))
    (leaf lsp-ui
      :custom (lsp-ui-sideline-show-hover . t))))

(leaf language
  :doc "programming language setup"
  :tag "lang"
  :config
  (leaf elm-mode
    :mode "\\.elm\\'")
  (leaf ess-site
    :mode ("\\.R\\'" . R-mode))
  (leaf fish-mode
    :mode "\\.fish\\'")
  (leaf geiser
    :pre-setq
    (geiser-active-implementations . '(guile))
    :custom
    (geiser-guile-binary . "~/.guix-profile/bin/guile"))
  (leaf gnuplot
    :mode ("\\.gp\\'" . gnuplot-mode)
    :custom (gnuplot-program . "@gnuplot@/bin/gnuplot"))
  (leaf idris-mode
    :mode "\\.idr\\'"
    :custom
    (idris-interpreter-path . "@idris@/bin/idris"))
  (leaf julia-mode
    :mode "\\.jl\\'"
    :custom
    (inferior-julia-program-name . "@julia@/bin/julia")
    :config
    (leaf lsp-julia
      :after lsp-mode
      :require t
      :custom
      (lsp-julia-package-dir . nil)
      (lsp-julia-default-environment . "~/.julia/julials-compiled")
      (lsp-julia-flags . `("--startup-file=no" "--history-file=no" "-J/home/hnakano/.julia/julials-compiled/julia-ls-sysimage.so")))
    (leaf julia-repl
      :hook
      (julia-mode-hook . julia-repl-mode)))
  (leaf jupyter
    :preface
    (defun jupyter-command-advice (&rest args)
      (let ((jupyter-executable "@jupyterCmdFHS@/bin/jupyter-command"))
	(with-temp-buffer
	  (when (zerop (apply #'process-file jupyter-executable nil t nil args))
	    (string-trim-right (buffer-string))))))
    :advice
    (:override jupyter-command jupyter-command-advice))
  (leaf nix-mode :mode "\\.nix\\'")
  (leaf python-mode
    :custom (python-guess-indent . nil)
    :mode "\\.py\\'"))

(leaf org
  :doc "org-mode and its extentions"
  :tag "org"
  :config
  (leaf org-startup
    :doc "startup settings"
    :custom
    (org-startup-indented . t)
    (org-startup-folded . nil)
    (org-startup-with-inline-images . t))
  (leaf org-appearance
    :doc "indentation, fontify, etc"
    :custom
    (org-indent-indentation-per-level . 1)
    (org-hide-emphasis-markers . t)
    (org-pretty-entities . t)
    (org-fontify-quote-and-verse-blocks . t))
  (leaf org-files
    :doc "file, directory settings"
    :custom
    (org-directory . "~/Org")
    (org-agenda-files . '("~/Org/notes.org" "~/Org/knowledge"))
    (org-refile-targets . '((org-agenda-files :maxlevel . 1)))
    (org-default-notes-file . "~/Org/notes.org")
    `(org-archive-location . ,(concat org-directory
				      "/archives/%s_archive_"
				      (format-time-string "%Y" (current-time))
				      "::")))
  (leaf org-todo
    :doc "setting for org-todo"
    :custom
    (org-todo-keywords . '((sequence "TODO(t)"
				     "STRT(s)"
				     "WAIT(w)"
				     "HOLD(h)"
				     "|"
				     "DONE(d)"
				     "KILL(k)")))
    (org-todo-keyword-faces . '(("TODO" . org-todo)
				("STRT" . org-todo)
				("WAIT" . warning)
				("HOLD" . warning))))
  (leaf org-capture
    :doc "setting for org-capture"
    :custom
    (org-capture-templates
     . '(("t" "Task" entry (file+headline org-default-notes-file "Tasks")
          "* TODO %?\n:PROPERTIES:\n:Entered:  %U\n:END:\n%i\n")
         ("n" "Note" entry (file+headline org-default-notes-file "Notes")
          "* %?\n:PROPERTIES:\n:Entered:  %U\n:END:\n%i\n"))))
  (leaf org-latex
    :doc "setting for org-latex"
    :custom
    (org-latex-package-alist . '(("" "physics" t)))
    (org-format-latex-options
     . '(:foreground default
		     :background default
		     :scale 1.6
		     :html-foreground "Black"
		     :html-background "Transparent"
		     :html-scale 1.6
		     :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))))
  (leaf org-babel
    :doc "setting for org-babel"
    :custom
    (org-confirm-babel-evaluate . nil)
    (org-src-fontify-natively . t)
    (org-src-preserve-indentation . t)
    (org-src-tab-acts-natively . t)
    (org-babel-default-header-args:jupyter-julia . '((:async . "yes")
						     (:session . "jl")
						     (:results . "scalar")
						     (:display . "text/plane")))
    :preface
    (defun doom/org-fix-newline-and-indent-in-src-blocks (&optional indent arg interactive)
      (when (and org-src-tab-acts-natively (org-in-src-block-p t))
	(org-babel-do-in-edit-buffer
	 (call-interactively #'indent-for-tab-command))))

    (defun ek/babel-ansi ()
      (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
	(save-excursion
	  (goto-char beg)
	  (when (looking-at org-babel-result-regexp)
            (let ((end (org-babel-result-end))
		  (ansi-color-context-region nil))
              (ansi-color-apply-on-region beg end))))))
    :advice
    (:after org-return doom/org-fix-newline-and-indent-in-src-blocks)
    :hook
    (org-tab-first-hook . doom/org-fix-newline-and-indent-in-src-blocks)
    (org-babel-after-execute-hook . org-redisplay-inline-images)
    (org-babel-after-execute-hook . ek/babel-ansi)
    :init
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (shell . t)
       (gnuplot . t)
       (python . t)
       (julia . t)
       (jupyter . t)
       (restclient . t)))
    (add-to-list 'org-structure-template-alist '("jj" . "src jupyter-julia\n"))
    (add-to-list 'org-structure-template-alist '("jd" . "src jupyter-julia :display image/svg\n")))
  (leaf org-bullets
    :hook (org-mode-hook . org-bullets-mode)
    :custom
    (org-bullets-bullet-list . '("✿" "◉" "✸" "○")))
  (leaf org-journal
    :custom
    (org-journal-file-type . 'weekly)
    (org-journal-file-format . "%Y-%m-%d")
    `(org-journal-dir . ,(concat "~/Org/journal/" (format-time-string "%Y" (current-time)))))
  (leaf org-pomodoro
    :custom
    (org-pomodoro-format . "🍅~%s")
    (org-pomodoro-length . 20)
    (org-pomodoro-play-sounds . nil))
  (leaf org-projectile
    :after org-agenda
    :custom
    (org-projectile-per-project-filepath . "todos.org")
    :config
    (org-projectile-per-project)
    (setq org-agenda-files (append org-agenda-files (org-projectile-project-todo-files))))
  (leaf evil-org
    :hook
    (org-mode-hook . evil-org-mode)
    (evil-org-mode-hook . evil-org-set-key-theme)
    :config
    (leaf evil-org-agenda
      :after org-agenda
      :require t))
  (leaf org-eldoc
    :hook (org-mode-hook . org-eldoc-load))
  (leaf org-roam
    :custom
    (org-roam-directory . "~/Org/roam")
    (emacsql-sqlite3-executable . "@sqlite@/bin/sqlite3")
    (org-roam-graph-executable . "@graphviz@/bin/dot")
    (org-roam-graph-extra-config . '(("layout" . "neato")
				     ("overlap" . "false")
				     ("splines" . "true")))
    :config
    (with-eval-after-load 'org-roam
      (org-roam-mode 1))))

(leaf hydra
  :config
  (leaf hydra-gitgutter
    :defun
    (git-gutter:next-hunk git-gutter:previous-hunk
     git-gutter:stage-hunk git-gutter:revert-hunk git-gutter:popup-hunk)
    :config
    (defhydra hydra-git-gutter (:color red :hint nil)
      "
_j_: next _k_: previous _s_: stage _r_: revert _d_: popup diff"
      ("j" git-gutter:next-hunk)
      ("k" git-gutter:previous-hunk)
      ("s" git-gutter:stage-hunk)
      ("r" git-gutter:revert-hunk)
      ("d" git-gutter:popup-hunk)
      ("ESC" nil :exit t))))

(leaf general
  :config
  (leaf my/bind-root
    :config
    (my/bind
      :prefix "SPC"
      "" nil
      "RET" 'vterm-toggle
      "SPC" 'consult-buffer
      "b" '(:ignore t :wk "buffer")
      "f" '(:ignore t :wk "file")
      "g" '(:ignode t :wk "git")
      "h" '(:ignore t :wk "help")
      "o" '(:ignore t :wk "org")
      "q" '(:ignore t :wk "quit")
      "t" '(:ignore t :wk "toggle")
      "w" '(:ignore t :wk "window"))
    (my/bind 'projectile-mode-map
      :prefix "SPC"
      "p" '(:keymap projectile-command-map :wk "projectile")))
  (leaf my/bind-buffer
    :preface
    (defun switch-to-scratch-buffer ()
      "Switch or create *scratch* buffer."
      (interactive)
      (switch-to-buffer (get-buffer-create "*scratch*"))
      (lisp-interaction-mode)
      (message "switched to *scratch* buffer"))
    (defun revert-buffer-no-confirm ()
      "Revert buffer but no confirm."
      (interactive)
      ;;(message "force-reverting value is %s" force-reverting)
      (if (not (buffer-modified-p))
	  (revert-buffer :ignore-auto :noconfirm)
	(error "The buffer has been modified"))
      (if (not (buffer-modified-p))
      (revert-buffer :ignore-auto :noconfirm)
      (error "The buffer has been modified")))
    :config
    (my/bind
      :prefix "SPC b"
      "b" 'switch-to-buffer
      "d" 'kill-current-buffer
      "l" 'evil-switch-to-windows-last-buffer
      "r" 'revert-buffer-no-confirm
      "s" 'switch-to-scratch-buffer))
  (leaf my/bind-file
    :config
    (my/bind
      :prefix "SPC f"
      "f" 'find-file
      "r" 'consult-recent-file
      "t" 'treemacs
      "u" 'undo-tree-visualize))
  (leaf my/bind
    :config
    (my/bind
     :prefix "SPC w"
     "h" 'evil-window-left
     "j" 'evil-window-down
     "k" 'evil-window-up
     "l" 'evil-window-right
     "H" 'evil-window-move-far-left
     "J" 'evil-window-move-very-bottom
     "K" 'evil-window-move-very-top
     "L" 'evil-window-move-far-right
     "s" 'evil-window-split
     "v" 'evil-window-vsplit
     "d" 'evil-window-delete
     "D" 'delete-other-windows
     "0" 'treemacs-select-window
     "1" 'winum-select-window-1
     "2" 'winum-select-window-2
     "3" 'winum-select-window-3
     "4" 'winum-select-window-4
     "5" 'winum-select-window-5
     "6" 'winum-select-window-6
     "7" 'winum-select-window-7
     "8" 'winum-select-window-8
     "9" 'winum-select-window-9
     "w" 'winum-select-window-by-number))
  (leaf my/bind-help
    :config
    (my/bind
      :prefix "SPC h"
      "a" 'consult-apropos
      "f" 'describe-function
      "v" 'describe-variable
      "k" 'describe-key
      "m" 'describe-mode
      "w" 'dictionary-match-words))
  (leaf my/bind-quit
    :config
    (my/bind
      :prefix "SPC q"
      "q" 'save-buffers-kill-terminal
      "Q" 'evil-quit-all-with-error-code
      "r" 'restart-emacs
      "R" '((lambda () (interactive "P") (restart-emacs '("--debug-init")))
	    :wk "restart-debug-init")))
  (leaf my/bind-git
    :config
    (my/bind
      :prefix "SPC g"
      "g" 'magit-status
      "s" 'magit-status
      "h" 'hydra-git-gutter/body))
  (leaf my/bind-toggle
    :config
   (my/bind
     :prefix "SPC t"
     "t" 'toggle-truncate-lines
     "l" 'display-line-numbers-mode
     "f" 'treemacs
     "v" 'vterm-toggle))
  (leaf my/bind-org
    :config
    (my/bind
      :prefix "SPC o"
      "c" 'org-capture
      "n" '((lambda () (interactive) (find-file org-default-notes-file))
	    :wk "open notes")
      "a" 'org-agenda
      "j" 'org-journal-new-entry
      "p" '(org-projectile-project-todo-completing-read :wk "project todo")
      "f" 'org-roam-find-file
      "i" 'org-roam-insert
      "I" 'org-roam-insert-immediate
      "t" 'org-roam-tag-add
      "r" 'org-roam
      "g" 'org-roam-graph)))

(provide 'my-init)
;;; my-init.el ends here
