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
  (require 'leaf)
  (require 'inheritenv))
(eval-when-compile
  (require 'smartparens)
  (require 'org)
  (require 'consult)
  (general-create-definer my/bind
    :states '(motion normal)
    :keymaps 'override)
  (require 'evil))

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
  (epg-pinentry-mode . 'loopback)
  :hook
  (before-save-hook . delete-trailing-whitespace)
  (emacs-startup-hook . print-startup-stats)
  (term-mode-hook . eterm-256color-mode)
  :init
  (blink-cursor-mode -1)
  (add-hook 'term-mode-hook  #'(lambda () (setq-local global-hl-line-mode nil))))

(leaf git-gutter :global-minor-mode global-git-gutter-mode)
(leaf hideshow :hook (emacs-lisp-mode-hook . hs-minor-mode))
(leaf hl-line :global-minor-mode global-hl-line-mode)
(leaf recentf :global-minor-mode t)
(leaf undo-fu-session :global-minor-mode global-undo-fu-session-mode)
(leaf winum :global-minor-mode t)

(leaf auth-source
  :custom
  (auth-source-pass-filename . "~/.local/share/password-store")
  :config
  (auth-source-pass-enable))

(leaf magit
  :custom
  (magit-process-find-password-functions . '(magit-process-password-auth-source))
  :hook (after-save-hook . magit-after-save-refresh-status)
  :config
  (setenv "PASSWORD_STORE_DIR" "/home/hnakano/.local/share/password-store")
  (leaf forge
    :after magit
    :require t))

(leaf which-key
  :global-minor-mode t
  :config (which-key-setup-side-window-bottom))

(leaf helpful
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  (:emacs-lisp-mode-map
   ("C-c C-d" . helpful-at-point)))

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
    :config
    (evil-collection-init '(pdf-view magit vterm))))

(leaf completion-framework
  :doc "Settings for completion framework."
  :tag "completion"
  :config
  (leaf savehist :global-minor-mode t)
  (leaf orderless
    :custom
    (completion-styles . '(orderless))
    (completion-category-defaults . nil)
    (completion-category-overrides . '((file (styles partial-completion)))))
  (leaf vertico :global-minor-mode t)
  (leaf consult
    :bind ("C-s" . consult-line)
    :config
    (consult-customize
     consult-recent-file consult--source-recent-file consult--source-project-recent-file
     :preview-key '(:debounce 0.5 any)))
  (leaf marginalia :global-minor-mode t)
  (leaf embark
    :bind
    ("C-." . embark-act)
    ("C-;" . embark-dwim)
    ("C-h b" . embark-bindings)
    :config
    (setq prefix-help-command #'embark-prefix-help-command))
  (leaf embark-consult
    :hook (embark-collect-mode-hook . consult-preview-at-point-mode)))

(leaf projectile
  :global-minor-mode t
  :custom
  (projectile-indexing-method . 'alien)
  (projectile-sort-order . 'recentf-active)
  (projectile-enable-caching . t)
  (projectile-project-search-path . '("~/repos/" "~/experiments/" "~/ghq/"))
  ; fix for https://github.com/bbatsov/projectile/issues/1777
  (projectile-globally-ignored-directories . '(".idea"
					       ".vscode"
					       ".ensime_cache"
					       ".eunit"
					       ".git"
					       ".hg"
					       ".fslckout"
					       "_FOSSIL_"
					       ".bzr"
					       "_darcs"
					       ".tox"
					       ".svn"
					       ".stack-work"
					       ".ccls-cache"
					       ".cache"
					       ".clangd")))

(leaf treemacs
  :doc "treemacs and its extentions"
  :defun treemacs-git-mode
  :custom
  (treemacs-width . 30)
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
  :preface
  (defun skk-isearch-setup-maybe ()
    (require 'skk-vars)
    (when (or (eq skk-isearch-mode-enable 'always)
              (and (boundp 'skk-mode)
                   skk-mode
                   skk-isearch-mode-enable))
      (skk-isearch-mode-setup)))
  (defun skk-isearch-cleanup-maybe ()
    (require 'skk-vars)
    (when (and (featurep 'skk-isearch)
               skk-isearch-mode-enable)
      (skk-isearch-mode-cleanup)))
  :hook
  (isearch-mode-hook . skk-isearch-setup-maybe)
  (isearch-mode-end-hook . skk-isearch-cleanup-maybe)
  (evil-insert-state-entry-hook . (lambda () (skk-mode 1)))
  (evil-insert-state-exit-hook . (lambda () (skk-mode -1)))
  :bind
  ("C-x C-j" . skk-mode)
  :custom
  (skk-cdb-large-jisyo . "@skkdicts@/share/skk/SKK-JISYO.combined.utf8.cdb")
  (skk-cdb-coding-system . 'utf-8-unix)
  (skk-inhibit-ja-dic-search . t)
  (default-input-method . "japanese-skk")
  (skk-use-color-cursor . t)
  :config
  (leaf ddskk-posframe
    :hook skk-mode-hook)
  (leaf context-skk
    :hook
    (skk-load-hook . (lambda () (require 'context-skk)))
    (skk-mode-hook . context-skk-mode)
    :config
    (setq context-skk-programming-mode
	  (append context-skk-programming-mode
		  '(dataform-mode lookml-mode)))))

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
    (nerd-icons-scale-factor . 1.1)
    (doom-modeline-height . 1)
    (doom-modeline-bar-width . 3)
    (doom-modeline-buffer-file-name-style . 'truncate-with-project)
    :config
    (doom-modeline-def-modeline 'my/main
      '(bar window-number modals buffer-info " " buffer-position)
      '(misc-info process repl lsp vcs indent-info buffer-encoding "   "))
    (doom-modeline-set-modeline 'my/main t)
    (set-face-attribute 'mode-line nil :family "HackGen" :height 120)
    (set-face-attribute 'mode-line-inactive nil :family "HackGen" :height 120)))

(leaf shackle
  :global-minor-mode t
  :custom
  (shackle-rules . '(("\\*Help\\*" :align below :size 0.42 :select t :popup t)
		     (helpful-mode :align below :size 0.42 :select t :popup t)
                     (magit-status-mode :align right :select t :popup t)
                     (vterm-mode :align below :size 0.35 :select t :popup t)
		     (org-roam-mode :align right :size 0.33 :select nil :popup t))))

(leaf vterm
  :hook
  (vterm-mode-hook . (lambda () (setq-local global-hl-line-mode nil)))
  :config
  (leaf vterm-toggle
    :after evil
    :custom (vterm-toggle-cd-auto-create-buffer . t)))

(leaf pdf-tools :mode ("\\.pdf\\'" . pdf-view-mode))

(leaf dictionary
  :custom
  (dictionary-server . "localhost")
  (dictionary-default-dictionary . "dictd-db-eijiro")
  (dictionary-default-strategy . "re")
  (dictionary-default-popup-strategy . "re"))

(leaf elfeed
  :custom
  (elfeed-feeds . '("https://tech.andpad.co.jp/feed"
		    "https://www.juliabloggers.com/feed")))

(leaf ide
  :doc "provide ide-like features"
  :tag "ide" "company" "flycheck" "lsp"
  :config
  (leaf eglot :custom (eglot-connect-timeout . 180))
  (leaf flycheck :hook (emacs-lisp-mode-hook . flycheck-mode))
  (leaf envrc :global-minor-mode envrc-global-mode)
  (leaf company
    :hook
    (emacs-lisp-mode-hook . company-mode)
    :bind
    (:company-active-map
     ("C-n" . company-select-next)
     ("C-p" . company-select-previous)))
  (leaf smartparens
    :hook
    ((emacs-lisp-mode-hook org-mode-hook) . smartparens-mode)
    :config
    (leaf smartparens-config
      :defun
      (sp-local-pair sp-with-modes)
      :after smartparens
      :require t
      :config
      (sp-local-pair 'org-mode "\\[" "\\]")
      (sp-local-pair 'org-mode "<" nil :actions :rem)))
  (leaf rainbow-delimiters
    :hook ((emacs-lisp-mode-hook org-mode-hook) . rainbow-delimiters-mode))
  (leaf prism
    :defer-config
    (prism-set-colors :lightens '(0 5 10) :desaturations '(-2.5 0 2.5)
      :colors (-map #'doom-color '(red orange yellow green blue violet))))
  (leaf yasnippet
    :defer-config
    (yas-reload-all)))

(leaf language
  :doc "programming language setup"
  :tag "lang"
  :config
  (leaf coq-mode
    :hook (coq-mode-hook . company-coq-mode))
  (leaf elm-mode :mode "\\.elm\\'")
  (leaf ess-site
    :mode ("\\.R\\'" . R-mode))
  (leaf fish-mode
    :mode "\\.fish\\'")
;  (leaf geiser
;    :pre-setq
;    (geiser-active-implementations . '(guile))
;    :custom
;    (geiser-guile-binary . "~/.guix-profile/bin/guile"))
  (leaf gnuplot
    :mode ("\\.gp\\'" . gnuplot-mode)
    :custom (gnuplot-program . "@gnuplot@/bin/gnuplot"))
  (leaf haskell-mode
    :mode "\\.hs\\'")
  (leaf js
    :custom (js-indent-level . 2)
    :hook
    (js-mode-hook . smartparens-mode))
  (leaf julia-mode
    :mode "\\.jl\\'"
    :hook
    (julia-mode-hook . smartparens-mode)
    :custom
    (inferior-julia-program-name . "@julia@/bin/julia")
    :config
    (leaf julia-repl
      :hook (julia-mode-hook . julia-repl-mode)
      :config
      (inheritenv-add-advice #'julia-repl-inferior-buffer)
      (julia-repl-set-terminal-backend 'vterm))
    (leaf eglot-jl
      :custom
      (eglot-jl-language-server-project . "~/.julia/environments/v1.9/")
      :hook (julia-mode-hook . eglot-ensure)
      :init
      (eglot-jl-init))
    (require 'smartparens-python)
    (sp-with-modes 'julia-mode
      (sp-local-pair "\"" "\"" :post-handlers '(:add sp-python-fix-triple-quotes))
      (sp-local-pair "\"\"\"" "\"\"\"")))
  (leaf nix-mode
    :mode "\\.nix\\'"
    :hook
    (nix-mode-hook . smartparens-mode)
    (nix-mode-hook . eglot-ensure)
    :config
    (sp-with-modes 'nix-mode
      (sp-local-pair "[ " " ]")
      (sp-local-pair "{ " " }")
      (sp-local-pair "( " " )")))
  (leaf python
    :custom
    (python-guess-indent . nil)
    (python-shell-interpreter . "ipython")
    :mode "\\.py\\'"
    :hook
    (python-mode-hook . smartparens-mode)
    (python-mode-hook . eglot-ensure))
  (leaf toml-mode :mode "\\.toml\\'")
  (leaf yaml-mode :mode "\\.ya?ml\\'")
  (leaf dockerfile-mode :mode "Dockerfile\\'")
  (leaf docker-compose-mode
    :hook (docker-compose-mode-hook . company-mode))
  (leaf lisp-mode
    :mode "\\.cl\\'" "\\.lisp\\'"
    :custom (inferior-lisp-program . "sbcl")
    :hook
    (lisp-mode-hook . smartparens-mode)
    (lisp-mode-hook . rainbow-delimiters-mode)
    (lisp-mode-hook . company-mode)
    (lisp-mode-hook . flycheck-mode)
    :config
    (leaf slime
      :hook lisp-mode-hook
      :bind (:slime-editing-map ("SPC" . nil))
      :config
      (leaf slime-autodoc
	:bind (:slime-autodoc-mode-map ("SPC" . nil)))
      (leaf slime-company
	:after company
	:defvar company-backends
	:require t
	:hook
	(slime-mode-hook . (lambda ()
			     (setq-local company-backends
					 (cons 'company-slime company-backends)))))))
  (leaf haskell-mode :mode "\\.hs\\'")
  (leaf csv-mode :mode "\\.csv\\'")
  (leaf plantuml-mode
    :custom
    (plantuml-executable-path . "plantuml")
    (plantuml-default-exec-mode . 'executable)
    :mode "\\.puml\\'")
  (leaf jupyter
    :custom (jupyter-long-timeout . 100)
    :config
    (inheritenv-add-advice #'jupyter-command))
  (leaf markdown-mode
    :mode ("\\.md\\'" . gfm-mode)
    :custom (markdown-command . "pandoc --from gfm"))
  (leaf lean4-mode :mode "\\.lean\\'")
  (leaf mermaid-mode :mode "\\.mermaid\\'")
  (leaf rust-mode :mode "\\.rs\\'")
  (leaf js-mode :mode "\\.gs\\'")
  (leaf dataform-mode
    :custom
    (yas-indent-line . 'fixed)
    :preface
    (define-derived-mode dataform-mode prog-mode "Dataform"
      "Major mode for dataform"
      (setq-local tab-width 2)
      (setq indent-tabs-mode nil))
    :mode "\\.sqlx\\'"
    :hook
    (dataform-mode-hook . prism-mode)
    (dataform-mode-hook . yas-minor-mode))
  (leaf lookml-mode
    :custom
    (yas-indent-line . 'fixed)
    :preface
    (define-derived-mode lookml-mode prog-mode "LookML"
      "Major mode for LookML"
      (setq-local tab-width 2)
      (setq indent-tabs-mode nil))
    :mode "\\.lkml\\'"
    :hook
    (lookml-mode-hook . prism-mode)
    (lookml-mode-hook . yas-minor-mode)))

(leaf org
  :doc "org-mode and its extentions"
  :tag "org"
  :custom
  (org-return-follows-link . t)
  :preface
  (defun org-syntax-table-modify ()
    "Modify `org-mode-syntax-table' for the current org buffer."
    (modify-syntax-entry ?< "." org-mode-syntax-table)
    (modify-syntax-entry ?> "." org-mode-syntax-table))
  :hook (org-mode-hook . org-syntax-table-modify)
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
    (org-directory . "~/Dropbox/Org")
    (my/org-notes-directory . `,(concat (file-name-as-directory org-directory) "notes/"))
    (org-refile-targets . '((org-agenda-files :maxlevel . 1)))
    (org-archive-location . `,(concat (file-name-as-directory org-directory)
				      "archives/%s_archive_"
				      (format-time-string "%Y" (current-time))
				      "::")))
  (leaf org-capture
    :doc "setting for org-capture"
    :custom
    (org-todo-keywords . '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "KILL(k)")))
    (org-todo-keyword-faces . '(("TODO" . org-todo) ("WAIT" . warning)))
    (org-default-notes-file . `,(concat  my/org-notes-directory "todos.org"))
    (org-capture-templates
     . '(("c" "Todo" entry (file+headline org-default-notes-file "Todos")
          "* TODO %?\n:PROPERTIES:\n:Project:%^{Project}p\n:END:")
	 ("t" "Todo witout properties" entry (file+headline org-default-notes-file "Todos")
          "* TODO %?"))))
  (leaf org-agenda
    :custom
    (org-agenda-files . `,(list my/org-notes-directory))
    (org-agenda-span . 'day)
    :config
    (leaf evil-org-agenda
      :require t
      :after org-agenda
      :config
      (evil-org-agenda-set-keys)
      (evil-define-key 'motion org-agenda-mode-map
        "w" 'org-save-all-org-buffers
	"l" 'org-agenda-log-mode
	"gw" 'org-agenda-week-view
	"gW" 'org-agenda-day-view
	"ci" 'org-agenda-clock-in
	"co" 'org-agenda-clock-out
	"cs" 'org-agenda-schedule
	"cd" 'org-agenda-deadline
	"cC" 'org-agenda-clock-cancel
	"cc" 'org-agenda-set-tags)))
  (leaf org-latex
    :doc "setting for org-latex"
    :custom
    (org-latex-packages-alist . '(("" "physics" t)
				  ("" "mhchem" t)))
    (org-format-latex-options
     . '(:foreground default
		     :background default
		     :scale 1.6
		     :html-foreground "Black"
		     :html-background "Transparent"
		     :html-scale 1.6
		     :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))))
  (leaf ob
    :doc "setting for org-babel"
    :after org
    :custom
    (org-confirm-babel-evaluate . nil)
    (org-src-fontify-natively . t)
    (org-src-preserve-indentation . t)
    (org-src-tab-acts-natively . t)
    (org-babel-default-header-args:jupyter-python . '((:kernel . "python3")
						      (:async . "yes")
						      (:session . "py")
						      (:results . "scalar")
						      (:display . "text/plain")))
    (org-babel-default-header-args:jupyter-julia . '((:kernel . "julia-1.9")
						     (:async . "yes")
						     (:session . "jl")
						     (:results . "scalar")
						     (:display . "text/plain")))
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
    :config
    (let ((cmds '(("jj" . "src jupyter-julia\n")
		  ("jd" . "src jupyter-julia :display image/png\n")
		  ("pp" . "src jupyter-python\n")
		  ("pd" . "src jupyter-python :display image/png\n"))))
      (dolist (cmd cmds)
	(push cmd org-structure-template-alist)))
    :defer-config
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (lisp . t)
       (dot . t)
       (shell . t)
       (gnuplot . t)
       (python . t)
       (haskell . t)
       (julia . t)
       (jupyter . t)
       (restclient . t))))
  (leaf org-bullets
    :hook (org-mode-hook . org-bullets-mode)
    :custom
    (org-bullets-bullet-list . '("✿" "◉" "✸" "○")))
  (leaf org-journal
    :custom
    (org-journal-file-type . 'weekly)
    (org-journal-file-format . "%Y-%m-%d")
    (org-journal-dir . `,(concat (file-name-as-directory org-directory)
				 "journal/"
				 (format-time-string "%Y" (current-time)))))
  (leaf evil-org
    :hook
    (org-mode-hook . evil-org-mode)
    (evil-org-mode-hook . evil-org-set-key-theme))
  (leaf org-eldoc
    :hook (org-mode-hook . org-eldoc-load))
  (leaf org-roam
    :custom
    `(org-roam-db-gc-threshold . ,most-positive-fixnum)
    (org-roam-v2-ack . t)
    (org-roam-directory . `,(concat (file-name-as-directory org-directory) "roam/"))
    (org-roam-graph-extra-config . '(("layout" . "neato")
				     ("overlap" . "false")
				     ("splines" . "true")))
    (org-roam-capture-templates . '(("d" "default" plain "%?"
				     :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
							"#+title: ${title}\n")
				     :unnarrowed t)
				    ("r" "research log")
				    ("rf" "create file" plain
				     (file "~/Dropbox/Org/roam/templates/research-log")
				     :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
							"#+title: ${title}\n")
				     :unnarrowed t)
				    ("ri" "add item" entry
				     (file "~/Dropbox/Org/roam/templates/research-log-item")
				     :target (file "${slug}.org"))))
    (org-roam-node-display-template . `,(concat "${title:60} "
						(propertize "${tags:*}" 'face 'org-tag)))
    :bind (:org-roam-mode-map
	   ("SPC" . nil)
	   ("S-SPC" . nil))
    :config
    (org-roam-setup))
  (leaf org-cite
    :custom
    (org-cite-global-bibliography . '("~/Dropbox/bibliography/default.bib"))
    (citar-bibliography . '("~/Dropbox/bibliography/default.bib"))
    (org-cite-insert-processor . 'citar)
    (org-cite-follow-processor . 'citar)
    (org-cite-activate-processor . 'citar)))

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
  (general-define-key
    :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "C-c C-e" 'macrostep-expand)
  (general-def 'insert smartparens-mode-map
    "C-c [ s" 'sp-backward-slurp-sexp
    "C-c [ b" 'sp-backward-barf-sexp
    "C-c ] s" 'sp-forward-slurp-sexp
    "C-c ] b" 'sp-forward-barf-sexp)
  (leaf my/bind-root
    :config
    (my/bind
      :prefix "SPC"
      "" nil
      "RET" 'vterm-toggle
      "SPC" 'consult-buffer
      "b" '(:ignore t :wk "buffer")
      "f" '(:ignore t :wk "file")
      "g" '(:ignore t :wk "git")
      "h" '(:ignore t :wk "help")
      "o" '(:ignore t :wk "org")
      "q" '(:ignore t :wk "quit")
      "s" '(:ignore t :wk "search")
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
      ;"a" 'consult-apropos
      "f" 'helpful-callable
      "v" 'helpful-variable
      "k" 'helpful-key
      "m" 'describe-mode
      "w" 'dictionary-match-words
      "i" 'info
      "b" 'embark-bindings))
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
     "d" 'toggle-debug-on-error
     "t" 'toggle-truncate-lines
     "l" 'display-line-numbers-mode
     "f" 'treemacs)
    (my/bind
     :keymaps 'prog-mode-map
     :prefix "SPC t"
     "i" 'imenu-list-smart-toggle))
  (leaf my/bind-org
    :config
    (my/bind
      :prefix "SPC o"
      "c" '((lambda () (interactive) (org-capture nil "c"))
	    :wk "capture todos")
      "C" 'org-capture
      "a" 'org-agenda-list
      "A" 'org-agenda
      "t" 'org-todo-list
      "x" 'org-mru-clock-in
      "n" '((lambda ()
	      (interactive)
	      (let ((default-directory my/org-notes-directory))
		(call-interactively 'find-file)))
	    :wk "open notes")
      "r" '(:ignore t :wk "org roam")
      "r n" 'org-roam-node-find
      "r f" 'org-roam-node-find
      "r c" 'org-roam-capture)
    (my/bind
     :prefix "SPC o r"
     :keymaps 'org-mode-map
       "r" 'org-roam-buffer-toggle
       "b" 'org-roam-buffer-display-dedicated
       "t" 'org-roam-tag-add
       "i" 'org-roam-node-insert
       "a" 'org-roam-alias-add
       "g" 'org-roam-graph))
  (leaf my/bind-search
    :config
    (my/bind
     :prefix "SPC s"
     "b" 'consult-line
     "i" 'consult-imenu)
    (my/bind
     :prefix "SPC s"
     :keymaps 'flycheck-mode-map
     "e" 'consult-flycheck)))


(provide 'my-init)
;;; my-init.el ends here
