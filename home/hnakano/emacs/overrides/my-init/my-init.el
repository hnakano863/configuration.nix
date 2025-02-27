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

(require 'my-init-common)

(leaf treemacs
  :doc "treemacs and its extentions"
  :defun treemacs-git-mode
  ;; TODO treemacs-git-modeって本当に必要？ treemacs-magitと被ってない？
  :hook (treemacs-mode-hook . (lambda () (treemacs-git-mode 'deferred)))
  :config
  ;; TODO treemacs-magitの機能を調べる
  (leaf treemacs-magit
    :after treemacs
    :require t)
  ;; TODO treemacs-projectileの機能を調べる
  (leaf treemacs-projectile
    :after treemacs
    :require t))

(leaf context-skk
  :config
  (setq context-skk-programming-mode
	(append context-skk-programming-mode
		'(dataform-mode lookml-mode))))

;; TODO remove shackle
(leaf shackle
  :global-minor-mode t
  :custom
  (shackle-rules . '(("\\*Help\\*" :align below :size 0.42 :select t :popup t)
		     (helpful-mode :align below :size 0.42 :select t :popup t)
                     (magit-status-mode :align right :select t :popup t)
                     (vterm-mode :align below :size 0.35 :select t :popup t)
		     (org-roam-mode :align right :size 0.33 :select nil :popup t))))

(leaf vterm
  :config
  (leaf vterm-toggle
    :after evil
    :custom (vterm-toggle-cd-auto-create-buffer . t)))

(leaf elfeed
  :custom
  (elfeed-feeds . '("https://tech.andpad.co.jp/feed"
		    "https://www.juliabloggers.com/feed")))

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
  :config
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
       (python . t)
       (haskell . t)
       (julia . t)
       (jupyter . t)
       (restclient . t))))

  (leaf org-eldoc
    :hook (org-mode-hook . org-eldoc-load))
  (leaf org-roam
    :custom
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
    :bind (:org-roam-mode-map
	   ("SPC" . nil)
	   ("S-SPC" . nil)))
  (leaf org-cite
    :custom
    (org-cite-global-bibliography . '("~/Dropbox/bibliography/default.bib"))
    (citar-bibliography . '("~/Dropbox/bibliography/default.bib"))
    (org-cite-insert-processor . 'citar)
    (org-cite-follow-processor . 'citar)
    (org-cite-activate-processor . 'citar)))

(leaf general
  :config

  (leaf my/bind-root
    :config
    (my/bind
      :prefix "SPC"
      "RET" 'vterm-toggle))
  (leaf my/bind-toggle
    :config
    (my/bind
     :keymaps 'prog-mode-map
     :prefix "SPC t"
     ;; TODO imenuを使うかどうか
     "i" 'imenu-list-smart-toggle))
  (leaf my/bind-search
    :config
    (my/bind
     :prefix "SPC s"
     :keymaps 'flycheck-mode-map
     "e" 'consult-flycheck))) ; TODO: 見直しす

(provide 'my-init)
;;; my-init.el ends here
