;; initchart setup
(require 'initchart)
(initchart-record-execution-time-of load file)
(initchart-record-execution-time-of require feature)

;; some constants setup
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      auto-save-list-file-prefix nil
      read-process-output-max (* 3 1024 1024))

;; ui setup
(blink-cursor-mode -1)
(global-hl-line-mode 1)
(make-variable-buffer-local 'global-hl-line-mode)
(add-hook 'term-mode-hook (lambda () (setq-local global-hl-line-mode nil)))

;; remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; require for configuration
(require 'general)
(require 'hydra)

;; recentf-mode
(add-hook 'emacs-startup-hook '(lambda () (recentf-mode +1)))

;; undo-patch
(defun my/undo-fu-before-advice (&optional arg)
  "Turn off undo-tree-mode if it is enabled.
ARG is required for advising."
  (when (and (featurep 'undo-tree) undo-tree-mode)
    (undo-tree-mode -1)))

(defun my/undo-tree-before-advice (&optional arg)
  "Turn on undo-tree-mode if it is disabled.
ARG is required for advising."
  (unless (or (not (featurep 'undo-tree)) undo-tree-mode)
    (undo-tree-mode +1)))

(advice-add 'undo-fu-only-undo :before #'my/undo-fu-before-advice)
(advice-add 'undo-fu-only-redo :before #'my/undo-fu-before-advice)
(advice-add 'undo-tree-visualize :before #'my/undo-tree-before-advice)

;;; prelude.el ends here.
