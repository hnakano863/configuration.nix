;;; my-init-common.el --- My init.el -*- lexical-binding: t; -*-

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

;; My init configurations common with Linux and WSL2 machines.

;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'consult)) ;to compile consult-customize

(defun print-startup-stats ()
  "Prints some basic startup statistics."
  (let ((elapsed (float-time (time-subtract after-init-time before-init-time))))
    (message "Startup took %.2fs with %d GCs" elapsed gcs-done)))

;;; Customize variables
(use-package emacs
  :custom
  (backup-directory-alist '((".*" . "/tmp/")))
  (epg-pinentry-mode 'loopback)
  (custom-file (locate-user-emacs-file "custom.el"))
  (read-process-output-max (* 3 1024 1024))
  :init
  (add-hook 'before-save-hook #'delete-trailing-whitespace)
  (add-hook 'emacs-startup-hook #'print-startup-stats)
  (blink-cursor-mode -1))

;;; Useful functionalities
(use-package recentf
  :unless noninteractive ; avoid load when byte compile
  :config
  (recentf-mode 1))

(use-package winum
  :config
  (winum-mode 1))

(use-package which-key
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode 1))

;; auth source
(use-package auth-source-pass
  :after auth-source
  :custom
  (auth-source-pass-filename "~/.local/share/password-store")
  :config
  (auth-source-pass-enable))

;;; Undo systems
(use-package undo-fu
  :defer t
  :custom
  (undo-limit 67108864) ; 64mb.
  (undo-strong-limit 100663296) ; 96mb.
  (undo-outer-limit 1006632960)) ; 960mb.

(use-package undo-fu-session
  :unless noninteractive ; avoid load when byte compile
  :config
  (undo-fu-session-global-mode 1))

;;; Version Control Systems
(use-package magit
  :commands magit-after-save-refresh-status
  :custom
  (magit-process-find-password-functions '(magit-process-password-auth-source))
  :init
  (add-hook 'after-save-hook #'magit-after-save-refresh-status))

(use-package forge :after magit)

;;; Completions
;; Cool completion UI
(use-package vertico
  :custom
  (vertico-cycle t)
  (enable-recursive-minibuffers t)
  :config
  (vertico-mode 1))

;; Orderless completion style
(use-package orderless
  :defer t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Useful completion commands
(use-package consult
  :bind ("C-s" . consult-line)
  :config
  (consult-customize
   consult-recent-file consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.5 any)))

;; Rich minibuffer annotations
(use-package marginalia
  :config
  (marginalia-mode 1))

;;; Highlight
(use-package hl-line
  :hook
  ((vterm-mode eshell-mode comint-mode term-mode) . (lambda () (hl-line-mode -1)))
  :config
  (global-hl-line-mode 1))

;;; Evil
(use-package evil
  :custom
  (evil-want-keybinding nil) ; for evil-collection
  (evil-want-C-u-scroll t)
  (evil-undo-system 'undo-fu)
  :config
  (evil-mode 1))

(provide 'my-init-common)
;;; my-init-common.el ends here
