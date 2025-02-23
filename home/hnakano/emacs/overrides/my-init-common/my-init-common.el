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
  (require 'use-package))

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

;;; Global minor modes
(use-package recentf
  :config
  (recentf-mode 1))

(use-package winum
  :config
  (winum-mode 1))

(use-package which-key
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode 1))

;;; Auth source
(use-package auth-source-pass
  :after auth-source
  :custom
  (auth-source-pass-filename "~/.local/share/password-store")
  :config
  (auth-source-pass-enable))

;;; Version Control Systems
(use-package magit
  :commands magit-after-save-refresh-status
  :custom
  (magit-process-find-password-functions '(magit-process-password-auth-source))
  :init
  (add-hook 'after-save-hook #'magit-after-save-refresh-status))

(use-package forge :after magit)

(provide 'my-init-common)
;;; my-init-common.el ends here
