;;; my-early-init.el --- My early-init.el -*- lexical-binding: t; -*-

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

;; My early init file.

;;; Code:
(require 'benchmark-init)
(add-hook 'after-init-hook 'benchmark-init/deactivate)

(defun early-init-reduce-gc ()
  "Reduce the frequency of garbage collection."
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6))

(defun early-init-restore-gc ()
  "Restore the frequency of garbage collection."
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

;; Make GC more rare during init, while minibuffer is active, and
;; when shutting down. In the latter two cases we try doing the
;; reduction early in the hook.
(early-init-reduce-gc)
(add-hook 'minibuffer-setup-hook #'early-init-reduce-gc -50)
(add-hook 'kill-emacs-hook #'early-init-reduce-gc -50)

;; But make it more regular after startup and after closing minibuffer.
(add-hook 'emacs-startup-hook #'early-init-restore-gc)
(add-hook 'minibuffer-exit-hook #'early-init-restore-gc)

;; Avoid expensive frame resizing.
(setq frame-inhibit-implied-resize t)

;; Disable menu-bar
(push '(menu-bar-lines . 0) default-frame-alist)

;; Disable tool-bar
(push '(tool-bar-lines . 0) default-frame-alist)

;; Disable scroll-bar
(push '(vertical-scroll-bars . nil) default-frame-alist)

;; Set font
(push '(font . "HackGen-14") default-frame-alist)

;; Fullscreen at startup
(push '(fullscreen . maximized) initial-frame-alist)

;; Speed up package activation
(setq package-quickstart t)

(provide 'my-early-init)
;;; my-early-init.el ends here
