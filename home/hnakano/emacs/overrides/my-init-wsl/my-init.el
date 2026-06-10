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

(require 'my-init-common)
(eval-when-compile
  (require 'use-package))

(require 'compat-31) ; set-locals workaround for emacs 30

;;; Development Support
;; 日本語入力をclaude-codeで実施するためのヘルパー関数
(defun my/claude-code-japanese-input ()
  "Input Japanese text via minibuffer with SKK and send to terminal."
  (interactive)
  (let ((text (minibuffer-with-setup-hook
                  (lambda () (skk-mode 1))
                (read-string "日本語入力: "))))
    (unless (string-empty-p text)
      (claude-code-ide--terminal-send-string text))))

(use-package claude-code-ide
  :bind (:map vterm-mode-map
         ("C-c j" . my/claude-code-japanese-input))
  :config
  (claude-code-ide-emacs-tools-setup))

(use-package gptel-commit
  :after magit
  :custom
  (gptel-commit-use-claude-code t))

;;; Programming Languages
(use-package dataform-mode
  :mode "\\.sqlx\\'"
  :hook
  (dataform-mode . prism-mode)
  (dataform-mode . yas-minor-mode)
  (dataform-mode . (lambda () (setq-local yas-indent-line 'fixed)))
  :config
  (push 'dataform-mode context-skk-programming-mode)
  (push '(dataform-mode 2) copilot-indentation-alist))

(use-package lookml-mode
  :mode "\\.lkml\\'"
  :hook
  (lookml-mode . prism-mode)
  (lookml-mode . yas-minor-mode)
  (lookml-mode . (lambda () (setq-local yas-indent-line 'fixed)))
  :config
  (push 'lookml-mode context-skk-programming-mode)
  (push '(lookml-mode 2) copilot-indentation-alist))

(my/bind
  :prefix "SPC c" ; claude codeなので
  "c" 'claude-code-ide-menu)

(my/bind
  :keymaps 'git-commit-mode-map
  :prefix "SPC c"
  "g" 'gptel-commit)

(provide 'my-init)
;;; my-init.el ends here
