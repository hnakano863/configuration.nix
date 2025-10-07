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

;;; Development Support
;; copilot.el configurations
(use-package copilot
  :hook prog-mode
  :bind
  (:map copilot-completion-map
   ("TAB" . copilot-accept-completion)))

(defun my/auth-source-get-gemini-api-key ()
  "Get gemini api key using auth-source."
  (auth-info-password (car (auth-source-search :host "gemini"))))

(use-package gptel
  :defer t
  :custom
  (gptel-model 'gemini-2.5-pro)
  (gptel-default-mode 'org-mode)
  :config
  (setq gptel-backend
        (gptel-make-gh-copilot "Gemini")))

(use-package gptel-commit
  :bind
  (:map git-commit-mode-map
   ("C-c g" . gptel-commit)))

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
  :prefix "SPC l" ; LLMなので
  "l" 'gptel
  "a" 'gptel-add
  "f" 'gptel-add-file
  "w" 'gptel-rewrite)

(provide 'my-init)
;;; my-init.el ends here
