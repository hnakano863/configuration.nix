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
;; LLM tools
(defun my/auth-source-get-gemini-api-key ()
  "Get gemini api key using auth-source."
  (auth-info-password (car (auth-source-search :host "gemini"))))

(use-package gptel
  :defer t
  :custom
  (gptel-model 'gemini-2.0-flash-exp)
  (gptel-default-mode 'org-mode)
  (gptel-crowdsourced-prompts-file (locate-user-emacs-file ".cache/gptel-crowdsourced-prompts.csv"))
  :config
  (setq gptel-backend
	(gptel-make-gemini "Gemini"
	  :key #'my/auth-source-get-gemini-api-key
	  :stream t)))

(my/bind
  :prefix "SPC l"
  "l" 'gptel)

(provide 'my-init)
;;; my-init.el ends here
