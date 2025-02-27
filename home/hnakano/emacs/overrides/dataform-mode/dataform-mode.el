;;; dataform-mode.el --- Major mode for Dataform -*- lexical-binding: t; -*-

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

;; Major mode for Dataform.

;;; Code:

(defgroup dataform-mode nil
  "Dataform mode customizations."
  :group 'languages)

;;;###autoload
(define-derived-mode dataform-mode prog-mode "Dataform"
  "Major mode for Dataform."
  :group 'dataform-mode

  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (setq-local comment-start "--"))

(provide 'dataform-mode)
;;; dataform-mode.el ends here
