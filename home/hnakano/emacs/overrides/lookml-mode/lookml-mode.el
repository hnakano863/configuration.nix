;;; lookml-mode.el --- Major mode for editing Lookml files -*- lexical-binding: t; -*-

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

;; Major mode for editing Lookml.

;;; Code:

(defgroup lookml-mode nil
  "Lookml mode customizations."
  :group 'languages)

;;;###autoload
(define-derived-mode lookml-mode prog-mode "Lookml"
  "Major mode for editing Lookml files."
  :group 'lookml-mode

  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (setq-local comment-start "# "))

(provide 'lookml-mode)
;;; lookml-mode.el ends here
