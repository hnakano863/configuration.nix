;;; org-config.el --- org-mode config after package loading.

;;; Commentary:
;;; This file is for configuration about org-mode.
;;; The codes below are executed after loading org-mode.

;;; Code:

(defun doom/org-fix-newline-and-indent-in-src-blocks (&optional indent arg interactive)
  (when (and org-src-tab-acts-natively (org-in-src-block-p t))
    (org-babel-do-in-edit-buffer
     (call-interactively #'indent-for-tab-command))))

(advice-add 'org-return :after #'doom/org-fix-newline-and-indent-in-src-blocks)
(add-hook 'org-tab-first-hook #'doom/org-fix-newline-and-indent-in-src-blocks)
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (gnuplot . t)
   (python . t)
   (julia . t)
   (jupyter . t)
   (restclient . t)))

;;; org-config.el ends here.
