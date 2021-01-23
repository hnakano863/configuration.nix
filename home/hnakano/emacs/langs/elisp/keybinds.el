(general-def 'visual emacs-lisp-mode-map
  ("M-(" 'sp-wrap-round)
  ("M-[" 'sp-wrap-square)
  ("M-{" 'sp-wrap-curly)
  ("M-\"" #'(lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "\""))))
