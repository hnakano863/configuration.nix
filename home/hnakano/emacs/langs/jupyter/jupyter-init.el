(defun my/jupyter-command-advice (orig-fun &rest args)
  (with-temp-buffer
    (when (zerop (apply #'process-file jupyter-executable nil t nil args))
      (string-trim-right (buffer-string)))))

(advice-add 'jupyter-command :around #'my/jupyter-command-advice)
