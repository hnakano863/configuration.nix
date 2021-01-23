(general-create-definer my-bind
  :states '(motion normal)
  :keymaps 'override)

;; root keybinds
(my-bind
  :prefix "SPC"
  "" nil
  "RET" 'vterm-toggle
  "SPC" 'consult-buffer
  "b" '(:ignore t :wk "buffer")
  "e" '(hydra-smartparens-edit/body t :wk "edit")
  "f" '(:ignore t :wk "file")
  "g" '(:ignode t :wk "git")
  "h" '(:ignore t :wk "help")
  "o" '(:ignore t :wk "org")
  "q" '(:ignore t :wk "quit")
  "t" '(:ignore t :wk "toggle")
  "w" '(:ignore t :wk "window"))

(my-bind 'projectile-mode-map
  :prefix "SPC"
  "p" '(:keymap projectile-command-map :wk "projectile"))

;; buffer related keybinds

;;;###autoload
(defun switch-to-scratch-buffer ()
  "Switch or create *scratch* buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (message "switched to *scratch* buffer"))

;;;###autoload
(defun revert-buffer-no-confirm ()
  "Revert buffer but no confirm."
  (interactive)
  ;;(message "force-reverting value is %s" force-reverting)
  (if (not (buffer-modified-p))
      (revert-buffer :ignore-auto :noconfirm)
      (error "The buffer has been modified")))

(my-bind
  :prefix "SPC b"
  "b" 'switch-to-buffer
  "d" 'kill-current-buffer
  "l" 'evil-switch-to-windows-last-buffer
  "r" 'revert-buffer-no-confirm
  "s" 'switch-to-scratch-buffer)

;; file related keybinds
(my-bind
 :prefix "SPC f"
 "f" 'find-file
 "r" 'consult-recent-file
 "t" 'treemacs
 "u" 'undo-tree-visualize)

;; window related keybinds
(my-bind
  :prefix "SPC w"
  "h" 'evil-window-left
  "j" 'evil-window-down
  "k" 'evil-window-up
  "l" 'evil-window-right
  "H" 'evil-window-move-far-left
  "J" 'evil-window-move-very-bottom
  "K" 'evil-window-move-very-top
  "L" 'evil-window-move-far-right
  "s" 'evil-window-split
  "v" 'evil-window-vsplit
  "d" 'evil-window-delete
  "D" 'delete-other-windows
  "0" 'treemacs-select-window
  "1" 'winum-select-window-1
  "2" 'winum-select-window-2
  "3" 'winum-select-window-3
  "4" 'winum-select-window-4
  "5" 'winum-select-window-5
  "6" 'winum-select-window-6
  "7" 'winum-select-window-7
  "8" 'winum-select-window-8
  "9" 'winum-select-window-9
  "w" 'winum-select-window-by-number)

;; help related keybinds
(my-bind
  :prefix "SPC h"
  "a" 'consult-apropos
  "f" 'describe-function
  "v" 'describe-variable
  "k" 'describe-key
  "m" 'describe-mode)

;; quit related keybinds
(my-bind
  :prefix "SPC q"
  "q" 'save-buffers-kill-terminal
  "Q" 'evil-quit-all-with-error-code
  "r" 'restart-emacs
  "R" '((lambda () (interactive "P") (restart-emacs '("--debug-init")))
	:wk "restart-debug-init"))

;; git keybinds
(my-bind
  :prefix "SPC g"
  "g" 'magit-status
  "s" 'magit-status
  "h" 'hydra-git-gutter/body)

;; toggle keybinds
(my-bind
  :prefix "SPC t"
  "t" 'toggle-truncate-lines
  "l" 'display-line-numbers-mode
  "f" 'treemacs
  "v" 'vterm-toggle)

;; org-mode keybinds
(my-bind
  :prefix "SPC o"
  "c" 'org-capture
  "n" '((lambda () (interactive) (find-file org-default-notes-file))
	:wk "open notes")
  "a" 'org-agenda
  "j" 'org-journal-new-entry
  "p" '(org-projectile-project-todo-completing-read :wk "project todo")
  "f" 'org-roam-find-file
  "i" 'org-roam-insert
  "I" 'org-roam-insert-immediate
  "t" 'org-roam-tag-add
  "r" 'org-roam
  "g" 'org-roam-graph)
