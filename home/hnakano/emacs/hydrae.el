;; hydra-git-gutter
;; toggle git-gutter popup window

;;;###autoload
(defun my--git-gutter:toggle-popup-hunk ()
  (interactive)
  (if (window-live-p (git-gutter:popup-buffer-window))
      (delete-window (git-gutter:popup-buffer-window))
    (git-gutter:popup-hunk)))

;; hydra maps
(defhydra hydra-git-gutter (:color red :hint nil)
  "
  _j_: next _k_: previous _s_: stage _r_: revert _d_: popup diff
_TAB_: toggle diff _ESC_: exit"
  ("j" git-gutter:next-hunk)
  ("k" git-gutter:previous-hunk)
  ("s" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  ("d" git-gutter:popup-hunk)
  ("TAB" my--git-gutter:toggle-popup-hunk)
  ("ESC" nil :exit t))

;; hydra-smartparens
(defhydra hydra-smartparens-edit (:color red :hint nil)
  "
_j_: join     _J_: join all   _C-h_: evil-h    _(_: b/s forward
_s_: split    _S_: split all  _C-l_: evil-l    _)_: b/s backward
_u_: unwrap                                  _ESC_: exit"
  ("j" sp-join-sexp)
  ("J" (sp-join-sexp '(4)))
  ("s" sp-split-sexp)
  ("S" (sp-split-sexp '(4)))
  ("u" sp-splice-sexp)
  ("C-h" evil-backward-char)
  ("C-l" evil-forward-char)
  ("(" hydra-smartparens-forward/body :exit t)
  (")" hydra-smartparens-forward/body :exit t)
  ("ESC" nil :exit t))

(defhydra hydra-smartparens-forward (:color red :hint nil)
  "
_h_: slurp    _C-h_: evil-h   _)_: forward 
_l_: barf     _C-l_: evil-l   _(_: backward
_e_: edit                   _ESC_: exit"
  ("h" sp-forward-barf-sexp)
  ("l" sp-forward-slurp-sexp)
  ("e" hydra-smartparens-edit/body :exit t)
  ("C-h" evil-backward-char)
  ("C-l" evil-forward-char)
  (")" nil)
  ("(" hydra-smartparens-backward/body :exit t)
  ("ESC" nil :exit t))

(defhydra hydra-smartparens-backward (:color red :hint nil)
  "
_h_: slurp    _C-h_: evil-h   _)_: forward 
_l_: barf     _C-l_: evil-l   _(_: backward
_e_: edit                   _ESC_: exit"
  ("h" sp-backward-slurp-sexp)
  ("l" sp-backward-barf-sexp)
  ("e" hydra-smartparens-edit/body :exit t)
  ("C-h" evil-backward-char)
  ("C-l" evil-forward-char)
  (")" hydra-smartparens-forward/body :exit t)
  ("(" nil)
  ("ESC" nil :exit t))
