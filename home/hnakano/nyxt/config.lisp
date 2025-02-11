;; search engine configuration
(define-configuration context-buffer
  ((search-engines
    ;; DuckDuckGo以外を削除
    (remove-if-not #'(lambda (engine)
		       (equal (name engine) "DuckDuckGo"))
		   %slot-value%))))

;; 基本的にvimキーバインドで移動する
(define-configuration buffer
  ((default-modes
    (pushnew 'nyxt/mode/vi:vi-normal-mode %slot-value%))))

;; execute-commandをM-xにbindする
(define-configuration input-buffer
  ((override-map
    (let ((map (make-keymap "override-map")))
      (define-key map "M-x" 'execute-command "C-space" 'nothing)))))

;; prompt-bufferのデフォルトをvi-insert modeにする
(define-configuration prompt-buffer
  ((default-modes
    (pushnew 'nyxt/mode/vi:vi-insert-mode %slot-value%))))

;; キーマップ
(define-configuration base-mode
  ((keyscheme-map
    (define-keyscheme-map "my-base" (list :import %slot-value%)
      nyxt/keyscheme:vi-normal (list "space space" 'switch-buffer
				     "?" 'describe-bindings
				     "space h b" 'describe-bindings
				     "space h f" 'describe-function
				     "space h c" 'describe-command
				     "space h C" 'describe-class
				     "space h k" 'describe-key
				     "space h m" 'describe-mode
				     "space h v" 'describe-variable
				     "space h s" 'describe-slot
				     "space f f" 'set-url-new-buffer
				     "space f F" 'set-url
				     "space b b" 'switch-buffer
				     "space b r" 'reload-current-buffer
				     "space b d" 'delete-current-buffer
				     "space b D" 'delete-buffer
				     "space q q" 'quit
				     "y f" 'copy-hint-url)))))

(define-configuration document-mode
  ((keyscheme-map
    (define-keyscheme-map "my-document" (list :import %slot-value%)
      nyxt/keyscheme:vi-normal (list "space" nil
				     "C-e" 'scroll-down
				     "C-y" 'scroll-up)))))

(define-configuration vi-insert-mode
  ((keyscheme-map
    (define-keyscheme-map "my-vi-insert" (list :import %slot-value%)
      nyxt/keyscheme:vi-insert (list "C-[" 'switch-to-vi-normal-mode)))))

(define-configuration search-buffer-mode
  ((keyscheme-map
    (define-keyscheme-map "my-search-buffer" (list :import %slot-value%)
      nyxt/keyscheme:vi-normal (list "?" 'nil
				     "C-[" 'remove-search-marks
				     "escape" 'remove-search-marks
				     "space s b" 'search-buffer)))))

(define-configuration hint-mode
  ((keyscheme-map
    (define-keyscheme-map "my-hint" (list :import %slot-value%)
      nyxt/keyscheme:vi-normal (list "space f b" 'follow-hint-new-buffer-focus
				     "space f B" 'follow-hint)))))

(define-configuration prompt-buffer-mode
  ((keyscheme-map
    (define-keyscheme-map "my-prompt-buffer" (list :import %slot-value%)
      nyxt/keyscheme:vi-normal (list "C-n" 'next-suggestion
				     "C-p" 'previous-suggestion
				     "C-g" 'quit-prompt-buffer
				     "C-return" 'toggle-mark-forwards)
      nyxt/keyscheme:vi-insert (list "C-n" 'next-suggestion
				     "C-p" 'previous-suggestion
				     "C-g" 'quit-prompt-buffer
				     "C-return" 'toggle-mark-forwards)))))

;; prompt-buffer-modeではdefault以外のkeyschemeが定義されていない
;; そのため、prompt-buffer-modeでkeyschemeを定義した場合、同じものを再定義しないとエラーになる
(define-configuration hint-prompt-buffer-mode
  ((keyscheme-map
    (define-keyscheme-map "my-hint-prompt-buffer" nil
      nyxt/keyscheme:vi-normal (list "C-n" 'next-suggestion
				     "C-p" 'previous-suggestion
				     "C-g" 'quit-prompt-buffer
				     "C-return" 'toggle-mark-forwards)
      nyxt/keyscheme:vi-insert (list "C-n" 'next-suggestion
				     "C-p" 'previous-suggestion
				     "C-g" 'quit-prompt-buffer
				     "C-return" 'toggle-mark-forwards)))))
