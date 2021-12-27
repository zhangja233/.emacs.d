;; finding help and documentations

(define-key help-mode-map (kbd "f") 'help-go-forward)
(define-key help-mode-map (kbd "b") 'help-go-back)

(setq find-function-C-source-directory "~/lib/emacs/src")

(provide 'init-help)
