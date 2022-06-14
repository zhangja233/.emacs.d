(use-package magit
:ensure  t
:init
(global-auto-revert-mode) ; revert any buffer if the file changes on disk
:bind (("C-z C-x c" . magit-clone))
  )

; refresh the status buffer whenever a buffer within the corresponding repo
(with-eval-after-load 'magit-mode
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))



(provide 'init-magit)
