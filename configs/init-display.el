(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t) ;do not show welcome page
(setq initial-scratch-message "Have fun!\n\n")
(setq visible-bell nil)
(setq auto-save-no-message t)

(use-package diminish
  :ensure t
  :config
  (diminish 'my-mode)
  (diminish 'eldoc-mode)
  )

(defun set-font-size(size)
   "190 for external; 220 for normal; 170 for internal double"
  (interactive "p")
   (set-face-attribute 'default nil :height size)
)

(global-set-key (kbd "C-<f13>") 'set-font-size)

(setq-default fill-column 80)
(global-set-key (kbd "C-z h") 'fill-paragraph)

(provide 'init-display)
