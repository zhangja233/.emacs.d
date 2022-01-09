(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t) ;do not show welcome page
(setq visible-bell nil)
(setq auto-save-no-message t)

(use-package diminish
  :ensure t
  :config
  (diminish 'my-mode)
  (diminish 'eldoc-mode)
  )

(defun set-font-size(size)
  (interactive)
  (set-face-attribute 'default nil :height size)
)

(defun font-size-extscreen() (interactive)
(set-font-size 281)
)
(global-set-key (kbd "C-<f13>") 'font-size-extscreen)

(setq-default fill-column 120)

(provide 'init-display)
