(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t) ;do not show welcome page
(setq initial-scratch-message "Have fun!\n\n")
(setq visible-bell nil)
(setq auto-save-no-message t)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(set-face-attribute 'region nil :background "blue")

(use-package diminish
  :ensure t
  :config
  (diminish 'my-mode)
  (diminish 'eldoc-mode)
  (diminish 'abbrev-mode))

(defun set-font-size(size)
   "190 for external; 220 for normal; 170 for internal double"
  (interactive "p")
   (set-face-attribute 'default nil :height size)
)

(when (eq system-type 'darwin)
  (set-font-size 190))

(global-set-key (kbd "C-<f13>") 'set-font-size)

(setq-default fill-column 80)
(global-set-key (kbd "C-z h") 'fill-paragraph)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;; the cursor
(blink-cursor-mode 0)

(provide 'init-display)
