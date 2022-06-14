(defun display-prefix(arg)
  "Display the value of the raw prefix arg."
  (interactive "P")
  (message "%s" arg))

(bind-keys :map emacs-lisp-mode-map
	   ("C-c C-r" . eval-region)
	   ("C-M-q" . crux-indent-defun)
	   ("M-DEL" . mark-defun)
	   ("C-j" . newline-and-indent))

(use-package lispy
  :ensure t
  :bind (:map lispy-mode-map
	      ("M-m" . nil))
  :hook (emacs-lisp-mode . lispy-mode)
  :config
)

;; solve the problem that electric-indent-mode makes intent too much after return
(defun electric-indent-mode-configure ()
  "Delete newline (?\n) from `electric-indent-chars'."
  (setq electric-indent-chars (delq 10 electric-indent-chars)))

(add-hook 'emacs-lisp-mode-hook #'electric-indent-mode-configure)

(provide 'init-elisp)
