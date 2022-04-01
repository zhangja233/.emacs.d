(defun display-prefix(arg)
  "Display the value of the raw prefix arg."
  (interactive "P")
  (message "%s" arg))


; solve the problem that electric-indent-mode makes intent too much after return
(defun electric-indent-mode-configure ()
  "Delete newline (?\n) from `electric-indent-chars'."
  (setq electric-indent-chars (delq 10 electric-indent-chars)))

(add-hook 'emacs-lisp-mode-hook #'electric-indent-mode-configure)
