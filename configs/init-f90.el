;; f90-mode
;; Set Fortran 90 mode for .F
(setq auto-mode-alist
      (cons '("\\.F$" . f90-mode) auto-mode-alist))

(defun insert-exclamation()
  (interactive)
  (insert "!"))
(bind-keys :map f90-mode-map
	   ("C-j" . newline-and-indent)
	   ("C-;" . insert-exclamation))

(provide 'init-f90)
