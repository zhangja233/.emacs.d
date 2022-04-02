;; f90-mode
;; Set Fortran 90 mode for .F
(setq auto-mode-alist
      (cons '("\\.F$" . f90-mode) auto-mode-alist))

(defun insert-exclamation()
  (interactive)
  (insert "!"))
(eval-after-load "f90"
'(progn
(bind-keys :map f90-mode-map
	   ("C-j" . newline-and-indent)
	   ("C-;" . insert-exclamation)
	   ("C-M-a" . f90-beginning-of-block)
	   ("C-M-e" . f90-end-of-block)
	   ("C-c C-f" . f90-next-block)
	   ("C-c C-b" . f90-previous-block)))
)
(setq f90-smart-end 'no-blink)

(provide 'init-f90)
