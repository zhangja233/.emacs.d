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
	   ("RET" . newline-and-indent)
	   ("C-;" . insert-exclamation)
	   ("C-M-a" . f90-beginning-of-defun)
	   ("C-M-e" . f90-end-of-defun)
	   ("C-c C-f" . f90-next-block)
	   ("C-c C-b" . f90-previous-block))

(defconst f90-defun-re
  (concat "\\(\\(?:block[ \t]*data\\|"
          (regexp-opt '("function" "subroutine"))
          "\\)\\_>\\)")
  "Regexp potentially indicating a \"defun\" of F90 code.")

(defun f90-beginning-of-defun (&optional arg)
  (interactive "p")
  (or (consp arg) (region-active-p) (push-mark))    
  (re-search-backward (concat "^[\t ]*" f90-defun-re) nil 'move)
  (back-to-indentation))

(defun f90-end-of-defun (&optional arg)
  (interactive "p")
  (or (consp arg) (region-active-p) (push-mark))  
  (re-search-forward (concat "^[\t ]*end[\t ]*" f90-defun-re) nil 'move)
  (end-of-line))

)) ; f90 ends here

(setq f90-smart-end 'no-blink)
(setq f90-critical-indent 2)
(setq f90-do-indent 2)
(setq f90-if-indent 2)
(setq f90-type-indent 2)
(setq f90-program-indent 2)


(provide 'init-f90)
