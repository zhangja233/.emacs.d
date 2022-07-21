;; f90-mode
;; Set Fortran 90 mode for .F

(setq auto-mode-alist
      (cons '("\\.F$" . f90-mode) auto-mode-alist))

(add-hook 'f90-mode-hook
	  (lambda ()
	    (bind-keys :map f90-mode-map
		       ("C-j" . newline-and-indent)
		       ("RET" . newline-and-indent)
		       ("C-;" . comment-line)
		       ("`" . read-only-mode)
		       ("C-M-a" . f90-beginning-of-defun)
		       ("C-M-e" . f90-end-of-defun)
		       ("C-c C-f" . f90-end-of-block)
		       ("C-c C-r" . f90-fill-region)
		       ("M-h" . f90-mark-block)
		       ("C-c C-b" . f90-beginning-of-block)
		       ("C-c C-l" . downcase-region))
	    
	    (defconst f90-defun-re
	      (concat "\\(\\(?:block[ \t]*data\\|"
		      (regexp-opt '("function" "subroutine"))
		      "\\)\\_>\\)")
	      "Regexp potentially indicating a \"defun\" of F90 code.")

	    (defun f90-mark-block ()
	      "Put mark at end of F90 block, point at beginning, push mark."
	      (interactive)
	      (let ((pos (point)) my-block)
		(f90-end-of-block)
		(push-mark)
		(goto-char pos)
		(setq my-block (f90-beginning-of-block))
		(setq mark-active t
		      deactivate-mark nil)
		my-block))

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
     
	    (defun f90-hideshow-forward-sexp-function (_arg)
	      "f90 specific `forward-sexp' function for `hs-minor-mode'.
Argument ARG is ignored."
	      (f90-end-of-defun)
	      ;; (unless (python-info-current-line-empty-p)
	      ;;   (backward-char))
	      )

	    (add-to-list
	     'hs-special-modes-alist
	     '(f90-mode
	       "\\s-*\\_<\\(?:subroutine\\|function\\)\\_>"
	       ;; Use the empty string as end regexp so it doesn't default to
	       ;; "\\s)".  This way parens at end of defun are properly hidden.
	       ""
	       "!"
	       f90-hideshow-forward-sexp-function
	       nil))
     
	    (hs-minor-mode)
	    (flyspell-prog-mode)
	    (view-mode)
	    (bind-keys :map f90-mode-map
		       ("S-<tab>" . hs-hide-all)
		       ("C-c <tab>" . hs-show-block))
	    )) ; f90 ends here

(setq f90-smart-end 'no-blink)
(setq f90-critical-indent 2)
(setq f90-do-indent 2)
(setq f90-if-indent 2)
(setq f90-type-indent 2)
(setq f90-program-indent 2)

(provide 'init-f90)
