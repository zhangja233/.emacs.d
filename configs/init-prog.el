(defun my-compile(&optional arg)
  (interactive "P") 
  (save-some-buffers 1) 
  (cond ((equal arg '(4))
         ; use compile-buffer in comint mode[note that current-prefix-arg is '(4) ]
	 (call-interactively 'compile t (vector compile-command)))
	(t
	 (setq-local compilation-read-command nil)
	 (call-interactively 'compile)
	 )
	)
  )

(global-set-key (kbd "C-z C-z") 'my-compile)

(eval-after-load "compile"
  '(progn (define-key compilation-minor-mode-map (kbd "C-;") 'quit-window)
	  (define-key compilation-shell-minor-mode-map (kbd "C-;") 'quit-window)
  )
  )

(global-set-key (kbd "C-z C-s") 'shell-command)

(use-package ggtags
  :ensure t
  )

(add-hook 'c-mode-common-hook
    (lambda ()
      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
  (ggtags-mode 1))))

;; perl
(fset 'perl-mode 'cperl-mode)

;; f90-mode
;; Set Fortran 90 mode for .F
(setq auto-mode-alist
      (cons '("\\.F$" . f90-mode) auto-mode-alist))
(eval-after-load "f90"
'(progn
   (define-key f90-mode-map (kbd "C-j") 'newline-and-indent) ; electric-newline-and-maybe-indent does not do what I want
   (define-key f90-mode-map (kbd "C-c w") (kbd "WRITE(*,*) SPC ")) ; make life easier
))

;; lisp-mode
(eval-after-load "lisp-mode"
'(progn
(define-key emacs-lisp-mode-map (kbd "C-c jl") (lambda () (interactive) (insert "(lambda () (interactive) () )") (backward-char 3)))
))
; solve the problem that electric-indent-mode makes intent too much after return
(defun electric-indent-mode-configure ()
  "Delete newline (?\n) from `electric-indent-chars'."
  (setq electric-indent-chars (delq 10 electric-indent-chars)))

(add-hook 'emacs-lisp-mode-hook #'electric-indent-mode-configure)

(use-package markdown-mode
  :ensure t
  )

(use-package json-mode
  :ensure t
  :config
  (define-key json-mode-map (kbd "C-;") 'json-pretty-print-buffer)
  )

(provide 'init-prog)
