(defun my-compile(&optional arg)
  (interactive "P") 
  (save-some-buffers 1) 
  (cl-case major-mode
  ('latex-mode (TeX-command-run-all nil))  
  (t (cond ((equal arg '(4))
         ; use compile-buffer in comint mode[note that current-prefix-arg is '(4) ]
	 (call-interactively 'compile t (vector compile-command)))
	(t
	 (setq-local compilation-read-command nil)
	 (call-interactively 'compile))))))

(global-set-key (kbd "C-z C-z") 'my-compile)

(eval-after-load "compile"
  '(progn (define-key compilation-minor-mode-map (kbd "C-;") 'quit-window)
	  (define-key compilation-shell-minor-mode-map (kbd "C-;") 'quit-window)))

(global-set-key (kbd "C-z C-s") 'shell-command)

(global-set-key (kbd "<f2> e") 'eshell)


(use-package ggtags
  :ensure t)

(add-hook 'c-mode-common-hook
    (lambda ()
      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
  (ggtags-mode 1))))

;; perl
;(fset 'perl-mode 'cperl-mode)
;(setq-default cperl-invalid-face nil)


(defun insert-number-sign () (interactive) (insert "#"))

(eval-after-load "perl"
'(progn
   (bind-keys :map perl-mode-map
	      ("C-;" . insert-single-dollar)
	      ("M-;" . insert-number-sign))))   

;; f90-mode
;; Set Fortran 90 mode for .F
(setq auto-mode-alist
      (cons '("\\.F$" . f90-mode) auto-mode-alist))
(eval-after-load "f90"
'(progn
   (define-key f90-mode-map (kbd "C-j") 'newline-and-indent) ; electric-newline-and-maybe-indent does not do what I want
   (define-key f90-mode-map (kbd "C-c w") (kbd "WRITE(*,*) SPC ")))) ; make life easier

(use-package matlab
  :ensure matlab-mode
  :config
  (add-to-list
   'auto-mode-alist
   '("\\.m\\'" . matlab-mode))
  (setq exec-path (append exec-path '("/Applications/MATLAB_R2021b.app/bin"))))

(use-package markdown-mode
  :ensure t
  :bind 
(:map markdown-mode-map
      ("M-p" . nil)
      ("M-n" . nil)))

(use-package json-mode
  :ensure t
  :config
  (define-key json-mode-map (kbd "C-;") 'json-pretty-print-buffer))

(provide 'init-prog)
