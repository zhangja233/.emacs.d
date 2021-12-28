(defun my-compile()
     (interactive) 
     (save-some-buffers 1) 
     (setq current-prefix-arg '(4)) ; to use compile-buffer in comint mode
     (call-interactively 'compile t (vector compile-command))
    )
(global-set-key (kbd "C-z C-z") 'my-compile)

(define-key compilation-minor-mode-map (kbd "C-;") 'quit-window)
(define-key compilation-shell-minor-mode-map (kbd "C-;") 'quit-window)

(global-set-key (kbd "C-z C-s") 'shell-command)

(use-package ggtags
  :ensure t
  )

(add-hook 'c-mode-common-hook
    (lambda ()
      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
  (ggtags-mode 1))))

;; cpp
;(require 'company-c-headers)
;(add-to-list 'company-backends 'company-c-headers)
;(add-to-list 'company-c-headers-path-system "/usr/include/c++/9/")
(eval-after-load "cc"
'(progn
   (define-key c++-mode-map (kbd "C-j") 'newline-and-indent)
   (defun insert-cout()
 (interactive) (insert "cout <<  << endl;") (backward-char 9)
     )
 (define-key c++-mode-map (kbd "C-c c")  'insert-cout)
))



;; python-mode
(eval-after-load "python"
'(progn
  ; (elpy-enable)
   (remove-hook 'elpy-modules 'elpy-module-flymake)
   (defun python-insert-comment ()
     (interactive)
     (insert "#"))
   (define-key python-mode-map (kbd "C-;") 'python-insert-comment)
   (define-key python-mode-map (kbd "C-j") 'newline-and-indent) ; electric-newline-and-maybe-indent does not do what I want
   (define-key python-mode-map (kbd "C-c p")  (lambda() (interactive) (insert "print()") (backward-char)))
   (define-key python-mode-map (kbd "C-c jd")  (lambda() (interactive) (insert "import pandas as pd") (newline-and-indent) (insert "import numpy as np") (newline-and-indent) (insert "from matplotlib import pyplot as plt") (newline-and-indent) (newline-and-indent)))
;(add-to-list 'company-backends 'company-jedi)
))

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

(use-package json-mode
  :ensure t
  :config
  (define-key json-mode-map (kbd "C-;") 'json-pretty-print-buffer)
  )

(provide 'init-prog)
