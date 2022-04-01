(custom-set-variables
 '(c-basic-offset 2))


(defun c-insert-comment()
  (interactive) (insert "//") 
  )

(defun c-open-line-above()
  (interactive)
  (beginning-of-line)
  (c-context-open-line)
  (indent-for-tab-command)
  )
(add-hook 'c-mode-hook 'auto-fill-mode)
(add-hook 'c++-mode-hook 'auto-fill-mode)

(bind-keys :map c-mode-base-map
	   ("C-;" . c-insert-comment)
;	   ("C-o" . open-line-above)
	   )  
; completion of headers
(use-package company-c-headers
  :ensure t
  :config
  (add-to-list 'company-backends 'company-c-headers)
  (add-to-list 'company-c-headers-path-system 
    (cl-case system-type
      ('gnu/linux "/usr/include/c++/9/")
      ('darwin "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/c++/v1") 
      ))
  )

(eval-after-load "cc"
'(progn
   (define-key c++-mode-map (kbd "C-j") 'newline-and-indent)
 (define-key c++-mode-map (kbd "C-;")  'c-insert-comment)
))

(provide 'init-c)
