(defun c-insert-comment()
  (interactive) (insert "//") 
  )

(add-hook 'c-mode-common-hook
	  (lambda ()
    (define-key c-mode-base-map (kbd "C-;")  'c-insert-comment)
	    ))
  
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

