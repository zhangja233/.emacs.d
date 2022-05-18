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
(add-hook 'c-mode-common-hook '(lambda () (modify-syntax-entry ?_ "w" c-mode-syntax-table) ))




(defun my-prettify-c-block-comment (orig-fun &rest args)
  "see https://emacs.stackexchange.com/questions/14563/how-to-automatically-create-neat-c-comment-blocks-while-typing"
  (let* ((first-comment-line (looking-back "/\\*\\s-*.*"))
         (star-col-num (when first-comment-line
                         (save-excursion
                           (re-search-backward "/\\*")
                           (1+ (current-column))))))
    (apply orig-fun args)
    (when first-comment-line
      (save-excursion
        (newline)
        (dotimes (cnt star-col-num)
          (insert " "))
        (move-to-column star-col-num)
        (insert "*/"))
      (move-to-column star-col-num) ; comment this line if using bsd style
      (insert "*") ; comment this line if using bsd style
     ))
  ;; Ensure one space between the asterisk and the comment
  ;(when (not (looking-back " "))
   ; (insert " "))
    )
(advice-add 'c-indent-new-comment-line :around #'my-prettify-c-block-comment)

(bind-keys :map c-mode-base-map
	   ("C-;" . c-insert-comment)
	   ("M-;" . c-indent-new-comment-line)
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
