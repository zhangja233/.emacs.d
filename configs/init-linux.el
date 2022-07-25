(when (eq system-type 'gnu/linux)
  ;; this font size works well on ubuntu desktop
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:foundry "outline" :slant normal :weight normal :height 165 :width normal)))))
  (set-face-attribute 'region nil :background "yellow")    
  (custom-set-variables
   '(markdown-command "/usr/bin/pandoc"))
  
  (define-key key-translation-map [(control ?\h)]  [127]) ; bind C-h to Backspace, otherwise in searching C-h just literally becomes ^H
  (global-set-key (kbd "C-h") (kbd "<backspace>"))
  
  )

(provide 'init-linux)
