(when (eq system-type 'gnu/linux)
  ;; this font size works well on ubuntu desktop
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:foundry "outline" :slant normal :weight normal :height 165 :width normal)))))
  (set-face-attribute 'region nil :background "yellow")    
  (defun find-linux-dot-org()
    (interactive)
    (find-file "~/org/linux.org"))
  (global-set-key (kbd "C-z p") 'find-linux-dot-org)
  (custom-set-variables
   '(markdown-command "/usr/bin/pandoc"))
  )

(provide 'init-linux)
