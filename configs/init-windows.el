(when (eq system-type 'windows-nt) 
  (custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Microsoft YaHei Mono" :foundry "outline" :slant normal :weight normal :height 199 :width normal)))))

  (setq w32-pass-rwindow-to-system nil) ; don't pass right win when hitting it alone
  (setq w32-rwindow-modifier 'super) ; right win as super

  ; some frequently used M- key bindings
  (global-set-key (kbd "s-d") 'kill-word)
  (define-key smartparens-mode-map (kbd "s-f") 'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "s-b") 'sp-backward-sexp)
  (define-key my-mode-map (kbd "s-a") 'beginning-of-buffer)
  (define-key my-mode-map (kbd "s-e") 'end-of-buffer)
  (global-set-key (kbd "s-r") 'replace-string)
  (global-set-key (kbd "s-w") 'easy-kill)
  (global-set-key (kbd "s-x") 'helm-M-x)
  (global-set-key (kbd "s-v") 'scroll-down-command)
  
  ;don't pass these 
  (w32-register-hot-key [s-]) ; don't pass all key combinitions of the form "s-"

  (set-language-environment "utf-8")
  
  (use-package ahk-mode
    :ensure t
    )
  (global-set-key (kbd "C-x C-c")  'kill-emacs)
)

(provide 'init-windows)
