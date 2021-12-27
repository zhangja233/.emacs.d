(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-math-abbrev-prefix [C-return])
 '(LaTeX-math-list
   '((104 "hbar ")
     (105 "infty ")
     (117 "hat ")
     (39 "prime ")
     (124 "bigg|")
     ("SPC" "quad ")
     ("C-SPC"
      (lambda nil
	(interactive)
	(insert " &= ")))
     ("C-b" insert-bra "" nil)
     ("C-k" insert-ket "" nil)
     ("DEL" insert-hline "" nil)))
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(spacemacs-theme diminish disable-mouse ggtags google-this valign yasnippet-snippets yasnippet easy-kill paredit smartparens use-package htmlize org-bullets auctex openwith ox-reveal keyfreq json-mode elpy pyim-basedict browse-kill-ring ace-window company counsel magit helm mu4e-maildirs-extension ivy avy ess markdown-mode pdf-tools pyim auctex-cluttex exec-path-from-shell csv-mode gnu-elpa-keyring-update))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Microsoft YaHei Mono" :foundry "outline" :slant normal :weight normal :height 199 :width normal))))
 '(font-latex-sectioning-0-face ((t :height 301 :foreground "red" :weight bold)))
 '(font-latex-sectioning-1-face ((t (:height 301 :foreground "red" :weight bold))))
 '(font-latex-sectioning-2-face ((t (:height 281 :foreground "RoyalBlue4" :weight bold))))
 '(font-latex-sectioning-3-face ((t (:height 261 :foreground "orange" :weight bold))))
 '(font-latex-sectioning-4-face ((t (:height 241 :foreground "green" :weight bold))))
 '(font-latex-sectioning-5-face ((t (:height 221 :foreground "purple" :weight bold)))))


