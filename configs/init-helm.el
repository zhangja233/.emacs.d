(use-package helm
  :ensure t
  :diminish helm-mode
  :config
  (helm-mode 1) ;turn on helm-mode at startup
  (setq helm-locate-command  
    (cl-case system-type
      ('gnu/linux "locate -i -r %s")
      ('windows-nt "es %s -sort run-count -p -n 50 %s") ;; commandline interface for Everything.exe, from voidtools
      ('darwin "mdfind -name %s %s") ;; mdfind seems to work way better than locate in os x 
      (t "locate %s")))
  
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-,") 'helm-buffers-list)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "M-s o") 'helm-occur)
  (define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)
  (global-set-key (kbd "C-x r b") 'helm-filter-bookmarks)

  (global-set-key (kbd "C-z C-f") 'helm-locate)
  (global-set-key (kbd "C-z y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-z SPC") 'helm-all-mark-rings)
  )

(provide 'init-helm)
