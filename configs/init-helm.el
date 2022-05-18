(use-package helm
  :ensure t
  :diminish helm-mode
  :config
  (use-package swiper-helm
    :ensure t
    :config
    (global-set-key (kbd "C-s") 'swiper)
    )
  (helm-mode 1) ;turn on helm-mode at startup
;  (require 'helm-fd)
  (setq helm-locate-command  
    (cl-case system-type
      ('gnu/linux "locate %s -e -A --regex %s")
      ('windows-nt "es %s -sort run-count -p -n 50 %s") ;; commandline interface for Everything.exe, from voidtools
      ('darwin "mdfind -name %s %s") ;; mdfind seems to work way better than locate in os x
      (t "locate %s")))
  (setq helm-candidate-number-limit 1000) ; increase the number of helm candidates to see, e.g., more files from recentf
					
  ; don't use helm in following cases
  (setq helm-mode-handle-completion-in-region nil) ; don't use helm for completion-at-point
  (dolist (item '( (dired-do-rename . nil)
		   (dired-create-directory . nil)
		   (dired-do-copy . nil))
		)
    (add-to-list 'helm-completing-read-handlers-alist item)
    )

  (setq helm-move-to-line-cycle-in-source t) ;C-p to the bottom when at first candidate

  ; sort helm-buffer list such that last visited is on the top
  (defun helm-buffers-sort-transformer@donot-sort (_ candidates _)
  candidates)
  (advice-add 'helm-buffers-sort-transformer :around 'helm-buffers-sort-transformer@donot-sort)

  (global-set-key (kbd "M-x") 'helm-M-x)
;  (global-set-key (kbd "C-,") 'helm-multi-files)
  (global-set-key (kbd "C-,") 'helm-buffers-list)
  (global-set-key (kbd "C-，") 'helm-buffers-list)  
  (global-set-key (kbd "C-x f") 'helm-find-files)
;  (global-set-key (kbd "M-s o") 'helm-occur)
  (define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)
  (global-set-key (kbd "C-x r b") 'helm-bookmarks)

  (global-set-key (kbd "C-z C-f") 'helm-locate)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (define-key dired-mode-map (kbd "l") 'helm-find-files)
  (bind-keys :map global-map
	     ("C-S-SPC" . helm-mark-ring))
  )

(provide 'init-helm)
