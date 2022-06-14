(use-package helm
  :ensure t
  :diminish helm-mode
  ;;  :bind (:map helm-command-map
  ;;	      )
  :bind (("C-z C-f" . helm-locate)
	 ("M-y" . helm-show-kill-ring)
	 ("C-x f" . helm-find-files))
  :config
  ;; (helm-mode 1)
					;  (require 'helm-fd)
  (setq helm-locate-command  
	(cl-case system-type
	  ('gnu/linux "locate %s -e -A --regex %s")
	  ('windows-nt "es %s -sort run-count -p -n 50 %s") ;; commandline interface for Everything.exe, from voidtools
	  ('darwin "mdfind -name %s %s") ;; mdfind seems to work way better than locate in os x
	  (t "locate %s")))
  (setq helm-ff-skip-boring-files t)
  (setq helm-candidate-number-limit 1000) ; increase the number of helm candidates to see, e.g., more files from recentf

  (setq helm-move-to-line-cycle-in-source t) ;C-p to the bottom when at first candidate

					; sort helm-buffer list such that last visited is on the top
  (defun helm-buffers-sort-transformer@donot-sort (_ candidates _)
    candidates)
  (advice-add 'helm-buffers-sort-transformer :around 'helm-buffers-sort-transformer@donot-sort)
  

  ;; (global-set-key (kbd "M-x") 'helm-M-x)
  ;;  (global-set-key (kbd "C-,") 'helm-multi-files)
  ;; (define-key my-mode-map (kbd "C-,") 'helm-buffers-list)
  ;; (define-key my-mode-map (kbd "C-，") 'helm-buffers-list)
  
  ;; 					;  (global-set-key (kbd "M-s o") 'helm-occur)
  ;; (define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)
  ;; (global-set-key (kbd "C-x r b") 'helm-bookmarks)

  ;; (define-key dired-mode-map (kbd "l") 'helm-find-files)
  ;; (bind-keys :map global-map
  ;; 	     ("C-S-SPC" . helm-mark-ring)))
  ) 					; helm ends here
  (provide 'init-helm)
