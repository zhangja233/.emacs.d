;; filter and completion

;;; completion
(when (display-graphic-p)
(define-key input-decode-map (kbd "C-i") (kbd "H-i"))
)
(global-set-key (kbd "H-i") 'dabbrev-expand)
(global-set-key (kbd "C-z C-e") 'hippie-expand) 

(use-package company
:ensure t
:bind (:map my-mode-map
	   ("M-i" . company-complete))
:diminish company-mode
:config
(global-company-mode) 
(defun my-company-show-doc-buffer ()
  "Temporarily show the documentation buffer for the selection."
  (interactive)
  (let* ((selected (nth company-selection company-candidates))
         (doc-buffer (or (company-call-backend 'doc-buffer selected)
                         (error "No documentation available"))))
    (with-current-buffer doc-buffer
      (goto-char (point-min)))
    (display-buffer doc-buffer t)))

(setq company-dabbrev-downcase nil) ;make completion case sensitive
(setq company-idle-delay nil) ; do not give suggestions unless invoked manually
(setq company-async-timeout 120)
(define-key company-active-map (kbd "<tab>") 'company-complete-selection))

(setq-default abbrev-mode t)
(setq abbrev-file-name "~/.emacs.d/.abbrev_defs") 


(use-package counsel
  :ensure t
  :bind (:map global-map
	      ("C-s" . swiper)
	      ;; ("M-y" . counsel-yank-pop)
	      ("C-z C-x u" . counsel-unicode-char)
	      ("C-z C-x s" . counsel-set-variable)
	      ("C-z C-x h" . counsel-descbinds)
	      ;; ivy-view
	      ("C-z v" . ivy-push-view)
	      ("C-z C-v" . ivy-switch-view)
	      ("C-z V" . ivy-pop-view)
	      :map my-mode-map
	      ("C-," . ivy-switch-buffer)
	      ("C-，" . ivy-switch-buffer)
	      ("M-x" . counsel-M-x)
	      ("C-r C-r" . counsel-rg)
	      :map ivy-minibuffer-map
	      ("C-o" . ivy-dispatching-done)
	      ("M-<return>" . ivy-avy)
	      ("C-;" . insert-tilde)
	      ("C-<return>" . hydra-ivy/body)
	      :map ivy-occur-mode-map
	      )
  :config
  (counsel-mode 1)
  (ivy-mode 1)
  ;; Enable bookmarks and recentf
  (setq ivy-use-virtual-buffers t)
  ;; google as search engine
  (setq counsel-search-engine 'google)
  ;; C-p/C-n cycles when at first/last
  (setq ivy-wrap t) 			
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")
  )

(use-package ivy-hydra
  :ensure t)

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
  ;; 					;  (global-set-key (kbd "M-s o") 'helm-occur)
  ;; (define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)
  ;; (global-set-key (kbd "C-x r b") 'helm-bookmarks)

  ;; (define-key dired-mode-map (kbd "l") 'helm-find-files)
  ;; (bind-keys :map global-map
  ;; 	     ("C-S-SPC" . helm-mark-ring)))
  ) 					; helm ends here

;;; snippets
(use-package yasnippet 
  :ensure t
  :config
  (global-set-key (kbd "C-z <tab>") 'yas-expand-from-trigger-key) ; sometimes <tab> is redefined in certain modes, use this as a backup solution
  (global-set-key (kbd "C-z R") 'yas-reload-all)
  (define-key yas-minor-mode-map (kbd "C-c &") nil)
  (yas-global-mode)
  :diminish yas-minor-mode)

;; (when (eq system-type 'darwin)
;;   (use-package yasnippet-snippets
;;     :ensure t
;;     :config)
;;   )

(provide 'init-completion)
