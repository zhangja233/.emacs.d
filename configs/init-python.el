(eval-after-load "python"
'(progn
   (define-key python-mode-map (kbd "C-;") 'insert-number-sign)
   (define-key python-mode-map (kbd "C-j") 'newline-and-indent) ; electric-newline-and-maybe-indent does not do what I want
))

;; code navigation, doc lookup and completion for python
(use-package anaconda-mode
  :ensure t
  :hook python-mode
  :config
  (define-key anaconda-mode-map (kbd "C-c r") 'anaconda-mode-find-references)
  (define-key anaconda-mode-map (kbd "M-r") nil)
  )

;; python completion with company
(use-package company-anaconda
  :ensure t
  :config
  (eval-after-load "company"
 '(add-to-list 'company-backends 'company-anaconda))
)

;; virtual environment support
(use-package virtualenvwrapper
  :ensure t
  :config
  (setq venv-location 
	(cl-case system-type
	  ('gnu/linux "~/opt/miniconda3/envs/")
	  ('darwin "/opt/homebrew/Caskroom/miniforge/base/envs") 
	  )
	)
  (venv-initialize-interactive-shells) ;;  interactive shell support
  (venv-initialize-eshell) ;;  eshell support
  )

;; jupyter notebook support
(use-package ein
  :ensure t
  :config
  (setq ein:output-area-inlined-images t)
  :bind (:map ein:notebook-mode-map
	      ("M-p" . ein:worksheet-goto-prev-input-km)
	      ("M-n" . ein:worksheet-goto-next-input-km)
	      ("S-<return>" . ein:worksheet-execute-cell-km)
	      ("C-<return>" . ein:worksheet-insert-cell-below-km)
	      )
  )

(provide 'init-python)
