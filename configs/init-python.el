(add-hook 'python-mode-hook
	  (lambda() (setq-default indent-tabs-mode t)))
(add-hook 'python-mode-hook 'auto-fill-mode)
(eval-after-load "python"
'(progn
   (defun my-python-indent-shift-left()
     (interactive)
     (save-excursion
       (beginning-of-line)
       (delete-char 4)))
   (defun my-python-indent-shift-right()
     (interactive)
     (save-excursion
       (beginning-of-line)
       (insert "    ")))   
   (bind-keys :map python-mode-map
	      ("<return>" . newline-and-indent)
	      ("M-DEL" . python-mark-defun)
	      ("<left>" . my-python-indent-shift-left)
	      ("<backtab>" . my-python-indent-shift-left)
	      ("<right>" . my-python-indent-shift-right)
	      ("C-<left>" . python-indent-shift-left)
	      ("C-<right>" . python-indent-shift-right))
   (define-key python-mode-map (kbd "C-;") 'insert-number-sign)
   (define-key python-mode-map (kbd "C-j") 'newline-and-indent)))
 ; electric-newline-and-maybe-indent does not do what I want
;   (hs-minor-mode)

;; code navigation, doc lookup and completion for python
(use-package anaconda-mode
  :ensure t
  :hook python-mode
  :config
  (define-key anaconda-mode-map (kbd "C-c r") 'anaconda-mode-find-references)
  (define-key anaconda-mode-map (kbd "M-r") nil)
  (define-key anaconda-mode-map (kbd "C-M-f") 'python-nav-forward-defun)
  (define-key anaconda-mode-map (kbd "C-M-b") 'python-nav-backward-defun))

;; python completion with company
(use-package company-anaconda
  :ensure t
  :config
  (eval-after-load "company"
 '(add-to-list 'company-backends 'company-anaconda)))

;; virtual environment support
(use-package virtualenvwrapper
  :ensure t
  :config
  (setq venv-location 
	(cl-case system-type
	  ('gnu/linux "~/opt/miniconda3/envs/")
	  ('darwin "/opt/homebrew/Caskroom/miniforge/base/envs")))
  (venv-initialize-interactive-shells) ;;  interactive shell support
  (venv-initialize-eshell)) ;;  eshell support

;; jupyter notebook support
(use-package ein
  :ensure t
  :config
  (setq ein:output-area-inlined-images t)
  :bind (:map ein:notebook-mode-map
	      ("M-p" . ein:worksheet-goto-prev-input-km)
	      ("M-n" . ein:worksheet-goto-next-input-km)
	      ("S-<return>" . ein:worksheet-execute-cell-km)
	      ("C-<return>" . ein:worksheet-insert-cell-below-km)))

(provide 'init-python)
