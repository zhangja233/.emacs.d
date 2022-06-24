;;; compile
(defun my-compile(&optional arg)
  (interactive "P") 
  (save-some-buffers 1) 
  (cl-case major-mode
  ('latex-mode (TeX-command-run-all nil))  ; special treatment for tex
  (t (cond ((equal arg '(4))
         ; use compile-buffer in comint mode[note that current-prefix-arg is '(4) ]
	 (call-interactively 'compile t (vector compile-command)))
	(t
	 (setq-local compilation-read-command nil)
	 (call-interactively 'compile))))))


(bind-keys :map global-map
	   ("C-z C-z" . my-compile)
	   ("C-z z" . recompile))

(defhydra hydra-error (global-map "C-x e")
  "goto-error"
  ("a" first-error "first")
  ("n" next-error "next")
  ("p" previous-error "prev")
  ("q" nil "quit"))

(bind-keys :map compilation-mode-map
	   ("M-n" . my-forward-paragraph)
	   ("M-p" . my-backward-paragraph))
(eval-after-load "compile"
  '(progn (define-key compilation-minor-mode-map (kbd "C-;") 'quit-window)
	  (define-key compilation-shell-minor-mode-map (kbd "C-;") 'quit-window)))

(global-set-key (kbd "C-z C-s") 'shell-command)
(global-set-key (kbd "C-z M-e") 'eshell)

;; (use-package eglot
;;   :ensure t
;;   :bind (:map eglot-mode-map
;; 	      ("C-c r" . eglot-rename)
;; 	      ("C-c h" . eldoc))
;;   :config
;;   (add-to-list 'eglot-stay-out-of 'flymake)
;;   )
;; (add-hook 'python-mode-hook 'eglot-ensure)

;; (use-package lsp-mode
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "M-u")
;;   (setq lsp-diagnostic-package :none)
;;   (setq lsp-diagnostics-provider :none)
;;   (setq lsp-ui-sideline-enable nil)
;; (setq lsp-ui-sideline-show-diagnostics nil)
  
;;   (setq lsp-ui-doc-enable nil)
;;   (setq lsp-eldoc-enable-hover nil)
;;   (setq lsp-signature-auto-activate nil)
;;   (setq lsp-enable-symbol-highlighting nil)
;;   ; don't show distraction when cursor is on an object
;;   (setq lsp-ui-doc-show-with-cursor nil)
;;   (setq lsp-ui-doc-show-with-mouse nil)
  
  

;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;         ((python-mode c-mode f90-mode) . lsp))
;;          ;; if you want which-key integration
;; ;         (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)

;; (with-eval-after-load 'lsp-mode
;;   (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]build.*\\'"))

					;(use-package lsp-ui
					;  :ensure t)

;; (use-package dumb-jump
;;   :ensure t
;;   :config
;;   (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
;;   (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

(eval-after-load 'makefile-mode
    '(bind-keys :map makefile-mode-map
	   ("M-p" . nil)
	   ("M-n" . nil)))

(use-package cmake-mode
  :ensure t)

;(add-hook 'prog-mode-hook #'auto-fill-mode)


(eval-after-load "sh-script"
  '(progn  
     (bind-keys :map sh-mode-map
		("C-;" . comment-line))))

;; (use-package flycheck
;;   :ensure t
;;   :config
;;   (flycheck-add-mode 'python-flake8 'python-mode)
;;   )



(use-package ggtags
  :ensure t)

(add-hook 'c-mode-common-hook
    (lambda ()
      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
  (ggtags-mode 1))))

;; perl
;(setq-default cperl-invalid-face nil)

(defalias 'perl-mode 'cperl-mode)

(add-hook 'cperl-mode-hook
	  (lambda ()
	    (bind-keys :map cperl-mode-map
		       ("C-;" . insert-single-dollar)
		       ("M-;" . insert-number-sign))))

(use-package matlab
  :ensure matlab-mode
  :config
  (add-to-list
   'auto-mode-alist
   '("\\.m\\'" . matlab-mode))
  (setq exec-path (append exec-path '("/Applications/MATLAB_R2021b.app/bin"))))

(use-package markdown-mode
  :ensure t
  :bind 
(:map markdown-mode-map
      ("M-p" . nil)
      ("C-c C-v" . markdown-preview)
      ("M-n" . nil)))

(use-package json-mode
  :ensure t
  :bind (:map json-mode-map
	      ("C-;" . json-pretty-print-buffer)
	      ("M-c M-b" . hs-hide-block)
	      ("M-c b" . hs-show-block))
  :config
  )
;; (use-package origami
;;   :ensure t)

;; (use-package yafolding
;;   :ensure t)

(provide 'init-prog)

