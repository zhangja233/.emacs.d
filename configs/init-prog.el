(defun my-compile(&optional arg)
  (interactive "P") 
  (save-some-buffers 1) 
  (cl-case major-mode
  ('latex-mode (TeX-command-run-all nil))  
  (t (cond ((equal arg '(4))
         ; use compile-buffer in comint mode[note that current-prefix-arg is '(4) ]
	 (call-interactively 'compile t (vector compile-command)))
	(t
	 (setq-local compilation-read-command nil)
	 (call-interactively 'compile))))))

(global-set-key (kbd "C-z C-z") 'my-compile)

(bind-keys :map compilation-mode-map
	   ("M-n" . my-forward-paragraph)
	   ("M-p" . my-backward-paragraph))
(eval-after-load "compile"
  '(progn (define-key compilation-minor-mode-map (kbd "C-;") 'quit-window)
	  (define-key compilation-shell-minor-mode-map (kbd "C-;") 'quit-window)))

(global-set-key (kbd "C-z C-s") 'shell-command)

(global-set-key (kbd "<f2> e") 'eshell)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "M-u")
  (setq lsp-diagnostic-package :none)

;  (setq lsp-ui-doc-enable nil)
;  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-enable-symbol-highlighting nil) ; don't show distraction when cursor is on an object

  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
        ((python-mode c-mode f90-mode) . lsp))
         ;; if you want which-key integration
;         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :ensure t)


(use-package ggtags
  :ensure t)

(add-hook 'c-mode-common-hook
    (lambda ()
      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
  (ggtags-mode 1))))

;; perl
;(fset 'perl-mode 'cperl-mode)
;(setq-default cperl-invalid-face nil)


(defun insert-number-sign () (interactive) (insert "#"))

(eval-after-load "perl"
'(progn
   (bind-keys :map perl-mode-map
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
  :config
  (define-key json-mode-map (kbd "C-;") 'json-pretty-print-buffer))

(provide 'init-prog)
