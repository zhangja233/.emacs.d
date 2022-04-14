;;; moving the cursor
(bind-keys :map my-mode-map
	   ("C-q" . backward-word)
	   ("C-t" . forward-word)
	   ("C-a" . my-beginning-of-line)
	   ("C-e" . my-end-of-line))

(global-set-key (kbd "M-p") 'my-backward-paragraph)
(global-set-key (kbd "M-n") 'my-forward-paragraph)

(global-set-key (kbd "C-S-b") 'backward-sentence)
(global-set-key (kbd "C-S-f") 'forward-sentence)

(defun forward-half-sentence(&optional arg)
  (interactive)
  (re-search-forward "[;,]" nil nil arg))

(defun backward-half-sentence(&optional arg)
  (interactive)
  (re-search-backward "[;,=]" nil nil arg))

(bind-keys :map global-map
	   ("C-M-f" . forward-half-sentence)
	   ("C-M-b" . backward-half-sentence))

(setq sentence-end-double-space nil) ;make backward-sentence and forward-sentence behave as in fundamental mode

(bind-keys :map global-map
	   ("C-M-l" . downcase-word)
	   ("C-M-c" . capitalize-word)) 

;; avy-mode
(use-package avy
  :ensure t
  :bind ("M-l" . avy-goto-char)
  :config
(global-set-key (kbd "C-l") 'avy-goto-word-1)
(global-set-key (kbd "M-g") 'avy-goto-line)
;(setq avy-keys (nconc (number-sequence ?a ?z)
;		      ))
(setq avy-keys '(?a ?b ?c ?d ?e ?f ?g ?h ?j ?k ?l ?m ?o ?p ?q ?r ?s ?u ?v ?w ?z))
(setq avy-keys-alist nil)
(add-to-list 'avy-orders-alist '(avy-goto-line . avy-order-closest))
(add-to-list 'avy-orders-alist '(avy-goto-word-1 . avy-order-closest))
(add-to-list 'avy-orders-alist '(avy-goto-char . avy-order-closest))
(setq avy-timeout-seconds 0.2))


(define-key my-mode-map (kbd "M-a") 'beginning-of-buffer)
(define-key my-mode-map (kbd "M-e") 'end-of-buffer)

(use-package drag-stuff
  :ensure t
  :config
  (bind-keys :map global-map
	     ("M-<up>" . drag-stuff-up)
	     ("M-<down>" . drag-stuff-down)))

;; mark ring
(setq set-mark-command-repeat-pop t) ; repeat pop by C-SPC after C-u C-SPC

(global-set-key (kbd "<prior>") 'scroll-other-window-down)
(global-set-key (kbd "<next>") 'scroll-other-window)

; simple editing
(global-set-key (kbd "<deletechar>") 'quoted-insert)

;;; search and replace
(global-set-key (kbd "M-r") 'replace-string)

(bind-keys :map global-map
	   ("M-r" . replace-string)
	   ("M-5" . query-replace-regexp))

(use-package visual-regexp-steroids
  :ensure t
  :config
  (bind-keys :map my-mode-map
   ("C-M-s" . vr/isearch-forward)
   ("C-z r" . vr/replace)))

(when (eq system-type 'darwin)
(use-package pcre2el
 :ensure t))

(defun backward-upcase-word()
  (interactive)
  (upcase-word -1))
(global-set-key (kbd "C-z U") 'backward-upcase-word)

(electric-indent-mode -1)

(defun break-line-at-point()
  (interactive)
  (newline-and-indent)
  (newline-and-indent)
  (previous-line)
  (indent-for-tab-command))		
;(global-set-key (kbd "C-z j") 'break-line-at-point)

(defun open-line-below()
  (interactive)
  (end-of-line)
  (newline-and-indent)
  )
;(define-key input-decode-map (kbd "C-m") (kbd "H-m"))
;(global-set-key (kbd "H-m") 'open-line-below)

;(define-key input-decode-map (kbd "C-\[") (kbd "H-\["))

(defun open-line-above()
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (indent-for-tab-command))
(global-set-key (kbd "C-o") 'open-line-above)

;; delete, kill, copy and paste
;(setq kill-whole-line t) ; kill whole line if point at beginning of line

(defun backward-kill-word-or-kill-region(&optional arg)
  "backward kill word if region is not active, otherwise kill region"
  (interactive "p")
  (if (region-active-p)
      (kill-region (mark) (point) 'region)
      (backward-kill-word arg)))
 
(define-key my-mode-map (kbd "C-w") 'backward-kill-word-or-kill-region)

(bind-keys :map my-mode-map
	   ("C-z d" . prelude-duplicate-current-line-or-region))

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(use-package expand-region
  :ensure t
  :config
  (define-key my-mode-map (kbd "C-=") 'er/expand-region))

(unless (eq system-type 'darwin)
  (define-key key-translation-map [(control ?\h)]  [127]) ; bind C-h to Backspace, otherwise in searching C-h just literally becomes ^H
  (global-set-key (kbd "C-h") (kbd "<backspace>")))

(delete-selection-mode) ; using C-d to delete a selected region
(setq delete-active-region 'kill) ; kill the selected region while using delete and backspace. Note that you can still use C-d to delete a region.

(require 'misc)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(define-key my-mode-map (kbd "C-k") 'kill-line)

(global-set-key (kbd "C-z C-k") 'delete-line)
(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "C-<backspace>") 'backward-delete-word)
(global-set-key (kbd "C-<escape>") 'delete-word)

(global-set-key (kbd "C-S-w") 'my-copy-line)

; copy to clipboard when M-w
(setq x-select-enable-clipboard t)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  :bind
  ("M-/" . 'undo-tree-redo)
  :diminish undo-tree-mode)

;; About sexps 
(show-paren-mode 1)

(setq blink-matching-paren nil)

; smartparens
(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode)
  (define-key my-mode-map (kbd "M-f") 'sp-forward-sexp)
  (define-key my-mode-map (kbd "M-b") 'sp-backward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)

  (define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)
  (define-key smartparens-mode-map (kbd "C-M-[") 'sp-select-previous-thing)  
  (define-key my-mode-map (kbd "C-M-i") 'sp-change-enclosing)
  (define-key smartparens-mode-map (kbd "C-M-r") 'sp-rewrap-sexp)
  (define-key smartparens-mode-map (kbd "C-M-u") 'sp-unwrap-sexp)
  :bind(
  :map smartparens-mode-map
  ("M-k" . sp-kill-symbol))
  :config
  (require 'smartparens-config)
  (setq sp-navigate-consider-symbols nil) ; don't treat a word as a sexp
  (setq sp-highlight-pair-overlay nil) ; don't hightlight inner content of a pair
  
  (sp-with-modes '(latex-mode)
    (sp-local-pair "\\begin" "\\end")
    (sp-local-pair "|" "|")
    (sp-local-pair "\\|" "\\|"))
  
  (advice-remove 'delete-backward-char #'ad-Advice-delete-backward-char) ;prevent smartparens from deleting the whole \right) when using backspace
  :diminish smartparens-mode)

;;; snippets
(use-package yasnippet 
:ensure t
:config
(global-set-key (kbd "C-z <tab>") 'yas-expand-from-trigger-key) ; sometimes <tab> is redefined in certain modes, use this as a backup solution
(global-set-key (kbd "C-z R") 'yas-reload-all)
(define-key yas-minor-mode-map (kbd "C-c &") nil)
(global-set-key (kbd "<f2> i") 'yas-new-snippet)
(yas-global-mode)
:diminish yas-minor-mode)

(use-package yasnippet-snippets
:ensure t

:config)

;;; completion
(when (display-graphic-p)
(define-key input-decode-map (kbd "C-i") (kbd "H-i"))
)
(global-set-key (kbd "H-i") 'dabbrev-expand)
(global-set-key (kbd "C-z C-e") 'hippie-expand) 

(use-package company
:ensure t
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

(bind-keys :map global-map
	   ("M-i" . company-complete)
	   ("C-M-/" . my-company-show-doc-buffer))
(setq company-dabbrev-downcase nil) ;make completion case sensitive
(setq company-idle-delay nil) ; do not give suggestions unless invoked manually
(setq company-async-timeout 120)
(define-key company-active-map (kbd "<tab>") 'company-complete-selection))

(setq-default abbrev-mode t)
(setq abbrev-file-name "~/.emacs.d/.abbrev_defs") 

;;; buffer, window, frame and file management

(global-set-key (kbd "C-S-r") 'recenter-top-bottom)
(global-set-key (kbd "C-x x") 'delete-window)
(bind-keys :map global-map
	   ("C-1" . delete-other-windows)
	   ("C-." . other-window)
	   ("C-\\" . split-window-right))

;; register

(define-prefix-command 'my-register-prefix-keymap)
(define-key my-mode-map (kbd "C-r") 'my-register-prefix-keymap)

(defun my-window-configuration-to-register()
  (interactive)
  (window-configuration-to-register ?w)
  (message "window layout saved"))

(defun my-point-to-register()
  (interactive)
  (point-to-register ? )
  (message "point location saved"))

(defun my-jump-to-saved-window()
  (interactive)
  (jump-to-register ?w))

(defun my-jump-to-saved-location()
  (interactive)
  (jump-to-register ? ))

(bind-keys :map my-mode-map
	   ("C-r w" . my-window-configuration-to-register)
	   ("C-r C-w" . my-jump-to-saved-window)
	   ("C-r SPC" . my-point-to-register)
	   ("C-r C-SPC" . my-jump-to-saved-location))

(setq winner-dont-bind-my-keys t)
(winner-mode 1)

(when (display-graphic-p)
(define-key input-decode-map (kbd "C-\[") (kbd "H-\["))
)
(global-set-key (kbd "H-\[") 'winner-undo)

(global-set-key (kbd "C-z ,") 'winner-undo)
(global-set-key (kbd "C-z .") 'winner-redo)
(bind-keys :map global-map
	   ("C-\]" . winner-redo))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window))

(use-package window-purpose
  :ensure t
  :config
;  (purpose-mode)
  (setq purpose-mode-map (make-sparse-keymap)) ; prevent from overriding existing keybindings
  (global-set-key (kbd "C-z t") 'purpose-toggle-window-buffer-dedicated))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-mode +1)
  (global-set-key (kbd "M-j") 'projectile-switch-project)
  (define-key projectile-mode-map (kbd "M-J") 'projectile-find-file)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (setq projectile-indexing-method 'hybrid)
;  (setq projectile-ignored-projects '("~") )
  (setq projectile-track-known-projects-automatically nil) ; only allow manually adding projects
  (setq projectile-auto-discover nil)
  (setq projectile-sort-order 'recentf)
  (setq projectile-current-project-on-switch 'keep) ; leave the current project at the default position
;  (setq projectile-dynamic-mode-line nil)
;  (setq-default projectile-mode-line-function nil)

  (defun projectile-find-notes()
  (interactive)
  (let ((default-directory "~/Dropbox/notes"))
    (projectile-find-file)))
  
  (global-set-key (kbd "C-z n") 'projectile-find-notes)
  
  (defun projectile-find-root()
    (interactive)
    (find-file (projectile-project-root)))
  (global-set-key (kbd "C-z C-r") 'projectile-find-root))

(use-package helm-projectile
  :ensure t)


;; dired mode
(require 'dired-x)
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))
(define-key my-mode-map (kbd "C-x j") 'dired-jump)
(define-key my-mode-map (kbd "C-x J") 'dired-jump-other-window)

(define-key dired-mode-map (kbd "SPC") 'browse-url-of-dired-file)
(define-key dired-mode-map (kbd "E") 'wdired-change-to-wdired-mode)

; a trick to make dired be able to access ~/Downloads and folders alike
(when (eq system-type 'darwin)
  (setq insert-directory-program "gls" dired-use-ls-dired t))

(setq dired-listing-switches "-alht --group-directories-first")
;(setq dired-listing-switches "-AlBGh  --group-directories-first")

(setq dired-deletion-confirmer #'y-or-n-p)
;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)


(global-set-key (kbd "C-z C-v") 'find-file-other-window)

(defun find-scratch()
  (interactive) (switch-to-buffer "*scratch*"))
(global-set-key (kbd "C-z s") 'find-scratch)
(global-set-key (kbd "C-z N") 'make-frame)



(defun save-all-buffers()
  (interactive)
  (save-some-buffers 1)
  (message "all buffers saved"))
(global-set-key (kbd "C-x C-s")  'save-all-buffers)
(global-set-key (kbd "M-s")  'save-all-buffers)
(global-set-key (kbd "C-x C-S-s") 'save-buffer)

(defun my-quit-emacs()
  (interactive)
  (condition-case nil
      (org-clock-out)
  (error nil)) ; ignore the error when there's no clock so that the remaining code could be executed
  (save-some-buffers 1)
  (save-buffers-kill-terminal))
(global-set-key (kbd "C-x C-c")  'my-quit-emacs)
(setq confirm-kill-emacs 'y-or-n-p)

;; setup of minibuffer
(define-key minibuffer-local-map (kbd "C-p") 'previous-history-element) ; for the rare case to go to the previous line just use the arrow key
(define-key minibuffer-local-map (kbd "C-n") 'next-history-element) 

(defun insert-tilde()
  (interactive)
  (insert "~"))
(define-key minibuffer-local-map (kbd "C-;") 'insert-tilde)

;; magit
(use-package magit
:ensure t)

;; input method
; Chinese
;(require 'pyim)
;(require 'pyim-basedict) ; 拼音词库设置，五笔用户 *不需要* 此行设置
;(pyim-basedict-enable)   ; 拼音词库，五笔用户 *不需要* 此行设置
(setq default-input-method "pyim")
;(setq pyim-punctuation-translate-p '(yes no auto))   ;使用全角标点。
(setq pyim-punctuation-dict nil)
;(setq pyim-punctuation-translate-p '(no yes auto))   ;使用半角标点。
(setq pyim-page-tooltip 'popup)

; japanese
(defun set-japanese()
  (interactive)
  (set-input-method "japanese"))
(global-set-key (kbd "C-z J") 'set-japanese)
 (defun set-chinese()
  (interactive)
  (set-input-method "pyim"))
(global-set-key (kbd "C-z C") 'set-chinese)

(setq-default ispell-program-name "aspell")

;; fly
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
(use-package flyspell
:init
  (setq flyspell-mode-map (make-sparse-keymap)) ; prevent flyspell from overriding existing keybindings
:config
:custom
  (flyspell-abbrev-p t)
:diminish flyspell-mode)
(use-package flyspell-correct
  :ensure t
  :config
  (define-key my-mode-map (kbd "M-q") 'flyspell-correct-previous))

(defun find-dot-emacs()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(defun find-emacs-configs()
  (interactive)
  (find-file "~/.emacs.d/configs/"))
(global-set-key (kbd "C-z E") 'find-dot-emacs)
(global-set-key (kbd "C-z e") 'find-emacs-configs)
(defun find-my-info()
  (interactive)
  (find-file "~/Dropbox/org_non_agenda/personal/info.org"))
(defun find-planer()
  (interactive)
(find-file "~/Dropbox/org/plan.org"))

(defun find-capture()
  (interactive)
(find-file "~/Dropbox/org/capture.org"))

(defun find-literature()
  (interactive)
  (find-file "~/Dropbox/research/literature/"))

(when (eq system-type 'darwin)
(bind-keys :map global-map
	   ("C-z l" . find-literature)
	   ("C-z p" . find-planer)
	   ("C-z C-c" . find-capture)
	   ("C-z F" . find-my-info))
) ; end os x




(defun find-download()
  (interactive)
  (find-file "~/Downloads/"))
(global-set-key (kbd "C-z C-d")  'find-download)

 


;; interact with the world outside emacs

; to open file browser from emacs
(defun browse-file-directory ()
  "Open the current file's directory however the OS would."
  (interactive)
  (if default-directory
      (browse-url-of-file (expand-file-name default-directory))
    (error "No `default-directory' to open")))
(global-set-key (kbd "C-z b") 'browse-file-directory)

(use-package rg
  :ensure t)

(use-package google-this
:ensure t  
:init
(global-set-key (kbd "C-z <RET>") 'google-this-mode-submap))

(use-package exec-path-from-shell
:ensure t
:init
(unless (eq system-type 'windows-nt)
  (exec-path-from-shell-initialize) ;get $PATH from shell
))
(use-package disable-mouse
:ensure t
:diminish disable-mouse-global-mode
:config
(global-disable-mouse-mode) ; in case I move the mouse accidentally
)

(setq initial-major-mode 'org-mode)

(provide 'init-general-editing)
