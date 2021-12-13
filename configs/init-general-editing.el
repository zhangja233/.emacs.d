;;; moving the cursor
(global-set-key (kbd "C-q") 'backward-word)
(global-set-key (kbd "C-t") 'forward-word)

(defun my-beginning-of-line()
  (interactive)
  (push-mark)
  (beginning-of-line)
  )
(define-key my-mode-map (kbd "C-a") 'my-beginning-of-line)

(defun my-end-of-line()
  (interactive)
  (push-mark)
  (end-of-line)
  )
(define-key my-mode-map (kbd "C-e") 'my-end-of-line)


(global-set-key (kbd "C-S-b") 'backward-sentence)
(global-set-key (kbd "C-S-f") 'forward-sentence)
(setq sentence-end-double-space nil) ;make backward-sentence and forward-sentence behave as in fundamental mode

(global-set-key (kbd "C-9") 'recenter-top-bottom)

;; avy-mode
(use-package avy
  :ensure t
  )
(global-set-key (kbd "C-l") 'avy-goto-word-1)
(global-set-key (kbd "C-z g") 'avy-goto-line)
(global-set-key (kbd "C-M-l") 'avy-goto-char-in-line)

(global-set-key (kbd "M-a") 'beginning-of-buffer)
(global-set-key (kbd "M-e") 'end-of-buffer)

(global-set-key (kbd "M-s") 'isearch-forward-regexp)

(setq set-mark-command-repeat-pop t) ; repeat pop by C-SPC after C-u C-SPC

; simple editing
(global-set-key (kbd "M-t") 'transpose-chars)
(global-set-key (kbd "M-g") 'quoted-insert)
(global-set-key (kbd "M-q") 'ispell-word)
(global-set-key (kbd "M-r") 'replace-string)
(defun backward-upcase-word()
  (interactive)
  (upcase-word -1)
  )
(global-set-key (kbd "M-u") 'backward-upcase-word)

(electric-indent-mode -1)

(defun break-line-at-point()
  (interactive)
  (newline-and-indent)
  (newline-and-indent)
  (previous-line)
  (indent-for-tab-command)
)		
;(global-set-key (kbd "C-z j") 'break-line-at-point)

(defun open-line-below()
  (interactive)
  (end-of-line)
  (newline-and-indent)
  )
(define-key input-decode-map (kbd "C-m") (kbd "H-m"))
(global-set-key (kbd "H-m") 'open-line-below)

(defun open-line-above()
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (indent-for-tab-command)
  )
(global-set-key (kbd "C-o") 'open-line-above)



;; delete, kill, copy and paste
(global-set-key (kbd "C-<escape>") 'kill-word)
(define-key key-translation-map [(control ?\h)]  [127]) ; bind C-h to Backspace, otherwise in searching C-h just literally becomes ^H
(global-set-key (kbd "C-h") (kbd "<backspace>"))

(delete-selection-mode) ; using C-d to delete a selected region
(setq delete-active-region 'kill) ; kill the selected region while using delete and backspace. Note that you can still use C-d to delete a region.

; copy to clipboard when M-w
(setq x-select-enable-clipboard t)

(setq mouse-yank-at-point t) ; paste at the cursor instead of where you click when using middle button of the mouse

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] #'easy-kill)
  (global-set-key (kbd "M-w") 'easy-kill)
  (global-set-key [remap mark-sexp] #'easy-mark))

;; About sexps 
(show-paren-mode 1)

(global-set-key (kbd "C-M-SPC") (lambda() (interactive) (backward-up-list) (mark-sexp)))
; smartparens
(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode)

  (define-key smartparens-mode-map (kbd "M-f") 'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "M-b") 'sp-backward-sexp)
  ; down a level
  (define-key smartparens-mode-map (kbd "C-M-n") 'sp-down-sexp)
  (define-key smartparens-mode-map (kbd "C-M-p") 'sp-backward-down-sexp)
  ; up a level
  (define-key smartparens-mode-map (kbd "C-M-d") 'sp-up-sexp)
  (define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
 
  (define-key smartparens-mode-map (kbd "C-M-a") 'sp-beginning-of-sexp)
  (define-key smartparens-mode-map (kbd "C-M-e") 'sp-end-of-sexp)


;;  (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)




;; (define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
;; (define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

;; (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
;; (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)

;; (define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
;; (define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
;; (define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)





  (define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing)
  (define-key input-decode-map (kbd "C-\[") (kbd "H-\["))
  (define-key smartparens-mode-map (kbd "H-\[") 'sp-select-previous-thing)  
  (define-key smartparens-mode-map (kbd "C-M-i") 'sp-change-enclosing)
  (define-key smartparens-mode-map (kbd "C-M-r") 'sp-rewrap-sexp)
  (defun my-sp-kill-sexp (&optional arg)
    "make C-u sp-kill-sexp behave as killing inner"
    (interactive "P")
    (let* (
         (arg (prefix-numeric-value arg))
         (n (abs arg))
	 )
      (if (= n 4)
	  (sp-kill-sexp 0)
	(sp-kill-sexp arg))
      ))
  ;; (defun sp-kill-whole-sexp()
  ;;   (interactive)
  ;;   ()
  ;;   )
  (define-key smartparens-mode-map (kbd "M-k") 'my-sp-kill-sexp)
  (define-key smartparens-mode-map (kbd "C-M-k") 'sp-unwrap-sexp)
  :config
  (require 'smartparens-config)
  (setq sp-navigate-consider-symbols nil) ; don't treat a word as a sexp
  (advice-remove 'delete-backward-char #'ad-Advice-delete-backward-char) ;prevent smartparens from deleting the whole \right) when using backspace
  :diminish smartparens-mode)

;;; snippets
(use-package yasnippet 
:ensure t

:config
;(global-set-key (kbd "C-,") 'yas-expand)
(yas-global-mode)
)
(use-package yasnippet-snippets
:ensure t

:config
)


;;; helm-mode
(use-package helm
  :ensure t
  :init
  :config
  (helm-mode 1) ;turn on helm-mode at startup
  (setq helm-locate-command "mdfind -name %s %s") ;; mdfind seems to work way better than locate in os x 
  ;; (case system-type
  ;;   ('gnu/linux "locate -i -r %s")
  ;;   ('berkeley-unix "locate -i %s")
  ;;   ('windows-nt "es %s")
  ;;   ('darwin "mdfind -name %s %s")
  ;;   (t "locate %s")))

  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-,") 'helm-buffers-list)
  (global-set-key (kbd "C-z C-f") 'helm-locate)
  (global-set-key (kbd "C-z y") 'helm-show-kill-ring)
  )

;;; macros
(global-set-key (kbd "M-.") 'kmacro-start-macro)
(global-set-key (kbd "C-.") 'kmacro-end-or-call-macro)

;;; completion
(define-key input-decode-map (kbd "C-i") (kbd "H-i"))
(global-set-key (kbd "H-i") 'dabbrev-expand)

(use-package company
:ensure t
)
(global-company-mode) 
(global-set-key (kbd "M-i") 'company-complete)
(setq company-dabbrev-downcase nil) ;make completion case sensitive
(setq company-idle-delay nil) ; do not give suggestions unless invoked manually
(define-key company-active-map (kbd "<tab>") 'company-complete-selection)




;;; buffer, window, frame and file management
(global-set-key (kbd "C-<left>") 'mac-previous-tab)
(global-set-key (kbd "C-<right>") 'mac-next-tab)

(global-set-key (kbd "M-<tab>") 'other-window)
(global-set-key (kbd "C-1") 'delete-other-windows)

;; dired mode
(require 'dired-x)
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))
(global-set-key (kbd "C-x j") 'dired-jump)
;(setq insert-directory-program "gls" dired-use-ls-dired t)

; a trick to make dired be able to access ~/Downloads and folders alike
(setq insert-directory-program "gls" dired-use-ls-dired t)
(setq dired-listing-switches "-al --group-directories-first")
;(setq dired-listing-switches "-AlBGh  --group-directories-first")

(global-set-key (kbd "C-z C-v") 'find-file-other-window)

(defun find-scratch()
  (interactive) (switch-to-buffer "*scratch*")
  )
(global-set-key (kbd "C-z s") 'find-scratch)
(global-set-key (kbd "C-z N") 'make-frame)



(defun save-all-buffers()
  (interactive)
  (save-some-buffers 1)
    )
(global-set-key (kbd "C-x C-s")  'save-all-buffers)

(defun my-quit-emacs()
  (interactive)
  (condition-case nil
      (org-clock-out)
  (error nil)) ; ignore the error when there's no clock so that the remaining code could be executed
  (save-some-buffers 1)
  (save-buffers-kill-terminal)
  )
(global-set-key (kbd "C-x C-c")  'my-quit-emacs)

;; setup of minibuffer
(define-key minibuffer-local-map (kbd "C-p") 'previous-history-element) ; for the rare case to go to the previous line just use the arrow key
(define-key minibuffer-local-map (kbd "C-n") 'next-history-element) 
(define-key minibuffer-local-map (kbd "C-;") (kbd "~"))

;; magit
(use-package magit
:ensure t
  )

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
  (set-input-method "japanese")
  )
(global-set-key (kbd "C-z J") 'set-japanese)
 (defun set-chinese()
  (interactive)
  (set-input-method "pyim")
  )
(global-set-key (kbd "C-z C") 'set-chinese)

;; fly
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
(use-package flyspell
:init
  (setq flyspell-mode-map (make-sparse-keymap)) ; prevent flyspell from overriding existing keybindings
:config
   )

;(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e") ;load mu4e in case emacs can not find it

;; mu4e
;(require 'mu4e) 
;(setq mu4e-get-mail-command "offlineimap")
;(global-set-key (kbd "C-z m") 'mu4e)
;(setq mu4e-update-interval 7200) ; set interval of mu4e updates to 2 hours

(defun find-dot-emacs()
  (interactive)
  (find-file "~/.emacs.d/init.el")
  )
(defun find-emacs-configs()
  (interactive)
  (find-file "~/.emacs.d/configs/")
  )
(global-set-key (kbd "C-z E") 'find-dot-emacs)
(global-set-key (kbd "C-z e") 'find-emacs-configs)
(defun find-my-info()
  (interactive)
  (find-file "~/org/personal/info.org")
  )
(global-set-key (kbd "C-z f") 'find-my-info)

;; interact with the world outside emacs

; to open file browser from emacs
(defun browse-file-directory ()
  "Open the current file's directory however the OS would."
  (interactive)
  (if default-directory
      (browse-url-of-file (expand-file-name default-directory))
    (error "No `default-directory' to open")))
(global-set-key (kbd "C-z b") 'browse-file-directory)

(use-package google-this
:ensure t  
:init
(global-set-key (kbd "C-z <RET>") 'google-this-mode-submap)
  )

(use-package exec-path-from-shell
:ensure t
:init
(unless (eq system-type 'windows-nt)
  (exec-path-from-shell-initialize) ;get $PATH from shell 
)
  )

(provide 'init-general-editing)
