;; a hack to prevent my keybindings from being overriden
;; https://emacs.stackexchange.com/questions/352/how-to-override-major-mode-bindings
(defvar my-mode-map (make-sparse-keymap)
  "Keymap for `my-mode'.")

(define-minor-mode my-mode
  "A minor mode so that my key settings override annoying major modes."
  ;; If init-value is not set to t, this mode does not get enabled in
  ;; `fundamental-mode' buffers even after doing \"(global-my-mode 1)\".
  ;; More info: http://emacs.stackexchange.com/q/16693/115
  :init-value t
  :lighter " my-mode"
  :keymap my-mode-map)

(define-globalized-minor-mode global-my-mode my-mode my-mode)

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((my-mode . ,my-mode-map)))

(provide 'my-mode)

(define-prefix-command 'my-prefix-keymap)
(global-set-key (kbd "C-z") 'my-prefix-keymap)

(define-prefix-command 'my-register-prefix-keymap)
(define-key my-mode-map (kbd "C-r") 'my-register-prefix-keymap)

(define-prefix-command 'my-secondary-prefix-keymap)
(global-set-key (kbd "C-z C-x") 'my-secondary-prefix-keymap)
(global-set-key (kbd "C-x C-z") nil)

;; make M-c a secondary mode specific prefix 
(define-prefix-command 'mode-prefix-keymap)
(global-set-key (kbd "M-c") 'mode-prefix-keymap) 

;; package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;(add-to-list 'package-archives '("melpa" . "http://elpa.emacs-china.org/melpa/") t)
;(add-to-list 'package-archives '("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/") t) ; see https://mirror.tuna.tsinghua.edu.cn/help/elpa/ for more
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents)
)
;; (package-refresh-contents)

; install use-package if it's not installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package)
  )

; make the existing emacs process as a server
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(setq custom-file "~/.emacs.d/emacs-custom.el")
;;(message "helo")
(load custom-file)

(setq echo-keystrokes 0.01) ; echo unfinished commands(e.g., C-x) immediately

(use-package keyfreq
:ensure t

:config
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
(global-set-key (kbd "C-z S") 'keyfreq-show)
(setq keyfreq-excluded-commands
      '(lsp-ui-doc--handle-mouse-movement
	disable-mouse--handle))
;; (setq keyfreq-excluded-regexp '("wspecial.*"))
)

(when (display-graphic-p)
(define-key input-decode-map (kbd "C-\[") (kbd "H-\["))
(define-key input-decode-map (kbd "C-m") (kbd "H-m"))
)

(unless (display-graphic-p)
  (define-key key-translation-map (kbd "M-1") (kbd "C-;"))
  (define-key key-translation-map (kbd "M-2") (kbd "H-i"))
  (define-key key-translation-map (kbd "M-3") (kbd "H-\["))
  (define-key key-translation-map (kbd "M-4") (kbd "C-\]"))
  (define-key key-translation-map (kbd "M-5") (kbd "C-,"))
  (define-key key-translation-map (kbd "M-6") (kbd "C-."))
  (define-key key-translation-map (kbd "M-7") (kbd "C-'"))
)

; my personal configs
(add-to-list 'load-path (expand-file-name "configs" user-emacs-directory))
(require 'init-base)
(require 'init-linux)
(require 'init-osx)
(require 'init-windows)
(require 'init-my)
(require 'init-display)
(require 'init-general-editing)
(require 'init-prelude)
(require 'init-completion)
(require 'init-help)
(require 'init-org)
(require 'init-auctex)
(require 'init-prog)
(require 'init-c)
(require 'init-f90)
(require 'init-python)
(require 'init-elisp)
(require 'init-magit)
(require 'init-app)

(put 'downcase-region 'disabled nil)
