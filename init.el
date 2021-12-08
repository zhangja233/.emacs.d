(setq mac-command-modifier 'control)
(setq mac-option-modifier 'meta)
(add-to-list 'load-path (expand-file-name "configs" user-emacs-directory))

;; a hack to prevent my keybindings from being overriden
;; https://emacs.stackexchange.com/questions/352/how-to-override-major-mode-bindings
(defvar my-mode-map (make-sparse-keymap)
  "Keymap for `my-mode'.")

;;;###autoload
(define-minor-mode my-mode
  "A minor mode so that my key settings override annoying major modes."
  ;; If init-value is not set to t, this mode does not get enabled in
  ;; `fundamental-mode' buffers even after doing \"(global-my-mode 1)\".
  ;; More info: http://emacs.stackexchange.com/q/16693/115
  :init-value t
  :lighter " my-mode"
  :keymap my-mode-map)

;;;###autoload
(define-globalized-minor-mode global-my-mode my-mode my-mode)

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((my-mode . ,my-mode-map)))

;; Turn off the minor mode in the minibuffer
(defun turn-off-my-mode ()
  "Turn off my-mode."
  (my-mode -1))
(add-hook 'minibuffer-setup-hook #'turn-off-my-mode)

(provide 'my-mode)
    
(define-prefix-command 'my-prefix-keymap)
(global-set-key (kbd "C-z") 'my-prefix-keymap)


;; package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;(add-to-list 'package-archives '("melpa" . "http://elpa.emacs-china.org/melpa/") t)
;(add-to-list 'package-archives '("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/") t) ; see https://mirror.tuna.tsinghua.edu.cn/help/elpa/ for more
(package-initialize)

(require 'init-general-editing)
(require 'init-help)
(require 'init-org)
(require 'init-auctex)
(require 'init-prog)
(require 'init-windows)

; make the existing emacs process as a server
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;; emacs display
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t) ;do not show welcome page
(setq visible-bell nil)

(defun set-font-size(size)
  (interactive)
  (set-face-attribute 'default nil :height size)
)

(defun font-size-extscreen() (interactive)
(set-font-size 261))

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)


(use-package keyfreq
:ensure t

:config
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

)
(global-set-key (kbd "C-z S") 'keyfreq-show)
