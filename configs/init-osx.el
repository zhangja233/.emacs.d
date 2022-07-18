(when (eq system-type 'darwin)
  (setq mac-command-modifier 'control)
  (setq mac-option-modifier 'meta)
  (add-to-list 'load-path "/Users/zja/.emacs.d/local")
  (require 'ob-applescript)
  (unless (package-installed-p 'hyperbole)
    (package-refresh-contents)
    (package-install 'hyperbole))
  ;; (hyperbole-mode 1)
  ;; (bind-keys :map hyperbole-mode-map
  ;; 	     ("C-c ." . nil)
  ;; 	     ("C-c /" . nil))

  (require 'alert)
  (setq alert-default-style 'osx-notifier)
  (require 'dot-mode)
  (autoload 'dot-mode "dot-mode" nil t) ; vi `.' command emulation
  (dot-mode 1)
  (global-set-key (kbd "<f5>") 'dot-mode-execute)
  (use-package bufler
    :ensure t)  
  ;; (global-set-key (kbd "<f5>") (lambda () (interactive) (dot-mode 1)
  ;; 				 (message "Dot mode activated.")))
  
  ;; mu4e
  ;;   (use-package mu4e
  ;;     :load-path "/opt/homebrew/Cellar/mu/1.6.10/share/emacs/site-lisp/mu/mu4e/"
  ;;     :config
  ;;     (global-set-key (kbd "C-x m") 'mu4e)
  ;;     (setq mu4e-get-mail-command "mbsync -a"
  ;; 	  user-mail-address "jz627@cornell.edu"
  ;; 	  user-full-name  "Ju-an Zhang"
  ;; 	  smtpmail-smtp-server "localhost"
  ;; 	  smtpmail-smtp-service  1025
  ;; 	  )
  ;; ;    (setq
  ;; ;  mu4e-index-cleanup nil      ;; don't do a full cleanup check
  ;; ;  mu4e-index-lazy-check t)    ;; don't consider up-to-date dirs

  ;;     ;; (setq
  ;;     ;;  mu4e-sent-folder   "/Sent"
  ;;     ;;  mu4e-drafts-folder "/Drafts"
  ;;     ;;  mu4e-trash-folder  "/Trash"
  ;;     ;;  mu4e-refile-folder "/Archive")
  ;; ;    (setq mu4e-user-mailing-lists '("jz627@cornell.edu"))
  ;;     (setq mu4e-update-interval 300 ; set interval of mu4e updates
  ;; ;	  mu4e-index-update-in-background nil
  ;; 	  )
  ;; ;    (setq

  ;;     ;; don't save messages to Sent Messages, IMAP takes care of this
  ;;     (setq mu4e-sent-messages-behavior 'delete)
  ;;     (setq mu4e-index-update-error-warning nil)
  ;;     (add-hook 'mu4e-compose-mode-hook 'turn-off-auto-fill)
  ;;     (add-hook 'mu4e-compose-mode-hook 'visual-line-mode)

  ;;     (setq mu4e-attachment-dir "~/Downloads")
  ;;     (setq mu4e-hide-index-messages t) ; don't show notifications in mode line
  ;;     (use-package mu4e-alert
  ;;       :ensure t
  ;;       :after mu4e
  ;;       )
  ;; ;   mu4e-compose-signature
  ;; ;    (concat
  ;; ;      "Foo X. Bar\n"
  ;; ;      "http://www.example.com\n"))
  ;; ;    )
  ;;   ) ; mu4e ends
  ;;  (add-to-list 'load-path "/Users/zja/.emacs.d/pyim-tsinghua-dict")
  ;;  (require 'pyim-tsinghua-dict)
  ;;  (pyim-tsinghua-dict-enable)

					;  (set-fontset-font
					;  (frame-parameter nil 'font)
					;   'han
					;   (font-spec :family "PingHei"))

  (custom-set-variables
   '(markdown-command "/opt/homebrew/bin/pandoc")))

(provide 'init-osx)
