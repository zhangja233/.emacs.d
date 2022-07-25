;; general editing

;;; moving the cursor
(bind-keys :map my-mode-map
	   ("C-a" . my-beginning-of-line)
	   ("C-e" . my-end-of-line)
	   ("M-[" . isearch-backward)
	   ("M-]" . isearch-forward)
	   ("C-q" . (lambda () (interactive) (message "use M-b"))))

(use-package hydra
  :ensure t
  :init
  :config
  (setq hydra-hint-display-type 'message))

(when (eq system-type 'darwin)
(use-package pcre2el
 :ensure t))

;;; it bears repeating
(when (version<= "28" emacs-version)
  (repeat-mode 1))
(bind-keys :map global-map
	   ("<f9>" . repeat))

(defun forward-half-sentence(&optional arg)
  (interactive)
  (re-search-forward "[;,]" nil nil arg))

(defun backward-half-sentence(&optional arg)
  (interactive)
  (re-search-backward "[;,=]" nil nil arg))

(bind-keys :map global-map
	   ("M-F" . forward-half-sentence)
	   ("M-B" . backward-half-sentence)
	   ("C-S-f" . forward-sentence)
	   ("C-S-b" . backward-sentence))

(setq sentence-end-double-space nil) ;make backward-sentence and forward-sentence behave as in fundamental mode

(bind-keys :map global-map
	   ("M-p" . my-backward-paragraph)
	   ("M-n" . my-forward-paragraph)
	   ("M-h" . my-mark-paragraph)
	   ("C-z M-l" . global-display-line-numbers-mode))

(bind-keys :map global-map
	   ("C-M-l" . downcase-word)
	   ("C-M-c" . capitalize-word))


;; (defadvice upcase-word (before upcase-word-advice activate)
;;   (unless (looking-back "\\b")
;;     (backward-word)))

;; (defadvice downcase-word (before downcase-word-advice activate)
;;   (unless (looking-back "\\b")
;;     (backward-word)))

;; (defadvice capitalize-word (before capitalize-word-advice activate)
;;   (unless (looking-back "\\b")
;;     (backward-word)))

;; avy-mode
(use-package avy
  :ensure t
  :bind (("C-l" . avy-goto-word-1)
	 ("M-l" . avy-goto-char-timer)
	 ("M-g" . avy-goto-line)
	 ;; ("C-<esc>" . avy-resume)
	 )
  :config
  (avy-setup-default)
  (setq avy-keys '(?a ?b ?c ?d ?e ?f ?g ?h ?j ?k ?l ?o ?p ?q ?r ?s ?u ?v ?w))
					;(setq avy-keys-alist nil)
  (setq avy-orders-alist
	'((avy-goto-char-timer . avy-order-closest)
          (avy-goto-word-1 . avy-order-closest)
	  (avy-goto-line . avy-order-closest)
	  (avy-kill-region . avy-order-closest)))

  ;;(setq avy-all-windows nil)
  ;;(setq avy-all-windows-alt t)
  (setq avy-timeout-seconds 0.2)

  ;; https://karthinks.com/software/avy-can-do-anything/
  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

  (setf (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char) ; space as mark to char
  ;; i as the action to correct spelling mistake
  (defun avy-action-flyspell (pt)
    (flyspell-correct-previous pt))
  (setf (alist-get ?i  avy-dispatch-alist) 'avy-action-flyspell)
  
  (defun avy-action-copy-latex-inline-equation (pt)
    "copy inline equation in LaTeX mode, or in general copy things inside $ $"
    ;; note that this save-excursion trick doesn't work when calling avy across different buffers
    (save-excursion
      (goto-char pt)
      (copy-region-as-kill (+ (search-backward "$") 1)
			   (- (search-forward "$" nil nil 2) 1))))
  (setf (alist-get ?4 avy-dispatch-alist) 'avy-action-copy-latex-inline-equation))

(define-key my-mode-map (kbd "M-a") 'beginning-of-buffer)
(define-key my-mode-map (kbd "M-e") 'end-of-buffer)

(setq view-read-only t)
(defadvice read-only-mode (before read-only-mode-advice activate)
  (lispy-mode -1))
(add-hook 'view-mode-hook (lambda () (bind-keys :map view-mode-map
	   ("G" . end-of-buffer))))

(use-package drag-stuff
  :ensure t
  :config
  (bind-keys :map global-map
	     ("M-<up>" . drag-stuff-up)
	     ("M-<down>" . drag-stuff-down)))

;; mark ring
(setq set-mark-command-repeat-pop t) ; repeat pop by C-SPC after C-u C-SPC

;(global-set-key (kbd "<prior>") 'scroll-other-window-down)
;(global-set-key (kbd "<next>") 'scroll-other-window)

; simple editing
(global-set-key (kbd "C-z q") 'quoted-insert)

;;; search and replace
(bind-keys :map global-map
	   ("M-r" . replace-string)
	   ("C-z M-r" . query-replace))

(use-package visual-regexp-steroids
  :ensure t
  :config
  (bind-keys :map my-mode-map
   ("C-M-s" . vr/isearch-forward)
   ("C-z R" . vr/replace)))

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

(define-prefix-command 'my-mark-prefix-keymap)
(define-key global-map (kbd "H-m") 'my-mark-prefix-keymap)

(defun my-mark-line()
  (interactive)
  (beginning-of-line)
  (set-mark-command nil)
  (end-of-line))

(bind-keys :map global-map
	   ("H-m H-m" . my-mark-line)
	   ("H-m m" . mark-word))

(defun open-line-above()
  (interactive)
  (beginning-of-line)
;  (open-line 1)
;  (indent-for-tab-command)
  (crux-smart-open-line-above)
  )
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
	   ("C-w" . backward-kill-word-or-kill-region)
	   ("M-k" . kill-whole-line)
	   ("C-<backspace>" . (lambda () (interactive) (message "use M-k"))))


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

(delete-selection-mode) ; using C-d to delete a selected region
(setq delete-active-region 'kill) ; kill the selected region while using delete and backspace. Note that you can still use C-d to delete a region.

;; (require 'misc)
;; (global-set-key (kbd "M-z") 'zap-up-to-char)

;(global-set-key (kbd "C-S-w") 'my-copy-line)

; copy to clipboard when M-w
(setq x-select-enable-clipboard t)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history nil)
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
  (define-key my-mode-map (kbd "C-M-f") 'sp-forward-sexp)
  (define-key my-mode-map (kbd "C-M-b") 'sp-backward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)
  (define-key smartparens-mode-map (kbd "C-M-[") 'sp-select-previous-thing)  
  (define-key my-mode-map (kbd "C-M-i") 'sp-change-enclosing)
  (define-key smartparens-mode-map (kbd "C-M-r") 'sp-rewrap-sexp)
  (define-key smartparens-mode-map (kbd "C-M-u") 'sp-unwrap-sexp)
  :bind(
  :map smartparens-mode-map
  ("C-M-k" . sp-kill-sexp)
  ("C-M-w" . sp-backward-kill-sexp)
;  ("C-M-f" . sp-forward-whitespace)
;  ("C-M-b" . sp-backward-whitespace)
  ("C-M-f" . sp-down-sexp)
  ("C-M-b" . sp-backward-up-sexp)
  ("C-S-a" . sp-beginning-of-sexp)
  ("C-S-e" . sp-end-of-sexp)  
  )

  :config
  (require 'smartparens-config)
  (setq sp-navigate-consider-symbols nil) ; don't treat a word as a sexp
  (setq sp-highlight-pair-overlay nil) ; don't hightlight inner content of a pair
  
  (sp-with-modes '(latex-mode)
    (sp-local-pair "|" "|")
    (sp-local-pair "\\|" "\\|"))
  (defun my-sp-wrap-with-pair()
     (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "("))
)
  
  (advice-remove 'delete-backward-char #'ad-Advice-delete-backward-char) ;prevent smartparens from deleting the whole \right) when using backspace
  :diminish smartparens-mode)

(define-key my-mode-map (kbd "C-z M-s") (defhydra hydra-smartparens (:hint nil)
				      ;; https://github-wiki-see.page/m/abo-abo/hydra/wiki/Smartparens
				      ;;  Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
				      ;;------------------------------------------------------------------------------------------------------------------------				      
				      "
 [_a_] beginning  [_n_] down      [_h_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
 [_e_] end        [_N_] bw down   [_H_] bw barf    [_u_]   unwrap        [_s_] kill-symbol  [_A_] absorb      [_C_] change outer
 [_f_] forward    [_p_] up        [_l_] slurp      [_U_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_g_] quit
 [_b_] backward   [_P_] bw up     [_L_] barf       [_(__{__[_] wrap (){}[]   [_j_] join    [_o_] convolute   [_K_] bw kill       [_q_] quit"
				      ;; Moving
				      ("a" sp-beginning-of-sexp)
				      ("e" sp-end-of-sexp)
				      ("f" sp-forward-symbol)
				      ("b" sp-backward-symbol)
				      ("n" sp-down-sexp)
				      ("N" sp-backward-down-sexp)
				      ("p" sp-up-sexp)
				      ("P" sp-backward-up-sexp)
  
				      ;; Slurping & barfing
				      ("h" sp-backward-slurp-sexp)
				      ("H" sp-backward-barf-sexp)
				      ("l" sp-forward-slurp-sexp)
				      ("L" sp-forward-barf-sexp)
  
				      ;; Wrapping
				      ("R" sp-rewrap-sexp)
				      ("u" sp-unwrap-sexp)
				      ("U" sp-backward-unwrap-sexp)
				      ("(" sp-wrap-round)
				      ("{" sp-wrap-curly)
				      ("[" sp-wrap-square)
  
				      ;; Sexp juggling
				      ("S" sp-split-sexp)
				      ("s" sp-kill-symbol :exit t)
				      ("r" sp-raise-sexp)
				      ("j" sp-join-sexp)
				      ("t" sp-transpose-sexp)
				      ("A" sp-absorb-sexp)
				      ("E" sp-emit-sexp)
				      ("o" sp-convolute-sexp)
  
				      ;; Destructive editing
				      ("c" sp-change-inner :exit t)
				      ("C" sp-change-enclosing :exit t)
				      ("k" sp-kill-sexp)
				      ("K" sp-backward-kill-sexp)
				      ("w" sp-copy-sexp)

				      ("q" nil)
				      ("g" nil)))

(defhydra hydra-outline (:color pink :hint nil :foregign-keys nil)
;; https://github.com/abo-abo/hydra/wiki/Emacs  
  "
^Hide^             ^Show^           ^Move
^^^^^^------------------------------------------------------
_q_: sublevels     _a_: all         _u_: up
_t_: body          _e_: entry       _n_: next visible
_o_: other         _i_: children    _p_: previous visible
_c_: entry         _k_: branches    _f_: forward same level
_l_: leaves        _s_: subtree     _b_: backward same level
_S_: subtree

"
  ;; Hide
  ("q" hide-sublevels)    ; Hide everything but the top-level headings
  ("t" hide-body)         ; Hide everything but headings (all body lines)
  ("o" hide-other)        ; Hide other branches
  ("c" hide-entry)        ; Hide this entry's body
  ("l" hide-leaves)       ; Hide body lines in this entry and sub-entries
  ("S" hide-subtree)      ; Hide everything in this entry and sub-entries
  ;; Show
  ("a" show-all)          ; Show (expand) everything
  ("e" show-entry)        ; Show this heading's body
  ("i" show-children)     ; Show this heading's immediate child sub-headings
  ("k" show-branches)     ; Show all sub-headings under this heading
  ("s" show-subtree)      ; Show (expand) everything in this heading & below
  ;; Move
  ("u" outline-up-heading)                ; Up
  ("n" outline-next-visible-heading)      ; Next
  ("p" outline-previous-visible-heading)  ; Previous
  ("f" outline-forward-same-level)        ; Forward - same level
  ("b" outline-backward-same-level)       ; Backward - same level
  ("g" nil "leave"))


;;; region
(bind-keys :map global-map
	   ("C-z w" . widen))

(use-package expand-region
  :ensure t
  :config
  (define-key my-mode-map (kbd "C-=") 'er/expand-region))

(use-package wrap-region
  :ensure t
  :diminish wrap-region-mode
  :config
  (wrap-region-mode))

;;; buffer, window, frame and file management
(bind-keys :map global-map
	   ("C-x k" . kill-this-buffer)
	   ("C-x K" . kill-buffer))

(global-set-key (kbd "<f2>") 'recenter-top-bottom)
(global-set-key (kbd "C-z x") 'delete-window)
(global-set-key (kbd "C-z X") 'delete-frame)
(bind-keys :map my-mode-map
	   ("C-1" . delete-other-windows)
	   ("C-." . other-window)
	   ("C-。" . other-window)
	   ("C-\\" . split-window-right))

;; register

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

(defun my-string-to-register ()
  (interactive)
  (copy-to-register ?s (mark) (point)))

(defun my-insert-register-string ()
  (interactive)
  (insert-register ?s))

(bind-keys :map my-mode-map
	   ("C-r w" . my-jump-to-saved-window)
	   ("C-r SPC" . my-jump-to-saved-location)
	   ("C-r s" . my-insert-register-string))

(bind-keys :map global-map
	   ("H-m w" . my-window-configuration-to-register)
	   ("H-m SPC" . my-point-to-register)
	   ("H-m s" . my-string-to-register))

(setq winner-dont-bind-my-keys t)
(winner-mode 1)

;(global-set-key (kbd "C-z ,") 'winner-undo)
;(global-set-key (kbd "C-z .") 'winner-redo)
(bind-keys :map my-mode-map
	   ("C-<escape>" . winner-undo)
	   ("M-<escape>" . winner-redo))

(use-package ace-window
  :ensure t
  :bind (:map my-mode-map
	      ("M-o" . ace-window))
  :config
  (setq aw-keys '(?j ?k ?l ?\; ?h))
  (setq aw-dispatch-always t)
  (defun my-aw-switch-buffer-in-window (window)
    (aw-switch-to-window window)
    (call-interactively 'ivy-switch-buffer))
  (add-to-list 'aw-dispatch-alist '(?b my-aw-switch-buffer-in-window "switch"))
  )

(use-package window-purpose
  :ensure t
  :config
;  (purpose-mode)
  (setq purpose-mode-map (make-sparse-keymap)) ; prevent from overriding existing keybindings
  (global-set-key (kbd "C-z t") 'purpose-toggle-window-buffer-dedicated))

;; projects and files

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind (;; :map projectile-mode-map
	 ;; ("M-j" . projectile-command-map)
	 :map my-mode-map
	 ("M-j" . projectile-command-map)
	 :map projectile-command-map
	 ("j" . projectile-switch-project)
	 ("A" . projectile-add-known-project)
	 ("r" . projectile-ripgrep)
	 ("C-r" . projectile-replace)
	 ("R" . projectile-remove-known-project))  
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'ivy)
  (setq projectile-indexing-method 'alien)
					;  (setq projectile-ignored-projects '("~") )
  (setq projectile-track-known-projects-automatically nil) ; only allow manually adding projects
  (setq projectile-auto-discover nil)
  (setq projectile-sort-order 'recentf)
  ;; (setq projectile-switch-project-action 'counsel-projectile-find-file)
  ;; (setq projectile-switch-project-action 'counsel-projectile-find-dir)
  (setq projectile-switch-project-action 'projectile-commander)
  ;; (setq projectile-current-project-on-switch 'keep)
					; leave the current project at the default position
					;  (setq projectile-dynamic-mode-line nil)
					;  (setq-default projectile-mode-line-function nil)
  (add-to-list 'projectile-other-file-alist '("tex" "org"))

  (defun projectile-find-notes()
    (interactive)
    (let ((default-directory "~/Dropbox/notes"))
      (projectile-find-file)))
  
  (global-set-key (kbd "C-z n") 'projectile-find-notes)
  
  (defun projectile-find-root()
    (interactive)
    (find-file (projectile-project-root)))
  (global-set-key (kbd "C-z C-r") 'projectile-find-root))

;; (use-package helm-projectile
;;   :ensure t
;;   :config
;;   (helm-projectile-on))

(use-package counsel-projectile
  :ensure t
  :config
  ;; (counsel-projectile-mode)
  )

(recentf-mode 1)
;; (bind-keys :map my-mode-map
;; 	   ("C-x C-r" . helm-recentf))
(setq recentf-max-saved-items 1000)

;; dired mode
(require 'dired-x)
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))
(diminish 'dired-omit-mode)
(setq dired-dwim-target t) ; so that dired tries to guess traget dir
;(setq-default dired-omit-files-p t)
(setq dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'\\|\.org_archive")
(define-key my-mode-map (kbd "C-x j") 'dired-jump)
(define-key my-mode-map (kbd "C-x J") 'dired-jump-other-window)


(bind-keys :map dired-mode-map
	   ("f" . find-file)
	   ("l" . find-file))
(define-key dired-mode-map (kbd "C-/") (lambda() (interactive) (message "C-/ is disabled in dired")))
(define-key dired-mode-map (kbd "SPC") 'browse-url-of-dired-file)
(define-key dired-mode-map (kbd "E") 'wdired-change-to-wdired-mode)


(use-package dired-ranger
  :ensure t
  :bind (:map dired-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste)))

;; a trick to make dired be able to access ~/Downloads and folders alike
(when (eq system-type 'darwin)
  (setq insert-directory-program "gls" dired-use-ls-dired t))

(setq dired-listing-switches "-alht --group-directories-first")
;(setq dired-listing-switches "-AlBGh  --group-directories-first")

(setq dired-deletion-confirmer #'y-or-n-p)
;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

(defun dired-move-to-literature (&optional arg)
  (interactive "P")
  (cl-letf* ((default-dest-dir "~/Dropbox/research/literature/")
             ((symbol-function 'dired-dwim-target-directory)
              (lambda ()
                default-dest-dir)))
    (call-interactively #'dired-do-rename)))
(define-key dired-mode-map (kbd "L") 'dired-move-to-literature)

(defun find-scratch ()
  (interactive)
  (switch-to-buffer
   "*scratch*"))
(global-set-key (kbd "C-z s") 'find-scratch)
(global-set-key (kbd "C-z N") 'make-frame)



(defun save-all-buffers()
  (interactive)
  (save-some-buffers 1)
  (message "all buffers saved"))
(global-set-key (kbd "C-x C-s")  'save-all-buffers)

(bind-keys :map my-mode-map
	   ("M-s" . save-all-buffers))
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

;; input method

;; Chinese
;; (global-set-key (kbd "C-z \\") 'toggle-input-method)
;; (use-package pyim
;;   :ensure t
;;   :config
;;   ;; 拼音词库设置
;;   (use-package pyim-basedict
;;     :ensure t)
;;   (pyim-basedict-enable)
;;   )

;; (setq default-input-method "pyim")
;; ;(setq pyim-punctuation-translate-p '(yes no auto))   ;使用全角标点。
;; (setq pyim-punctuation-dict nil)
;; ;(setq pyim-punctuation-translate-p '(no yes auto))   ;使用半角标点。
;; (setq pyim-page-tooltip 'popup)

;; (use-package cnfonts
;;   :ensure t)

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
  (if current-prefix-arg
      (find-file "~/.emacs.d/configs/")
    (let ((default-directory "~/.emacs.d/configs/"))
	(counsel-rg))))   

(global-set-key (kbd "C-z E") 'find-dot-emacs)
(global-set-key (kbd "C-z e") 'find-emacs-configs)
(defun find-my-info()
  (interactive)
  (find-file "~/Dropbox/org_non_agenda/personal/info.org"))
(defun find-planer()
  (interactive)
(find-file "~/Dropbox/org/plan.org"))

(defun find-non-agenda()
  (interactive)
(find-file "~/Dropbox/org_non_agenda/"))

(defun find-capture()
  (interactive)
(find-file "~/Dropbox/org/capture.org"))

(defun find-stack()
  (interactive)
  (find-file "~/Dropbox/org/stack.org"))

(defun find-phone()
  (interactive)
  (find-file "~/Dropbox/org/phone.org"))

(defun find-literature()
  (interactive)
  (find-file "~/Dropbox/research/literature/"))

(defun find-download()
  (interactive)
  (find-file "~/Downloads/"))

(when (eq system-type 'darwin)
  (bind-keys :map global-map
	   ("C-z M-p" . find-phone)	   
	   ("C-z M-s" . find-stack))
) ; end os x
(bind-keys :map global-map
	   ("C-z p" . find-planer)	   
	   ("C-z C-d" . find-download)
	   ("C-z l" . find-literature)
	   ("C-z C-c" . find-capture)
	   ("C-z F" . find-my-info)
	   ("C-z P" . find-non-agenda))
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
  :ensure t
  :bind
  (:map my-mode-map
	("C-r m" . rg-menu)
	("C-r C-f" . rg-dwim-current-file)
	("C-r C-d" . rg-dwim-current-dir)
	("C-r C-p" . rg-dwim-project-dir)))

(use-package evil
  :ensure t
  :bind 
  :config)

(define-key my-mode-map (kbd "C-]") (defhydra my-find-char (:body-pre (progn (setq my-vim-f-char (read-char))
									     (evil-find-char 1 my-vim-f-char))
								      :post (setq my-vim-f-char nil)
								      :hint nil)
				      (";" (evil-repeat-find-char))))

(define-key my-mode-map (kbd "H-[") (defhydra my-find-char-backward (:body-pre (progn (setq my-vim-F-char (read-char))
										      (evil-find-char-backward 1 my-vim-F-char))
									       :post (setq my-vim-F-char nil)
									       :hint nil)
				      (";" (evil-repeat-find-char))))

;; (use-package embark
;;   :ensure t
;;   :bind
;;   (:map ivy-minibuffer-map ("C-'" . embark-act)
;; 	;; pick some comfortable binding
;; 					;   ("C-;" . embark-dwim)        ;; good alternative: M-.
;; 					;   ("C-h B" . embark-bindings) ;; alternative for `describe-bindings'
;; 	)
;;   :init

;;   ;; Optionally replace the key help with a completing-read interface
;;   (setq prefix-help-command #'embark-prefix-help-command)

;;   :config

;;   ;; Hide the mode line of the Embark live/completions buffers
;;   (add-to-list 'display-buffer-alist
;;                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                  nil
;;                  (window-parameters (mode-line-format . none)))))

(use-package exec-path-from-shell
  :ensure t
  :init
  (unless (eq system-type 'windows-nt)
    (exec-path-from-shell-initialize)	;get $PATH from shell
    ))
;; (use-package disable-mouse
;; :ensure t
;; :diminish disable-mouse-global-mode
;; :config
;; (global-disable-mouse-mode) ; in case I move the mouse accidentally
;; )

;; interacting with the world outside emacs

(use-package google-this
:ensure t  
:init
(global-set-key (kbd "C-z G") 'google-this-mode-submap)
(global-set-key (kbd "C-z g") 'google-this-noconfirm))

(bind-keys :map global-map
	   ("C-z C-q" . exit-recursive-edit))

(provide 'init-general-editing)
