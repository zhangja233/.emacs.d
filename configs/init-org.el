(require 'org)
;;; moving the cursor
(defun org-beginning-of-headline-text ()
  "when on a heading line, move the cursor to the beginning of the text. E.g.,
  * ba|r  will become * |bar
  * TODO foo| will become * TODO |foo"
  (interactive)
  (let ((org-special-ctrl-a t))
    (org-beginning-of-line)
    ))

(bind-keys :map org-mode-map
	   ("C-c a" . org-beginning-of-headline-text))

(defun org-create-list-item-above()
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (insert ?-)
  (insert ? ))
   (setq org-export-with-sub-superscripts nil)
   (bind-keys :map org-mode-map
	      ("C-j" . newline-and-indent)
;	      ("C-<return>" . org-insert-heading)
	      ("C-<return>" . org-meta-return)	      
	      ("C-;" . my-org-insert-todo-heading)
;	      ("M-p" . org-backward-paragraph)
;	      ("M-n" . org-forward-paragraph)	      
	      ("C-<right>" . org-metaright)
	      ("C-<left>" . org-metaleft)
;	      ("M-;" . org-metaright)
;	      ("M-'" . org-metaleft)	      
	      ("C-<up>" . org-metaup)
	      ("C-<down>" . org-metadown)	      
	      ("M-;" . org-insert-todo-subheading)
	      ("M-<return>" . org-insert-subheading)
	      ("S-<return>" . my-org-shiftreturn)	      
;	      ("M-<return>" . org-meta-return)
	      ("M-[" . org-metaleft)
	      ("M-]" . org-metaright)
	      ("C-c m" . org-mark-subtree)
	      ("C-c n" . org-narrow-to-subtree)
	      ("C-c N" . widen)
	      ("C-c w". org-cut-subtree)
	      ("C-c y" . org-paste-subtree)
	      ("C-c ]" . org-ref-insert-link)
	      ("C-c E" . org-set-effort)
	      ("C-<up>" . org-timestamp-up-day) ; timestamp
;	      ("C-c C-n" . org-timestamp-down-day)
	      )
   
;;; structure
(when (eq system-type 'darwin)
  (require 'worf)
  (add-hook 'org-mode-hook 'worf-mode)
  (bind-keys :map org-mode-map
	     ("C-c g" . worf-goto))
  (bind-keys :map worf-mode-map
	     ("C-d" . nil)
	     ("[" . nil)
	     ("]" . nil)
	     ("x" . wspecial-worf-cut-subtree))
  )

(bind-keys :map org-mode-map
	   ("C-c r" . avy-org-refile-as-child)
	   ("C-c J" . avy-org-goto-heading-timer))
   
;;; org-table   
 
   (define-prefix-command 'org-table-prefix-keymap)
   (define-key org-mode-map (kbd "C-c t") 'org-table-prefix-keymap)   
   (bind-keys :map org-mode-map
	      ("C-c C-j" . org-table-copy-down)
	      ("C-|" . org-table-hline-and-move)
	      ("C-c t a" . org-table-beginning-of-field)
	      ("C-c t e" . org-table-end-of-field)	      
	      ("C-c x" . org-table-delete-column)
	      ("C-c -" . org-table-insert-hline)
	      ("｜" . (lambda() (interactive) (insert "|")))
	      ("C-c t p" . org-table-insert-row-above)
	      ("C-c t b" . org-table-insert-column)
	      ("C-c t f" . org-table-insert-column-right)
	      ("C-c t n" . org-table-insert-row-below))
   
(defun org-table-insert-column-right()
  "Insert a new column into the table on the right."
  (interactive)
  (unless (org-at-table-p) (user-error "Not at a table"))
  (search-forward "|") 
  (org-table-insert-column))

(defun org-table-insert-row-above()
  "Insert a new row above, and also make the point be at the cell
   above where it was"
  (interactive)
  (save-excursion (org-table-insert-row))
  (previous-line))

(defun org-table-insert-row-below()
  "Insert a new row below, and also make the point be at the cell
   below where it was"
  (interactive)
  (save-excursion (org-table-insert-row t))
  (next-line))

(eval-after-load "org-agenda"
  '(progn
     (bind-keys :map org-agenda-mode-map
		("C-;" . org-agenda-columns))))
(bind-keys :map global-map
	   ("C-z m" . org-store-link)
	   ("C-z M-c" . calendar))
   
;;; document structure
(defun my-org-insert-todo-heading(&optional arg)
  (interactive "P")
  (cond ((equal arg '(4))
	 (call-interactively 'org-insert-todo-subheading))
	(t
	 (call-interactively 'org-insert-todo-heading))))
(defun my-org-previous-row(&optional arg)
    (interactive)
  (org-table-align)
  (previous-line))

(defun my-org-shiftreturn (&optional arg)
  "Act on the current element according to context.
   This does one of the following:

- in a table, move to the previous row"
  (interactive "P")
  (call-interactively (cond ((org-at-table-p) #'my-org-previous-row)
			    (t #'previous-line))))
(setq org-yank-adjusted-subtrees t)
   
;;; org-agenda   
(global-set-key (kbd "C-z a") 'org-agenda)
(setq org-agenda-start-on-weekday nil) ; make org-agenda start at the current day
(setq org-agenda-show-future-repeats 'next)

(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((agenda "")
          (alltodo "")))))

;(setq org-agenda-prefix-format )
(setq org-agenda-prefix-format '(
  ;; (agenda  . " %i %-12:c%?-12t% s") ;; file name + org-agenda-entry-type
  (agenda  . "  %?-12t% s")
;  (timeline  . "  % s")
  (todo  . " %i %-12:c")
  (tags  . "%e ")
  (search . " %i %-12:c")))
(setq org-agenda-custom-commands
      '(("p" tags "+pwd")
;	("n" ((tags-todo) (tags "+next")) )
	("n"  tags "+next")	
	("P" tags "pwd+{c[0-9].*}")
	("T" tags "+today")
	("3" tags "+30")))
(setq org-agenda-skip-function-global '(org-agenda-skip-entry-if 'todo 'done))

;; column view
(setq org-columns-default-format "%30ITEM(Task)  %daily %CLOCKSUM_T
%CLOCKSUM{:} %6Effort(Estim){:} %DEADLINE %ALLTAGS")

;; org-property
(bind-keys :map org-mode-map
	   ("C-c p" . org-set-property))


;; tags
;; (setq org-tags-match-list-sublevels nil)
					;don't list sublevels when searching for tags
(setq org-use-tag-inheritance nil)
(setq org-fast-tag-selection-single-key t) ; fast tag selection exits after first change
(setq org-use-fast-tag-selection t)
;; shortcuts to tags
(setq org-tag-alist '(("next" . ?n) ("pwd" . ?p) ("easy" . ?e) ("short" . ?s) ("best" . ?b)))


;;; org-capture
(global-set-key (kbd "C-z c") 'org-capture)
;; Define the custum capture templates
(setq org-capture-templates
       '(("t" "todo" entry (file "") "* TODO %? \n%U\n%i\n")
	 ("T" "Todo" entry (file+headline "~/Dropbox/org/roam/work.org" "two minutes tasks")
"* TODO %? \n%U\n%i\n")
	 ("i" "Idea" entry (file org-default-notes-file)
	  "* %? :IDEA: \n%t")
	 ("n" "Notes" entry (file org-default-notes-file)
	  "* %? :NOTES: \n%t")))

;; org-refile
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
;(setq org-refile-use-outline-path nil)


;   (setq org-support-shift-select t)
     ;;  (org-map-entries
  ;; '(org-toggle-tag "ARCHIVE" 'on )
  ;; "/+DONE" 'file 'archive 'comment)
   (defun cycle-this-heading()
     (interactive)
     (outline-previous-heading)
     (org-cycle))
   (define-key org-mode-map (kbd "C-c <tab>") 'cycle-this-heading)
      
   (defun insert-heading-and-demote()
     (interactive)
     (org-insert-heading-respect-content)
     (org-do-demote))
   (defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'file))
;(define-key org-mode-map (kbd "C-c <return>") 'org-archive-done-tasks)

(defun org-insert-superheading()
  (interactive)
  (org-insert-heading)
  (org-metaleft))
(define-key org-mode-map (kbd "C-S-<return>") 'org-insert-superheading)

;(define-key org-mode-map (kbd "M-h") nil) ; override org-mode key binding
;; (define-key org-mode-map (kbd "C-,") nil)
					; override org-mode key binding

;; org-clock
(bind-keys :map org-mode-map
	   ("C-c i" . org-clock-in)
	   ("C-c o" . org-clock-out))
(global-set-key (kbd "C-z o") 'org-clock-out)

(setq org-clock-persist 'history)
(setq org-clock-history-length 100)
(org-clock-persistence-insinuate) ; save the clock history across emacs sessions
(setq org-clock-continuously t)
(when (eq system-type 'darwin)
  (require 'org-clock-budget)
  (define-key my-mode-map (kbd "C-z r") 'org-clock-budget-report)
  (setq org-clock-budget-daily-budgetable-hours '12)
  (setq org-clock-budget-intervals '(("BUDGET_MONTH" org-clock-budget-interval-this-month)
				    ("BUDGET_WEEK" org-clock-budget-interval-this-week))))


(defun my-org-clocktable-indent-string (level)
  (if (= level 1)
      ""
    (let ((str "<"))
      (while (> level 2)
        (setq level (1- level)
              str (concat str "--")))
      (concat str "-> "))))

(advice-add 'org-clocktable-indent-string :override #'my-org-clocktable-indent-string)

(setq org-clock-mode-line-total 'today)
(setq org-duration-format (quote h:mm))

(use-package counsel-org-clock
  :ensure t
  :bind (:map my-mode-map
              ("C-z j" . counsel-org-clock-history)
)
)

(use-package org-mru-clock
  :ensure t
  :bind* (("C-z i" . org-mru-clock-in)
          ;; ("C-z j" . org-mru-clock-select-recent-task)
	  )
  :config
  (setq org-mru-clock-how-many 100)
  ;; (setq org-mru-clock-completing-read #'helm--completing-read-default)
  (setq org-mru-clock-completing-read #'ivy-completing-read)
  ;;  (add-hook 'minibuffer-setup-hook #'org-mru-clock-embark-minibuffer-hook)
  )

;;; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t) (latex . t) (perl . t) (shell . t) (fortran . t) (R . t)))

(add-to-list 'org-src-lang-modes '("fortran" . f90)) ; use f90-mode when specify fortran language
(add-to-list 'org-src-lang-modes '("R" . ess-r))
(setq org-confirm-babel-evaluate nil) ; don't ask for confirmation

;; setup about todo
(define-key org-mode-map (kbd "C-c C-SPC") 'org-todo)
;(setq org-todo-keywords
;      '((sequence  "TODO" "NEXT"  "|" "DONE" "CANCELED")))
(setq calendar-week-start-day 1) ;start week on Mon

;; osx specific stuff
(when (eq system-type 'darwin)
  (setq org-agenda-files (directory-files-recursively "~/Dropbox/org" "\.org$"));my personal org files which store my to-do lists
(setq org-default-notes-file "~/Dropbox/org/capture.org") ; the file to store captured items
) ; end osx

(setq org-adapt-indentation nil) ; do not indent when using c-j after a title
; '(org-startup-truncated nil)
(setq org-return-follows-link t) ; use return to open link

;; mark-up


(define-key org-mode-map (kbd "M-u") (defhydra hydra-org (:hint nil)
;; https://github-wiki-see.page/m/abo-abo/hydra/wiki/Smartparens
;;   "
;;  Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
;; ------------------------------------------------------------------------------------------------------------------------
;;  [_a_] beginning  [_n_] down      [_h_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
;;  [_e_] end        [_N_] bw down   [_H_] bw barf    [_u_]   unwrap        [_s_] splice  [_A_] absorb      [_C_] change outer
;;  [_f_] forward    [_p_] up        [_l_] slurp      [_U_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_g_] quit
;;  [_b_] backward   [_P_] bw up     [_L_] barf       [_(__{__[_] wrap (){}[]   [_j_] join    [_o_] convolute   [_K_] bw kill       [_q_] quit"
  ;; Moving
  ("n" org-next-visible-heading)
  ("p" org-previous-visible-heading)
  ("f" org-forward-heading-same-level)
  ("b" org-backward-heading-same-level)
  ("u" outline-up-heading)
  ("q" nil)
  ("g" nil)))

;;; org-display
(setq org-startup-with-inline-images t)
;; display inline image that's in pdf format
(use-package org-inline-pdf
  :ensure t
  :hook (org-mode . org-inline-pdf-mode))
(bind-keys :map org-mode-map
	    ("C-c H-i" . org-redisplay-inline-images))
;; org-latex
(define-key org-mode-map (kbd "C-c l") 'org-latex-preview)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

;;; counsel-org
(bind-keys :map my-mode-map
	   ("C-z C-j" . counsel-org-goto-all)
	   ("C-c g" . counsel-org-goto)
	   )

(use-package helm-org-rifle
  :ensure t
  :bind (:map my-mode-map
	      ("C-r r" . helm-org-rifle)))

(setq-default org-catch-invisible-edits 'smart)

(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)
;(add-hook 'org-mode-hook 'visual-line-mode)

(use-package org-roam
  :ensure t
  :bind
  (:map global-map
	("C-z f" . org-roam-node-find)
	:map org-mode-map
	("C-c S" . org-roam-db-sync))
  :config
  (setq org-roam-directory "~/Dropbox/org/roam")
  (setq org-roam-capture-templates '(("d" "default" plain "%?" :target
  (file+head "${slug}.org" "#+title: ${title}
")
  :unnarrowed t))) ; remove date info from the default value 		 
  (org-roam-db-autosync-mode)
  (define-key org-mode-map (kbd "C-c T") 'org-id-get-create))

(use-package org-ref
  :ensure t
  :config
  (setq org-ref-bibliography-notes "~/Dropbox/org/ref/notes.org"
      org-ref-default-bibliography '("~/Dropbox/org/ref/master.bib")
      org-ref-pdf-directory "~/Dropbox/org/ref/pdfs/"))


(use-package valign ; visual alignment for org tables when using Chinese
:ensure t
:diminish valign-mode
)

;(add-hook 'org-mode-hook #'valign-mode)


(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))


(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([+]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◦"))))))

(setq initial-major-mode 'org-mode)


(bind-keys ("C-z C-x o" . org-open-at-point-global))

(provide 'init-org)
