;; general editing

(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)
;(add-hook 'org-mode-hook 'visual-line-mode)

(use-package org-roam
  :ensure t
  :config
  (setq org-roam-directory "~/Dropbox/org/roam")
  (org-roam-db-autosync-mode)
  (define-key org-mode-map (kbd "C-c i") 'org-roam-node-insert)
  (define-key org-mode-map (kbd "C-c t") 'org-id-get-create)
  :bind
  ("C-z f" . org-roam-node-find)
  )

(use-package valign ; visual alignment for org tables when using Chinese
:ensure t
:diminish valign-mode
)
;(add-hook 'org-mode-hook #'valign-mode)


(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode)))
  )

;(add-hook 'org-mode-hook (lambda () (flyspell-mode)))

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([+]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◦"))))))

(eval-after-load "org"
'(progn
   (bind-keys :map org-mode-map
	      ("C-<return>" . org-insert-heading)
	      ("C-;" . org-insert-todo-heading)
	      ("M-p" . org-previous-visible-heading)
	      ("M-n" . org-next-visible-heading)
	      ("M-;" . org-insert-todo-subheading)
	      ("M-<return>" . org-insert-subheading)
	      ("M-'" . org-metaright)
	      ("C-c m" . org-mark-subtree)
	      ("C-c w". org-cut-subtree)
	      )
   (bind-keys :map global-map
	      ("C-z m" . org-store-link)
	      ("C-z G" . org-clock-goto)
	      )
;; configs for org-mode
;;; customize org-agenda   
(setq org-agenda-start-on-weekday nil) ; make org-agenda start at the current day

(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((agenda "")
          (alltodo "")))))

;(setq org-agenda-prefix-format )


;; org-capture
(global-set-key (kbd "C-z c") 'org-capture)

; Define the custum capture templates
(setq org-capture-templates
       '(("t" "todo" entry (file org-default-notes-file)
	  "* TODO %?\n%u\n%a\n")
	 ("i" "Idea" entry (file org-default-notes-file)
	  "* %? :IDEA: \n%t")
	 ("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
	  "** NEXT %? \nDEADLINE: %t") ))

;   (setq org-support-shift-select t)
     ;;  (org-map-entries
  ;; '(org-toggle-tag "ARCHIVE" 'on )
  ;; "/+DONE" 'file 'archive 'comment)
   (defun cycle-this-heading()
     (interactive)
     (outline-previous-heading)
     (org-cycle)
     )
   (define-key org-mode-map (kbd "C-c <tab>") 'cycle-this-heading)
      
   (defun insert-heading-and-demote()
     (interactive)
     (org-insert-heading-respect-content)
     (org-do-demote)
       )
   (defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'file))
(define-key org-mode-map (kbd "C-c <return>") 'org-archive-done-tasks)

(defun org-insert-superheading()
  (interactive)
  (org-insert-heading)
  (org-metaleft)
  )
(define-key org-mode-map (kbd "C-S-<return>") 'org-insert-superheading)

;(define-key org-mode-map (kbd "M-h") nil) ; override org-mode key binding
(define-key org-mode-map (kbd "C-,") nil) ; override org-mode key binding
(define-key org-mode-map (kbd "C-'") nil) ; override org-mode key binding
;(define-key org-mode-map (kbd "S-<right>") nil) ; override org-mode key binding
;(define-key org-mode-map (kbd "S-<left>") nil) ; override org-mode key binding
;(define-key org-mode-map (kbd "M-a") 'beginning-of-buffer)
;(define-key org-mode-map (kbd "M-e") 'end-of-buffer)

; table
(define-key org-mode-map (kbd "C-c n") 'org-table-next-row)
(define-key org-mode-map (kbd "C-c p") 'org-table-insert-row)
(define-key org-mode-map (kbd "C-c b") 'org-table-insert-column)
(define-key org-mode-map (kbd "C-c <left>") 'org-table-move-cell-left)
(define-key org-mode-map (kbd "C-c <right>") 'org-table-move-cell-right)
(define-key org-mode-map (kbd "C-c <up>") 'org-table-move-cell-up)
(define-key org-mode-map (kbd "C-c <down>") 'org-table-move-cell-down)

; clock
(global-set-key (kbd "C-z i") 'org-clock-in)
(global-set-key (kbd "C-z o") 'org-clock-out)

(setq org-clock-mode-line-total 'today)
(define-key org-mode-map (kbd "C-c a") 'org-metaleft)
(define-key org-mode-map (kbd "C-c d") 'org-metaright)
(define-key org-mode-map (kbd "C-c r") 'org-clock-report)
; miscellaneous
(define-key org-mode-map (kbd "C-c v") 'org-latex-preview)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t) (latex . t)))

(defun my-org-confirm-babel-evaluate (lang body)
    (not (string= lang "python")))  ; don't ask 
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; setup about todo
(define-key org-mode-map (kbd "C-c C-SPC") 'org-todo)
;(setq org-todo-keywords
;      '((sequence  "TODO" "NEXT"  "|" "DONE" "CANCELED")))
(setq calendar-week-start-day 1) ;start week on Mon
(setq org-agenda-files (directory-files-recursively "~/Dropbox/org" "\.org$"));my personal org files which store my to-do lists
(setq org-default-notes-file "~/Dropbox/org/capture.org") ; the file to store captured items
(setq org-adapt-indentation nil) ; do not indent when using c-j after a title
; '(org-startup-truncated nil)
(setq org-return-follows-link t) ; use return to open link
))

(setq-default org-catch-invisible-edits 'smart)

(defun find-work() 
  (interactive) (find-file "~/Dropbox/org/work.org")
  )
(global-set-key (kbd "C-z w")  'find-work)
(global-set-key (kbd "C-z a") 'org-agenda)



(defun find-planer()
  (interactive)
(find-file "~/Dropbox/org/plan.org")
  )
(global-set-key (kbd "C-z p")  'find-planer)

(defun find-download()
  (interactive)
  (find-file "~/Downloads/")
  )
(global-set-key (kbd "C-z C-d")  'find-download)

(provide 'init-org)
