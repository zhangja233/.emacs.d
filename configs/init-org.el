(eval-after-load "org"
'(progn
   (defun org-create-list-item-above()
     (interactive)
     (beginning-of-line)
     (open-line 1)
     (insert ?-)
     (insert ? ))
   (setq org-export-with-sub-superscripts nil)
   (bind-keys :map org-mode-map
	      ("C-j" . newline-and-indent)
	      ("C-<return>" . org-insert-heading)
	      ("C-;" . org-insert-todo-heading)
	      ("<up>" . org-previous-visible-heading)
	      ("<down>" . org-next-visible-heading)
;	      ("<right>" . org-forward-heading-same-level)
;	      ("<left>" . org-backward-heading-same-level)
	      ("<right>" . org-metaright)
	      ("<left>" . org-metaleft)	      
	      ("M-;" . org-insert-todo-subheading)
	      ("M-<return>" . org-insert-subheading)
	      ("S-<return>" . org-meta-return)
	      ("M-[" . org-metaleft)
	      ("M-]" . org-metaright)
	      ("C-c m" . org-mark-subtree)
	      ("C-c w". org-cut-subtree)
	      ("C-c j" . org-table-copy-down)
	      ("C-c o" . org-create-list-item-above)
	      ("C-c ]" . org-ref-insert-link)
	      ("C-|" . org-table-hline-and-move)
	      ("C-c e" . insert-equation)
	      ("C-c E" . org-set-effort)
	      ("C-<up>" . org-timestamp-up-day) ; timestamp
;	      ("C-c C-n" . org-timestamp-down-day)
	      )
	 (eval-after-load "org-agenda"
	 '(progn
   (bind-keys :map org-agenda-mode-map
	      ("C-;" . org-agenda-columns))))
   (bind-keys :map global-map
	      ("C-z m" . org-store-link)
	      ("C-z G" . org-clock-goto)
	      ("C-x c" . calendar))
;; configs for org-mode
;;; customize org-agenda   
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
	("P" tags "+pwd-life-entertainment")))
(setq org-columns-default-format "%30ITEM(Task) %CLOCKSUM{:} %CLOCKSUM_W %6Effort(Estim){:} %DEADLINE %TAGS %done")


; tags
;(setq org-tags-match-list-sublevels nil) ;don't list sublevels when searching for tags

;; org-capture
(global-set-key (kbd "C-z c") 'org-capture)

; Define the custum capture templates
(setq org-capture-templates
       '(("t" "todo" entry (file org-default-notes-file)
	  "* TODO %?\n%u\n%a\n")
	 ("i" "Idea" entry (file org-default-notes-file)
	  "* %? :IDEA: \n%t")
	 ("n" "Notes" entry (file org-default-notes-file)
	  "* %? :NOTES: \n%t")))

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
(define-key org-mode-map (kbd "C-c <return>") 'org-archive-done-tasks)

(defun org-insert-superheading()
  (interactive)
  (org-insert-heading)
  (org-metaleft))
(define-key org-mode-map (kbd "C-S-<return>") 'org-insert-superheading)

;(define-key org-mode-map (kbd "M-h") nil) ; override org-mode key binding
(define-key org-mode-map (kbd "C-,") nil) ; override org-mode key binding
(define-key org-mode-map (kbd "C-'") nil) ; override org-mode key binding
;(define-key org-mode-map (kbd "S-<right>") nil) ; override org-mode key binding
;(define-key org-mode-map (kbd "S-<left>") nil) ; override org-mode key binding
;(define-key org-mode-map (kbd "M-a") 'beginning-of-buffer)
;(define-key org-mode-map (kbd "M-e") 'end-of-buffer)

; table
(define-key org-mode-map (kbd "C-c p") 'org-table-insert-row)
(define-key org-mode-map (kbd "C-c b") 'org-table-insert-column)

; clock
(global-set-key (kbd "C-z i") 'org-clock-in)
(global-set-key (kbd "C-z o") 'org-clock-out)

(setq org-clock-mode-line-total 'today)
(setq org-duration-format (quote h:mm))

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

(when (eq system-type 'darwin)
  (setq org-agenda-files (directory-files-recursively "~/Dropbox/org" "\.org$"));my personal org files which store my to-do lists
((setq )etq org-default-notes-file "~/Dropbox/org/capture.org") ; the file to store captured items
) ; end osx

(setq org-adapt-indentation nil) ; do not indent when using c-j after a title
; '(org-startup-truncated nil)
(setq org-return-follows-link t) ; use return to open link
))

(setq-default org-catch-invisible-edits 'smart)

(global-set-key (kbd "C-z a") 'org-agenda)

(add-hook 'org-mode-hook 'flyspell-mode)
;(add-hook 'org-mode-hook 'auto-fill-mode)
;(add-hook 'org-mode-hook 'visual-line-mode)

(use-package org-roam
  :ensure t
  :config
  (setq org-roam-directory "~/Dropbox/org/roam")
  (org-roam-db-autosync-mode)
  (define-key org-mode-map (kbd "C-c i") 'org-roam-node-insert)
  (define-key org-mode-map (kbd "C-c t") 'org-id-get-create)
  :bind
  ("C-z f" . org-roam-node-find))

(use-package org-ref
  :ensure t
  :config
  (setq org-ref-bibliography-notes "~/Dropbox/org/ref/notes.org"
      org-ref-default-bibliography '("~/Dropbox/org/ref/master.bib")
      org-ref-pdf-directory "~/Dropbox/org/ref/pdfs/"))


(use-package valign ; visual alignment for org tables when using Chinese
:ensure t
:diminish valign-mode)
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



(provide 'init-org)
