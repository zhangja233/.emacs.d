(setq initial-major-mode 'org-mode)

(add-hook 'org-mode-hook 'flyspell-mode)

;;org-mobile
(setq org-mobile-directory "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/")
(setq org-mobile-inbox-for-pull "~/org/phone.org")
(global-set-key (kbd "C-z M") 'org-mobile-push)

; Don't add PROPERTIES to my headings(As long as there is no structure like
; * heading1
; ** heading2
; ** heading2
; there won't be sync conflicts)
(setq org-mobile-force-id-on-agenda-items nil)

; minor modes
(use-package valign ; visual alignment for org tables when using Chinese
:ensure t
)
(add-hook 'org-mode-hook #'valign-mode)

(add-hook 'org-mode-hook (lambda () (org-bullets-mode)))
;(add-hook 'org-mode-hook (lambda () (flyspell-mode)))

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([+]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◦"))))))

(eval-after-load "org"
'(progn
;; configs for org-mode
;;; customize org-agenda   
(setq org-agenda-start-on-weekday nil) ; make org-agenda start at the current day

(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((agenda "")
          (alltodo "")))))


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
   (define-key org-mode-map (kbd "C-c C-<tab>") 'cycle-this-heading)
      
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

(define-key org-mode-map (kbd "M-<return>") 'insert-heading-and-demote)   
(define-key org-mode-map (kbd "M-h") nil) ; override org-mode key binding
(define-key org-mode-map (kbd "C-<tab>") nil) ; override org-mode key binding
(define-key org-mode-map (kbd "C-,") nil) ; override org-mode key binding
(define-key org-mode-map (kbd "C-'") nil) ; override org-mode key binding
;(define-key org-mode-map (kbd "S-<right>") nil) ; override org-mode key binding
;(define-key org-mode-map (kbd "S-<left>") nil) ; override org-mode key binding
(define-key org-mode-map (kbd "M-a") 'beginning-of-buffer)
(define-key org-mode-map (kbd "M-e") 'end-of-buffer)

; table
(define-key org-mode-map (kbd "C-c n") 'org-table-next-row)
(define-key org-mode-map (kbd "C-c p") 'org-table-insert-row)
(define-key org-mode-map (kbd "C-c b") 'org-table-insert-column)

; clock
(global-set-key (kbd "C-z i") 'org-clock-in)
(global-set-key (kbd "C-z o") 'org-clock-out)

(setq org-clock-mode-line-total 'today)
(define-key org-mode-map (kbd "C-;") 'org-ctrl-c-ctrl-c)
(define-key org-mode-map (kbd "C-c a") 'org-metaleft)
(define-key org-mode-map (kbd "C-c c") 'org-insert-todo-heading)
(define-key org-mode-map (kbd "C-c d") 'org-metaright)
(define-key org-mode-map (kbd "C-c r") 'org-clock-report)
(define-key org-mode-map (kbd "C-c js") (lambda()
  (interactive)
  (insert (concat "#+BEGIN_" "SRC")) (newline-and-indent) (newline) (insert (concat "#+END_" "SRC")) (previous-line)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(defun my-org-confirm-babel-evaluate (lang body)
    (not (string= lang "python")))  ; don't ask 
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; setup about todo
(define-key org-mode-map (kbd "C-c C-SPC") 'org-todo)
;(setq org-todo-keywords
;      '((sequence  "TODO" "NEXT"  "|" "DONE" "CANCELED")))
(setq calendar-week-start-day 1) ;start week on Mon
(setq org-agenda-files '("~/org"));my personal org files which store my to-do lists
(setq org-default-notes-file "~/org/capture.org") ; the file to store captured items
(setq org-adapt-indentation nil) ; do not indent when using c-j after a title
 '(org-startup-truncated nil)
))

(defun find-work() 
  (interactive) (find-file "~/org/work.org")
  )
(global-set-key (kbd "C-z w")  'find-work)
(global-set-key (kbd "C-z a") 'org-agenda)



(defun find-planer()
  (interactive)
(find-file "~/org/plan.org")
  )
(global-set-key (kbd "C-z p")  'find-planer)

(defun find-daily()
  (interactive)
(find-file "~/org/daily.org")
  )
(global-set-key (kbd "C-z d")  'find-daily)

(defun find-download()
  (interactive)
  (find-file "~/Downloads/")
  )
(global-set-key (kbd "C-z C-d")  'find-download)

(defun find-learn()
  (interactive)
(find-file "~/org/learn.org")
  )
(global-set-key (kbd "C-z l")  'find-learn)

(provide 'init-org)
