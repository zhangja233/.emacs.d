(add-hook 'LaTeX-mode-hook 'flyspell-mode)
;(add-hook 'LaTeX-mode-hook 'auto-fill-mode)

(use-package latex
  :ensure auctex
  :defer t
  :config
  ; to be able to jump to the relevant page in the PDF document
  (TeX-source-correlate-mode)
  (when (eq system-type 'darwin)
    (setq TeX-view-program-list '(("skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b" )))
    (setq TeX-view-program-selection '((output-pdf "skim"))))
 ; some facilites

  (defun insert-backslash() 
    (interactive)(insert "\\"))
  (defun insert-dollar() 
    (interactive)
    (insert "$$")
    (backward-char))
  (defun insert-single-dollar() 
    (interactive)
    (insert "$"))
  (defun insert-percent() 
    (interactive)
    (insert "%"))  
  (defun latex-insert-prime() 
    (interactive)(insert "^{\\prime }"))
  (defun latex-append-ampersand()
    (interactive)
    (save-excursion
      (beginning-of-line)
      (search-forward "=")
      (insert "&")))
  (bind-keys* 
   :map LaTeX-mode-map
   ("C-;" . insert-backslash)
   ("M-;" . insert-dollar)
   ("C-:" . insert-percent)
   ("M-:" . insert-single-dollar)
   ("C-c '" . latex-insert-prime)
   ("C-c i" . TeX-complete-symbol)
   ("C-c m" . LaTeX-mark-section)
   ("C-c F" . LaTeX-fill-buffer)
   ("C-c >" . LaTeX-mark-environment-inner)
   ("C-c =" . latex-append-ampersand))

  (defun LaTeX-mark-environment-inner (&optional count)
    "modified based on the auctex function LaTeX-mark-environment.
     mark inner content of the environment"
    (interactive "p")
    (setq count (if count (abs count) 1))
    (let ((cur (point)) beg end)
      ;; Only change point and mark after beginning and end were found.
      ;; Point should not end up in the middle of nowhere if the search fails.
      (save-excursion
	(dotimes (_ count) (LaTeX-find-matching-end))
	(previous-line)
	(end-of-line)
	(setq end (line-beginning-position 2))
	(goto-char cur)
	(dotimes (_ count) (LaTeX-find-matching-begin))
	(next-line)
	(beginning-of-line)
	(setq beg (point)))
      (push-mark end)
      (goto-char beg)
      (TeX-activate-region))))

;;; minor modes
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-math-mode-hook (lambda() 
(setq TeX-electric-sub-and-superscript t) 
(customize-set-variable 'LaTeX-math-abbrev-prefix (kbd "C-<return>"))
  (defun insert-ket()
   (interactive)
   (insert "\\left|\\right\\rangle ") (backward-word 2) (backward-char))
   (defun insert-bra()
   (interactive)
   (insert "\\left\\langle  \\right|")  (backward-word) (backward-char))
   (defun insert-hline()
   (interactive)
   (insert "\\hline "))
(customize-set-variable 'LaTeX-math-list
       '((?h "hbar ")
        (?i "infty ")
	(?u "hat ")
	(?o "omega ")
	(?O "Omega ")
	(?x "times ")
	(?' "prime ")
	("SPC" "quad ")
	(?= "approx ")
	("C-<return>" (lambda() (interactive) (insert " =& ")))
	("C-b" insert-bra "" nil)
	("C-n" "nabla " )
	("<left>" "left")
	("<right>" "right")
	("C-k" insert-ket "" nil)
	("DEL" insert-hline "" nil)))))

(add-hook 'LaTeX-mode-hook (lambda()
 (outline-minor-mode)
 (setq outline-minor-mode-prefix nil)
 (diminish outline-minor-mode)
 ;; easier outline keybindings
 (define-key LaTeX-mode-map (kbd "C-c C-c") 'outline-show-subtree)
 (define-key LaTeX-mode-map (kbd "C-c C-SPC") 'outline-hide-body)
 (define-key LaTeX-mode-map (kbd "S-<tab>") 'outline-hide-body)
 (define-key LaTeX-mode-map (kbd "C-c C-n") 'outline-next-visible-heading)
 (define-key outline-minor-mode-map (kbd "C-c C-p") 'outline-previous-visible-heading) ; override preview-map
 (define-key LaTeX-mode-map (kbd "<up>") 'outline-previous-visible-heading)
 (define-key LaTeX-mode-map (kbd "<down>") 'outline-next-visible-heading)
 (define-key LaTeX-mode-map (kbd "C-<up>") 'outline-up-heading)
 (define-key LaTeX-mode-map (kbd "<right>") 'outline-forward-same-level)
 (define-key LaTeX-mode-map (kbd "<left>") 'outline-backward-same-level)
 (define-key LaTeX-mode-map (kbd "C-c <tab>") 'outline-show-all)
 
 
 (define-key LaTeX-mode-map (kbd "M-<left>") 'outline-promote)
 (define-key LaTeX-mode-map (kbd "M-<right>") 'outline-demote)))

(eval-after-load "LaTeX"  
'(progn
  (setq-local company-backends
	     (append '((company-math-symbols-latex company-latex-commands))
		     company-backends))

 (setq LaTeX-section-label nil)
 (setq TeX-insert-macro-default-style 'mandatory-args-only)

 

 (define-key LaTeX-mode-map (kbd "\"")  (lambda() (interactive) (insert "\"\"") (backward-char))) ; prevent latex quote 

 (define-key LaTeX-mode-map (kbd "C-j") 'newline-and-indent)

 (define-key LaTeX-mode-map (kbd "C-c N") 'TeX-normal-mode)
 (define-key LaTeX-mode-map (kbd "<f5>") 'TeX-font)

 (define-key LaTeX-mode-map (kbd "C-c <return>") 'LaTeX-insert-item)
 (define-key LaTeX-mode-map (kbd "C-c C-j") 'TeX-insert-macro)
 (define-key LaTeX-mode-map (kbd "C-c j") 'TeX-insert-macro)

  (defun insert-bold()
    (interactive) (insert "\\b{}") (backward-char))
 (define-key LaTeX-mode-map (kbd "C-c b")  'insert-bold)
 (define-key LaTeX-mode-map (kbd "C-c C-b") 'insert-bold)

   (defun insert-rm()
    (interactive) (insert "\\mathrm{}") (backward-char))
 (define-key LaTeX-mode-map (kbd "C-c C-r")  'insert-rm)  
 
 ;; setting marks
 (defun LaTeX-mark-inner(delim)
   "mark the content inside a pair of delimiters"
   (search-forward delim)
   (backward-char)
   (push-mark nil t t)
   (search-backward delim nil nil 1)
   (forward-char))
  
 (defun LaTeX-mark-inline-equation()
 "mark the content inside an inline equation"
   (interactive) 
   (LaTeX-mark-inner "$"))


 
 (define-key LaTeX-mode-map (kbd "C-c $") 'LaTeX-mark-inline-equation)


  ;; table editing 
  (defun LaTeX-mark-cell()
 "mark a cell in table, array etc."
   (interactive) 
   (LaTeX-mark-inner "&"))

  (define-key LaTeX-mode-map (kbd "C-c &") 'LaTeX-mark-cell)
 
 (defun LaTeX-next-cell(&optional count)
   "go to next cell or previous cell according to count"
   (interactive "p")
   (setq count (if count count 1))
   (search-forward "&" nil t count)
   (unless (or (equal (if (> count 0) (char-after) (char-after (+ (point) count) ))
		  ?&)
	       (equal (char-after) ?\C-j) )
     (forward-char count))
   (align-current))
 (define-key LaTeX-mode-map (kbd "C-<right>") 'LaTeX-next-cell)
 (define-key LaTeX-mode-map (kbd "C-<left>") (lambda() (interactive) (LaTeX-next-cell -1)))
 
 (defun insert-latex-env(env-name)
   (interactive)
   (insert (concat "\\begin{" env-name "}")) (newline-and-indent) (newline) (insert (concat "\\end{" env-name "}")) (previous-line))
 (defun insert-equation()
   (interactive)
   (insert-latex-env "equation"))
 (define-key LaTeX-mode-map (kbd "C-c e") 'insert-equation)
 
 (defun insert-equation-aligned()
   (interactive)
   (insert-latex-env "equation")
   (insert-latex-env "aligned"))
 (define-key LaTeX-mode-map (kbd "C-c a") 'insert-equation-aligned)

 (defun insert-lstlisting()
   (interactive)
   (insert-latex-env "lstlisting"))
 (define-key LaTeX-mode-map (kbd "C-c l") 'insert-lstlisting)

 (defun insert-definition()
   (interactive)
   (insert-latex-env "definition")
   (previous-line)
   (end-of-line)
   (insert "[]")
   (backward-char))
 (define-key LaTeX-mode-map (kbd "C-c d") 'insert-definition)
 
 (defun insert-frac()
   (interactive)
   (yas-expand-snippet (yas-lookup-snippet "frac")))

 (define-key LaTeX-mode-map (kbd "C-c f") 'insert-frac)
 
  (defun insert-sqrt()
   (interactive)
   (insert "\\sqrt{}") (backward-char))
 (define-key LaTeX-mode-map (kbd "C-c s") 'insert-sqrt)

 (setq TeX-command-force "XeLaTeX")

;;  (defun latex-compile-and-view()
;;     (interactive) (save-some-buffers 1) (TeX-command-run-all nil)
;; )
;; (define-key LaTeX-mode-map (kbd "C-c C-a") 'latex-compile-and-view) ; let C-c C-l be save-and-compile

(setq TeX-save-query nil) ;don't ask for saving, just save it

(defun latex-save-and-compile()
  (interactive)
  (save-some-buffers 1) (TeX-command-master))
(define-key LaTeX-mode-map (kbd "C-x C-s") 'latex-save-and-compile) ; compile tex file every time hit C-x C-s, thus making it up to date.
(define-key LaTeX-mode-map (kbd "C-'") 'latex-save-and-compile) 

(load "~/lib/el/preamble.el" nil t t) ; let auctex parse the auto style file
;(defun LaTeX-label (name &optional type no-insert)) ; make LaTeX-label function do nothing
;(setq TeX-auto-private '("~/lib/auto/" ) )

; some plain text environments or macros
(setq LaTeX-verbatim-environments '("verbatim" "lstlisting" ))


(setq LaTeX-verbatim-macros-with-braces '("input" ))
(setq LaTeX-verbatim-macros-with-delims '("l" ))

; Update PDF buffers after successful LaTeX runs
;(add-hook 'TeX-after-compilation-finished-functions
;          #'TeX-revert-document-buffer)

(setq TeX-auto-save t) ;parse on load
(setq TeX-parse-self t) ;parse on save
;(setq-default TeX-master nil) ; let auctex ask for a master file

;(setq LaTeX-electric-left-right-brace t) ;auto insert braces

;; So that RefTeX finds my bibliography
(setq reftex-default-bibliography '("~/lib/bib/phylab.bib" "~/lib/bib/main.bib" ))))

(setq TeX-engine 'xetex)

(provide 'init-auctex)
