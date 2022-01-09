(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)

(use-package latex
  :ensure auctex
  :defer t
  :config
  ; to be able to jump to the relevant page in the PDF document
  (TeX-source-correlate-mode)
  (when (eq system-type 'darwin)
    (setq TeX-view-program-list '(("skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b" )))
    (setq TeX-view-program-selection '((output-pdf "skim")))
    )
 ; some facilites
  (defun insert-backslash() 
    (interactive)(insert "\\")
    )
  (defun latex-insert-prime() 
    (interactive)(insert "^{\\prime }")
    )
  (bind-keys* 
   :map LaTeX-mode-map
   ("C-;" . insert-backslash)
   ("C-c '" . latex-insert-prime)
   ("C-c F" . LaTeX-fill-buffer)
   )

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
      (TeX-activate-region)))
  
  (define-key LaTeX-mode-map (kbd "C-c >") 'LaTeX-mark-environment-inner)
  
  )

;;; minor modes
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-math-mode-hook (lambda() 
(setq TeX-electric-sub-and-superscript t) 
(customize-set-variable 'LaTeX-math-abbrev-prefix (kbd "C-<return>"))
  (defun insert-ket()
   (interactive)
   (insert "\\left|\\right\\rangle ") (backward-word 2) (backward-char)
)
   (defun insert-bra()
   (interactive)
   (insert "\\left\\langle  \\right|")  (backward-word) (backward-char) 
)
   (defun insert-hline()
   (interactive)
   (insert "\\hline ")
)
(customize-set-variable 'LaTeX-math-list
       '((?h "hbar ")
        (?i "infty ")
	(?u "hat ")
	(?' "prime ")
	(?| "bigg|")
	("SPC" "quad ")
	("C-SPC" (lambda() (interactive) (insert " &= ")))
	("C-b" insert-bra "" nil)
	("C-k" insert-ket "" nil)
	("DEL" insert-hline "" nil)
	))
))

(eval-after-load "LaTeX"  
'(progn
  (setq-local company-backends
	     (append '((company-math-symbols-latex company-latex-commands))
		     company-backends))
 (setq LaTeX-section-label nil)
 (setq TeX-insert-macro-default-style 'mandatory-args-only)

 

 (define-key LaTeX-mode-map (kbd "\"")  (lambda() (interactive) (insert "\"\"") (backward-char))) ; prevent latex quote 

 (outline-minor-mode)
 (setq outline-minor-mode-prefix nil)
 (diminish outline-minor-mode)

 (define-key LaTeX-mode-map (kbd "C-j") 'newline-and-indent)
 
 ;; easier outline keybindings
 (define-key LaTeX-mode-map (kbd "C-c C-c") 'outline-show-subtree)
 (define-key LaTeX-mode-map (kbd "C-c C-SPC") 'outline-hide-body)
 (define-key LaTeX-mode-map (kbd "C-c C-n") 'outline-next-visible-heading)
 (define-key LaTeX-mode-map (kbd "C-c C-p") 'outline-previous-visible-heading)
 (define-key LaTeX-mode-map (kbd "C-c C-u") 'outline-up-heading)
 (define-key LaTeX-mode-map (kbd "C-c C-f") 'outline-forward-same-level)
 (define-key LaTeX-mode-map (kbd "C-c C-b") 'outline-backward-same-level)
 (define-key LaTeX-mode-map (kbd "C-c <tab>") 'outline-show-all)
 
 
 (define-key LaTeX-mode-map (kbd "M-<left>") 'outline-promote)
 (define-key LaTeX-mode-map (kbd "M-<right>") 'outline-demote)

 (define-key LaTeX-mode-map (kbd "C-c N") 'TeX-normal-mode)
 (define-key LaTeX-mode-map (kbd "<f5>") 'TeX-font)

 (define-key LaTeX-mode-map (kbd "C-c <return>") 'LaTeX-insert-item)
 (define-key LaTeX-mode-map (kbd "C-c C-j") 'TeX-insert-macro)

  (defun insert-bold()
    (interactive) (insert "\\b{}") (backward-char)
   )
 (define-key LaTeX-mode-map (kbd "C-c b")  'insert-bold)

   (defun insert-rm()
    (interactive) (insert "\\r{}") (backward-char)
   )
 (define-key LaTeX-mode-map (kbd "C-c C-r")  'insert-rm)  
 
 ;; setting marks
 (defun LaTeX-mark-inner(delim)
   "mark the content inside a pair of delimiters"
   (search-forward delim)
   (backward-char)
   (push-mark nil t t)
   (search-backward delim nil nil 1)
   (forward-char)   
   )
  
 (defun LaTeX-mark-inline-equation()
 "mark the content inside an inline equation"
   (interactive) 
   (LaTeX-mark-inner "$")
   )

 (defun LaTeX-mark-cell()
 "mark a cell in table, array etc."
   (interactive) 
   (LaTeX-mark-inner "&")
   )
 
 (define-key LaTeX-mode-map (kbd "C-c $") 'LaTeX-mark-inline-equation)
 (define-key LaTeX-mode-map (kbd "C-c &") 'LaTeX-mark-cell)
 (define-key LaTeX-mode-map (kbd "C-c @") 'LaTeX-mark-section)
 
 (defun insert-latex-env(env-name)
   (interactive)
   (insert (concat "\\begin{" env-name "}")) (newline-and-indent) (newline) (insert (concat "\\end{" env-name "}")) (previous-line)
   )
 (defun insert-equation()
   (interactive)
   (insert-latex-env "equation")
)
 (define-key LaTeX-mode-map (kbd "C-c e") 'insert-equation)
 
 (defun insert-equation-aligned()
   (interactive)
   (insert-latex-env "equation")
   (insert-latex-env "aligned")
)
 (define-key LaTeX-mode-map (kbd "C-c a") 'insert-equation-aligned)

 (defun insert-lstlisting()
   (interactive)
   (insert-latex-env "lstlisting")
   )
 (define-key LaTeX-mode-map (kbd "C-c l") 'insert-lstlisting)

 (defun insert-definition()
   (interactive)
   (insert-latex-env "definition")
   (previous-line)
   (end-of-line)
   (insert "[]")
   (backward-char)
   )
 (define-key LaTeX-mode-map (kbd "C-c d") 'insert-definition)
 
 (defun insert-frac()
   (interactive)
   (yas-expand-snippet (yas-lookup-snippet "frac"))
)

 (define-key LaTeX-mode-map (kbd "C-c f") 'insert-frac)
 
  (defun insert-sqrt()
   (interactive)
   (insert "\\sqrt{}") (backward-char)
)
 (define-key LaTeX-mode-map (kbd "C-c s") 'insert-sqrt)
 
 (defun insert-preamble()
   (interactive) (insert "\\documentclass[12pt,a4paper,openany]{book}") (newline-and-indent) (insert "\\input{$HOME/lib/preamble}") 
)
 (define-key LaTeX-mode-map (kbd "C-c jd")  'insert-preamble)
 (defun insert-font()
    (interactive) (insert "\\fontsize{pt}{\\baselineskip}\\selectfont ") (backward-word 3) 
)
(define-key LaTeX-mode-map (kbd "C-c jf") 'insert-font) 
(defun insert-include-graph()
 (interactive) (insert "\\includegraphics[width=1\\textwidth]{}") (backward-char)   
)
(define-key LaTeX-mode-map (kbd "C-c ji") 'insert-include-graph)
(define-key LaTeX-mode-map (kbd "C-c ja") (lambda() (interactive) (insert "\\langle \\rangle") (backward-char 7)))
 (setq TeX-command-force "XeLaTeX")
 (defun latex-compile-and-view()
    (interactive) (save-some-buffers 1) (TeX-command-run-all nil)
)
(define-key LaTeX-mode-map (kbd "C-c C-a") 'latex-compile-and-view) ; let C-c C-l be save-and-compile
(defun latex-save-and-compile()
  (interactive)
  (save-some-buffers 1) (TeX-command-master nil)
  )
(define-key LaTeX-mode-map (kbd "C-x C-s") 'latex-save-and-compile) ; compile tex file every time hit C-x C-s, thus making it up to date.
(define-key LaTeX-mode-map (kbd "C-'") 'latex-save-and-compile) 

(load "~/lib/el/preamble.el" nil t t) ; let auctex parse the auto style file
;(defun LaTeX-label (name &optional type no-insert)) ; make LaTeX-label function do nothing
;(setq TeX-auto-private '("~/lib/auto/" ) )

; some plain text environments or macros
(setq LaTeX-verbatim-environments-local '("Verbatim" "lstlisting" ))

(setq LaTeX-verbatim-macros-with-braces '("input" ))
(setq LaTeX-verbatim-macros-with-delims '("l" ))
; Use pdf-tools to open PDF files
;(setq TeX-view-program-selection '((output-pdf "Evince")))
;      TeX-source-correlate-start-server t)


;restore default paragraph definitions
(defun use-default-paragraph-delimiters ()
  (setq paragraph-start (default-value 'paragraph-start)
        paragraph-separate (default-value 'paragraph-separate)))

(add-hook 'LaTeX-mode-hook 'use-default-paragraph-delimiters)

; Update PDF buffers after successful LaTeX runs
;(add-hook 'TeX-after-compilation-finished-functions
;          #'TeX-revert-document-buffer)

(setq TeX-auto-save t) ;parse on load
(setq TeX-parse-self t) ;parse on save
;(setq-default TeX-master nil) ; let auctex ask for a master file

;(setq LaTeX-electric-left-right-brace t) ;auto insert braces
;(setq TeX-electric-math (cons "$" "$")) ;automatically insert a pair of dollars
;; So that RefTeX finds my bibliography
(setq reftex-default-bibliography '("~/lib/bib/phylab.bib" "~/lib/bib/main.bib" ))
))

(setq TeX-engine 'xetex)
;; fonts
;(setq font-latex-fontify-sectioning 'color)

(provide 'init-auctex)
