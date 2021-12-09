(add-hook 'LaTeX-mode-hook 'flyspell-mode)

(use-package latex
  :ensure auctex
  :defer t
  :config
 ; some facilites
  (defun insert-backslash() 
    (interactive)(insert "\\")
    )
 ; (define-key LaTeX-mode-map (kbd "C-;")  'insert-backslash)
  (bind-keys* 
   :map LaTeX-mode-map
   ("C-;" . insert-backslash)
   )
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
	("C-b" insert-bra "" nil)
	("C-k" insert-ket "" nil)
	("DEL" insert-hline "" nil)
	))
))

(add-hook 'LaTeX-mode-hook #'outline-minor-mode)
(add-hook 'outline-minor-mode-hook
	  (lambda () (local-set-key "\C-c\C-c"
				    outline-mode-prefix-map)))
;(add-hook 'LaTeX-mode-hook 'outline-hide-body)
(eval-after-load "LaTeX"  
'(progn
 (outline-minor-mode)
  (setq-local company-backends
	     (append '((company-math-symbols-latex company-latex-commands))
		     company-backends))
 (setq LaTeX-section-label nil)
; (setq TeX-quote-after-quote t)
 (setq TeX-insert-macro-default-style 'mandatory-args-only)
 ; several key bindings

 (define-key LaTeX-mode-map (kbd "\"")  (lambda() (interactive) (insert "\"\"") (backward-char))) ; prevent latex quote 
; (define-key LaTeX-mode-map (kbd "C-'")  (lambda()(interactive)(insert "{}") (backward-char))) 
;  (define-key LaTeX-mode-map (kbd "C-<return>")  (lambda()(interactive)(insert "}")))
 (define-key LaTeX-mode-map (kbd "C-c <tab>") 'outline-show-subtree)
 (define-key LaTeX-mode-map (kbd "C-c SPC") 'outline-hide-body) 
 (defun insert-begin()
    (interactive) (insert "\\begin{}") (backward-char)
   )
 (define-key LaTeX-mode-map (kbd "C-c b")  'insert-begin) ; entering \begin faster

  (defun insert-bold()
    (interactive) (insert "\\b{}") (backward-char)
   )
 (define-key LaTeX-mode-map (kbd "C-c C-b")  'insert-bold)

   (defun insert-rm()
    (interactive) (insert "\\r{}") (backward-char)
   )
 (define-key LaTeX-mode-map (kbd "C-c C-r")  'insert-rm)  
 
 (defun mark-inline-equation()
 "mark the content inside an inline equation"
   (interactive) 
   (search-forward "$")
   (backward-char)
   (push-mark nil t t)
   (search-backward "$" nil nil 1)
   (forward-char)
   )
 (define-key LaTeX-mode-map (kbd "C-c $") 'mark-inline-equation)
 (define-key LaTeX-mode-map (kbd "S-<tab>") 'outline-show-subtree)
; (define-key LaTeX-mode-map (kbd "C-c h") 'outline-hide-body)
; (define-key LaTeX-mode-map (kbd "C-c s") 'outline-show-entry)
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
   (insert "\\frac{}{}") (backward-char 3)
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
(load "~/lib/el/preamble.el" nil t t) ; let auctex parse the auto style file
;(defun LaTeX-label (name &optional type no-insert)) ; make LaTeX-label function do nothing
;(setq TeX-auto-private '("~/lib/auto/" ) )





;(setq latex-run-command "xelatex")


; some plain text environments or macros
(setq LaTeX-verbatim-environments-local '("Verbatim" "lstlisting" ))

(setq LaTeX-verbatim-macros-with-braces '("input" ))
(setq LaTeX-verbatim-macros-with-delims '("l" ))
; Use pdf-tools to open PDF files
;(setq TeX-view-program-selection '((output-pdf "Evince")))
;      TeX-source-correlate-start-server t)


;restore default paragraph definitions
;(setq paragraph-start "\\|[ 	]*$")
;(setq paragraph-separate "[ 	]*$")

;(add-hook 'latex-mode-hook (lambda ()(setq paragraph-start "\\|[ 	]*$" paragraph-separate  "[ 	]*$")))

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
;; 
