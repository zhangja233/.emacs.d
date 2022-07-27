(add-hook 'LaTeX-mode-hook '(lambda()
			      (flyspell-mode)
			      (reftex-mode)
			      ;(modify-syntax-entry (string-to-char TeX-esc) "w"
			      ;LaTeX-mode-syntax-table) ; make \ part of a word
			      (modify-syntax-entry ?_ "w"
			      tex-mode-syntax-table)
			      (modify-syntax-entry ?_ "w"
			      LaTeX-mode-syntax-table)			      
			      (modify-syntax-entry ?^ "w"
			      tex-mode-syntax-table)
			      (modify-syntax-entry ?^ "w"
			      LaTeX-mode-syntax-table) 			      
			      ))
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)

(add-hook 'TeX-mode-hook '(lambda () (bind-keys :map TeX-mode-map
						("C-;" . insert-backslash))))

(use-package latex
  :ensure auctex
  :bind (:map LaTeX-mode-map
	      ("C-;" . insert-backslash)
	      ("M-;" . insert-dollar)
	      ("C-:" . insert-percent)
	      ("`" . latex-insert-inline-src)
	      ("M-:" . insert-single-dollar)
	      ("M-2" . latex-insert-sub-2)
	      ("C-c '" . latex-insert-prime)
	      ("C-c i" . TeX-complete-symbol)
	      ("C-c m" . LaTeX-mark-section)
	      ("C-c w" . latex-kill-section)
	      ("C-c C-w" . latex-kill-environment)
	      ("C-c g" . counsel-imenu)
	      ("C-c A" . latex-convert-equation-to-aligned)
	      ("C-c B" . latex-bold-region)
	      ("C-c F" . LaTeX-fill-buffer)
	      ("C-c C-." . LaTeX-mark-environment-inner)
	      ("C-c c" . LaTeX-copy-environment)   
	      ("C-c C-=" . latex-append-ampersand)
	      ("C-c l" . latex-wrap-left-right))
  :config
  (TeX-source-correlate-mode) ; to be able to jump to the relevant page in the PDF document
  (when (eq system-type 'darwin)
    (setq TeX-view-program-list '(("skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b" )))
    (setq TeX-view-program-selection '((output-pdf "skim"))))
  )


  (defun latex-insert-prime() 
    (interactive)(insert "^{\\prime }"))
  
  (defun latex-insert-sub-2()
    (interactive)
    (insert "$_2$ "))

  
  (defun latex-kill-section()
    (interactive)
    (LaTeX-mark-section)
    (kill-region (mark) (point) 'region))
  
  (defun latex-append-ampersand()
    (interactive)
    (save-excursion
      (beginning-of-line)
      (search-forward "=")
      (insert "&")))

  (defun latex-convert-equation-to-aligned()
    (interactive)
    (LaTeX-mark-environment-inner)
    (kill-region (mark) (point) 'region)
    (open-line-above)
    (insert-latex-env "aligned")
    (yank)
    (delete-char -1))
  
  (defun latex-bold-region()
    (interactive)
    (if (region-active-p)
	(kill-region (mark) (point) 'region)
      (delete-char 1 t))
    (insert "\\b{")
    (yank)
    (insert "}"))
  
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

(defun LaTeX-copy-environment()
  (interactive)
  (save-excursion (LaTeX-mark-environment)
		  (kill-ring-save (mark) (point) 'region))
  (message "environment copied"))

(defun latex-kill-environment()
  (interactive)
  (save-excursion (LaTeX-mark-environment)
		  (kill-region (mark) (point) 'region))
  (message "environment killed"))

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
 (define-key LaTeX-mode-map (kbd "C-c C-SPC") 'outline-hide-body)
 (define-key LaTeX-mode-map (kbd "S-<tab>") 'outline-hide-body)
 (define-key LaTeX-mode-map (kbd "<backtab>") 'outline-hide-body)
;; (define-key LaTeX-mode-map (kbd "C-c C-n") 'outline-next-visible-heading)
;; (define-key outline-minor-mode-map (kbd "C-c C-p") 'outline-previous-visible-heading) ; override preview-map
; (define-key LaTeX-mode-map (kbd "<up>") 'outline-previous-visible-heading)
; (define-key LaTeX-mode-map (kbd "<down>") 'outline-next-visible-heading)
;; (define-key LaTeX-mode-map (kbd "C-c C-u") 'outline-up-heading)
;; (define-key LaTeX-mode-map (kbd "C-c C-f") 'outline-forward-same-level)
;; (define-key LaTeX-mode-map (kbd "C-c M-b") 'outline-backward-same-level)
 (define-key LaTeX-mode-map (kbd "C-c <tab>") 'outline-show-all)
 (define-key LaTeX-mode-map (kbd "C-c C-f") 'TeX-font) 
 
 
 (define-key LaTeX-mode-map (kbd "M-<left>") 'outline-promote)
 (define-key LaTeX-mode-map (kbd "M-<right>") 'outline-demote)))

(add-hook 'outline-minor-mode-hook
	  (lambda () (local-set-key "\M-u"
				    outline-mode-prefix-map)))

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
     (define-key LaTeX-mode-map (kbd "C-c $") 'LaTeX-mark-inline-equation)


     (defun insert-bold()
       (interactive) (insert "\\b{}") (backward-char))
     (define-key LaTeX-mode-map (kbd "C-c b")  'insert-bold)
     (define-key LaTeX-mode-map (kbd "C-c C-b") 'insert-bold)

     (defun insert-rm()
       (interactive) (insert "\\mathrm{}") (backward-char))
     (define-key LaTeX-mode-map (kbd "C-c C-r")  'insert-rm)  

     (defun latex-wrap-left-right(arg)
       "given a delimeter, wrap the sexp with \\left and \\right. E.g., [x,p] becomes \\left[x,p\\right]"
       (interactive (list
		     (char-to-string (read-char "wrap the delimeter:"))))
       (search-backward arg)
       (if (equal arg "{")
	   (backward-char))
       (insert "\\left")   
       (let ((conjugate (cond ((equal arg "(") ")")
			      ((equal arg "[") "]")
			      ((equal arg "{") "\\}"))))
	 (search-forward conjugate)
	 (if (equal arg "{")
	     (backward-char 2)
	   (backward-char))
	 (insert "\\right")))
 
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
 
     (defun insert-latex-env (env-name)
       (interactive)
       (insert (concat "\\begin{" env-name "}"))
       (newline-and-indent)
       (newline)
       (insert (concat "\\end{" env-name "}"))
       (previous-line)
       (indent-for-tab-command))
 
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
					; (define-key LaTeX-mode-map (kbd "C-c l") 'insert-lstlisting)

     (defun latex-insert-inline-src()
       (interactive)
       (insert "\\lstinline;;")
       (backward-char))
 
     (defun insert-frac()
       (interactive)
       (yas-expand-snippet (yas-lookup-snippet "frac")))

     (define-key LaTeX-mode-map (kbd "C-c f") 'insert-frac)
 
     (defun insert-sqrt()
       (interactive)
       (insert "\\sqrt{}") (backward-char))
     (define-key LaTeX-mode-map (kbd "C-c s") 'insert-sqrt)

     (setq TeX-save-query nil)		;don't ask for saving, just save it

     (defun latex-save-and-compile()
       (interactive)
       (save-some-buffers 1)
       (let ((TeX-command-force "LaTeX"))
	 (TeX-command-master nil)))

     (define-key LaTeX-mode-map (kbd "C-x C-s") 'latex-save-and-compile) ; compile tex file every time hit C-x C-s, thus making it up to date.

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

     (setq TeX-auto-save t)		;parse on load
     (setq TeX-parse-self t)		;parse on save
					;(setq-default TeX-master nil) ; let auctex ask for a master file

     ;; So that RefTeX finds my bibliography
     (setq reftex-plug-into-AUCTeX t)
     (setq reftex-default-bibliography '("~/lib/bib/zhangja.bib" "~/lib/bib/url.bib"
					 "~/Dropbox/research/literature/main.bib")))) ; be aware, eval-after-load ends here

(setq TeX-engine 'xetex)
;(setq TeX-engine 'default)

(provide 'init-auctex)
