;; patch for my-mode, generally minior changes for internal commands

(defun delete-line (&optional arg)
  "equivalence of kill-line without affecting kill-ring"
  (interactive "P")
  (delete-region (point)
	       ;; It is better to move point to the other end of the kill
	       ;; before killing.  That way, in a read-only buffer, point
	       ;; moves across the text that is copied to the kill ring.
	       ;; The choice has no effect on undo now that undo records
	       ;; the value of point from before the command was run.
	       (progn
		 (if arg
		     (forward-visible-line (prefix-numeric-value arg))
		   (if (eobp)
		       (signal 'end-of-buffer nil))
		   (let ((end
			  (save-excursion
			    (end-of-visible-line) (point))))
		     (if (or (save-excursion
			       ;; If trailing whitespace is visible,
			       ;; don't treat it as nothing.
			       (unless show-trailing-whitespace
				 (skip-chars-forward " \t" end))
			       (= (point) end))
			     (and kill-whole-line (bolp)))
			 (forward-visible-line 1)
		       (goto-char end))))
		 (point))))

(defun my-copy-line (&optional arg)
  "equivalence of kill-line without deleting the text"
  (interactive "P")
  (kill-ring-save (point)
	       ;; It is better to move point to the other end of the kill
	       ;; before killing.  That way, in a read-only buffer, point
	       ;; moves across the text that is copied to the kill ring.
	       ;; The choice has no effect on undo now that undo records
	       ;; the value of point from before the command was run.
	       (progn
		 (if arg
		     (forward-visible-line (prefix-numeric-value arg))
		   (if (eobp)
		       (signal 'end-of-buffer nil))
		   (let ((end
			  (save-excursion
			    (end-of-visible-line) (point))))
		     (if (or (save-excursion
			       ;; If trailing whitespace is visible,
			       ;; don't treat it as nothing.
			       (unless show-trailing-whitespace
				 (skip-chars-forward " \t" end))
			       (= (point) end))
			     (and kill-whole-line (bolp)))
			 (forward-visible-line 1)
		       (goto-char end))))
		 (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun my-beginning-of-line(&optional arg)
  "save mark when using beginning-of-line"
  (interactive "^p")
  (or (consp arg) (region-active-p) (push-mark))
  (cl-case major-mode
      ('org-mode (org-beginning-of-line arg))
      ('eshell-mode (eshell-bol))
      (t (beginning-of-line arg))
      )
  )
(defun my-end-of-line(&optional arg)
  "save mark when using end-of-line"
  (interactive "^p")
  (or (consp arg) (region-active-p) (push-mark))
  (cl-case major-mode
      ('org-mode (org-end-of-line arg))
      (t (end-of-line arg))
      )
  )

(setq my-paragraph-start (default-value 'paragraph-start))
(setq my-paragraph-separate (default-value 'paragraph-separate))

(defun my-latex-set-paragraph()
(make-local-variable 'my-paragraph-start)
(make-local-variable 'my-paragraph-separate)  
(setq my-paragraph-start
      (concat
       "\\(?:[ \t]*$"
       "\\|" 
       "[ \t]*"
       "\\(?:"
       "part\\|chapter\\|"       
       "section\\|subsection\\|subsubsection\\|"
       "paragraph\\|include\\|includeonly\\|"
       "tableofcontents\\|appendix"
       "\\)"
       "\\|"
       "[ \t]*\\$\\$"         ; display math delimitor
       "\\)" ))

(setq my-paragraph-separate
      (concat
       "[ \t]*"
       "\\(?:"
        "par\\|"
       "\\(?:"
       "part\\|chapter\\|"       
       "section\\|subsection\\|subsubsection\\|"
       "paragraph\\|include\\|includeonly\\|"
       "tableofcontents\\|appendix" 
       "\\)"
       "\\)")))
(add-hook 'LaTeX-mode-hook 'my-latex-set-paragraph)

(defun my-f90-set-paragraph()
(make-local-variable 'my-paragraph-start)
(make-local-variable 'my-paragraph-separate)  
(setq my-paragraph-start
      (concat
       "[ \t]*$"       
       "\\|^[ \t]*!+[ \t]*$" ; line with only !
       "\\|^[ \t]*end subroutine"       
       ))
(setq my-paragraph-separate
      (concat
       "[ \t]*$"       
       "\\|^[ \t]*!+[ \t]*$" ; line with only !
       "\\|^[ \t]*end subroutine"
       ))
)
(add-hook 'f90-mode-hook 'my-f90-set-paragraph)

(defun my-forward-paragraph (&optional arg)
  "Move forward to end of paragraph.
With argument ARG, do it ARG times;
a negative argument ARG = -N means move backward N paragraphs.

A line which `my-paragraph-start' matches either separates paragraphs
\(if `my-paragraph-separate' matches it also) or is the first line of a paragraph.
A paragraph end is the beginning of a line which is not part of the paragraph
to which the end of the previous line belongs, or the end of the buffer.
Returns the count of paragraphs left to move."
  (interactive "^p")
  (or arg (setq arg 1))
  (let* ((opoint (point))
	 (fill-prefix-regexp
	  (and fill-prefix (not (equal fill-prefix ""))
	       (not paragraph-ignore-fill-prefix)
	       (regexp-quote fill-prefix)))
	 ;; Remove ^ from my-paragraph-start and paragraph-sep if they are there.
	 ;; These regexps shouldn't be anchored, because we look for them
	 ;; starting at the left-margin.  This allows paragraph commands to
	 ;; work normally with indented text.
	 ;; This hack will not find problem cases like "whatever\\|^something".
	 (parstart (if (and (not (equal "" my-paragraph-start))
			    (equal ?^ (aref my-paragraph-start 0)))
		       (substring my-paragraph-start 1)
		     my-paragraph-start))
	 (parsep (if (and (not (equal "" my-paragraph-separate))
			  (equal ?^ (aref my-paragraph-separate 0)))
		     (substring my-paragraph-separate 1)
		   my-paragraph-separate))
	 (parsep
	  (if fill-prefix-regexp
	      (concat parsep "\\|"
		      fill-prefix-regexp "[ \t]*$")
	    parsep))
	 ;; This is used for searching.
	 (sp-parstart (concat "^[ \t]*\\(?:" parstart "\\|" parsep "\\)"))
	 start found-start)
    (while (and (< arg 0) (not (bobp)))
      (if (and (not (looking-at parsep))
	       (re-search-backward "^\n" (max (1- (point)) (point-min)) t)
	       (looking-at parsep))
	  (setq arg (1+ arg))
	(setq start (point))
	;; Move back over paragraph-separating lines.
	(forward-char -1) (beginning-of-line)
	(while (and (not (bobp))
		    (progn (move-to-left-margin)
			   (looking-at parsep)))
	  (forward-line -1))
	(if (bobp)
	    nil
	  (setq arg (1+ arg))
	  ;; Go to end of the previous (non-separating) line.
	  (end-of-line)
	  ;; Search back for line that starts or separates paragraphs.
	  (if (if fill-prefix-regexp
		  ;; There is a fill prefix; it overrides parstart.
		  (let (multiple-lines)
		    (while (and (progn (beginning-of-line) (not (bobp)))
				(progn (move-to-left-margin)
				       (not (looking-at parsep)))
				(looking-at fill-prefix-regexp))
		      (unless (= (point) start)
			(setq multiple-lines t))
		      (forward-line -1))
		    (move-to-left-margin)
		    ;; This deleted code caused a long hanging-indent line
		    ;; not to be filled together with the following lines.
		    ;; ;; Don't move back over a line before the paragraph
		    ;; ;; which doesn't start with fill-prefix
		    ;; ;; unless that is the only line we've moved over.
		    ;; (and (not (looking-at fill-prefix-regexp))
		    ;;      multiple-lines
		    ;;      (forward-line 1))
		    (not (bobp)))
		(while (and (re-search-backward sp-parstart nil 1)
			    (setq found-start t)
			    ;; Found a candidate, but need to check if it is a
			    ;; REAL parstart.
			    (progn (setq start (point))
				   (move-to-left-margin)
				   (not (looking-at parsep)))
			    (not (and (looking-at parstart)
				      (or (not use-hard-newlines)
					  (bobp)
					  (get-text-property
					   (1- start) 'hard)))))
		  (setq found-start nil)
		  (goto-char start))
		found-start)
	      ;; Found one.
	      (progn
		;; Move forward over paragraph separators.
		;; We know this cannot reach the place we started
		;; because we know we moved back over a non-separator.
		(while (and (not (eobp))
			    (progn (move-to-left-margin)
				   (looking-at parsep)))
		  (forward-line 1))
		;; If line before paragraph is just margin, back up to there.
		(end-of-line 0)
		(if (> (current-column) (current-left-margin))
		    (forward-char 1)
		  (skip-chars-backward " \t")
		  (if (not (bolp))
		      (forward-line 1))))
	    ;; No starter or separator line => use buffer beg.
	    (goto-char (point-min))))))

    (while (and (> arg 0) (not (eobp)))
      ;; Move forward over separator lines...
      (while (and (not (eobp))
		  (progn (move-to-left-margin) (not (eobp)))
		  (looking-at parsep))
	(forward-line 1))
      (unless (eobp) (setq arg (1- arg)))
      ;; ... and one more line.
      (forward-line 1)
      (if fill-prefix-regexp
	  ;; There is a fill prefix; it overrides parstart.
	  (while (and (not (eobp))
		      (progn (move-to-left-margin) (not (eobp)))
		      (not (looking-at parsep))
		      (looking-at fill-prefix-regexp))
	    (forward-line 1))
	(while (and (re-search-forward sp-parstart nil 1)
		    (progn (setq start (match-beginning 0))
			   (goto-char start)
			   (not (eobp)))
		    (progn (move-to-left-margin)
			   (not (looking-at parsep)))
		    (or (not (looking-at parstart))
			(and use-hard-newlines
			     (not (get-text-property (1- start) 'hard)))))
	  (forward-char 1))
	(if (< (point) (point-max))
	    (goto-char start))))
    (constrain-to-field nil opoint t)
    ;; Return the number of steps that could not be done.
    arg))

(defun my-backward-paragraph (&optional arg)
  "Move backward to start of paragraph.
With argument ARG, do it ARG times;
a negative argument ARG = -N means move forward N paragraphs.

A paragraph start is the beginning of a line which is a
`my-paragraph-start' or which is ordinary text and follows a
`my-paragraph-separate'ing line; except: if the first real line of a
paragraph is preceded by a blank line, the paragraph starts at that
blank line.

See `my-forward-paragraph' for more information."
  (interactive "^p")
  (or arg (setq arg 1))
  (my-forward-paragraph (- arg)))

(defun my-mark-paragraph (&optional arg allow-extend)
  " A slight modification of mark-paragraph to use my-forward/backward-paragraph
instead of forward/backward-paragraph.

Put point at beginning of this paragraph, mark at end.
The paragraph marked is the one that contains point or follows point.

With argument ARG, puts mark at end of a following paragraph, so that
the number of paragraphs marked equals ARG.

If ARG is negative, point is put at end of this paragraph, mark is put
at beginning of this or a previous paragraph.

Interactively (or if ALLOW-EXTEND is non-nil), if this command is
repeated or (in Transient Mark mode) if the mark is active,
it marks the next ARG paragraphs after the ones already marked."
  (interactive "p\np")
  (unless arg (setq arg 1))
  (when (zerop arg)
    (error "Cannot mark zero paragraphs"))
  (cond ((and allow-extend
	      (or (and (eq last-command this-command) (mark t))
		  (and transient-mark-mode mark-active)))
	 (set-mark
	  (save-excursion
	    (goto-char (mark))
	    (forward-paragraph arg)
	    (point))))
	(t
	 (my-forward-paragraph arg)
	 (push-mark nil t t)
	 (my-backward-paragraph arg))))

(provide 'init-my)
