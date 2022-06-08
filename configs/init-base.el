;;; collection of basic functions, e.g., self-inserting

;;; inserting 
(defun insert-number-sign()
  (interactive)
  (insert "#"))

(defun insert-backslash()
  (interactive)
  (insert "\\"))

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

(defun insert-exclamation()
  (interactive)
  (insert "!"))

(provide 'init-base)
