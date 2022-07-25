;; general applications 

(use-package ledger-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist
	       '("ledger.*\\.dat$" . ledger-mode))
  :bind
  (:map ledger-mode-map
	("C-;" . insert-single-dollar)))

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
	'(
	  "https://terrytao.wordpress.com/feed/"
	  "http://nullprogram.com/feed/"
	  "https://batsov.com/atom.xml" ; bbatsov
	  ;; "https://sachachua.com/blog/feed/"
          ;; "https://planet.emacslife.com/atom.xml"
	  )))

(provide 'init-app)
