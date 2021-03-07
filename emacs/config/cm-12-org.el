;; Org
;; Org mode setup
(defun cm/org-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org
  :straight t
  :hook
  (org-mode-hook . cm/org-setup)
  :config
  (setq org-directory "~/org")
  (setq org-default-notes-file "~/org/notes.org")
  (setq org-agenda-files
	'("~/org"
	  "~/.emacs.d"
	  "~/Documents"))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-deadline-warning-days 14)
  
  (setq org-refile-targets
	'((org-agenda-files . (:maxlevel . 9))
	  (nil . (:maxlevel . 9))))
  (setq org-refile-use-outline-path t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-reverse-note-order nil)

  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "CANCELLED(c)")))

  (setq org-tag-alist
	'(("work" . ?w)
	  ("idea" . ?i)
	  ("home" . ?h)
	  ("note" . ?n)
	  ("cancelled" . ?c)))

  (setq org-agenda-window-setup 'current-window)

  :bind (("C-c a" . org-agenda))
  )

(use-package org-capture
  :after org
  :config
  (setq org-capture-templates
	'(("t" "Task" entry
	   (file+headline "~/org/tasks.org" "Tasks to be done")
	   "* TODO %U\n :PROPERTIES:\n :CONTEXT: %a\n :END:\n\n %i")
	  ("i" "Idea" entry
	   (file "~/org/ideas.org")
	   "* IDEA %?\n %i")
	  ("s" "Study a topic" entry
	   (file "~/org/learning.org")
	   "* STUDY %U\n :PROPERTIES:\n :CONTEXT: %a\n :END:\n\n %i")
	  ("j" "Journal Entry" entry
	   (file+olp+datetree "~/org/journal.org")
	   "* %<%I:%M %p> - Journal :journal:\n\n%?\n\n" :clock-in :clock-resume)
	  ("r" "Reply to an email" entry
	   (file+headline "tasks.org" "Mail correspondence")
	   "* TODO %:subject\n SCHEDULED: %t\n :PROPERTIES:\n :CONTEXT: %a\n :END:\n\n %i %?")))
  (setq org-capture-templates-contexts
	'(("r" ((in-mode . "mu4e-headers-mode")
		(in-mode . "mu4e:view-mode")))))
  :bind (("C-c c" . org-capture))
  )

(dolist (mode '(org-mode-hook
		eshell-mode-hook
		Info-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(with-eval-after-load 'org
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))
		
;; org-bullets for better handling of org level markers
(use-package org-bullets
  :after org-mode
  :hook (org-mode-hook . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
