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
	'((sequence "TODO(t)" "NEXT(n)" "WAITING(w@)" "|" "DONE(d)" "CANCELLED(c)")
	  (sequence "TASK(T)")
	  (sequence "MEETING(m)")
	  (sequence "INACTIVE(i)" "SOMEDAY(s)" "|" "CANCELLED(c@/!)")))

  (setq org-tag-alist
	'(("work" . ?w)
	  ("idea" . ?i)
	  ("home" . ?h)
	  ("note" . ?n)
	  ("meeting" . ?m)
	  ("cancelled" . ?c)))

  (setq org-todo-state-tags-triggers
	'(("CANCELLED" ("CANCELLED" . t))
	  ("WAITING" ("SOMEDAY") ("INACTIVE") ("WAITING" . t))
	  ("INACTIVE" ("WAITING") ("SOMEDAY") ("INACTIVE" . t))
	  ("SOMEDAY" ("WAITING") ("INACTIVE") ("SOMEDAY" . t))
	  (done ("WAITING") ("INACTIVE") ("SOMEDAY"))
	  ("TODO" ("WAITING") ("CANCELLED") ("INACTIVE") ("SOMEDAY"))
	  ("TASK" ("WAITING") ("CANCELLED") ("INACTIVE") ("SOMEDAY"))
	  ("NEXT" ("WAITING") ("CANCELLED") ("INACTIVE") ("SOMEDAY"))
	  ("PROJ" ("WAITING") ("CANCELLED") ("INACTIVE") ("SOMEDAY"))
	  ("DONE" ("WAITING") ("CANCELLED") ("INACTIVE") ("SOMEDAY"))))
  
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-restore-windows-after-quit t)
  (setq org-tread-S-cursor-todo-selection-as-state-changes nil)

  ;; (setq org-agenda-custom-commands
  ;; 	'(("k" todo "MEETING")
  ;; 	  )
  ;; 	)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (emacs-lisp . t)
     (python . t)
     (shell . t)
     ))

  (setq org-src-fontify-natively t)
  (setq org-confirm-babel-evaluate nil)

  :bind
  (
   ("C-c a" . org-agenda)
   ("<f12>" . org-agenda-list)
   )
  )

(use-package org-capture
  :after org
  :config
  (setq org-capture-templates
	'(("t" "TODO" entry
	   (file+headline "~/org/tasks.org" "Tasks to be done")
	   "* TODO %U\n :PROPERTIES:\n :CONTEXT: %a\n :END:\n\n %i")
	  ("i" "Idea" entry
	   (file "~/org/ideas.org")
	   "* SOMEDAY %?\n %i")
	  ("s" "Study a topic" entry
	   (file "~/org/learning.org")
	   "* SOMEDAY %U\n :PROPERTIES:\n :CONTEXT: %a\n :END:\n\n %i")
	  ("j" "Journal Entry" entry
	   (file+olp+datetree "~/org/journal.org")
	   "* %<%I:%M %p> - Journal :journal:\n\n%?\n\n" :clock-in :clock-resume)
	  ("m" "Meeting" entry
	   (file+headline "~/org/tasks.org" "Meetings")
	   "* MEETING %U\n SCHEDULED: %t\n :PROPERTIES:\n :CONTEXT: %a\n :END:\n\n %i")
	  ("w" "Watch" entry
	   (file+headline "~/org/tasks.org" "To Watch")
	   "* WATCH %? \n\n")
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
  (add-to-list 'org-structure-template-alist '("py" . "src python :results output, value"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))
		
;; org-bullets for better handling of org level markers
(use-package org-bullets
  :after org-mode
  :hook (org-mode-hook . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package ob-async
  :straight t)
