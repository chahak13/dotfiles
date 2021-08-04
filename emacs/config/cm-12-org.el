;; Org
;; Org mode setup
(defun cm/org-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(use-package ob-async
  :straight t
  :config
  (setq ob-async-no-async-languages-alist '("jupyter-python"))
  )

(use-package jupyter
  :straight t)

(use-package org
  :straight t
  :hook
  (org-mode-hook . cm/org-setup)
  (org-babel-after-execute-hook . org-redisplay-inline-images)

  :init
  (setq org-directory "~/.org")
  (setq cm/org-agenda-directory (concat org-directory "/gtd"))
  (make-directory cm/org-agenda-directory t)

  :config
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-deadline-warning-days 14)
  
  (setq org-refile-use-outline-path t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-reverse-note-order nil)

  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-fast-tag-selection-single-key nil)

  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "CANCELLED(c)")
	  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

  (setq org-tag-alist
	'(("@work" . ?w)
	  ("@home" . ?h)
	  (:newline)
	  ("CANCELLED" . ?c)))

  (setq org-tread-S-cursor-todo-selection-as-state-changes nil)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t)
     (jupyter . t)))

  (setq org-src-fontify-natively t)
  (setq org-src-window-setup 'split-window-below)
  (setq org-confirm-babel-evaluate nil)
  (setq org-edit-src-content-indentation 0)

  (setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")
  )

(use-package org-agenda
  :after org
  :config
  (defun cm/switch-to-agenda ()
    (interactive)
    (setq org-agenda-files (directory-files-recursively cm/org-agenda-directory "\.org$"))
    (org-agenda nil " " nil))

  (setq org-start-th-log-mode t)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-restore-windows-after-quit t)
  (setq org-refile-targets
	'((org-agenda-files . (:maxlevel . 2))
	  (nil . (:maxlevel . 2))))

  
  (setq org-agenda-prefix-format
	'((agenda . " %i %-12:c%?-12t% s")
          (todo   . " ")
          (tags   . " %i %-12:c")
          (search . " %i %-12:c")))

  (setq org-agenda-custom-commands `((" " "Agenda"
                                      ((agenda ""
                                               ((org-agenda-span 'week)
                                                (org-deadline-warning-days 365)))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Inbox")
                                              (org-agenda-files '(,(expand-file-name "inbox.org" cm/org-agenda-directory)))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Emails")
                                              (org-agenda-files '(,(expand-file-name "emails.org" cm/org-agenda-directory)))))
                                       (todo "NEXT"
                                             ((org-agenda-overriding-header "In Progress")
                                              (org-agenda-files '(,(expand-file-name "projects.org" cm/org-agenda-directory)))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Active Projects")
                                              (org-agenda-skip-function #'cm/skip-projects)
                                              (org-agenda-files '(,(expand-file-name "projects.org" cm/org-agenda-directory)))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "One-off Tasks")
                                              (org-agenda-files '(,(expand-file-name "next.org" cm/org-agenda-directory)))
                                              (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))))))
  :bind
  (("C-c a" . cm/switch-to-agenda)
   ("<f1>" . cm/switch-to-agenda)))

(use-package org-capture
  :after (org org-agenda)
  :config
  (setq org-capture-templates
	`(("i" "Inbox" entry (file ,(expand-file-name "inbox.org" cm/org-agenda-directory))
	   "* TODO %?\n /Entered on/ %u")))

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
  (add-to-list 'org-structure-template-alist '("ip" . "src jupyter-python :results raw drawer"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))
		
;; org-bullets for better handling of org level markers
(use-package org-bullets
  :straight t
  :after org
  :hook (org-mode-hook . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(setq org-publish-project-alist
        '(("orgfiles"
           :base-directory "~/org-roam/"
	   :exclude "~/org-roam/notes/.ob-jupyter\\|*.undo-tree*"
           :publishing-directory "~/Documents/ephemeris/"
	   :recursive t
	   :publishing-function org-html-publish-to-html
           :section-numbers nil
           :table-of-contents nil
	   )
	  ("images"
	   :base-directory "~/org-roam/notes/images"
	   :base-extension "jpg\\|gif\\|png"
	   :publishing-directory "~/Documents/ephemeris/notes/images/"
	   :recursive t
	   :publishing-function org-publish-attachment)
	  ))

;; Org-tree-slides for presentation in org-mode
(use-package org-tree-slide
  :straight t)

;; Org-appear to toggle emphasis markers
(use-package org-appear
  :straight t
  ;; :hook (org-mode-hook . org-appear-mode)
  )
