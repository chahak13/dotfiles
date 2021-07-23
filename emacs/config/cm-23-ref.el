(use-package ivy-bibtex
  :straight t
  :bind (("C-c b" . ivy-bibtex))
  :config
  (setq bibtex-completion-bibliography "~/org-roam/references.bib")
  (setq bibtex-completion-pdf-field "file")
  (setq bibtex-completion-notes-path "~/org-roam/notes")
  (setq bibtex-completion-format-citation-functions
  '((org-mode      . bibtex-completion-format-citation-org-link-to-PDF)
    (latex-mode    . bibtex-completion-format-citation-cite)
    (default       . bibtex-completion-format-citation-default)))
  )

;; (use-package org-noter
;;   :straight t
;;   :after org
;;   :config
;;   (setq org-noter-notes-search-path '("~/org-roam/notes" "~/Documents"))
;;   (setq org-noter-always-create-frame nil)
;;   )
