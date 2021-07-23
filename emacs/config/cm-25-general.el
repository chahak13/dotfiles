(use-package general
  :straight t
  :config
  (general-create-definer cm/roam-leader
    :prefix "M-o")

  (cm/roam-leader
   "f" 'org-roam-node-find
   "c" 'org-roam-capture
   "i" 'org-roam-node-insert
   "o" 'org-roam-buffer-toggle)
  )
