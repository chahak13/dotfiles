;; Magit
(use-package magit
  :straight t
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Put customize variable elisp by emacs to another file
(use-package cus-edit
  :config
  (defvar cm/custom-file "~/.emacs.d/custom.el")

  (setq custom-file cm/custom-file)

  (defun cm/cus-edit ()
    (let ((file cm/custom-file))
      (unless (file-exists-p file)
        (make-empty-file file))
      (load-file file)))
  :hook (after-init-hook . cm/cus-edit))

;; Projectile
(use-package projectile
  :straight t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("C-x p" . projectile-command-map)))
