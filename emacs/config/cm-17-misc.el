;; Undo-tree package
;; This package is useful to use undo history as a tree instead of
;; a linear history.
(use-package undo-tree
  :straight t
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree")))
  (global-undo-tree-mode))

;; Olivetti mode
(use-package olivetti
  :straight t
  :diminish
  :hook
  (org-mode-hook . olivetti-mode)
  (Info-mode-hook . olivetti-mode)
  :config
  ;; (setq olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 120)
  (setq olivetti-recall-visual-line-mode-entry-state t)
  )

;; Beginend
;; Meaningful M-< and M->
(use-package beginend 
  :straight t
  :demand t
  :diminish
  :config 
  (dolist (mode (cons 'beginend-global-mode (mapcar #'cdr beginend-modes)))
          (diminish mode))
  (beginend-global-mode))

;; Hungry delete
(use-package hungry-delete
  :straight t
  :diminish
  :config
  (setq hungry-delete-join-reluctantly t)
  (global-hungry-delete-mode))

(use-package rainbow-mode
  :ensure
  :diminish
  :commands rainbow-mode
  :config
  (setq rainbow-ansi-colors nil)
  (setq rainbow-x-colors nil))

;; ESS (Emacs Speaks Statistics)
;; Provides major mode for R code
(use-package ess
  :straight t)

;; sx.el
;; Stackoverflow in emacs
(use-package sx
  :straight t)

;; Ripgrep
(use-package rg
  :straight t)

;; HTMLize
(use-package htmlize
  :straight t)


