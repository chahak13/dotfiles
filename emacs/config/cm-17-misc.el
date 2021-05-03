;; Undo-tree package
;; This package is useful to use undo history as a tree instead of
;; a linear history.
(use-package undo-tree
  :straight t
  :config
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



;; Aggressive indentation
;; (use-package aggressive-indent
;;   :straight t
;;   :hook (prog-mode-hook . aggressive-indent-mode))

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

;; Autothemer
;; Themeing package for easier customization for Emacs.
(use-package autothemer
  :straight t)
