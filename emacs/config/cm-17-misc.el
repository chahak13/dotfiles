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
  :config
  (setq olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t)
  :hook
  (org-mode-hook . olivetti-mode)
  (Info-mode-hook . olivetti-mode))


;; Aggressive indentation
;; (use-package aggressive-indent
;;   :straight t
;;   :hook (prog-mode-hook . aggressive-indent-mode))

;; Beginend
;; Meaningful M-< and M->
;; (use-package beginend 
;;   :straight t
;;   :demand t
;;   :diminish
;;   :config 
;;   (dolist (mode (cons 'beginend-global-mode (mapcar #'cdr beginend-modes)))
;;           (diminish mode))
;;   (beginend-global-mode))

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

;; Telegram
(use-package telega
  :straight t
  :commands (telega)
  :defer t
  :config
  (setq telega-use-images t)
  (setq telega-emoji-use-images t))
