(use-package vertico
  :straight t
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :straight nil
  :init
  (savehist-mode))

(use-package marginalia
  :straight t
  :init
  (marginalia-mode))
