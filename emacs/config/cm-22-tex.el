
(use-package tex-site
  :straight auctex
  :hook
  (latex-mode-hook . reftex-mode)
  :config
  (setq Tex-auto-save t)
  (setq Tex-parse-self t)
  )

(use-package company-auctex
  :straight t
  :after auctex)
