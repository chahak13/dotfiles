(use-package rust-mode
  :straight t
  :hook
  (rust-mode-hook . (lambda () (setq indent-tabs-mode nil))))
