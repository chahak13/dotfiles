(use-package ledger-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\ledger.dat$" . ledger-mode))
  )
