(use-package pipenv
  :straight t
  :hook ((python-mode-hook . pipenv-mode)
         (org-mode-hook . pipenv-mode))
  )
