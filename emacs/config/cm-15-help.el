;; which-key package:
;; which-key provides a visual buffer that shows the possible key
;; bindings that are available in the particular prefix
(use-package which-key
  :straight t
  :init
  (which-key-mode)
  :diminish
  :config
  (setq which-key-idle-delay 1))

;; Helpful package:
;; Helpful is an alternative to the built-in Emacs help that
;; provides much more contextual information.
(use-package helpful
  :straight t
  :bind (("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)
	 ("C-c C-d" . helpful-at-point)
	 ("C-h F" . helpful-function)
	 ))
