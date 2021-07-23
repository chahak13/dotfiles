;; Ivy
;; (use-package counsel
;;   :straight t
;;   :diminish)

;; (use-package swiper
;;   :straight t)

;; (use-package ivy
;;   :straight t
;;   :after counsel swiper
;;   :diminish
;;   :config
;;   (ivy-mode 1)
;;   (counsel-mode 1)
;;   (setq ivy-use-virtual-buffers t)
;;   (setq ivy-wrap 1)
;;   (setq enable-recursive-minibuffers t)
;;   (setq ivy-count-format "(%d/%d) ")
;;   (setq ivy-re-builders-alist
;;       '((t . ivy--regex-ignore-order)))
;;   :bind
;;   ("M-x" . counsel-M-x)
;;   ("C-x C-f" . counsel-find-file))

;; ;; Ivy rich
;; (use-package ivy-rich
;;   :straight t
;;   :config
;;   (ivy-rich-mode 1))

;; (use-package ivy-posframe
;;   :straight t
;;   :diminish 
;;   :config
;;   (setq ivy-posframe-height-alist
;; 	'((swiper . 20)
;; 	  (t . 10)))
;;   (setq ivy-posframe-display-functions-alist
;; 	'((counsel-M-x . ivy-posframe-display-at-window-center)
;; 	  (counsel-find-file . ivy-posframe-display-at-window-center)
;; 	  (ivy-switch-buffer . ivy-posframe-display-at-window-center)
;; 	  (t . ivy-display-function-fallback)))
;;   (setq ivy-posframe-parameters
;; 	'((left-fringe . 10)
;; 	  (right-fringe . 10)))
	
;;   (ivy-posframe-mode 1))

;; ;; Amx package:
;; ;; Amx is an alternative to M-x and is a fork of smex. It is
;; ;; designed to work with multiple completion frameworks and is
;; ;; currently being used with ivy.
;; (use-package amx
;;   :straight t
;;   :config
;;   (setq amx-backend 'auto)
;;   (amx-mode 1))

;; Yasnippet for snippets
(use-package yasnippet
  :straight t
  :diminish
  :config
  (setq yas-triggers-in-field t)
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

;; Company-mode
(use-package company
  :straight t
  :diminish
  :config
  (global-company-mode)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  )

(use-package lsp-mode
  :straight t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)
;; ;; if you are ivy user
;; (use-package lsp-ivy
;;   :straight t
;;   :commands lsp-ivy-workspace-symbol)
