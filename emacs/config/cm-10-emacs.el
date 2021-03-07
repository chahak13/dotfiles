(use-package emacs
  :ensure nil
  :bind (("s-q" . save-buffers-kill-emacs))
  :config
  (global-hl-line-mode))

;; Newcomment (built-in)
;; Rebind inbuilt comment function to better keys
(use-package newcomment
  :bind (("C-;" . comment-line)))

;; Diminish modeline
(use-package diminish
  :straight t)

;; Ibuffer
(use-package ibuffer
  :bind (("C-x C-b" . ibuffer-other-window)))

;; ;; ido built-in:
;; ;; The built-in ido completion framework
;; (use-package ido
;;   :bind
;;   :config
;;   (ido-mode 1)
;;   (setq ido-enable-flex-matching t)
;;   (setq ido-everywhere t)
;;   (setq ido-use-virtual-buffers 'auto))

;; ;; Ido-completing-read+ package:
;; ;; Required for Amx
;; (use-package ido-completing-read+
;;   :straight t
;;   :config
;;   (ido-ubiquitous-mode 1))

;; Modus themes
(use-package modus-themes
  :straight t)

(use-package async
  :straight t)

