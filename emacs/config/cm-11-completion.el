;; Ivy
(use-package counsel
  :straight t)

(use-package swiper
  :straight t)

(use-package ivy
  :straight t
  :after counsel swiper
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap 1)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file))

;; Ivy rich
(use-package ivy-rich
  :straight t
  :config
  (ivy-rich-mode 1))

(use-package ivy-posframe
  :straight t
  :config
  (setq ivy-posframe-height-alist
	'((swiper . 20)
	  (t . 10)))
  (setq ivy-posframe-display-functions-alist
	'((counsel-M-x . ivy-posframe-display-at-window-center)
	  (counsel-find-file . ivy-posframe-display-at-window-center)
	  (ivy-switch-buffer . ivy-posframe-display-at-window-center)
	  (t . ivy-display-function-fallback)))
  (setq ivy-posframe-parameters
	'((left-fringe . 10)
	  (right-fringe . 10)))
	
  (ivy-posframe-mode 1))

;; Amx package:
;; Amx is an alternative to M-x and is a fork of smex. It is
;; designed to work with multiple completion frameworks and is
;; currently being used with ido.
(use-package amx
  :straight t
  :config
  (setq amx-backend 'auto)
  (amx-mode 1))
