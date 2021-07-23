;; PDF tools
;; Add dwim functions to "u" and "d" since the current names are
;; confusing.
(use-package pdf-tools
  :straight t
  :defer
  :magic ("%PDF" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
	      ("q" . bury-buffer)
	      ("h" . image-backward-hscroll)
	      ("l" . image-forward-hscroll)
	      ("j" . pdf-view-next-line-or-next-page)
	      ("k" . pdf-view-previous-line-or-previous-page)
	      ("u" . pdf-view-scroll-down-or-previous-page)
	      ("d" . pdf-view-scroll-up-or-next-page)
	      ("M-i" . pdf-view-midnight-minor-mode))
  :config
  ;; (setq pdf-info-epdfinfo-program "/home/boticelli/.emacs.d/straight/build/pdf-tools/build/server/epdfinfo")
  (setq pdf-annot-activate-created-annotations t)
  (pdf-tools-install)
  ;; (pdf-annot-minor-mode)
  )
