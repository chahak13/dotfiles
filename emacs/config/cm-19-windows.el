(use-package window
  :init
  (setq display-buffer-alist
	'(;; Top side window
	  ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . 1)
           (window-parameters . ((no-other-window . t))))
	  ;; bottom side window
          ("\\*\\(Output\\|Register Preview\\).*"
           (display-buffer-in-side-window)
           (window-width . 0.16)       ; See the :hook
           (side . bottom)
           (slot . -1)
           (window-parameters . ((no-other-window . t))))
          (".*\\*Completions.*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . bottom)
           (slot . 0)
           (window-parameters . ((no-other-window . t))))
          ("^\\(\\*e?shell\\|vterm\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . bottom)
           (slot . 1))
          ;; ("^\\*Org Src doubt.org[ latex ]\*"
	  ;;  (display-buffer-in-side-window)
	  ;;  (window-height . 0.5)
	  ;;  (side . bottom)
	  ;;  (slot . 1))
	  ;; ;; left side window
          ("\\*Help.*"
           (display-buffer-in-side-window)
           (window-width . 0.25)       ; See the :hook
           (side . left)
           (slot . 0))))
  :hook ((helpful-mode-hook . visual-line-mode)
	 (help-mode-hook . visual-line-mode))
  :bind (("s-n" . next-buffer)
         ("s-p" . previous-buffer)
         ("s-o" . other-window)
         ("s-2" . split-window-below)
         ("s-3" . split-window-right)
         ("s-0" . delete-window)
         ("s-1" . delete-other-windows)
         ("s-5" . delete-frame)
         ("C-x +" . balance-windows-area)
	 ("s-+" . balance-windows-area)
         ("s-t" . window-toggle-side-windows)))

