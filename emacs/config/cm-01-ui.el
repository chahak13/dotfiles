;; ==========
;; UI Changes
;; ==========

(setq inhibit-startup-message t) ; Inhitbit the default startup message

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 8)	    ; Give some breathing room
(menu-bar-mode -1)	    ; Disable the menu bar

(global-display-line-numbers-mode t)	; Enable line numbers everywhere
(column-number-mode)		        ; Show column number in the mode-line

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		eshell-mode-hook
		Info-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Set fonts and faces
(defvar cm/default-font-size 125)
(defvar cm/default-variable-font-size 135)

(put 'narrow-to-region 'disabled nil)
