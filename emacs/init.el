;; Setup straight.el
;; Straigh.el is the package manager that I plan to use for my emacs configuration.
;; This piece of bootstrapping code is borrowed from the official README of straight.el
;; and can be found here: https://github.com/raxod502/straight.el#bootstrapping-straightel
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq-default straight-vc-git-default-clone-depth 5)

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
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Set fonts and faces
(defvar cm/default-font-size 125)
(defvar cm/default-variable-font-size 135)

;; ================
;; Custom Functions
;; ================

(defun cm/disable-all-themes ()
  "Disable all themes listed in `custom-enabled-themes`"
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(defun cm/load-theme (THEME)
  "Load the THEME only if it is not set already"
  (interactive)
  (unless (member THEME custom-enabled-themes)
    (load-theme THEME)))

(defadvice load-theme (before cm/disable-all-themes activate)
  (cm/disable-all-themes))

;; (defun black-format-python-file ()
;;   "Format the python file in the buffer using black"
;;   (message (shell-command-to-string (format "command -v black && black -l 80 %s" buffer-file-name)))
;;   (message (format "Executed black formatting hook on %s" buffer-file-name)))

;; (add-hook 'python-mode-hook
;; 	  (lambda ()
;; 	    (add-hook 'after-save-hook 'black-format-python-file nil 'local)))

(defun cm/set-font-faces ()
  (set-face-attribute 'default nil :font "Firacode Nerd Font" :height cm/default-font-size))

;; =====
;; Hooks
;; =====
(if (daemonp)
    (add-hook 'server-after-make-frame-hook 'cm/set-font-faces)
  (cm/set-font-faces))

;; ========
;; Packages
;; ========

;; Install use-package using straight.el
(straight-use-package 'use-package)

;; Configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-always-ensure nil)  ; ESSENTIAL for `straight.el'
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t)
  (setq use-package-compute-statistics nil)
  ;; Borrowed from Protesilaos' great dotfiles
  ;; The following is VERY IMPORTANT.  Write hooks using their real name
  ;; instead of a shorter version: after-init ==> `after-init-hook'.
  ;;
  ;; This is to empower help commands with their contextual awareness,
  ;; such as `describe-symbol'.
  (setq use-package-hook-name-suffix nil))

(use-package straight-x)

;; Newcomment (built-in)
;; Rebind inbuilt comment function to better keys
(use-package newcomment
  :bind (("C-;" . comment-line)))

;; Modus themes
(use-package modus-themes
  :straight t)

;; Ibuffer
(use-package ibuffer
  :bind (("C-x C-b" . ibuffer-other-window)))

;; ido built-in:
;; The built-in ido completion framework
(use-package ido
  :bind
  :config
  (ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t))

;; Ido-completing-read+ package:
;; Required for Amx
(use-package ido-completing-read+
  :straight t
  :config
  (ido-ubiquitous-mode 1))

;; Amx package:
;; Amx is an alternative to M-x and is a fork of smex. It is
;; designed to work with multiple completion frameworks and is
;; currently being used with ido.
(use-package amx
  :straight t
  :config
  (amx-mode 1))

;; Undo-tree package
;; This package is useful to use undo history as a tree instead of
;; a linear history.
(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode))

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

;; Org
;; Org mode setup
(defun cm/org-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org-mode
  :straight (:host nil :type git
             :repo "https://code.orgmode.org/bzg/org-mode.git") ; Get Org from its source
  :hook
  (org-mode-hook . cm/org-setup))

;; org-bullets for better handling of org level markers
(use-package org-bullets
  :after org-mode
  :hook (org-mode-hook . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Magit
(use-package magit
  :straight t
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Put customize variable elisp by emacs to another file
(use-package cus-edit
  :config
  (defvar cm/custom-file "~/.emacs.d/custom.el")

  (setq custom-file cm/custom-file)

  (defun cm/cus-edit ()
    (let ((file cm/custom-file))
      (unless (file-exists-p file)
        (make-empty-file file))
      (load-file file)))
  :hook (after-init-hook . cm/cus-edit))

;; Projectile
(use-package projectile
  :straight t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("C-x p" . projectile-command-map)))

;; Dired
(use-package dired
  :config
  (setq dired-listing-switches
	"-AGFhlv --group-directories-first --time-style=long-iso")
  (setq dired-dwim-target t)
  (setq dired-hide-details-hide-symlink-targets nil)
  :bind (:map dired-mode-map
	      ("+" . dired-create-empty-file))
  :hook ((dired-mode-hook . dired-hide-details-mode)
	 (dired-mode-hook . hl-line-mode)))

(use-package dired-aux
  :config
  (setq dired-create-destination-dirs 'ask)
  (setq dired-isearch-filenames 'dwim)
  (setq dired-vc-rename-file t)
  :bind (:map dired-mode-map
	      ("C-+" . dired-create-directory)))

(use-package async
  :straight t)

(use-package dired-async
  :after (dired async)
  :hook (dired-mode-hook . dired-async-mode))

(use-package wdired
  :after dired
  :commands wdired-change-to-wdired-mode
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

(use-package dired-subtree
  :straight t
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<C-tab>" . dired-subtree-cycle)
              ("<S-iso-lefttab>" . dired-subtree-remove)))

(use-package dired-x
  :after dired
  :config
  (setq dired-clean-up-buffers-too t)
  (setq dired-clean-confirm-killing-deleted-buffers t)
  (setq dired-x-hands-off-my-keys t)    ; easier to show the keys I use
  (setq dired-bind-man nil)
  (setq dired-bind-info nil)
  :bind (("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window)
         :map dired-mode-map
         ("I" . dired-info)))
