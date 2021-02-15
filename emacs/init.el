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
		eshell-mode-hook
		Info-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Set fonts and faces
(defvar cm/default-font-size 125)
(defvar cm/default-variable-font-size 135)

(put 'narrow-to-region 'disabled nil)

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
  (set-face-attribute 'default nil :font "Hack" :height cm/default-font-size)
  (set-face-attribute 'variable-pitch nil :font "FiraGo" :height cm/default-variable-font-size))

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

;; Diminish modeline
(use-package diminish
  :straight t)

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
  (setq ido-everywhere t)
  (setq ido-use-virtual-buffers 'auto))

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
  (setq dired-x-hands-off-my-keys t)
  (setq dired-bind-man nil)
  (setq dired-bind-info nil)
  :bind (("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window)
         :map dired-mode-map
         ("I" . dired-info)))

;; Olivetti mode
(use-package olivetti
  :straight t
  :diminish
  :config
  (setq olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t)
  :hook
  (org-mode-hook . olivetti-mode)
  (Info-mode-hook . olivetti-mode))

;; PDF tools
;; Add dwim functions to "u" and "d" since the current names are
;; confusing.
(use-package pdf-tools
  :straight t
  :defer
  :magic ("%PDF" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
	      ("q" . bury-buffer)
	      ("j" . pdf-view-next-line-or-next-page)
	      ("k" . pdf-view-previous-line-or-previous-page)
	      ("u" . pdf-view-scroll-down-or-previous-page)
	      ("d" . pdf-view-scroll-up-or-next-page)
	      ("M-i" . pdf-view-midnight-minor-mode))
  :config
  (pdf-tools-install)
  (setq pdf-view-mode 1))

;; Telegram
(use-package telega
  :straight t
  :commands (telega)
  :defer t
  :config
  (setq telega-use-images t)
  (setq telega-emoji-use-images t))

;; Mu4e Email settings
(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :config
  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/Mail")

  (setq message-send-mail-function 'smtpmail-send-it)
  (setq mu4e-compose-format-flowed t)
  ;; don't save messages to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)
  (setq mu4e-headers-fields '((:human-date . 12)
			     (:flags . 6)
			     (:from-or-to . 22)
			     (:subject)))
  

  (setq mu4e-contexts
	(list
	 (make-mu4e-context
	  :name "Personal"
	  :match-func (lambda (msg)
			(when msg
			  (string-prefix-p "/chahakcr7" (mu4e-message-field msg :maildir))))
	  :vars
	  '(
	    (user-mail-address . "chahakcr7@gmail.com")
	    (user-full-name . "Chahak Mehta")
	    (mu4e-drafts-folder . "/chahakcr7/[Gmail]/Drafts")
	    (mu4e-sent-folder . "/chahakcr7/[Gmail]/Sent Mail")
	    (mu4e-refile-folder . "/chahakcr7/[Gmail]/All Mail")
	    (mu4e-trash-folder . "/chahakcr7/[Gmail]/Trash")
	    ))

	 (make-mu4e-context
	  :name "Official"
	  :match-func (lambda (msg)
			(when msg
			  (string-prefix-p "/chahakmehta013" (mu4e-message-field msg :maildir))))
	  :vars
	  '(
	    (user-mail-address . "chahak.mehta013@gmail.com")
	    (user-full-name . "Chahak Mehta")
	    (mu4e-drafts-folder . "/chahakmehta013/[Gmail]/Drafts")
	    (mu4e-sent-folder . "/chahakmehta013/[Gmail]/Sent Mail")
	    (mu4e-refile-folder . "/chahakmehta013/[Gmail]/All Mail")
	    (mu4e-trash-folder . "/chahakmehta013/[Gmail]/Trash")
	    ))

	 (make-mu4e-context
	  :name "UT Mail"
	  :match-func (lambda (msg)
			(when msg
			  (string-prefix-p "/utexas" (mu4e-message-field msg :maildir))))
	  :vars
	  '(
	    (user-mail-address . "chahak@utexas.edu")
	    (user-full-name . "Chahak Mehta")
	    (mu4e-drafts-folder . "/utexas/[Gmail]/Drafts")
	    (mu4e-sent-folder . "/utexas/[Gmail]/Sent Mail")
	    (mu4e-refile-folder . "/utexas/[Gmail]/All Mail")
	    (mu4e-trash-folder . "/utexas/[Gmail]/Trash")
	    ))))

  (setq mu4e-maildir-shortcuts
	'(
	  ("/utexas/Inbox" . ?i)
	  ("/utexas/[Gmail]/Sent Mail" . ?s)
	  ("/utexas/[Gmail]/Trash" . ?t)
	  )
	)

  (defvar cm/mu4e-account-alist
    '(
      ("chahakcr7"
       (mu4e-sent-folder "/chahakcr7/[Gmail]/Sent Mail")
       (user-mail-address "chahakcr7@gmail.com")
       (smtpmail-smtp-user "chahakcr7")
       (smtpmail-local-domain "gmail.com")
       (smtpmail-default-smtp-server "smtp.gmail.com")
       (smtpmail-smtp-server "smtp.gmail.com")
       (smtpmail-smtp-service 587)
       )

      ("chahakmehta013"
       (mu4e-sent-folder "/chahakmehta013/[Gmail]/Sent Mail")
       (user-mail-address "chahak.mehta013@gmail.com")
       (smtpmail-smtp-user "chahak.mehta013")
       (smtpmail-local-domain "chahak.mehta013")
       (smtpmail-default-smtp-server "smtp.gmail.com")
       (smtpmail-smtp-server "smtp.gmail.com")
       (smtpmail-smtp-service 587)
       )

      ("utexas"
       (mu4e-sent-folder "/utexas/[Gmail]/Sent Mail")
       (user-mail-address "chahak@utexas.edu")
       (smtpmail-smtp-user "chahak")
       (smtpmail-local-domain "chahak")
       (smtpmail-default-smtp-server "smtp.gmail.com")
       (smtpmail-smtp-server "smtp.gmail.com")
       (smtpmail-smtp-service 587)
       
       )
      ))

  (defun cm/mu4e-set-account ()
    "Set the account for composing a message.
   This function is taken from: 
     https://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html"
    (let* ((account
	    (if mu4e-compose-parent-message
		(let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
		  (string-match "/\\(.*?\\)/" maildir)
		  (match-string 1 maildir))
	      (completing-read (format "Compose with account: (%s) "
				       (mapconcat #'(lambda (var) (car var))
						  cm/mu4e-account-alist "/"))
			       (mapcar #'(lambda (var) (car var)) cm/mu4e-account-alist)
			       nil t nil nil (caar cm/mu4e-account-alist))))
	   (account-vars (cdr (assoc account cm/mu4e-account-alist))))
      (if account-vars
	  (mapc #'(lambda (var)
		    (set (car var) (cadr var)))
		account-vars)
	(error "No email account found"))))

  (add-hook 'mu4e-compose-pre-hook 'cm/mu4e-set-account)
  )					; use-package block for mu4e ends

;; Dired for mu4e
(use-package gnus-dired
  :config
  ;; make the `gnus-dired-mail-buffers' function also work on
  ;; message-mode derived modes, such as mu4e-compose-mode
  (defun gnus-dired-mail-buffers ()
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer
	(dolist (buffer (buffer-list t))
	  (set-buffer buffer)
	  (when (and (derived-mode-p 'message-mode)
		     (null message-sent-message-via))
	    (push (buffer-name buffer) buffers))))
      (nreverse buffers)))

  (setq gnus-dired-mail-mode 'mu4e-user-agent)
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode))

;; Aggressive indentation
;; (use-package aggressive-indent
;;   :straight t
;;   :hook (prog-mode-hook . aggressive-indent-mode))

;; Beginend
;; Meaningful M-< and M->
(use-package beginend 
  :straight t
  :demand t
  :diminish
  :config 
  (dolist (mode (cons 'beginend-global-mode (mapcar #'cdr beginend-modes)))
          (diminish mode))
  (beginend-global-mode))

;; Hungry delete
(use-package hungry-delete
  :straight t
  :diminish
  :config
  (setq hungry-delete-join-reluctantly t)
  (global-hungry-delete-mode))

;; Dired-rainbow
;; NOT WORKING RIGHT NOW. Need to check this.
(use-package dired-rainbow
  :straight t
  :diminish
  :config
    (progn
      (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
      (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
      (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
      (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
      (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
      (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
      (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
      (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
      (dired-rainbow-define log "#c17d11" ("log"))
      (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
      (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
      (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
      (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
      (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
      (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
      (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
      (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
      (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
      (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
      (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
      ))

;; Other emacs settings
(use-package emacs
  :ensure nil
  :bind (("s-q" . save-buffers-kill-emacs))
  :config
  (global-hl-line-mode))
