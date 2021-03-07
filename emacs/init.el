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

;; ===== UI =======
(load-file "~/.emacs.d/config/cm-01-ui.el")

;; ===== Custom Functions ======
(load-file "~/.emacs.d/config/cm-02-custom-func.el")

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

;; ===== Emacs config ======
(load-file "~/.emacs.d/config/cm-10-emacs.el")

;; ==== Packages ====
(load-file "~/.emacs.d/config/cm-11-completion.el")
(load-file "~/.emacs.d/config/cm-12-org.el")
(load-file "~/.emacs.d/config/cm-13-project-management.el")
(load-file "~/.emacs.d/config/cm-14-dired.el")
(load-file "~/.emacs.d/config/cm-15-help.el")
(load-file "~/.emacs.d/config/cm-16-mu4e.el")
(load-file "~/.emacs.d/config/cm-17-misc.el")
(load-file "~/.emacs.d/config/cm-18-pdf-tools.el")
