;; Setup straight.el
;; Straigh.el is the package manager that I plan to use for my emacs configuration.
;; This piece of bootstrapping code is borrowed from the official README of straight.el
;; and can be found here: https://github.com/raxod502/straight.el#bootstrapping-straightel

;; Uncomment the following line to disable native compilation
;; (setq straight-disable-native-compile t)
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
(add-to-list 'exec-path "/home/boticelli/.local/bin/")

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

;; (use-package straight-x)

;; ===== Emacs config ======
(load-file "~/.emacs.d/config/cm-10-emacs.el")

;; ==== Package configs ====
(load-file "~/.emacs.d/config/cm-11-completion.el")
(load-file "~/.emacs.d/config/cm-12-org.el")
(load-file "~/.emacs.d/config/cm-13-project-management.el")
(load-file "~/.emacs.d/config/cm-14-dired.el")
(load-file "~/.emacs.d/config/cm-15-help.el")
(load-file "~/.emacs.d/config/cm-16-mu4e.el")
(load-file "~/.emacs.d/config/cm-17-misc.el")
(load-file "~/.emacs.d/config/cm-18-pdf-tools.el")
(load-file "~/.emacs.d/config/cm-19-windows.el")
;; (load-file "~/.emacs.d/config/cm-20-ledger.el")
(load-file "~/.emacs.d/config/cm-21-bongo.el")
(load-file "~/.emacs.d/config/cm-22-tex.el")
(load-file "~/.emacs.d/config/cm-23-ref.el")
;; (load-file "~/.emacs.d/config/cm-24-roam.el")
;; (load-file "~/.emacs.d/config/cm-25-general.el")
;; (load-file "~/.emacs.d/config/cm-26-evil.el")

;; ======= Programming =======
(load-file "~/.emacs.d/config/cm-30-rust.el")
(load-file "~/.emacs.d/config/cm-31-pipenv.el")
(load-file "~/.emacs.d/config/cm-32-vertico.el")

(setq modus-themes-slanted-constructs t
      modus-themes-bold-constructs nil
      modus-themes-no-mixed-fonts nil
      modus-themes-subtle-line-numbers nil
      modus-themes-success-deuteranopia t

      modus-themes-fringes nil ; {nil,'subtle,'intense}

      ;; Options for `modus-themes-lang-checkers': nil,
      ;; 'straight-underline, 'subtle-foreground,
      ;; 'subtle-foreground-straight-underline, 'intense-foreground,
      ;; 'intense-foreground-straight-underline, 'colored-background
      modus-themes-lang-checkers nil

      ;; Options for `modus-themes-mode-line': nil, '3d, 'moody,
      ;; 'borderless, 'borderless-3d, 'borderless-moody, 'accented,
      ;; 'accented-3d, 'accented-moody, 'borderless-accented,
      ;; 'borderless-accented-3d, 'borderless-accented-moody
      modus-themes-mode-line 'borderless-moody

      ;; Options for `modus-themes-syntax': nil, 'faint,
      ;; 'yellow-comments, 'green-strings,
      ;; 'yellow-comments-green-strings, 'alt-syntax,
      ;; 'alt-syntax-yellow-comments, 'faint-yellow-comments
      modus-themes-syntax 'alt-syntax

      ;; Options for `modus-themes-hl-line': nil, 'intense-background,
      ;; 'accented-background, 'underline-neutral,
      ;; 'underline-accented, 'underline-only-neutral,
      ;; 'underline-only-accented
      modus-themes-hl-line 'intense-background

      modus-themes-paren-match 'subtle-bold ; {nil,'subtle-bold,'intense,'intense-bold}

      ;; Options for `modus-themes-links': nil, 'faint,
      ;; 'neutral-underline, 'faint-neutral-underline, 'no-underline,
      ;; 'underline-only, 'neutral-underline-only
      modus-themes-links 'neutral-underline

      ;; Options for `modus-themes-prompts': nil, 'subtle-accented,
      ;; 'intense-accented, 'subtle-gray, 'intense-gray
      modus-themes-prompts 'subtle-gray

      modus-themes-completions 'moderate ; {nil,'moderate,'opinionated}

      modus-themes-mail-citations nil ; {nil,'faint,'monochrome}

      ;; Options for `modus-themes-region': nil, 'no-extend, 'bg-only,
      ;; 'bg-only-no-extend, 'accent, 'accent-no-extend
      modus-themes-region 'bg-only-no-extend

      ;; Options for `modus-themes-diffs': nil, 'desaturated,
      ;; 'bg-only, 'deuteranopia, 'fg-only-deuteranopia
      modus-themes-diffs nil

      modus-themes-org-blocks 'gray-background ; {nil,'gray-background,'tinted-background}
      modus-themes-org-habit nil ; {nil,'simplified,'traffic-light}

      modus-themes-headings ; this is an alist: read the manual or its doc string
      '((1 . line)
        (2 . rainbow-line-no-bold)
        (t . no-bold))

      modus-themes-variable-pitch-ui nil
      modus-themes-variable-pitch-headings t
      modus-themes-scale-headings t
      modus-themes-scale-1 1
      modus-themes-scale-2 1.1
      modus-themes-scale-3 1.15
      modus-themes-scale-4 1.2
      modus-themes-scale-5 1.25)
