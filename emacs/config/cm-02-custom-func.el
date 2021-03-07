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
