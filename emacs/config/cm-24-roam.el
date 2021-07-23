(use-package org-roam
  :straight t ;; (:host github :repo "org-roam/org-roam" :branch "v2")
  :config
  (setq org-roam-directory "~/org-roam/notes")
  (setq org-roam-v2-ack t)
  ;; (setq org-roam-capture-templates
  ;;       '(("d" "default" plain (function org-roam--capture-get-point)
  ;;          "%?"
  ;;          :file-name "%<%Y%m%d%H%M%S>-${slug}"
  ;;          :head "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+property:\n#+roam_tags:\n- tags :: \n"
  ;;          :unnarrowed t)
  ;;         ("p" "paper" entry (function org-roam--capture-get-point)
  ;;          "%?"
  ;;          :file-name "%<%Y%m%d%H%M%S>-${slug}"
  ;;          :head "#+TITLE: ${title}\n#+AUTHOR: ${authors}\n"
  ;;          :unnarrowed t)
  ;;         ))
  ;; (setq org-roam-dailies-directory "daily/")
  ;; (setq org-roam-dailies-capture-templates
  ;;       '(("d" "default" entry (function org-roam--capture-get-point)
  ;;          "* %?"
  ;;          :file-name "daily/%<%Y-%m-%d>"
  ;;          :head "#+TITLE: %<%Y-%m-%d>\n\n")
  ;; 	  ("m" "media consumed" entry (function org-roam-capture--get-point)
  ;; 	   "* %?"
  ;; 	   :file-name "daily/%<%Y-%m-%d>"
  ;; 	   :head "#+TITLE: %<%Y-%m-%d>\n\n"
  ;; 	   :olp ("Media Consumed"))))
  )

;; (use-package org-roam-protocol
;;   :after org-roam
;;   :config
;;   (setq org-roam-capture-ref-templates
;;         '(("r" "ref" plain (function org-roam-capture--get-point)
;;            "%?"
;;            :file-name "%<%Y%m%d%H%M%S>-${slug}"
;;            :head "#+TITLE: ${title}\n#+roam_key: ${ref}"
;;            :unnarrowed t)
;;           ("w" "webpage" plain (function org-roam--capture-get-point)
;;            "/${body}/"
;;            :file-name "%<%Y%m%d%H%M%S>-${slug}"
;;            :head "#+TITLE: ${title}\n#+roam_key: ${ref}\n#+roam_tags: \n\n"
;;            :unnarrowed t)
;;           ))
;;   )

;; (use-package org-roam-server
;;   :straight t
;;   :config
;;   (setq org-roam-server-host "127.0.0.1")
;;   (setq org-roam-server-port 8080)
;;   (setq org-roam-server-authenticate nil)
;;   (setq org-roam-server-export-inline-images t)
;;   (setq org-roam-server-serve-files nil)
;;   (setq org-roam-server-served-file-extensions '("pdf" "mp4" "ogv"))
;;   (setq org-roam-server-network-poll t)
;;   (setq org-roam-server-network-arrows nil)
;;   (setq org-roam-server-network-label-truncate t)
;;   (setq org-roam-server-network-label-truncate-length 60)
;;   (setq org-roam-server-network-label-wrap-length 20)
;;   (org-roam-server-mode)
;;   )

(use-package deft
  :straight t
  :bind
  ("<f8>" . deft)
  :config
  (setq deft-directory "~/org-roam/notes")
  (setq deft-extensions '("org" "md"))
  )
