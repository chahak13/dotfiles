(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e")

(setq mu4e-maildir "~/Mail")

(setq mu4e-get-mail-command "mbsync -a")
(setq mu4e-update-interval (* 10 60))

(setq mu4e-index-lazy-check t)

(setq mu4e-contexts
      (list
       (make-mu4e-context
	   :name "Private"
	   :match-func (lambda (msg)
			 (when msg
			   (string-prefix-p "/chahakcr7" (mu4e-message-field msg :maildir))))
	   :vars
	   '(
	     (user-mail-address . "chahakcr7@gmail.com")
	     (user-full-name . "Chahak")
	     (mu4e-drafts-folder . "/chahakcr7/[Gmail]/Drafts")
	     (mu4e-sent-folder . "/chahakcr7/[Gmail]/Sent Mail")
	     (mu4e-refile-folder . "/chahakcr7/[Gmail]/All Mail")
	     (mu4e-trash-folder . "/chahakcr7/[Gmail]/Trash")
	     ))
       ;; Official Email
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

       ;; UT Austin email
       (make-mu4e-context
	:name "UT Austin"
	:match-func (lambda (msg)
		      (when msg
			(string-prefix-p "/chahakut" (mu4e-message-field msg :maildir))))
	:vars
	'(
	  (user-mail-address . "chahak@utexas.edu")
	  (user-full-name . "Chahak Mehta")
	  (mu4e-drafts-folder . "/chahakut/[Gmail]/Drafts")
	  (mu4e-sent-folder . "/chahakut/[Gmail]/Sent Mail")
	  (mu4e-refile-folder . "/chahakut/[Gmail]/All Mail")
	  (mu4e-trash-folder . "/chahakut/[Gmail]/Trash")
	  ))))
