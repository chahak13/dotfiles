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
  (setq mu4e-view-use-gnus t)
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
