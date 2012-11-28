;;; tramp-hack.el --- cedet hack for different platform

(require 'tramp)

;;; example for emacs24 in windows
;;; use it like this "C-x C-f /plinkxxx:: RET"

;;(add-to-list 'tramp-methods '("plinkxxx" (tramp-login-program "plink") (tramp-login-args (("192.168.1.101") ("-l" "root") ("-pw" "mypassord") ("-ssh"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c")) (tramp-password-end-of-line "xy") (tramp-default-port 22)))

;;; to avoid fuction 'tramp-compute-multi-hops' check tramp-login-args ("%h")
;;(defun tramp-compute-multi-hops (vec)
;;  "Expands VEC according to `tramp-default-proxies-alist'.
;;Gateway hops are already opened."
;;  (let ((target-alist `(,vec))
;;	(choices tramp-default-proxies-alist)
;;	item proxy)
;;
;;    ;; Look for proxy hosts to be passed.
;;    (while choices
;;      (setq item (pop choices)
;;	    proxy (eval (nth 2 item)))
;;      (when (and
;;	     ;; host
;;	     (string-match (or (eval (nth 0 item)) "")
;;			   (or (tramp-file-name-host (car target-alist)) ""))
;;	     ;; user
;;	     (string-match (or (eval (nth 1 item)) "")
;;			   (or (tramp-file-name-user (car target-alist)) "")))
;;	(if (null proxy)
;;	    ;; No more hops needed.
;;	    (setq choices nil)
;;	  ;; Replace placeholders.
;;	  (setq proxy
;;		(format-spec
;;		 proxy
;;		 (format-spec-make
;;		  ?u (or (tramp-file-name-user (car target-alist)) "")
;;		  ?h (or (tramp-file-name-host (car target-alist)) ""))))
;;	  (with-parsed-tramp-file-name proxy l
;;	    ;; Add the hop.
;;	    (add-to-list 'target-alist l)
;;	    ;; Start next search.
;;	    (setq choices tramp-default-proxies-alist)))))
;;
;;    ;; Handle gateways.
;;    (when (string-match
;;	   (format
;;	    "^\\(%s\\|%s\\)$" tramp-gw-tunnel-method tramp-gw-socks-method)
;;	   (tramp-file-name-method (car target-alist)))
;;      (let ((gw (pop target-alist))
;;	    (hop (pop target-alist)))
;;	;; Is the method prepared for gateways?
;;	(unless (tramp-file-name-port hop)
;;	  (tramp-error
;;	   vec 'file-error
;;	   "Connection `%s' is not supported for gateway access." hop))
;;	;; Open the gateway connection.
;;	(add-to-list
;;	 'target-alist
;;	 (vector
;;	  (tramp-file-name-method hop) (tramp-file-name-user hop)
;;	  (tramp-compat-funcall 'tramp-gw-open-connection vec gw hop) nil))
;;	;; For the password prompt, we need the correct values.
;;	;; Therefore, we must remember the gateway vector.  But we
;;	;; cannot do it as connection property, because it shouldn't
;;	;; be persistent.  And we have no started process yet either.
;;	(tramp-set-file-property (car target-alist) "" "gateway" hop)))
;;
;;    ;; Foreign and out-of-band methods are not supported for multi-hops.
;;    (when (cdr target-alist)
;;      (setq choices target-alist)
;;      (while choices
;;	(setq item (pop choices))
;;	(when
;;	    (or
;;	     (not
;;	      (tramp-get-method-parameter
;;	       (tramp-file-name-method item) 'tramp-login-program))
;;	     (tramp-get-method-parameter
;;	      (tramp-file-name-method item) 'tramp-copy-program))
;;	  (tramp-error
;;	   vec 'file-error
;;	   "Method `%s' is not supported for multi-hops."
;;	   (tramp-file-name-method item)))))
;;
;;    ;; In case the host name is not used for the remote shell
;;    ;; command, the user could be misguided by applying a random
;;    ;; hostname.
;;    (let* ((v (car target-alist))
;;	   (method (tramp-file-name-method v))
;;	   (host (tramp-file-name-host v)))
;;      (unless
;;	  (or
;;	   ;; There are multi-hops.
;;	   (cdr target-alist)
;;	   ;; The host name is used for the remote shell command.
;;	   (member
;;	    '("%h") (tramp-get-method-parameter method 'tramp-login-args))
;;	   ;; The host is local.  We cannot use `tramp-local-host-p'
;;	   ;; here, because it opens a connection as well.
;;	   (string-match tramp-local-host-regexp host))
;;	(tramp-error
;;	 v 'file-error
;;	 "Host `%s' looks like a remote host, `%s' can only use the local host"
;;	 host method)))
;;
;;    ;; Result.
;;    target-alist))

;;; example for emacs23 in windows
;;; use it like this "C-x C-f /plinkxxx:: RET"

;;(add-to-list 'tramp-methods
;;	     '("plinkxxx"
;;	       (tramp-login-program        "plink")
;;	       (tramp-login-args           (("192.168.1.101") ("-l" "root") ("-pw" "mypassword")
;;					    ("-ssh")))
;;	       (tramp-remote-sh            "/bin/sh")
;;	       (tramp-copy-program         nil)
;;	       (tramp-copy-args            nil)
;;	       (tramp-copy-keep-date       nil)
;;	       (tramp-password-end-of-line "xy") ;see docstring for "xy"
;;	       (tramp-default-port         22)))

;;; to avoid fuction 'tramp-compute-multi-hops' check tramp-login-args ("%h")

(provide 'tramp-hack)
