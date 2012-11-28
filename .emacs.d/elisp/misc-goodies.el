(provide 'misc-goodies)

(setq my-number 0)
(setq my-cha 0)

(defun my-macro-init (first cha)
  "Run grep via grep-find, Collect output in a buffer.
easily repeat a find command."

  (interactive 
   (list (read-number "first number: " 1)
	 (read-number "differnce: " 1)))
  (setq my-number first)
  (setq my-cha cha)
  )

(defun my-macro-insert ()
  "Run grep via grep-find, Collect output in a buffer.
easily repeat a find command."

  (interactive)
  (insert (format "%d" (+ my-number)))
  (setq my-number (+ my-number my-cha))
  )

(defun cplusplus-grep-find (directory regex)
  "Run grep via grep-find, Collect output in a buffer.
While find runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to.

This command uses a special history list for its arguments, so you can
easily repeat a find command."

  (interactive 
   (list (read-string "Run find in Directory: " ".")
	 (read-string "Search regex: " (current-word))))

  (grep-find (concat "find " 
		     directory 
		     " -regex \".*\\.\\(c\\|h\\|cpp\\)\" -type f -print0 | xargs -0 -e grep -nH -e '" 
		     regex 
		     "'")))

(defun grep-find-replace (directory regex rep-str)
  "Run grep via grep-find, Collect output in a buffer.
While find runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to.

This command uses a special history list for its arguments, so you can
easily repeat a find command."

  (interactive 
   (list (read-string "Run find in Directory: " ".")
	 (read-string "Search regex: " (current-word))
	 (read-string "Replace string: " (current-word))))

  (setq grep-find-command (concat "find " 
				  directory 
				  " -type f -print0 | xargs -0 -e grep -nH -e '" 
				  regex 
				  "'"))
  (grep-find grep-find-command)

  (setq filelist (shell-command-to-string (concat "find " 
						  directory 
						  " -type f -print0 | xargs -0 -e grep -nH -e '" 
						  regex 
						  "' -l")))

  (setq grep-find-replace-command (concat "sed -i \"s/" regex "/" rep-str "/g\" " filelist))
  (shell-command grep-find-replace-command))

(defun cplusplus-grep-find-replace (directory regex rep-str)
  "Run grep via grep-find, Collect output in a buffer.
While find runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to.

This command uses a special history list for its arguments, so you can
easily repeat a find command."

  (interactive 
   (list (read-string "Run find in Directory: " ".")
	 (read-string "Search regex: " (current-word))
	 (read-string "Replace string: " (current-word))))

  (setq grep-find-command (concat "find " 
				  directory 
				  " -regex \".*\\.\\(c\\|h\\|cpp\\)\" -type f -print0 | xargs -0 -e grep -nH -e \"" 
				  regex 
				  "\""))
  (grep-find grep-find-command)

  (setq filelist (shell-command-to-string (concat "find " 
						  directory 
						  " -regex \".*\\.\\(c\\|h\\|cpp\\)\" -type f -print0 | xargs -0 -e grep -nH -e \"" 
						  regex 
						  "\" -l")))

  (setq grep-find-replace-command (concat "sed -i \"s/" regex "/" rep-str "/g\" " filelist))
  (shell-command grep-find-replace-command))

(defun cplusplus-quick-grep-find ()
  "Run grep via grep-find, Collect output in a buffer.
While find runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to.

This command uses a special history list for its arguments, so you can
easily repeat a find command."

  (interactive)
  (setq word (current-word))
  (unless (string= word "")
    (grep-find (concat "find " 
		     ".."
		     " -regex \".*\\.\\(c\\|h\\|cpp\\)\" -type f -print0 | xargs -0 -e grep -nH -e '" 
		     word
		     "'"))))

(setq tbbr-md "all")
(defun toggle-tabbar-mode ()
  "Toggles tabbar modes - all buffers vs. defined in the `tabbar-buffer-groups'."
  (interactive)
  (if (string= tbbr-md "groups")
      (progn ;; then
	(setq tabbar-buffer-groups-function
	      (lambda ()
		(list "All")))
	(setq tbbr-md "all"))
    (progn ;; else
      ;;(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)
      (setq tabbar-buffer-groups-function 'tabbar-buffer-my-groups)
      (setq tbbr-md "groups"))))

(defun tabbar-buffer-my-groups ()
  "Return the list of group names the current buffer belongs to.
Return a list of one element based on major mode."
  (list
   (cond
    ((or (get-buffer-process (current-buffer))
         ;; Check if the major mode derives from `comint-mode' or
         ;; `compilation-mode'.
         (tabbar-buffer-mode-derived-p
          major-mode '(comint-mode compilation-mode)))
     "Process")
    ((or (member (buffer-name) '("*scratch*" "*Messages*")) 
	 (memq major-mode
           '(todo-mode fundamental-mode completion-list-mode ediff-meta-mode)))
     "Common")
    ((memq major-mode
           '(help-mode apropos-mode Info-mode Man-mode woman-mode))
     "Help")
    ((memq major-mode
           '(c-mode c++-mode asm-mode makefile-mode makefile-automake-mode
		    makefile-bsdmake-mode makefile-gmake-mode makefile-imake-mode
		    makefile-makepp-mode make-mode))
     "C Program")
    (t
     ;; Return `mode-name' if not blank, `major-mode' otherwise.
     (if (and (stringp mode-name)
              ;; Take care of preserving the match-data because this
              ;; function is called when updating the header line.
              (save-match-data (string-match "[^ ]" mode-name)))
         mode-name
       (symbol-name major-mode))
     ))))

(defun recentfopen ()
  "recent opened files"
  (interactive)
  (let* ((all-files recentf-list)
         (tocpl (mapcar (function 
                         (lambda (x) (cons (file-name-nondirectory x) x))) all-files))
         (prompt (append '("File name: ") tocpl))
         (fname (completing-read (car prompt) (cdr prompt) nil nil)))
    (find-file (cdr (assoc-ignore-representation fname tocpl)))))

(defvar my-fullscreen-p t "Check if fullscreen is on or off")

(defun my-non-fullscreen ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
	  ;; WM_SYSCOMMAND restore #xf120
	  (w32-send-sys-command 61728)
	(progn (set-frame-parameter nil 'width 82)
		   (set-frame-parameter nil 'fullscreen 'fullheight))))

(defun my-fullscreen ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
	  ;; WM_SYSCOMMAND maximaze #xf030
	  (w32-send-sys-command 61488)
	(set-frame-parameter nil 'fullscreen 'fullboth)))

(defun my-toggle-fullscreen ()
  (interactive)
  (setq my-fullscreen-p (not my-fullscreen-p))
  (if my-fullscreen-p
	  (my-non-fullscreen)
	(my-fullscreen)))

(defun my-forward-word ()
  "my forward word."
  (interactive)
  (if (looking-at "[\t\f ]")
      (skip-chars-forward "[\t\f ]")
    (skip-chars-forward "^[\r\n\t\f ]")))

(defun my-kill-word()
  "my kill ring save."
  (interactive)
  (if (looking-at "[\t\f ]")
      (skip-chars-forward "[\t\f ]")
    (skip-chars-backward "^[\r\n\t\f ]"))
  (let* ((beg (point))
	 (end (+ beg (skip-chars-forward "^[\r\n\t\f ]"))))
    (kill-region beg end)
    (skip-chars-forward "[\t\f ]")))

(defun my-kill-ring-save()
  "my kill ring save."
  (interactive)
  (if (looking-at "[\t\f ]")
      (skip-chars-forward "[\t\f ]")
    (skip-chars-backward "^[\r\n\t\f ]"))
  (let* ((beg (point))
	 (end (+ beg (skip-chars-forward "^[\r\n\t\f ]"))))
    (kill-ring-save beg end)
    (skip-chars-forward "[\t\f ]")))

(fset 'ip2sql
   [?\C-a ?\C-c ?\M-w ?\C-x ?o ?\C-x ?o ?I ?N ?S ?E ?R ?T ?  ?I ?N ?T ?O ?  ?I ?P ?D ?a ?t ?a ?  ?V ?A ?L ?U ?E ?S ?\( ?\' ?\C-y ?\' ?, ?  ?\' ?\C-x ?o ?\C-c ?\M-w ?\C-x ?o ?\C-y ?\C-c ?u ?\C-x ?o ?\C-y ?\' ?, ?  ?\' ?\C-x ?o ?\C-c ?\M-w ?\C-x ?o ?\C-y ?\C-a ?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?s ?t ?r ?i ?n ?g return ?' return ?' ?' return ?\C-a ?\C-c ?m ?\C-e ?\C-w ?\C-x ?o ?\C-y ?\' ?, ?  ?\' ?\C-x ?o ?\C-c ?m ?\C-e ?\M-w ?\C-x ?o ?\C-y ?\C-a ?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?s ?t ?r ?i ?n ?g return ?' return ?' ?' return ?\C-a ?\C-c ?m ?\C-e ?\C-w ?\C-x ?o ?\C-y ?\' ?\) ?\; return ?\C-x ?o ?\C-e ?\C-f])

(fset 'ip2sql2
   [?\C-a ?\C-c ?f ?\' ?, ?\C-c ?m ?\C-c ?f ?\C-w ?  ?\' ?\C-c ?f ?\' ?, ?\C-c ?m ?\C-c ?f ?\C-w ?  ?\' ?\C-c ?f ?\' ?, ?\C-c ?m ?\C-c ?f ?\C-w ?  ?\C-c ?m ?\C-e ?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?s ?t ?r ?i ?n ?g return ?\' return ?\' ?\' return ?\' ?\C-e ?\' ?\) ?\; ?\C-n])


;;; misc-goodies ends here

