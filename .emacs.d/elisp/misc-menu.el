
(provide 'misc-menu)

;;; popup-menu-edit
(defun menu-copy() 
  "Wrapper to call `copy-region-as-kill' from menu."
  (interactive)
  (when mark-active 'copy-region-as-kill))

(defun menu-cut() 
  "Wrapper to call `kill-region' from menu."
  (interactive)
  (when (and mark-active (not buffer-read-only)
             (call-interactively 'kill-region))))

(defun menu-paste() 
  "Wrapper to call `yank' from menu."
  (interactive)
  (when (not buffer-read-only) (call-interactively 'yank)))

(defvar menu-edit
  (let ((menu (make-sparse-keymap "Commands")))
    (define-key menu [menu-copy] (cons "Copy" 'copy-region-as-kill))
    (define-key menu [menu-cut] (cons "Cut" 'kill-region))
    (define-key menu [menu-paste] (cons "Paste" 'yank))
    (define-key menu [--] (cons "--" 'nil)) ;separator
    (define-key menu [dabbrev-expand] (cons "Complete word" 'dabbrev-expand))
    (define-key menu [undo] (cons "Undo" 'undo))
    (define-key menu [redo] (cons "Redo" 'redo))
    (setcdr menu (nreverse (cdr menu)))
    menu))

(defun popup-menu-edit() 
  "Run the command selected from `menu-edit'."
  (interactive)
  (call-interactively (or (car (x-popup-menu t menu-edit)) 'ignore)))

;;; popup-menu-edit ends here

;;; popup-menu-insert

(defun menu-insert-time ()
  "insert current time"
  (interactive)
   (insert (format-time-string "%Y/%m/%d %H:%M:%S" (current-time))))

(defun menu-insert-func-template ()
  "insert currnt time"
  (interactive)
   (insert "int ()\n{\n   int     ret    = ERROR;\n\n   ret = OK;\n  end:\n   return ret;\n}\n"))

(defun menu-insert-muse ()
  "insert muse"
  (interactive)
  (insert "#title 
#author 

preface

reference:

<contents>

* 

* "))

(defun menu-insert-macro-header ()
  "insert .h MACRO"
  (interactive)
  (setq str (concat (buffer-name)))
  (dotimes (i (length str))
    (if (or (eq (elt str i) 46) (eq (elt str i) 45)) ;46 refer to '.', 45 to '-'
	(aset str i 95)))			     ;95 refer to '_'
  (setq str (concat "_" (upcase str)  "_"))
  (beginning-of-buffer)
  (insert "#ifndef " str "\n" "#define " str "\n")
  (end-of-buffer)
  (insert "\n" "#endif " "/* end of " (buffer-name) " */")
  (save-buffer))

(defun menu-insert-macro-if ()
  "insert currnt time"
  (interactive)
   (insert "#if \n#else \n#endif"))

(defun menu-insert-outline-header ()
  "insert outline file header line"
  (interactive)
  (beginning-of-buffer)
  (insert "                                 Hey Emacs, this is -*- outline -*- mode")
  (insert "\n"))

(defvar menu-insert
  (let ((menu (make-sparse-keymap "Commands")))
    (define-key menu [menu-insert-time] (cons "Current time" 'menu-insert-time))
    (define-key menu [menu-insert-muse] (cons "Current muse" 'menu-insert-muse))
    (define-key menu [menu-insert-outline-header] (cons "outline header" 'menu-insert-outline-header))
    (define-key menu [menu-insert-macro-header] (cons ".h file macro" 'menu-insert-macro-header))
    (define-key menu [doxymacs-insert-file-comment] (cons "File Comment" 'doxymacs-insert-file-comment))
    (define-key menu [doxymacs-insert-function-comment] (cons "Function Comment" 'doxymacs-insert-function-comment))
    (define-key menu [doxymacs-insert-grouping-comment] (cons "Grouping Comment" 'doxymacs-insert-grouping-comment))
    (define-key menu [doxymacs-insert-multiline-comment] (cons "Multiline Comment" 'doxymacs-insert-multiline-comment))
    (define-key menu [--] (cons "--" 'nil)) ;separator
    (setcdr menu (nreverse (cdr menu)))
    menu))

(defun popup-menu-insert() 
  "Run the command selected from `menu-insert'."
  (interactive)
  (call-interactively (or (car (x-popup-menu t menu-insert)) 'ignore)))

(defun popup-menu-bookmark ()
  (interactive)
  (bookmark-jump (x-popup-menu 
                  (list '(400 200) (selected-window)) 
                  (bookmark-menu-build-paned-menu 
                   "Select bookmark" 
                   (bookmark-all-names)))))

(defun bookmark-menu-build-paned-menu (name entries)
  "Build a multi-paned menu named NAME from the strings in ENTRIES.
That is, ENTRIES is a list of strings which appear as the choices
in the menu.  The number of panes depends on the number of entries.
The visible entries are truncated to `bookmark-menu-length', but the
strings returned are not."
  (let* ((f-height (/ (frame-height) 2))
         (pane-list
          (let (temp-pane-list
                (iter 0))
            (while entries
              (let (lst
                    (count 0))
                (while (and (< count f-height) entries)
                  (let ((str (car entries)))
                    (setq lst (cons
                               (cons
                                (if (> (length str) bookmark-menu-length)
                                    (substring str 0 bookmark-menu-length)
                                  str)
                                str)
                               lst))
                    (setq entries (cdr entries))
                    (setq count (1+ count))))
                (setq iter (1+ iter))
                (setq
                 temp-pane-list
                 (cons
                  (cons
                    (format "-*- %s (%d) -*-" name iter)
                   (nreverse lst))
                  temp-pane-list))))
            (nreverse temp-pane-list))))

    ;; Return the menu:
    (cons (concat "-*- " name " -*-") pane-list)))

(defun my-insert-namespace (start end)
  "Inserts namespace around the current region."
  (interactive "*r")

  (let* ((starter  "namespace WGTP {\n")
	 (ender  "} // namespace WGTP\n"))
    (save-excursion
      (goto-char end)
      (end-of-line)
      (insert ender)
      (goto-char start)
      (beginning-of-line)
      (insert starter))))

;;(setq menu-insert '("insert" 
;;	     ("insert" 
;;	      ("file comment" . "(doxymacs-insert-file-comment)")
;;	      ("function comment" . "(doxymacs-insert-function-comment)")
;;	      ("grouping comments" . "(doxymacs-insert-grouping-comments)")
;;	      ("multiline comment" . "(doxymacs-insert-blank-multiline-comment)")
;;	      )))
;;
;;(defun eval-menu-item (menu)
;;  "Show a popup menu of commands. See also `eval-menu-item'."
;;  (eval-expression 
;;   (car (read-from-string 
;;	 (x-popup-menu t menu)))))
;;
;;(global-set-key (kbd "C-c i") '(lambda nil (interactive) (eval-menu-item menu-insert)))

;;; popup-menu-insert ends here

