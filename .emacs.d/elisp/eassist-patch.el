;(require 'eassist)
(require 'ido)

;; ================================== My STRING utils ========================
(defun eassist-string-without-last (string n)
  "This function truncates from the STRING last N characters."
  (substring string 0 (max 0(- (length string) n))))

(defun eassist-string-ends-with (string end)
  "Check whether STRING ends with END substring."
  (string= end (substring string (- (length end)))))

(defvar eassist-dir-switches '(("h" . (".." "../src"))
			       ("hpp" . (".." "../src"))
			       ("cpp" . ("include" "../include"))
			       ("c" . ("include" "../include"))
			       ("C" . (".." "../src"))
			       ("H" . (".." "../src"))
			       ("cc" . ("include" "../include")))
  "This variable defines possible switches for `eassist-switch-h-cpp' function.
Its format is list of (from . (to1 to2 to3...)) elements.  From and toN are
strings which are extentions of the files.")

(defvar eassist-header-switches '(("h" . ("cpp" "cc" "c"))
				  ("hpp" . ("cpp" "cc"))
				  ("cpp" . ("h" "hpp"))
				  ("c" . ("h"))
				  ("C" . ("H"))
				  ("H" . ("C" "CPP" "CC"))
				  ("cc" . ("h" "hpp")))
  "This variable defines possible switches for `eassist-switch-h-cpp' function.
Its format is list of (from . (to1 to2 to3...)) elements.  From and toN are
strings which are extentions of the files.")

(defun eassist-switch-h-cpp ()
  "Switch header and body file according to `eassist-header-switches' var.
The current buffer's file name extention is searched in
`eassist-header-switches' variable to find out extention for file's counterpart,
for example *.hpp <--> *.cpp."
  (interactive)
  (let* ((ext (file-name-extension (buffer-file-name)))
         (base-name (eassist-string-without-last (buffer-name) (length ext)))
         (base-path (eassist-string-without-last (buffer-file-name) (length ext)))
	 (dir-name (eassist-string-without-last (buffer-file-name) (length (buffer-name))))
         (count-dir (cdr (find-if (lambda (i) (string= (car i) ext)) eassist-dir-switches)))
         (count-ext (cdr (find-if (lambda (i) (string= (car i) ext)) eassist-header-switches))))
    (cond
     (count-ext
      (unless
          (or
           (loop for b in (mapcar (lambda (i) (concat base-name i)) count-ext)
		 when (bufferp (get-buffer b)) return
 		 (if (get-buffer-window b)
 		     (switch-to-buffer-other-window b)
 		   (if (get-buffer-window b t)
 		       (switch-to-buffer-other-frame b)
 		     (switch-to-buffer b))))
           (loop for c in (mapcar (lambda (count-ext) (concat base-path count-ext)) count-ext)
                 when (file-exists-p c) return (find-file c))
	   (loop for d in count-dir
                 when (loop for e in (mapcar
				 (lambda (count-ext) (concat dir-name d "/" base-name count-ext))
				 count-ext)
		       when (file-exists-p e) return (find-file e)) return t))
        (message "There is no corresponding pair (header or body) file.")))
     (t
      (message "It is not a header or body file! See eassist-header-switches variable.")))))

(defun buffer-function-tags ()
  "Return all function tags from the current buffer using Semantic API.
The function first gets all toplevel function tags from the current buffer.
Then it searches for all toplevel type tags and gets all function tags that
are children to toplevel type tags.  Secondlevel function (member) tags are
annotated (without side effect) with :parent attribute to have the same
structure as toplevel function tags.
   Copy from eassist-function-tags."
  (nconc
   ;; for C++/C
   (semantic-find-tags-by-class 'function (semantic-something-to-tag-table (current-buffer)))
   ;; for Java and Python: getting classes and then methods for each class.
   ;; Adding parent property for each method, beacause semantic does not provide parents for
   ;; methods which are inside body of the class. This is true for Java class methods,
   ;; for C++ header definitions and for Python class methods.
   (mapcan
    (lambda (type)
      (mapcar
       (lambda (tag) (semantic-tag-put-attribute-no-side-effect tag :parent (semantic-tag-name type)))
       (semantic-find-tags-by-class 'function (semantic-tag-type-members type))))
    (semantic-find-tags-by-class 'type (semantic-something-to-tag-table (current-buffer))))))

(defun ido-jump-to-method ()
  "Find and jump to a method using Ido."
  (interactive)
  (let* ((pointer)
	 (method-tags (buffer-function-tags))
	 (method-names (mapcar 'semantic-tag-name
			      method-tags))
	 (method-names
	  (remove-duplicates method-names
			     :test #'string=))
	 (method-name (ido-completing-read "Choose Function: "
					   method-names
					   nil
					   t)))
    (dolist (tags method-tags pointer)
      (when (string= method-name (semantic-tag-name tags))
	(setq pointer (semantic-tag-start tags))
	(goto-char pointer)))))

(provide 'eassist-patch)
