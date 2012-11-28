;;; switch-h-cpp.el --- switch between C/C++ .h file and .cxx file.

;; Copyright (C) 2006, 2007, 2010 Anton V. Belyaev
;; Author: Anton V. Belyaev <anton.belyaev at the gmail.com>

;; Get it from eassist.el, modify by WuYao(wuyao721@163.com)

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

(defun neassist-string-without-last (string n)
  "This function truncates from the STRING last N characters."
  (substring string 0 (max 0(- (length string) n))))

(defvar neassist-dir-switches '(("h" . (".." "../src"))
			       ("hpp" . (".." "../src"))
			       ("cpp" . ("include" "../include"))
			       ("c" . ("include" "../include"))
			       ("C" . (".." "../src"))
			       ("H" . (".." "../src"))
			       ("cc" . ("include" "../include")))
  "This variable defines possible switches for `neassist-switch-h-cpp' function.
Its format is list of (from . (to1 to2 to3...)) elements.  From and toN are
strings which are extentions of the files.")

(defvar neassist-header-switches '(("h" . ("cpp" "cc" "c"))
				  ("hpp" . ("cpp" "cc"))
				  ("cpp" . ("h" "hpp"))
				  ("c" . ("h"))
				  ("C" . ("H"))
				  ("H" . ("C" "CPP" "CC"))
				  ("cc" . ("h" "hpp")))
  "This variable defines possible switches for `neassist-switch-h-cpp' function.
Its format is list of (from . (to1 to2 to3...)) elements.  From and toN are
strings which are extentions of the files.")

;;;###autoload
(defun neassist-switch-h-cpp ()
  "Switch header and body file according to `neassist-header-switches' var.
The current buffer's file name extention is searched in
`neassist-header-switches' variable to find out extention for file's counterpart,
for example *.hpp <--> *.cpp."
  (interactive)
  (let* ((ext (file-name-extension (buffer-file-name)))
         (base-name (neassist-string-without-last (buffer-name) (length ext)))
         (base-path (neassist-string-without-last (buffer-file-name) (length ext)))
	 (dir-name (neassist-string-without-last (buffer-file-name) (length (buffer-name))))
         (count-dir (cdr (find-if (lambda (i) (string= (car i) ext)) neassist-dir-switches)))
         (count-ext (cdr (find-if (lambda (i) (string= (car i) ext)) neassist-header-switches))))
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
        (message "There is no corresponding pair (header or body-) file.")))
     (t
      (message "It is not a header or body file! See neassist-header-switches variable.")))))

(provide 'switch-h-cpp)
