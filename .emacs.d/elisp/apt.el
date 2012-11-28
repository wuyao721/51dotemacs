;;;  apt-mode for emacs
;;  Copyright (C) 2001, 2002, 2003 Junichi Uekawa

;;  This program is free software; you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation; either version 2 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program; if not, write to the Free Software
;;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


;; This mode allows easy running of Debian apt-related software from emacs.
;; for the emacs addicts' eyes only!!
;; 31 Dec 2001 Junichi Uekawa <dancer@netfort.gr.jp>


;;The homepage can be found at : http://www.netfort.gr.jp/~dancer/software/apt-mode.html
;;The author can be contacted at : <dancer@netfort.gr.jp> or <dancer@debian.org> or <dancer@mikilab.doshisha.ac.jp>

;;; Code

(defgroup apt nil "Apt mode"
  :group 'tools
  :prefix "apt-mode-")

(defcustom apt-mode-superuser-command-string "sudo" 
  "*The command that provides superuser privilage for running apt and dpkg.
Such as sudo.
This needs to be noninteractive."
  :group 'apt
  :type 'string)

;;;###autoload
(defun apt ()
  "Create a new buffer with the APT mode."
  (interactive)
  (switch-to-buffer "*apt*")
  (kill-region (point-min) (point-max))
  (apt-mode)
  (insert 
   "APT front-end mode for Emacs\n\n"
   "Copyright 2002, 2003 Junichi Uekawa\n\n"
   "\ts - search\n"
   "\tS - showpkg\n"
   "\ta - show\n"
   "\td - show simulated dist-upgrade\n"
   "\tl - list installed packages\n"
   "\ti - install the package (simulate)\n"
   "\tr - remove the package (simulate)\n"
   "\tI - install the package \n"
   "\tR - remove the package \n"
   "\tu - update the apt database, with update\n"
;   "\th - hold the package\n"
;   "\tH - unhold the package\n"
   "\tq - quit\n"
   )
  )

(defun dpkg-l-mode ()
  "Major mode for dpkg -l output buffers.
Special commands: \\{apt-mode-map}
  Turning on `dpkg-l-mode' runs the hook `dpkg-l-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map apt-mode-map)
  (setq mode-name "dpkg-l")
  (setq major-mode 'apt-mode)
  (make-local-variable 'font-lock-defaults)
  (set-syntax-table dpkg-l-mode-syntax-table)
  (setq font-lock-defaults 
	'(
					;keywords start here
	  (("^ii" . font-lock-keyword-face) ;dpkg -l   output
	   ("^..  \\([^ ]*\\) +\\([^ ]*\\)" (1 font-lock-function-name-face) (2 font-lock-keyword-face)))
	  nil		;keywords-only
	  nil		;case-fold
	  ()		;syntax-alist
	  ))
  (run-hooks 'dpkg-l-mode-hook))

(defvar dpkg-l-mode-syntax-table nil "Syntax table for dpkg-l mode.")
(if dpkg-l-mode-syntax-table
         ()              ; Do not change the table if it is already set up.
       (setq dpkg-l-mode-syntax-table (make-syntax-table)))
  
(defun apt-mode ()
  "Major mode for apt controls.
Special commands:
\\{apt-mode-map}
  Turning on `apt-mode' runs the hook `apt-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map apt-mode-map)
  (setq mode-name "Apt")
  (setq major-mode 'apt-mode)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults 
	'(
					;keywords start here
	  (("^\\([^ ]*\\) - \\(.*\\)$" (1 font-lock-warning-face) (2 font-lock-keyword-face)) ;search results
	   ("^[-A-Za-z5]*:" . font-lock-keyword-face) ; Description: etc. lines
	   ("^\\(Inst\\|Conf\\|Remv\\) \\([^ ]*\\)" 
	    (1 font-lock-warning-face) 
	    (2 font-lock-keyword-face))) ; apt-get dist-upgrade fontify
	  nil		;keywords-only
	  nil		;case-fold
	  ()		;syntax-alist
	  ))
  (run-hooks 'apt-mode-hook))

(defvar apt-mode-map nil    ; Create a mode-specific keymap.
  "Keymap for Apt mode.")

(if apt-mode-map
    ()              ; Do not change the keymap if it is already set up.
  (setq apt-mode-map (make-sparse-keymap))
  (define-key apt-mode-map "f" 'apt-file-search)
  (define-key apt-mode-map "a" 'apt-cache-show)
  (define-key apt-mode-map "s" 'apt-cache-search)
  (define-key apt-mode-map "S" 'apt-cache-showpkg)
  (define-key apt-mode-map "q" 'apt-mode-kill-buffer)
  (define-key apt-mode-map "d" 'apt-get-simulate-dist-upgrade)
  (define-key apt-mode-map "i" 'apt-get-install-simulate)
  (define-key apt-mode-map "r" 'apt-get-remove-simulate)
  (define-key apt-mode-map "I" 'apt-get-install)
  (define-key apt-mode-map "R" 'apt-get-remove)
  (define-key apt-mode-map "u" 'apt-get-update)
  (define-key apt-mode-map "l" 'apt-dpkg-l)
;  (define-key apt-mode-map "h" 'apt-dpkg-hold)
;  (define-key apt-mode-map "H" 'apt-dpkg-unhold)
  )

(defun apt-mode-kill-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer))
)

;;;###autoload
(defun apt-cache-search () 
  "Apt cache search function.  Interactively search the package database with a regular expression."
  (interactive)
  (let* ((searchregex (read-string "Apt-cache search regex: " (apt-mode-current-word) ))
	 (searchregexbufname (concat "*apt-cache-search-" searchregex "*"))
         )
    (switch-to-buffer searchregexbufname)
    (apt-mode)
    (kill-region (point-min) (point-max))
    (call-process "apt-cache" nil 
		  searchregexbufname
		  nil "search" searchregex)
    (beginning-of-buffer)))

;;;###autoload
(defun apt-cache-show (packagename)
  "Apt cache show function for PACKAGENAME.  Interactively show a package description."
  (interactive (list (read-string "Apt-cache show packagename: " (apt-mode-current-word))))
  (let* ((packagenamebuf (concat "*apt-cache-show-" packagename "*")))
    (switch-to-buffer packagenamebuf)
    (apt-mode)
    (kill-region (point-min) (point-max))
    (call-process "apt-cache" nil 
		  packagenamebuf
		  nil "show" packagename)
    (beginning-of-buffer)))

;;;###autoload
(defun apt-cache-showpkg ()
  "Apt cache showpkg function.  Interactively show a package dependency info."
  (interactive)
  (let* ((packagename (read-string "Apt-cache showpkg packagename: " (apt-mode-current-word)))
	 (packagenamebuf (concat "*apt-cache-showpkg*"))
         )
    (switch-to-buffer packagenamebuf)
    (apt-mode)
    (kill-region (point-min) (point-max))
    (call-process "apt-cache" nil 
		  packagenamebuf
		  nil "showpkg" packagename)
    (beginning-of-buffer)))

;;;###autoload
(defun apt-file-search () 
  "apt-file search function.  Interactively search the package database with a regular expression."
  (interactive)
  (let* ((searchregex (read-string "Apt-file search regex: " (apt-mode-current-word) ))
	 (searchregexbufname (concat "*apt-file-search-" searchregex "*"))
         )
    (switch-to-buffer searchregexbufname)
    (apt-mode)
    (kill-region (point-min) (point-max))
    (call-process "apt-file" nil 
		  searchregexbufname
		  nil "search" searchregex)
    (beginning-of-buffer)))

;;;###autoload
(defun apt-get-install ()
  "Apt-get install, using sudo to gain root."
  (interactive)
  (let* ((packagename (read-string "Package to install: " (apt-mode-current-word)))
	 (agi-name "apt-get-install")
	 (packagenamebuf (concat "*" agi-name "*")))
    (switch-to-buffer packagenamebuf)
    (insert "\n --- start\n\n")
    (switch-to-buffer (make-comint agi-name apt-mode-superuser-command-string 
		 nil "apt-get" "install" packagename))
    (beginning-of-buffer)))

;;;###autoload
(defun apt-get-remove ()
  "Apt-get remove, using sudo to gain root."
  (interactive)
  (let* ((packagename (read-string "Package to remove: " (apt-mode-current-word)))
	 (packagenamebuf (concat "*apt-get-remove*"))
	 )
    (switch-to-buffer packagenamebuf)
    (apt-mode)
    (kill-region (point-min) (point-max))
    (call-process apt-mode-superuser-command-string nil 
		  packagenamebuf
		  t "apt-get" "-y" "remove" packagename)
    (insert "\n --- finished\n\n")
    (beginning-of-buffer)))

;;;###autoload
(defun apt-get-update ()
  "Apt-get install, using sudo to gain root."
  (interactive)
  (let* ((packagenamebuf (concat "*apt-get-update*"))
	 )
    (switch-to-buffer packagenamebuf)
    (apt-mode)
    (kill-region (point-min) (point-max))
    (call-process apt-mode-superuser-command-string nil 
		  packagenamebuf
		  t "apt-get" "update")
    (insert "\n --- finished\n\n")
    (beginning-of-buffer)))

(defun apt-get-simulate-dist-upgrade ()
  "Show apt get dist upgrade simulation, using sudo to gain root."
  (interactive)
  (let* ((packagenamebuf (concat "*apt-get-distupgrade*"))
			)
    (switch-to-buffer packagenamebuf)
    (apt-mode)
    (kill-region (point-min) (point-max))
    (call-process apt-mode-superuser-command-string nil 
		  packagenamebuf
		  nil "apt-get" "dist-upgrade" "-s")
    ))

(defcustom apt-dpkg-l-width "120" "*The COLUMN variable for dpkg -l."
  :group 'apt
  :type 'string)
(defun apt-dpkg-l ()
  "Use dpkg to list the current packages listing."
  (interactive)
  (let* ((packagenamebuf (concat "*apt-dpkg-l*")))
    (switch-to-buffer packagenamebuf)
    (dpkg-l-mode)
    (kill-region (point-min) (point-max))
    (setenv "COLUMNS" apt-dpkg-l-width)
    (call-process "dpkg" nil 
		  packagenamebuf
		  nil "-l" )
    ))
  
(defun apt-get-install-simulate ()
  "Show apt get install simulation, using sudo to gain root."
  (interactive)
  (let* ((packagename (read-string "Package to install (simulate):" (apt-mode-current-word)))
	 (packagenamebuf (concat "*apt-get-install*"))
	 )
    (switch-to-buffer packagenamebuf)
    (apt-mode)
    (kill-region (point-min) (point-max))
    (call-process apt-mode-superuser-command-string nil 
		  packagenamebuf
		  nil "apt-get" "install" "-s" packagename)
    (beginning-of-buffer)))

(defun apt-get-remove-simulate ()
  "Apt-get remove simulation, using sudo to gain root."
  (interactive)
  (let* ((packagename (read-string "Package to remove (simulate): " (apt-mode-current-word)))
	 (packagenamebuf (concat "*apt-get-remove*"))
	 )
    (switch-to-buffer packagenamebuf)
    (apt-mode)
    (kill-region (point-min) (point-max))
    (call-process apt-mode-superuser-command-string nil 
		  packagenamebuf
		  nil "apt-get" "remove" "-s" packagename)
    ))

(defun apt-mode-current-word ()
  "Returns the current word, in apt terms."
  (save-excursion
    (let* (start-pos end-pos)
      (if (setq start-pos (re-search-backward "[ \t\n]" nil t nil))
	  (setq start-pos (+ start-pos 1))
	(if (setq start-pos (re-search-backward "^"))
	    t
	  (error "Cannot find start of packagename")))
      (forward-char)
      (if (setq end-pos (re-search-forward "[ \t\n]" nil t nil))
	  (setq end-pos (- end-pos 1))
	(if (setq end-pos (re-search-forward "$"))
	    t
	  (error "Cannot find end of packagename")))
      (buffer-substring start-pos end-pos))))

(provide 'apt)
