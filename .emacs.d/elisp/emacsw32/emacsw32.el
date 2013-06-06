;;; emacsw32.el --- MS Windows style feeling

;; Copyright (C) 2004, 2005, 2006, 2007 by Lennart Borgman

;; Author:     Lennart Borgman <lennart DOT borgman DOT 073 AT student DOT lu DOT se>
;; Created: 2005-08-03
;; Modified by: wuyao721@163.com 2013-06-06
;; Version: 0.87
;; Last-Updated: 2008-01-21T20:21:32+0100 Mon
;; Keywords: convenience emulations w32
;; Features that might be required by this library:
;;
;;   `cl', `compile', `electric', `find-func',
;;   `grep', `hfyview', `menuacc', `noprint',
;;   ``timer', `tmm', `w32-grep',
;;   `w32-integ', `w32-meta', `w32shell'.
;;
;; This file does not do very much by itself, it serves more as a hub
;; for other modules. It uses Emacs customization to load the other
;; modules and it provides a bit of glue sometimes (between cua and
;; viper).  It also provides some keyboard keys (in
;; `emacsw32-mode') and adds some menu entries.
;;
;; Since this module is aimed mostly at users running MS Windows basic
;; documentation is provided in html format in the file
;; EmacsW32Util.html (which is the documentation for EmacsW32 where
;; this file is included).

;; To use this module put it in Emacs load-path and write at the end
;; of your .emacs (or in default.el):
;;
;;    (require 'emacsw32)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is not part of Emacs
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; To find out more about the GNU General Public License you can visit
;; Free Software Foundation's website http://www.fsf.org/.  Or, write
;; to the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
(require 'w32-grep)

(defun emacsw32-get-version ()
  (let ((auto-updated-version "1.56 2008-03-23"))
    auto-updated-version))

(defun emacsw32-version ()
  (interactive)
  (message "EmacsW32 Version %s" (emacsw32-get-version)))

;;_____________________________________________________
;;
;; Custom
;;_____________________________________________________

(defgroup emacsw32 nil
  "Selected options for users with experience of MS Windows.
Those options are collected here for your convenience."
  :group 'convenience
  :group 'environment
  )

;; Add to EmacsW32 group:
(custom-add-to-group 'emacsw32 'cua-mode        'custom-variable)
(custom-add-to-group 'emacsw32 'recentf-mode    'custom-variable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Specific to MS Window
(when (eq system-type 'windows-nt)
  (require 'w32shell nil t)
  (require 'w32-integ nil t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; These are the keys for the w32-send-sys-command
  ;; (stored here for your convenience)
  ;;SC_SIZE 0xF000
  ;;SC_MOVE 0xF010
  ;;SC_MINIMIZE 0xF020
  ;;SC_MAXIMIZE 0xF030
  ;;SC_NEXTWINDOW 0xF040
  ;;SC_PREVWINDOW 0xF050
  ;;SC_CLOSE 0xF060 (61536)
  ;;SC_VSCROLL 0xF070
  ;;SC_HSCROLL 0xF080
  ;;SC_MOUSEMENU 0xF090
  ;;SC_KEYMENU 0xF100
  ;;SC_ARRANGE 0xF110
  ;;SC_RESTORE 0xF120
  ;;SC_TASKLIST 0xF130
  ;;SC_SCREENSAVE 0xF140
  ;;SC_HOTKEY 0xF150
  ;;SC_DEFAULT 0xF160
  ;;SC_MONITORPOWER 0xF170
  ;;SC_CONTEXTHELP 0xF180
  ;;SC_SEPARATOR 0xF00F

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Frame w32 handling
  (defun emacsw32-restore-frame ()
    "Restore a minimized frame"
    (interactive)
    (w32-send-sys-command 61728))

  (defun emacsw32-maximize-frame ()
    "Maximize the current frame"
    (interactive)
    (w32-send-sys-command 61488))

  (defun emacsw32-make-frame-maximize (frame)
    (when (and emacsw32-max-frames
               window-system)
      (select-frame frame)
      (emacsw32-maximize-frame)))

  (defun emacsw32-window-setup-maximize ()
    ;;(run-with-idle-timer 2 nil 'emacsw32-maximize-frame)
    (emacsw32-maximize-frame))

  (defcustom emacsw32-max-frames nil
    "Non-nil means maximize new frames."
    :tag "Maximize new frames"
    :set (lambda (symbol value)
           (set-default symbol value)
           (unless noninteractive
             (if value
                 (add-hook 'after-make-frame-functions
                           'emacsw32-make-frame-maximize)
               (remove-hook 'after-make-frame-functions
                            'emacsw32-make-frame-maximize))
             (require 'find-func)
             (unless nil ;(assoc (find-library-name "default") load-history)
               ;; Run with the last hook and idle timer to avoid some
               ;; strange problems at startup
               (when value
                 (add-hook 'window-setup-hook
                           'emacsw32-window-setup-maximize t)))))
    :group 'emacsw32
    :type  'boolean)

  ) ;;when windows-nt


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A windowish frame title
(defvar emacsw32-old-frame-title-format nil)

(defcustom emacsw32-style-frame-title nil
  "If non-nil use a frame title consisting of buffer name followed by Emacs.
Also save the current `frame-title-format' to
`emacsw32-old-frame-title-format'.  When nil restore this value.

Notice: Frame in Emacs terminology is what normally is what a user
would call a window in MS Windows."
  :tag "W32 style window/frame title"
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (if value
             (progn
               (setq emacsw32-old-frame-title-format frame-title-format)
               (setq frame-title-format "%b - Emacs"))
           (when emacsw32-old-frame-title-format
             (unless (eq frame-title-format emacsw32-old-frame-title-format)
               (setq frame-title-format emacsw32-old-frame-title-format)))))
  :group 'emacsw32)

;; (defun emacsw32-find-file ()
;;   (interactive)
;;   (let ((last-nonmenu-event nil)
;;         (use-dialog-box t)
;;         (use-file-dialog t))
;;     (call-interactively 'find-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Ready:
(provide 'emacsw32)

;;; emacsw32.el ends here
