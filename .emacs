;;; 51dotemacs.el --- Display a tab bar in the header line

;; Copyright (C) 2011, 2012, 2013 WuYao

;; Author: WuYao <wuyao721@163.com>
;; Maintainer: WuYao <wuyao721@163.com>
;; Keywords: 51dotemacs

;;; key binding
;; [f4]                              kill-this-buffer
;; [f9]                              repeat
;; %                                 match-paren
;; M-y                               yank-pop
;; C-o                               lambda: open a new line at this line
;; C-z                               nil (for screen)
;; C-c b                             popup-menu-bookmark
;; C-c i                             popup-menu-insert
;; C-c j                             dired-jump
;; C-c l                             find-file
;; C-c o                             recentf-ido-find-file
;; C-c r                             redo
;; C-c f                             find-dired
;; C-c g                             grep-find
;; C-c G                             cplusplus-grep-find
;; C-c t                             grep-find-replace
;; C-c T                             cplusplus-grep-find-replace
;; C-c m                             set-mark-command
;; C-c u                             undo
;; C-c h                             tabbar-press-home
;; C-c d l                           dictionary-lookup-definition
;; C-c d s                           dictionary-search
;; C-c d m                           dictionary-match-words
;; C-c p a                           bashdb
;; C-c p C                           calculator
;; C-c p c                           compile
;; C-c p e                           eshell
;; C-c p g                           gdb
;; C-c p i                           info
;; C-c p m                           mail
;; C-c p p                           org2blog/wp-mode
;; C-c p P                           org2blog/wp-new-entry
;; C-c p r                           rot13-region
;; C-c p s                           shell-toggle-cd
;; C-c p t                           lambda
;; C-c p v                           svn-status
;; C-c p w                           woman
;; C-c p 3                           w3m
;; C-x C-b                           ibuffer                               
;; C-c C-j                           tabbar-forward-group
;; C-c C-k                           tabbar-backward-group
;; C-c C-h                           tabbar-backward-tab
;; C-c C-l                           tabbar-forward-tab
;; C-x C-e                           eval-region
;; C-c M-d                           my-kill-word
;; C-c M-f                           my-forward-word
;; C-c M-w                           my-kill-ring-save
;; C-<tab>                           tabbar-forward-tab
;; C-S-<iso-lefttab>                 tabbar-backward-tab
;; <C-S-tab>                         tabbar-backward-tab
;; <wheel-down>                      lambda: scroll up 2 line
;; <wheel-up>                        lambda: scroll down 2 line
;; [mouse-4]                         lambda: scroll down 2 line
;; [mouse-5]                         lambda: scroll up 2 line

(defconst 51dotemacs-version "1.51")

(setq my-lisp-root "~/.emacs.d/elisp")

;; set load-path
(setq load-path
      (append load-path (list my-lisp-root 
		    (concat my-lisp-root "/" "emacs-goodies-el")
		    (concat my-lisp-root "/" "doxymacs" )
		    (concat my-lisp-root "/" "dictionary")
		    (concat my-lisp-root "/" "w3m-el-snapshot")
		    (concat my-lisp-root "/" "muse")
		    (concat my-lisp-root "/" "org2blog")
		    (concat my-lisp-root "/" "wubi"))))

;; Appearance effects
;; 
(setq inhibit-startup-message t)                      ;forbid startup message
(setq display-time-day-and-date t)                    ;day, date and time
(display-time)                                        ;time on status bar
(column-number-mode 1)                                ;column number on status bar
(show-paren-mode 1)                                   ;show the corresponding parenthesis
(tool-bar-mode -1)                                    ;no tool bar
(menu-bar-mode 1)                                     ;remain menu bar
(global-auto-revert-mode 1)			      ;auto revert
(transient-mark-mode t)                               ;select text high light
(global-font-lock-mode t)                             ;key word colorfull
(setq frame-title-format "emacs@%b%@")                ;title name format
(delete-selection-mode 1)                             ;delete selection
(auto-image-file-mode t)                              ;show image when cross them
;(set-language-environment "UTF-8")
;(set-language-environment "Chinese-GBK")
;; Appearance effects

;;; Control
;; 
(ido-mode 1)					      ;ido mode
(global-set-key [f4] 'kill-this-buffer)
(global-set-key [f9] 'repeat)
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "[\[({]") (forward-sexp))
	((looking-back "[\])}]") (backward-sexp))
	(t (self-insert-command (or arg 1)))))
(global-set-key (kbd "C-o")
		'(lambda ()
		   (interactive)
		   (beginning-of-line 1)
		   (open-line 1)))
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-z") 'nil)
;;(global-set-key (kbd "C-<tab>") 'hippie-expand)
(global-set-key (kbd "C-c l") 'find-file)
(global-set-key (kbd "C-c m") 'set-mark-command)
(global-set-key (kbd "C-x C-e") 'eval-region)
(global-set-key (kbd "C-c M-d") 'my-kill-word)
(global-set-key (kbd "C-c M-f") 'my-forward-word)
(global-set-key (kbd "C-c M-w") 'my-kill-ring-save)
(global-set-key (kbd "C-c p C") 'calculator)
(global-set-key (kbd "C-c p e") 'eshell)
(global-set-key (kbd "C-c p c") 'compile)
(global-set-key (kbd "C-c p g") 'gdb)
(global-set-key (kbd "C-c p i") 'info)
(global-set-key (kbd "C-c p m") 'mail)
;(global-set-key (kbd "C-c p p") 'perldb)
(global-set-key (kbd "C-c p r") 'rot13-region)
;;(global-set-key (kbd "C-c p t") 'toggle-debug-on-error)
(global-set-key (kbd "C-c p t") (function 
				 (lambda nil (interactive) 
				   (find-file "~/.emacs.d/elisp/.tmp.org"))))
(global-set-key (kbd "C-c p w") 'woman)
(global-set-key (kbd "C-c p 3") 'w3m)

;; Use Habit
;; 
(setq hscroll-step 1)
(setq scroll-conservatively 100000)
(setq scroll-margin 3)
(setq scroll-step 1)
(setq woman-use-own-frame nil)
(setq-default make-backup-files nil)                  ;no backup file
(setq kill-ring-max 500)                              ;kill ring
(setq mouse-yank-at-point t)                          ;yank not at mouse but at cursor
(fset 'yes-or-no-p 'y-or-n-p)                         ;'y' and 'n' replay 'yes' and 'no'
(setq bookmark-default-file ".bookmark.el")           ;bookmark file name
(setq default-major-mode 'text-mode)                  ;default mode

;;; setting difference from window-system 
;;
(if (eq window-system nil)
    t
  (scroll-bar-mode 1)                                   ;remain scroll bar
  (set-scroll-bar-mode 'right)                          ;scroll bar on right
  (global-set-key (kbd "<wheel-down>") '(lambda nil (interactive) (scroll-up 2)))
  (global-set-key (kbd "<wheel-up>") '(lambda nil (interactive) (scroll-down 2)))
  (global-set-key [mouse-4] '(lambda nil (interactive) (scroll-down 2)))
  (global-set-key [mouse-5] '(lambda nil (interactive) (scroll-up 2)))

  ;; color-theme: choose my color theme
  (if (not (require 'color-theme nil t))
      (message "[warn] feature 'color-theme' not found!")
    (color-theme-gnome2))	                        ;kingsajz, classic, dark-blue and snow are also good.
  ;; color-theme

  ;; tabbar:
  (if (not (require 'tabbar nil t))
      (message "[warn] feature 'tabbar' not found!")
    (global-set-key (kbd "C-c h") 'tabbar-press-home)
    (global-set-key (kbd "C-<tab>") 'tabbar-forward-tab)
    (global-set-key (kbd "C-S-<iso-lefttab>") 'tabbar-backward-tab)
    (global-set-key (kbd "<C-S-tab>") 'tabbar-backward-tab)
    (global-set-key (kbd "C-c C-j") 'tabbar-forward-group)
    (global-set-key (kbd "C-c C-k") 'tabbar-backward-group)
    (global-set-key (kbd "C-c C-h") 'tabbar-backward-tab)
    (global-set-key (kbd "C-c C-l") 'tabbar-forward-tab)
    (setq tabbar-buffer-groups-function (lambda () (list "All")))
    (tabbar-mode))
  ;; tabbar 
  )

;; redo: Redo/undo system for Emacs
;; usage:    C-c r (or M-x redo)
(autoload 'undo "redo" "undo" t)
(autoload 'redo "redo" "redo" t)
(global-set-key (kbd "C-c u") 'undo)
(global-set-key (kbd "C-c r") 'redo)
;; redo

;; browse-kill-ring: browse kill ring and interactively insert items from kill-ring
;; usage:    M-y (or M-x yank-pop)
(if (not (require 'browse-kill-ring nil t) )
    (message "[warn] feature 'browse-kill-ring' not found!")    
  (browse-kill-ring-default-keybindings))
;; browse-kill-ring

;; unique: unique buffer name(guard duplication of buffer name)
;; usage:    auto
(if (not (require 'uniquify nil t))
    (message "[warn] feature 'uniquify' not found!")
  (setq uniquify-buffer-name-style 'forward))  
;; unique

;; dired: files(or directories) explorer
;; usage: C-c j (or M-x dired-jump)
(if (not (require 'dired nil t))
    (message "[warn] feature 'dired' not found!")

  (global-set-key (kbd "C-c j") 'dired-jump)
  (global-set-key (kbd "C-c f") 'find-dired)

  ;; wdired
  (autoload 'wdired-change-to-wdired-mode "wdired")
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

  ;; dired-x
  (if (not (require 'dired-x nil t))
      (message "[warn] feature 'dired-x' not found!")
    (setq dired-recursive-deletes t)                      ;delete file recursive 
    (setq dired-recursive-copies t)                       ;copy file recursive
    (setq dired-omit-files "^\\.")                   ;omit files
    (add-hook 'dired-mode-hook
	      (lambda () (dired-omit-mode t))))

  ;;  ;; dired-details
  ;;  (if (not (require 'dired-details nil t))
  ;;      (message "[warn] feature 'dired-details' not found!")
  ;;    (dired-details-install))

  ;; dired-single
  (if (not (require 'dired-single nil t))
      (message "[warn] feature 'dired-single' not found!")
    (define-key dired-mode-map "^" (function
				    (lambda nil (interactive) (dired-single-buffer ".."))))
    (define-key dired-mode-map (kbd "C-c j") (function
					      (lambda nil (interactive) (dired-single-buffer ".."))))
    (define-key dired-mode-map [return] 'dired-single-buffer)
    (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse))
  
  ;; dired-view
  (autoload 'dired-view-minor-mode-toggle "dired-view" "dired-view toggle" t)
  (define-key dired-mode-map (kbd ";") 'dired-view-minor-mode-toggle)

  ;;  ;; dired+
  ;;  (if (not (require 'dired+ nil t))
  ;;      (message "[warn] feature 'dired+' not found!"))
  )
;; dired

;; view-mode: 
(global-set-key (kbd "<escape>") (function
				    (lambda nil (interactive) (view-mode 1))))
(autoload 'view-mode "view" "view mode" t)
(defun my-view-mode-hook ()
  (setq view-read-only t)
  (define-key view-mode-map "C" 'nil)
Appearance effects  (define-key view-mode-map "c" 'nil)
  (define-key view-mode-map "o" 'nil)
  (define-key view-mode-map "k" 'View-scroll-line-backward)
  (define-key view-mode-map "j" 'View-scroll-line-forward)
  (define-key view-mode-map "g" 'goto-line)
  ;;  (define-key view-mode-map "G" 'View-goto-line-last)
  (define-key view-mode-map "b" 'View-scroll-page-backward)
  (define-key view-mode-map "f" 'View-scroll-page-forward))
(add-hook 'view-mode-hook 'my-view-mode-hook)
;; view-mode

;; imenu: 
(add-hook 'font-lock-mode-hook
	  (lambda () (condition-case nil (imenu-add-menubar-index) (error nil))))
;; imenu

;; c-mode: c-mode hook
;; usage:    just open the files
(defun my-c-mode-common-hook()
  "�ҵ�C/C++���Ա༭����"
  ;;(define-key c-mode-base-map [(f1)] 'next-error)
  ;;(define-key c-mode-base-map [(f2)] 'previous-error)

  ;; switch-h-cpp
  (autoload 'neassist-switch-h-cpp "switch-h-cpp" "Major mode for switch between .h file and .cpp file." t)
  (define-key c-mode-base-map (kbd "M-o") 'neassist-switch-h-cpp)

  (define-key c-mode-base-map [(f5)] 'compile)
  (define-key c-mode-base-map [(f1)] 'cplusplus-grep-find)
  (define-key c-mode-base-map (kbd "<mouse-3>") 'cplusplus-quick-grep-find)
  (c-toggle-electric-state nil)
  ;; (define-key c-mode-base-map [(f11)] 'hs-hide-block)
  ;; (define-key c-mode-base-map [(f12)] 'hs-show-block)
  ;; (define-key c-mode-base-map [(S-f11)] 'hs-hide-all)
  ;; (define-key c-mode-base-map [(S-f12)] 'hs-show-all)
  (setq c-macro-shrink-window-flag t)
  (setq c-macro-preprocessor "/lib/cpp -C");;use args '-C' to delete comment
  (setq c-macro-cppflags " ")
  (setq c-macro-prompt-flag t)
  (setq hs-minor-mode t)
  (setq abbrev-mode t)
  (c-set-style "ellemtel");;style
  (setq c-default-style "ellemtel"
	c-basic-offset 4)
  (setq tab-width 4 indent-tabs-mode nil)
  (setq auto-mode-alist (cons '("\\.h$"  . c++-mode) auto-mode-alist))
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook) ;
(add-hook 'c-mode-hook 'my-c-mode-common-hook)        ;
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook 'hs-minor-modenor-mode)
(add-hook 'perl-mode-hook 'hs-minor-mode)
(add-hook 'php-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
;; c-mode

;; gud 
(defun define-c-mode-fn-key-debug ()
  (define-key c-mode-base-map (kbd "<f5>") 'gud-go)
  (define-key c-mode-base-map (kbd "<f10>") 'gud-step)
  (define-key c-mode-base-map (kbd "<f11>") 'gud-next)
  (define-key c-mode-base-map (kbd "S-<f10>") 'gud-finish)
  (define-key c-mode-base-map (kbd "<f9>") 'gud-break)
  (define-key c-mode-base-map (kbd "S-<f9>") 'gud-remove)

  (define-key gud-mode-map (kbd "<f5>") 'gud-go)
  (define-key gud-mode-map (kbd "<f10>") 'gud-step)
  (define-key gud-mode-map (kbd "<f11>") 'gud-next)
  (define-key gud-mode-map (kbd "S-<f10>") 'gud-finish)
  (define-key gud-mode-map (kbd "<f9>") 'gud-break))

(mapc (lambda (mode-hook)
	(add-hook mode-hook (lambda ()
			      ;;(kill-buffers-when-gdb-quit)
			      (define-c-mode-fn-key-debug))))
      (list 'gdb-mode-hook 'gud-mode-hook))
;; gud

;; ctypes
;; function: 
;; usage:    
(if (not (require 'ctypes nil t))
    (message "[warn] feature 'ctypes' not found!")
  (ctypes-auto-parse-mode 1))  ;auto realize C language typedef type
;; ctypes

;; yasnippet: template system for Emacs, automatically expand 
;; usage: press [TAB]
(if (not (require 'yasnippet nil t))
    (message "[warn] feature 'yasnippet' not found!")
  (setq yas/root-directory (concat my-lisp-root "/" "snippets"))
  (yas/load-directory yas/root-directory)
  (yas/global-mode))
;; yasnippet

;; tramp-hack : open secure remote file
;; usage: C-x C-f /su:root@localhost:/etc or (/su::/etc)
(if (not (require 'tramp-hack nil t))
    (message "[warn] feature 'tramp-hack' not found!"))
;; tramp-hack

;; misc-goodies: 
;; usage:    
(if (not (require 'misc-goodies nil t))
    (message "[warn] feature 'misc-goodies' not found!")
  (global-set-key (kbd "C-c b") 'popup-menu-bookmark)
  (global-set-key (kbd "C-c i") 'popup-menu-insert)
  ;;(global-set-key (kbd "C-c e") 'popup-menu-edit)
  )
;; misc-goodies

;; misc-org: 
;; usage:    
(if (not (and (require 'org nil t) (require 'org-latex nil t)))
    (message "[warn] feature 'org' not found!")
  (require 'misc-org nil t))
;; misc-goodies

;; misc-menu
;; usage:    
(if (not (require 'misc-menu nil t))
    (message "[warn] feature 'misc-menu' not found!"))
;; misc-menu

;; highlight-symbol
;; usage:    
(if (not (require 'highlight-symbol nil t))
    (message "[warn] feature 'highlight-symbol' not found!")
  (global-set-key [(control f3)] 'highlight-symbol-at-point)
  (global-set-key [f2] 'highlight-symbol-next)
  (global-set-key [(shift f2)] 'highlight-symbol-prev)
  (global-set-key [f3] 'highlight-symbol-next-in-defun)
  (global-set-key [(shift f3)] 'highlight-symbol-prev-in-defun)
  (global-set-key [(meta f3)] 'highlight-symbol-query-replace)
  (global-set-key [(control meta f3)] 'highlight-symbol-query-replace))
;; highlight-symbol

;; doxymacs: quick insert comment
;; usage:    M-x doxymacs-...
(autoload 'doxymacs-insert-file-comment "doxymacs" "doxymacs-insert-file-comment" t)
(autoload 'doxymacs-insert-function-comment "doxymacs" "doxymacs-insert-function-comment" t)
(autoload 'doxymacs-insert-grouping-comment "doxymacs" "doxymacs-insert-grouping-comment" t)
(autoload 'doxymacs-insert-multiline-comment "doxymacs" "doxymacs-insert-multiline-comment" t)
;; doxymacs

;; protobuf: protobuf mode
;; usage:    bind to .proto files
(setq auto-mode-alist (cons '("\\.proto$"  . protobuf-mode) auto-mode-alist))
(autoload 'protobuf-mode "protobuf-mode" "edit google protobuf files use emacs" t)
;; protobuf

;; php: php mode
;; usage:    bind to .php files
(setq auto-mode-alist (cons '("\\.php$"  . php-mode) auto-mode-alist))
(autoload 'php-mode "php-mode" "edit php files use emacs" t)
;; php

;; vb: vb mode
;; usage:    bind to .frm files
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\)$" .
				 visual-basic-mode)) auto-mode-alist))
;; vb

;; grep: 
(global-set-key (kbd "C-c g") 'grep-find)
(global-set-key (kbd "C-c G") 'cplusplus-grep-find)
(global-set-key (kbd "C-c t") 'grep-find-replace)
(global-set-key (kbd "C-c T") 'cplusplus-grep-find-replace)
(setq grep-find-command "find . -name '*' -type f -print0 | xargs -0 -e grep -nH -e ")
;; grep 

;; psvn: subversion interface for emacs
;; usage:    M-x svn-status
(global-set-key (kbd "C-c p v") 'svn-status)
(autoload 'svn-status "psvn" "subversion interface for emacs" t)
;; psvn

;; w3m: web explorer
;; usage:    C-c p 3 (or M-x w3m)
(autoload 'w3m "w3m" "w3m" t)
;; w3m

;; dictionary: dictionary client and utils
;; usage:    C-c s d (or M-x dictionary-search)
(setq dictionary-server "localhost")                  ;choose a dictionary server, or "www.dict.org"
(autoload 'dictionary-search "dictionary" "dictionary-search" t)
(autoload 'dictionary-lookup-definition "dictionary" "dictionary-lookup-definition" t)
(autoload 'dictionary-match-words "dictionary" "dictionary-match-words" t)
(global-set-key (kbd "C-c d l") 'dictionary-lookup-definition)
(global-set-key (kbd "C-c d s") 'dictionary-search)
(global-set-key (kbd "C-c d m") 'dictionary-match-words)
;; dictionary

;; sr-speedbar: single frame speedbar
;;(if (not (require 'sr-speedbar nil t))
;;    (message "[warn] feature 'sr-speedbar' not found!")
;;  (global-set-key (kbd "C-c p t") 'sr-speedbar-toggle))
;; sr-speedbar

;; mail: send textual mail, and no attachment
;; usage:    M-x mail
(setq send-mail-function 'smtpmail-send-it) ; if you use `mail'
(setq smtpmail-auth-credentials
      '(("smtp.163.com"			;SMTP server name
	 25
	 "wuyao721"           		;user name
	 nil)))                         ;password, ask whem use if nil
(setq smtpmail-default-smtp-server "smtp.163.com")
(setq smtpmail-smtp-server "smtp.163.com")
;; mail

;; shell-toggle: quick to shell
;; usage:    C-c p s (or M-x shell-toggle)
(if (not (require 'shell-toggle nil t))
    (message "[warn] feature 'shell-toggle' not found!")
  (global-set-key (kbd "C-c p s") 'shell-toggle-cd))
;; shell-toggle

;; multi-shell: multi shell
;; usage:    C-c p n (or M-x multi-shell-new)
(autoload 'multi-shell-new "multi-shell" "multi-shell-new" t)
(setq multi-shell-buffer-name "shell")
(global-set-key (kbd "C-c p n") 'multi-shell-new)
(if (eq system-type 'windows-nt)
  (setq multi-shell-command "bash")
  (global-set-key (kbd "C-c p b") (function (lambda nil (interactive) (term "/bin/bash"))))) 

;;(global-set-key (kbd "C-c p b") (function (lambda nil (interactive) (term "/bin/bash"))))
;;(when (eq system-type 'windows-nt)
;;  (setq multi-shell-command "cmd.exe")
;;  (global-set-key (kbd "C-c p b") (function (lambda nil (interactive) 
;;					      (setq temp-multi-shell-command multi-shell-command)
;;					      (setq multi-shell-command "bash.exe")
;;					      (multi-shell-new)
;;					      (setq multi-shell-command temp-multi-shell-command))))
;;  ) 

;; xcscope: easily search for where symbols are used and defined.
;; usage:    C-c s (prefix key sequence)
(if (not (require 'xcscope nil t))
    (message "[warn] feature 'xcscope' not found!"))
;; xcscope

;; sawfish: an emacs mode for writing code for the sawfish window manager
;; usage:    just open the files
(autoload 'sawfish-mode "sawfish" "sawfish-mode" t)
(setq auto-mode-alist (cons '("\\.sawfishrc$"  . sawfish-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.jl$"         . sawfish-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.sawfish/rc$" . sawfish-mode) auto-mode-alist))
;; sawfish

;; lua: lua mode
;; usage:    just open the files
(autoload 'lua-mode "lua-mode" "lua-mode" t)
(setq auto-mode-alist (cons '("\\.lua" . lua-mode) auto-mode-alist))
(defun build-sln (action)
  "Run premake4 via shell-command, Collect output in a buffer.
While find runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to."

  (interactive 
   (list (read-string "Run Premake Action: " "vs2008")))
  (shell-command (concat "premake4 " action)))
(add-hook 'lua-mode-hook
	  (lambda ()   
	    (define-key lua-mode-map [(f7)] 'build-sln)
	    (define-key lua-mode-map [(f5)] 'find-sln)))
;; lua

;; sln-mode: MS sln
;; usage: M-x find-sln
(autoload 'find-sln "sln-mode")
(global-set-key (kbd "C-c p f") 'find-sln)
(eval-after-load "project-buffer-mode"
  '(progn
  (if (not (require 'project-buffer-mode+ nil t))
      (message "[warn] feature 'project-buffer-mode+' not found!")
       (project-buffer-mode-p-setup))))
;; sln-mode

;; bashdb: bash debugger
;; usage: M-x bashdb
(autoload 'bashdb "bashdb" "BASH Debugger mode via GUD and bashdb" t)
(global-set-key (kbd "C-c p a") 'bashdb)
;; bashdb

;; misc-muse: muse
;; usage: M-x muse-mode
(autoload 'muse-mode "misc-muse" "muse-mode" t)
(setq auto-mode-alist (cons '("\\.muse$"  . muse-mode) auto-mode-alist))
;;muse-publish-this-file
;;(defun my-muse-hook ()
;;  (local-set-key (kbd "C-c p t") (function 
;;				  (lambda nil (interactive) 
;;				    (find-file "~/.emacs.d/.tmp.muse")))))
;;(add-hook 'muse-mode-hook 'my-muse-hook)
;; misc-muse

;; pydb:
;; usage: M-x pydb
;;(if (not (require 'pydb nil t))
;;  (message "[warn] feature 'pydb' not found!"))
;; pydb

;; pod-mode: perl pod document reader
;; usage: 
(autoload 'pod-mode "pod-mode" "pod-mode" t)
(setq auto-mode-alist (cons '("\\.pod$"  . pod-mode) auto-mode-alist))
(add-hook 'pod-mode-hook 'font-lock-mode)
;; pod-mode

;; msf-abbrev
;; usage: 
(if (not (require 'msf-abbrev nil t))
    (message "[warn] feature 'msf-abbrev' not found!")
  (setq msf-abbrev-root "~/.emacs.d/elisp/mode-abbrevs")
  (global-msf-abbrev-mode t)
  (global-set-key (kbd "C-c l") 'msf-cmd-goto-root)
  (global-set-key (kbd "C-c a") 'msf-cmd-define))
;; msf-abbrev

;; gnugo: game of I-GO
;; usage: M-x gnugo
(autoload 'gnugo "gnugo" "GNUGO" t)
;; gnugo

;; (autoload 'table-insert "table" "WYGIWYS table editor");for make table
;;(autoload 'senator-try-expand-semantic "senator")

;; at last restore files, command history, kill-ring, and so on.

;; recentf: quick access for recent open file
;; usage:    C-c o (or M-x recentfopen)
;;(require 'cl-seq)
;;(require 'cl)
(if (not (require 'recentf nil t))
    (message "[warn] feature 'recentf' not found!")

  (defun recentf-ido-find-file ()
    "Find a recent file using Ido."
    (interactive)
    (if (or (not (boundp 'ido-mode)) (eq ido-mode nil)) ; whether ido enabled?
	(recentf-open-files)
      (let* ((file-assoc-list
	      (mapcar (lambda (x)
			(cons (file-name-nondirectory x) ;eg. "C:/Emacs/my-elisp/.emacs" -->> ( ".emacs" . "C:/Emacs/my-elisp/.emacs" )
			      x))
		      recentf-list))
	     (filename-list (mapcar #'car file-assoc-list))
	     ;;(filename-list
	     ;; (remove-duplicates (mapcar #'car file-assoc-list)
	     ;; 	       :test #'string=))
	     (filename (ido-completing-read "Choose recent file: "
					    filename-list
					    nil
					    t)))
	(when filename
	  (find-file (cdr (assoc filename
				 file-assoc-list)))))))

  (global-set-key (kbd "C-c o") 'recentf-ido-find-file)
  (recentf-mode 1))
;;quick access for recent open file
;; recentf

;; c#: http://code.google.com/p/csharpmode
;; usage: 
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
;; c#

;; apt: apt-get apt-cache apt-file dpkg
;; usage: 
;;(if (not (require 'apt nil t))
;;    (message "[warn] feature 'apt' not found!"))
;; apt

;; cedet: Emacs Development Tools
;; usage: M-x my-load-cedet
;(defvar my-cedet-loaded nil)
;(defun my-load-cedet ()
;  (interactive)
;  ;; Don't do the loads twice.
;  (when (null my-cedet-loaded)
;    (setq my-cedet-loaded t)
;    (if (not (require 'cedet "./cedet/common/cedet.el" t))
;	(message "[warn] feature 'cedet' not found!")
;      (semantic-mode 1)
;      (global-ede-mode 1)
;      (setq semanticdb-default-save-directory "~/.semanticdb/")
;      (require 'cedet-hack-platform "cedet-hack-platform" t)
;      (require 'eassist-patch "eassist-patch" t)
;      (semantic-load-enable-code-helpers)
;      (global-srecode-minor-mode 1))))

(defun my-load-cedet ()
  (interactive)
  (add-hook 'c-mode-common-hook 'my-cedet-hook))

(defun my-cedet-hook ()
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
;  (local-set-key (kbd "C-;") 'semantic-ia-complete-symbol-menu)
  (local-set-key (kbd "C-'") 'semantic-complete-analyze-inline)
  (local-set-key (kbd "C-c , s") 'semantic-symref-symbol)
  (local-set-key (kbd "C-c , h") 'semantic-analyze-proto-impl-toggle)
  (local-set-key (kbd "M-m") 'ido-jump-to-method)
  (local-set-key [f12] 'semantic-ia-fast-jump)
  (local-set-key [S-f12] 'semantic-decoration-include-visit))
;; cedet

;; session: remember kill-ring, command history
;; usage:    auto
(if (not (require 'session nil t))
    (message "[warn] feature 'session' not found!")
  (add-hook 'after-init-hook 'session-initialize))
;; session

;; evernote-mode: online note book
;; usage:    C-c e b
(autoload 'evernote-browser "evernote-mode" "evernote-mode" t)
(autoload 'evernote-open-note "evernote-mode" "evernote-mode" t)
(setq evernote-username "wuyao721@163.com")
(setq enh-enclient-command "C:/Ruby193/bin/enclient.rb")
(setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8"))
(global-set-key "\C-ceb" 'evernote-browser)
(global-set-key "\C-cec" 'evernote-create-note)
(global-set-key "\C-ceo" 'evernote-open-note)
(global-set-key "\C-ces" 'evernote-search-notes)
(global-set-key "\C-ceS" 'evernote-do-saved-search)
(global-set-key "\C-cew" 'evernote-write-note)
(global-set-key "\C-cep" 'evernote-post-region)
;; evernote-mode

;; org2blog
(if (not (require 'org2blog-autoloads nil t))
    (message "[warn] feature 'org2blog-autoloads' not found!")
  (global-set-key (kbd "C-c p p") 'org2blog/wp-mode)
  (global-set-key (kbd "C-c p P") 'org2blog/wp-new-entry)

  (setq org2blog/wp-entry-mode-map
	(let ((org2blog/wp-map (make-sparse-keymap)))
	  (set-keymap-parent org2blog/wp-map org-mode-map)
	  (define-key org2blog/wp-map (kbd "C-c C-c") 'org2blog/wp-post-buffer-and-publish)
	  (define-key org2blog/wp-map (kbd "C-c C-k") 'org2blog/wp-delete-entry)
	  org2blog/wp-map))

  (setq org2blog/wp-blog-alist
	'(("wuyao721"
	   :url "http://www.wuyao721.com/xmlrpc.php"
	   :username "wuyao721"
	   :keep-new-lines t
	   :confirm t
	   :wp-code nil
	   :tags-as-categories nil)))

  (setq org2blog/wp-buffer-template
	"#+DATE: %s
#+OPTIONS: toc:nil num:nil todo:nil pri:nil tags:nil ^:nil TeX:nil 
#+CATEGORY: 
#+TAGS: 
#+PERMALINK: 
#+TITLE:
\n")
  )

;; org2blog

;; desktop: remember opened files for next time run emacs
;; usage:    auto
;(if (not (require 'desktop nil t))
;    (message "[warn] feature 'desktop' not found!")
;  (setq desktop-base-file-name ".desktop.el")             ;desktop file name
;  ;;(setq desktop-dirname (shell-command-to-string "echo -n ${HOME}/"))
;  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
;  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
;  (if (>= emacs-major-version 22) 
;      (desktop-save-mode 1)
;    (desktop-load-default)
;    (desktop-read)
;    (setq desktop-enable t)))
;; desktop

(set-frame-size (selected-frame) 80 20)

(if (not (eq system-type 'windows-nt))
    (global-set-key [f11] 'my-toggle-fullscreen)
  (global-set-key [f11] 'w32-fullscreen)
  (autoload 'w32-fullscreen "w32-fullscreen"))

;; set enviroment 
(if (not (require 'env-platform nil t))
    (message "[warn] feature 'env-platform' not found!"))

;; emacs server: remember opened files for next time run emacs
;; usage:    emacsclient ...
(if (eq system-type 'windows-nt)
    (server-start)) 
;; emacs server