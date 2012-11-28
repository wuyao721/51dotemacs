;;; env-platform.el --- set enviroment for different platform

;;; set PATH 
;;(setenv "PATH" (concat "C:/devenv/bin;"
;;		       "C:/MinGW/bin;"
;;		       "C:/MinGW/msys/1.0/bin;"
;;		       "C:/MinGW/msys/1.0/local/bin;"
;;		       "D:/Program Files/Microsoft Visual Studio 8/Common7/IDE;"
;;		       (getenv "PATH")))


;;; set exec path
;;(setq exec-path (append exec-path '("C:/devenv/bin"
;;				    "C:/MinGW/bin"
;;				    "C:/MinGW/msys/1.0/bin"
;;				    "C:/MinGW/msys/1.0/local/bin"
;;				    "D:/Program Files/Microsoft Visual Studio 8/Common7/IDE"
;;				    )))

;;; set set-default-font
(if (eq system-type 'windows-nt)
    (set-default-font "-outline-YaHei Consolas Hybrid-normal-normal-normal-sans-19-*-*-*-p-*-iso8859-1")
  (set-default-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-19-*-*-*-m-0-iso10646-1"))

;;; no menu bar if in console
(unless (not (eq window-system nil)) 
  (menu-bar-mode -1))

;;; set user name and email
;;(setq user-full-name "WuYao")                         ;full name
;;(setq user-mail-address "wuyao721@163.com")           ;mail address

(provide 'env-platform)
