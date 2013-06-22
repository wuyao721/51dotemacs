;;; env-platform.el --- set enviroment for different platform

;;; set PATH 
(setenv "PATH" (concat "C:/devenv/bin;"
		       "C:/MinGW/bin;"
		       "C:/MinGW/msys/1.0/bin;"
		       "C:/MinGW/msys/1.0/local/bin;"
		       "D:/Program Files/Microsoft Visual Studio 8/Common7/IDE;"
		       (getenv "PATH")))

;;; set exec path
(setq exec-path (append exec-path '("C:/devenv/bin"
                                    "C:/MinGW/bin"
                                    "C:/MinGW/msys/1.0/bin"
                                    "C:/MinGW/msys/1.0/local/bin"
                                    "D:/Program Files/Microsoft Visual Studio 8/Common7/IDE"
                                    )))

;;; set set-default-font
(if (eq system-type 'windows-nt)
    (set-default-font "-outline-YaHei Consolas Hybrid-normal-normal-normal-sans-19-*-*-*-p-*-iso8859-1")
  (set-default-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-19-*-*-*-m-0-iso10646-1"))

;;; set email: send textual mail, and no attachment
;;(setq send-mail-function 'smtpmail-send-it)    ;if you use `mail'
;;(setq smtpmail-auth-credentials
;;      '(("smtp.163.com"                        ;SMTP server name
;;         25
;;         "wuyao721"                                  ;user name
;;         nil)))                         ;password, ask whem use if nil
;;(setq smtpmail-default-smtp-server "smtp.163.com")
;;(setq smtpmail-smtp-server "smtp.163.com")

;;; set user name and email
;;(setq user-full-name "wuyao721")                      ;full name
;;(setq user-mail-address "wuyao721@163.com")           ;mail address

;;; set wordpress account
;;(setq org2blog/wp-blog-alist
;;      '(("wuyao721"
;;         :url "http://www.wuyao721.com/xmlrpc.php"
;;         :username "wuyao721"
;;         :keep-new-lines t
;;         :confirm t
;;         :wp-code nil
;;         :tags-as-categories nil)))

;;; sql setting
;;(setq sql-user "root")
;;(setq sql-password "123456")
;;(setq sql-databases "test1")
;;(setq sql-server "localhost")

(provide 'env-platform)
