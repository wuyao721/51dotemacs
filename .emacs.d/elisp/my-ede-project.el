
(if (eq system-type 'windows-nt) 
    (setq sln-path "f:/webguard/trunk/webguard.sln")
    (setq sln-path "~/project/webguard/trunk/webguard.sln"))

(if (file-regular-p sln-path)
    (ede-cpp-root-project "webguard"
			  :name "webguard project"
			  :file sln-path
			  :include-path '("../include"
					  "/include"
					  "/WGM/include"
					  "/WGTP"
					  )
			  :system-include-path '("/usr/include/c++/4.2"
						 "/usr/include")
			  :spp-table '(("linux" . "1")
				       )
			  :spp-files '( "Poco/Config.h"
					"Poco/Platform.h"
					"Poco/Data/Data.h"
					"Poco/Data/SQLite/SQLite.h"
					"Poco/Net/Net.h"
					"Poco/XML/XML.h"
					"Poco/Zip/Zip.h"
					"Poco/Util/Util.h"
					"Poco/Crypto/Crypto.h"
					)))

;;(if (file-regular-p "~/project/webguard/trunk/webguard.sln")
;;    (ede-cpp-root-project "webguard"
;;			  :name "webguard project"
;;			  :file "~/project/webguard/trunk/webguard.sln"
;;			  :include-path '("/Vendor/poco-1.4.0-all/Foundation/include"
;;					  "/Vendor/poco-1.4.0-all/CppUnit/include"
;;					  "/Vendor/poco-1.4.0-all/Crypto/include"
;;					  "/Vendor/poco-1.4.0-all/Data/include"
;;					  "/Vendor/poco-1.4.0-all/Data/SQLite/include"
;;					  "/Vendor/poco-1.4.0-all/Net/include"
;;					  "/Vendor/poco-1.4.0-all/NetSSL_OpenSSL/include"
;;					  "/Vendor/poco-1.4.0-all/Util/include"
;;					  "/Vendor/poco-1.4.0-all/XML/include"
;;					  "/Vendor/poco-1.4.0-all/Zip/include"
;;					  "/WGM/include"
;;					  "/WGTP"
;;					  )
;;			  :system-include-path '("/usr/include/c++/4.2"
;;						 "/usr/include")
;;			  :spp-table '(("isLinux" . "1")
;;				       ("_XOPEN_SOURCE" . "500")
;;				       ("_REENTRANT" . "")
;;				       ("_THREAD_SAFE" . "")
;;				       ("_FILE_OFFSET_BITS" . "64")
;;				       ("_LARGEFILE64_SOURCE" . "")
;;				       ("Foundation_API". "")
;;				       ("Net_API". "")
;;				       ("NetSSL_API". "")
;;				       ("Util_API". "")
;;				       ("Data_API". "")
;;				       ("SQLite_API". "")
;;				       ("MySQL_API". "")
;;				       ("ODBC_API". "")
;;				       ("DOM_API". "")
;;				       ("SAX_API". "")
;;				       ("XML_API". "")
;;				       ("Crypto_API". "")
;;				       ("CppUnit_API". "")
;;				       ("Zip_API". "")
;;				       )
;;;;			  :spp-files '( "Poco/Config.h" )
;;			  )
;;  )

(provide 'my-ede-project)