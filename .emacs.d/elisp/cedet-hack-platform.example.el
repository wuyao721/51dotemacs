;;; cedet-platform.el --- cedet hack for different platform

;; add semantic-add-system-include like these:
;(semantic-add-system-include "C:/Program Files/Microsoft Visual Studio/VC98/Include" 'c++-mode)
;(semantic-add-system-include "D:/Program Files/Microsoft Visual Studio 8/VC98/Include" 'c++-mode))
;(semantic-add-system-include "/usr/include" 'c++-mode))

;; add semanticdb-project-roots like these:
;(add-to-list 'semanticdb-project-roots "/home/yao/project/Linux_0.5/service/include/")

;; add ede-cpp-root-project like these
;(ede-cpp-root-project "webguard"
;		      :name "webguard project"
;		      :file "~/webguard/trunk/webguard.sln"
;		      :include-path '("../include"
;				      "/include"
;				      "/WGM/include"
;				      "/WGTP"
;				      )
;		      :system-include-path '("/usr/include/c++/4.2"
;					     "/usr/include")
;		      :spp-table '(("linux" . "1")
;				   )
;		      :spp-files '( "Poco/Config.h"
;				    "Poco/Platform.h"
;				    "Poco/Data/Data.h"
;				    "Poco/Data/SQLite/SQLite.h"
;				    "Poco/Net/Net.h"
;				    "Poco/XML/XML.h"
;				    "Poco/Zip/Zip.h"
;				    "Poco/Util/Util.h"
;				    "Poco/Crypto/Crypto.h"
;				    ))

(provide 'cedet-hack-platform)
