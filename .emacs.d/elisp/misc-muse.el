(provide 'misc-muse)

(require 'muse-mode)
(require 'muse-html)         ;添加html格式的支持
(require 'muse-latex)        ; 添加latex格式的支持
;(require 'muse-texinfo)      ; 添加texinfo格式的支持
;(require 'muse-docbook)      ; 添加docbook格式的支持
;(require 'muse-wiki)   
;(require 'muse-project)      ; 添加wiki project的支持
(require 'htmlize-hack)      ; hack for htmlize

;;(setq muse-html-meta-content-type (concat "text/html; charset=gb2312"))

;;(setq muse-latex-markup-strings 
;;      '((image-with-desc . "\\includegraphics{%s.%s}")
;;	(image . "\\includegraphics{%s.%s}")
;;    (image-link      . "%% %s
;;\\includegraphics{%s.%s}")
;;    (anchor-ref      . "\\ref{%s}")
;;    (url             . "\\url{%s}")
;;    (url-and-desc    . "\\href{%s}{%s}\\footnote{%1%}")
;;    (link            . "\\href{%s}{%s}\\footnote{%1%}")
;;    (link-and-anchor . "\\href{%1%}{%3%}\\footnote{%1%}")
;;    (email-addr      . "\\verb|%s|")
;;    (anchor          . "\\label{%s}")
;;    (emdash          . "---")
;;    (comment-begin   . "% ")
;;    (rule            . "\\vspace{.5cm}\\hrule\\vspace{.5cm}")
;;    (no-break-space  . "~")
;;    (line-break      . "\\\\")
;;    (enddots         . "\\ldots{}")
;;    (dots            . "\\dots{}")
;;    (part            . "\\part{")
;;    (part-end        . "}")
;;    (chapter         . "\\chapter{")
;;    (chapter-end     . "}")
;;    (section         . "\\section{")
;;    (section-end     . "}")
;;    (subsection      . "\\subsection{")
;;    (subsection-end  . "}")
;;    (subsubsection   . "\\subsubsection{")
;;    (subsubsection-end . "}")
;;    (section-other   . "\\paragraph{")
;;    (section-other-end . "}")
;;    (footnote        . "\\footnote{")
;;    (footnote-end    . "}")
;;    (footnotetext    . "\\footnotetext[%d]{")
;;    (begin-underline . "\\underline{")
;;    (end-underline   . "}")
;;    (begin-literal   . "\\texttt{")
;;    (end-literal     . "}")
;;    (begin-emph      . "\\emph{")
;;    (end-emph        . "}")
;;    (begin-more-emph . "\\textbf{")
;;    (end-more-emph   . "}")
;;    (begin-most-emph . "\\textbf{\\emph{")
;;    (end-most-emph   . "}}")
;;    (begin-verse     . "\\begin{verse}\n")
;;    (end-verse-line  . " \\\\")
;;    (verse-space     . "~~~~")
;;    (end-verse       . "\n\\end{verse}")
;;    (begin-example   . "\\begin{quote}\n\\begin{verbatim}")
;;    (end-example     . "\\end{verbatim}\n\\end{quote}")
;;    (begin-center    . "\\begin{center}\n")
;;    (end-center      . "\n\\end{center}")
;;    (begin-quote     . "\\begin{quote}\n")
;;    (end-quote       . "\n\\end{quote}")
;;    (begin-cite     . "\\cite{")
;;    (begin-cite-author . "\\citet{")
;;    (begin-cite-year . "\\citet{")
;;    (end-cite        . "}")
;;    (begin-uli       . "\\begin{itemize}\n")
;;    (end-uli         . "\n\\end{itemize}")
;;    (begin-uli-item  . "\\item ")
;;    (begin-oli       . "\\begin{enumerate}\n")
;;    (end-oli         . "\n\\end{enumerate}")
;;    (begin-oli-item  . "\\item ")
;;    (begin-dl        . "\\begin{description}\n")
;;    (end-dl          . "\n\\end{description}")
;;    (begin-ddt       . "\\item[")
;;    (end-ddt         . "] \\mbox{}\n")))

(setq muse-html-style-sheet "<style type=\"text/css\">
body {
  background: white; color: black;
  margin-left: 3%; margin-right: 7%;
}

p { margin-top: 1% }
p.verse { margin-left: 3% }

.example { margin-left: 3% }

h2 {
  margin-top: 25px;
  margin-bottom: 0px;
}
h3 { margin-bottom: 0px; }
pre {
    border: #777777 1px solid;
    padding: 0.5em;
    margin-left: 1em;
    margin-right: 2em;
    white-space: pre;
    background-color: #e6e6e6;
    color: black;
}
    </style>")

(setq muse-project-alist
      '(("my wiki"
	 ("~/project/bkp/docs" :default "index")
	 (:base "html" :path "~/project/bkp/publish"))))

(define-key muse-mode-map [(f5)] 'muse-publish-this-file)

(defun muse-latex-pdf-generate (file output-path final-target)
  (apply
   #'muse-publish-transform-output
   file output-path final-target "PDF"
   (function
    (lambda (file output-path)
      (let* ((fnd (file-name-directory output-path))
             (command (format "%s \"%s\""
                              muse-latex-pdf-program
                              (file-relative-name file fnd)))
	     (outfile (muse-replace-regexp-in-string
			"\\.tex\\'" ".out" file t t)) 
	     (outfile2 (muse-replace-regexp-in-string
			"\\.tex\\'" ".out.bak" file t t)) 
	     (command2 (format "iconv -f utf-8 -t gbk \"%s\" > \"%s\"" 
			       outfile 
			       outfile2))
	     (command3 (format "mv \"%s\" \"%s\"" 
			       outfile2
			       outfile))
	     (command4 (format "gbk2uni \"%s\"" 
			       outfile))
             (times 0)
             (default-directory fnd)
             result)
        ;; XEmacs can sometimes return a non-number result.  We'll err
        ;; on the side of caution by continuing to attempt to generate
        ;; the PDF if this happens and treat the final result as
        ;; successful.
        (while (and (< times 2)
                    (or (not (numberp result))
                        (not (eq result 0))
                        ;; table of contents takes 2 passes
                        (file-readable-p
                         (muse-replace-regexp-in-string
                          "\\.tex\\'" ".toc" file t t))))
          (setq result (shell-command command)
                times (1+ times)))
	(shell-command command2)
	(shell-command command3)
	(shell-command command4)
	(shell-command command)
        (if (or (not (numberp result))
                (eq result 0))
            t
          nil))))
   muse-latex-pdf-cruft))

(setq muse-latexcjk-header
      "\\documentclass{article}

\\usepackage{CJK}
\\usepackage{indentfirst}
\\usepackage[pdftex]{graphicx}
\\usepackage{fancyhdr}
\\usepackage[CJKbookmarks=true]{hyperref}

%% Define a museincludegraphics command, which is
%%   able to handle escaped special characters in image filenames.
\\def\\museincludegraphics{%
  \\begingroup
  \\catcode`\\\|=0
  \\catcode`\\\\=12
  \\catcode`\\\#=12
  \\includegraphics[width=0.75\\textwidth]
}

\\begin{document}
\\begin{CJK*}{UTF8}{gbsn}

\\pagestyle{fancy}
\\title{<lisp>(muse-publish-escape-specials-in-string
  (muse-publishing-directive \"title\") 'document)</lisp>}
\\author{<lisp>(muse-publishing-directive \"author\")</lisp>}
\\date{<lisp>(muse-publishing-directive \"date\")</lisp>}

\\lhead{}
\\chead{}
\\rhead{}
\\lfoot{}
\\cfoot{\\thepage}
\\rfoot{} 

\\maketitle

<lisp>(and muse-publish-generate-contents
           (not muse-latex-permit-contents-tag)
           \"\\\\tableofcontents\n\\\\newpage\")</lisp>\n\n")

(setq muse-latex-slides-header
  "\\documentclass[CJK,notheorems,mathserif,table]{beamer}
\\useoutertheme[height=0.1\\textwidth,width=0.15\\textwidth,hideothersubsections]{sidebar}
\\usecolortheme{whale}      % Outer color themes, 其他选择: whale, seahorse, dolphin . 换一个编译看看有什么不同.
\\usecolortheme{orchid}     % Inner color themes, 其他选择: lily, orchid
\\useinnertheme[shadow]{rounded} % 对 box 的设置: 圆角、有阴影.
\\setbeamercolor{sidebar}{bg=blue!50} % sidebar的颜色, 50%的蓝色.
%\\setbeamercolor{background canvas}{bg=blue!9} % 背景色, 9%的蓝色. 去掉下一行, 试一试这个.
\\setbeamertemplate{background canvas}[vertical shading][bottom=white,top=structure.fg!25] %%背景色, 上25%的蓝, 过渡到下白.
\\usefonttheme{serif}  % 字体. 个人偏好有轮廓的字体. 去掉这个设置编译, 就看到不同了.
\\setbeamertemplate{navigation symbols}{}   %% 去掉页面下方默认的导航条.
%%------------------------常用宏包---------------------------------------------------------------------
%%注意, beamer 会默认使用下列宏包: amsthm, graphicx, hyperref, color, xcolor, 等等
%\\usepackage[english]{babel}
\\usepackage{CJK}
\\usepackage{subfigure} %%图形或表格并排排列
\\usepackage{xmpmulti}  %%支持文中的 \\multiinclude 等命令, 使 mp 文件逐帧出现. 具体讨论见 beamer 手册.
\\usepackage{colortbl,dcolumn}     %% 彩色表格
\\graphicspath{{figures/}}         %% 图片路径. 本文的图片都放在这个文件夹里了.
\\DeclareGraphicsRule{*}{mps}{*}{} %% 使 pdflatex 可以纳入 metapost 做的图片.

\\def\\museincludegraphics{%
  \\begingroup
  \\catcode`\\|=0
  \\catcode`\\\\=12
  \\catcode`\\#=12
  \\includegraphics[width=0.75\\textwidth]
}

\\begin{document}
\\begin{CJK*}{UTF8}{gbsn}

\\title{<lisp>(muse-publish-escape-specials-in-string
  (muse-publishing-directive \"title\") 'document)</lisp>}
\\author{<lisp>(muse-publishing-directive \"author\")</lisp>}
\\institute{<lisp>(muse-publishing-directive \"institute\")</lisp>}
\\date{<lisp>(muse-publishing-directive \"date\")</lisp>}

\\frame{\\titlepage}

<lisp>(and muse-publish-generate-contents
           \"\\\\frame{\\\\frametitle{目录}\\\\tableofcontents}\")</lisp>\n\n")

(muse-derive-style "slidescjk" "latexcjk"
                   :header 'muse-latex-slides-header
                   :tags   'muse-latex-slides-markup-tags)

(muse-derive-style "slides-pdfcjk" "pdfcjk"
                   :header 'muse-latex-slides-header
                   :tags   'muse-latex-slides-markup-tags)

