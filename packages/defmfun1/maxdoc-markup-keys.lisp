;;;  Copyright (C) (2012,2013) John Lapeyre. Licensed under GPL, v3 or greater. See the file
;;;  `LICENSE' in this directory.


(in-package :max-doc)
(mext:mext-optimize)

;;; :code -- computer code as in other markup systems
;;; :mref -- references to other entries in the maxdoc system
;;; :emref -- references to stock Maxima command. (info docs)
;;; :arg -- argument to function
;;; :var -- other variable

(fill-format-codes *format-codes-text*
   '( (:code "`~a'")   (:codedot "`~a'.")   (:codecomma "`~a',") 
      (:mref "`~a'")   (:mrefdot "`~a'.")   (:mrefcomma "`~a',") 
      (:emref "`~a'")  (:emrefdot "`~a'.")  (:emrefcomma "`~a',") 
      (:arg "<~a>")    (:argdot "<~a>.")    (:argcomma "<~a>,")
      (:var "<~a>")    (:vardot "<~a>.")    (:varcomma "<~a>,")
      (:opt "<~a>")    (:optdot "<~a>.")    (:optcomma "<~a>,")
      (:par "~%~%~%~a")
      (:dquote "\"~a\"") (:dquotedot "\"~a\".") (:dquotecomma "\"~a\",")
      (:dcode "~%~%`~a'~%~%")
      (:math "~a") (:tmath "~a") (:lif "~a")
      (:dmath "~a") (:dots " ... ")))

;; mref, mrefdot, mrefcomma below are not used. caught in earlier branch
;; emref etc. should eventually do external links
(fill-format-codes *format-codes-latex*
   '( (:code "{\\tt ~a}")  (:codedot "{\\tt ~a}.")  (:codecomma "{\\tt ~a},") 
      (:mref "{\\tt ~a}")  (:mrefdot "{\\tt ~a}.")  (:mrefcomma "{\\tt ~a},")
      (:emref "{\\tt ~a}")  (:emrefdot "{\\tt ~a}.")  (:emrefcomma "{\\tt ~a},")
      (:arg "{\\it ~a}")   (:argdot "{\\it ~a}.")   (:argcomma "{\\it ~a},")
      (:var "{\\it ~a}")   (:vardot "{\\it ~a}.")   (:varcomma "{\\it ~a},")
      (:opt "{\\it ~a}")   (:optdot "{\\it ~a}.")   (:optcomma "{\\it ~a},")
      (:dquote "``~a''") (:dquotedot "``~a''.") (:dquotecomma "``~a'',")
      (:par "~%~%~%~a")
      (:math "$~a$") (:tmath "$~a$") (:lif "~a")
      (:dcode "~%\\begin{verbatim}~%~a~%\\end{verbatim}~%~%")
      (:dmath "~%$$~a$$~%") (:dots "\\ldots")))
