(in-package :maxima)
(mext:mext-optimize)
(max-doc:set-cur-sec 'max-doc::doc-fandv)
(doc-system:set-source-file-name "maxima-utils.lisp")
(doc-system:set-source-package "maxdoc")

(maxima::ddefun keywordify (s)
  (intern (subseq (symbol-name s) 1) :keyword))
