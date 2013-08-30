(in-package :maxima)
(mext:mext-optimize)
(max-doc:set-cur-sec 'max-doc::doc-fandv)
(defmfun1:set-file-and-package "maxdoc-utils.lisp" "maxdoc")

(maxima::ddefun keywordify (s)
  "Convert a Maxima symbol to a lisp keyword. E.g. `$a' -> `:a' ."
  (intern (subseq (symbol-name s) 1) :keyword))
