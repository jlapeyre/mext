;; Only load mext once. Need this for gcl,
;; because, in gcl, loading mext always resets *default-pathname-defaults* to
;; whatever (truename ".") gives at the moment
(if (find-package :mext-maxima ) t
  ($load  "mext_load.lisp"))
