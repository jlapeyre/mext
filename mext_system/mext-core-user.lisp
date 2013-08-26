;; Only load mext once. Need this for gcl,
;; because, in gcl, loading mext always resets *default-pathname-defaults* to
;; whatever (truename ".") gives at the moment
;; Load the main packages, not repackaged third party things.
;; This file will be the first mext file loaded by the user:
;; e.g. load(mext_core);
;; The minimum mext system is loaded with load(mext).

; Try to reload all, but don't force
;(if (find-package :mext-maxima ) t
;  (progn

($load  "mext_load.lisp")
(loop for mext-package in '( "mext_defmfun1" "lists_aex" "discrete_aex" "numerical" ) do
      ($require mext-package))

