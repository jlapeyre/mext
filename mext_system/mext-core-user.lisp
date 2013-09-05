;; This file is copied to the user's maxima directory and
;; renamed: mext_core.lisp.  In fact, this message will be
;; visible. It may be the first file loaded to get
;; both the mext system and core packages. See below.

;; Only load mext once. Need this for gcl,
;; because, in gcl, loading mext always resets *default-pathname-defaults* to
;; whatever (truename ".") gives at the moment.
;; Load mostly the main packages, few repackaged third party things.
;; This file will be the first mext file loaded by the user:
;; e.g. load(mext_core);
;; The minimum mext system is loaded with load(mext).

($load "mext_load.lisp")
(loop :for mext-package :in '( "mext_defmfun1" "lists_aex" "runtime"
                             "discrete_aex" "numerical" 
                             "alt_eigen" "tpsolve" ) :do
      ($require mext-package))


