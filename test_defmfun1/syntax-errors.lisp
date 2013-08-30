(defmfun1:set-file-and-package "syntax-errors.lisp" "test_defmfun1")
#|

  Illegal defmfun1 invocations.
  Need to find a way to automate testing these.
  I test these by hand and see that they give
  an error message, rather than memory corruption.

|#

;; keyword before variable name
;(defmfun1 $etest1 ((:non-neg-int x )) x)

;; list of stuff before variable name
;(defmfun1 $etest1a (((:non-neg-int) x )) x)

;; no argument list
;(defmfun1 $etest2 x)

;; unknown test type
;(defmfun1 $etest3 ((yy :giraffe)))

;; unknown list test type
;(defmfun1 $etest3a ((yy (:giraffe))))

;(defmfun1 7 $etest3a ((yy (:giraffe))))

; null function name
(defmfun1 () (zz))


