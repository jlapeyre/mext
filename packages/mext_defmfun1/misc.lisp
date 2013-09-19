(in-package :maxima)

(max-doc:set-cur-sec 'max-doc::runtime-fandv)
(defmfun1:set-file-and-package "misc.lisp" "mext_defmfun1")

(defmfun1 ($format1 :doc) (expr)
  :desc ("This calls the lisp function format1. It is mostly for testing code.")
  (format1 expr))

(defmfun1 ($nformat :doc) (expr)
  :desc ("This calls the lisp function nformat. It is mostly for testing code.")
  (nformat expr))

(defmfun1 ($zerop :doc) (x)
  :desc 
  ("Returns " :code "true" " if " :arg "x" 
   " represents the number zero, otherwise, " :codedot "false"
   " This does not try to manipulate " :arg "x" " to determine
    if it can be reduced to the number zero.")
  (cond ((numberp x) (zerop x))
        (($bfloatp x) (zerop (cadr x)))
;        (($ratp x) (equal 0 (cadr x)))
        (($ratp x) (zerop (cadr x))) ; when would this fail ?
        (t nil)))

;; This is from RJF
;; zerop(x):=if bfloatp(x) then ?zerop(?cadr (x))
;;            else if ?numberp(x) then ?zerop(x)
;;            else if ratp(x) then ?equal(0, ?cadr (x))
;;            else false;

(defmfun1 ($chop :doc) ((x :thread) &opt ($eps 1e-16 :non-neg-number))
  :desc
  ("Return " :code "0" " if " :arg "x" " is closer to zero than
    the option " :optdot "eps")
  (if (and (numberp x) (<= (abs x) $eps)) 0
    x))
