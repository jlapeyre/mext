(defmfun1:set-file-and-package "test1.lisp" "test_defmfun1")

#|

 This file is meant to test, as well as document, features of defmfun1.
 But, mostly, it is for testing.

|#

;; Function must take 0 args.
(defmfun1 $dtest1 ()
  t)

;; Alternate syntax. There may be other directives with the function name
(defmfun1 ($dtest1a) ()
  t)

;; Function must take 1 arg
(defmfun1 $dtest2 ( x )
  x)

;; Function must take 1 arg, which is a non-negative integer
(defmfun1 $dtest3 ((x :non-neg-int))
  x)

#|  

 These are syntactically incorrect. I want to find a way to test
 that they produce the intended errors.

 This does not work
(handle-cases
 (defmfun1 $dtest3a ((x :giraffe))
   x)
 (error ()
    (when (not (eq (maxima::$error_code 'defmfun1_unknown_directive)))
      (error "Wrong error code from expanding dtest3a"))))
|#

; Invalid defmfun1 code.
;(defmfun1 $dtest3a ((:non-neg-int x ))
;  x)

;; The argument must be one of: -1,0,1
(defmfun1 $dtest4 ((x (:int-range -1 1)))
  x)

; (defmfun1::set-match-form `( $dtest1 $dtest1a $dtest2 $dtest3 $dtest4 ))

;; Takes an `option'.
;; We want to know whether the optional argument was passed.
;; In this case, we must supply a default value before the
;; name of the `supplied' variable.
(defmfun1 $dtest5 (x &opt ($extra nil e-supplied-p) )
  (format t "x is ~a, extra is ~a~%" x $extra)
  (format t "Was `extra' passed as an argument ? : ~a~%" e-supplied-p)
  (cons '(mlist) (list x $extra e-supplied-p )))

;; Takes two options.
(defmfun1 $dtest6 (x &opt ($extra nil e-supplied-p) ($opt2 1 opt2-supplied-p))
  (format t "x is ~a, extra is ~a, opt2 is ~a~%" x $extra $opt2)
  (format t "Was `extra' passed as an argument ? : ~a~%" e-supplied-p)
  (format t "Was `opt2' passed as an argument ? : ~a~%" opt2-supplied-p)
  (cons '(mlist) (list x $extra e-supplied-p $opt2 opt2-supplied-p )))

;; Use a lisp symbol for option that differs from option name
;; Option must be boolean.
(defmfun1 $dtest7 ( &opt (($myopt my-opt) t my-opt-p :bool))
  (cons '(mlist) (list my-opt my-opt-p)))
(defmfun1::set-match-form `$dtest7)

;; Can use &aux as with defun
(defmfun1 $dtest8 ( x &aux y )
  (setf y x)
  y)

(defmfun1 ($dtest9 :doc :match ) (x)
  :desc "This is the description of dtest9"
  x)
