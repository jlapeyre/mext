(defmfun1:set-file-and-package "test1.lisp" "test_defmfun1")

(defmfun1 $dtest1 ()
  t)

(defmfun1 ($dtest1a) ()
  t)

(defmfun1 $dtest2 ( x )
  x)

(defmfun1 $dtest3 ((x :non-neg-int))
  x)

#|  I have to find a way to check that these incorrect definitions
    cause the intended error.
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

(defmfun1 $dtest4 ((x (:int-range -1 1)))
  x)

; (defmfun1::set-match-form `( $dtest1 $dtest1a $dtest2 $dtest3 $dtest4 ))

;; We want to know whether the optional argument was passed.
;; In this case, we must supply a default value before the
;; name of the `supplied' variable.
(defmfun1 $dtest5 (x &opt ($extra nil e-supplied-p) )
  (format t "x is ~a, extra is ~a~%" x $extra)
  (format t "Was `extra' passed as an argument ? : ~a~%" e-supplied-p)
  (cons '(mlist) (list x $extra e-supplied-p )))

(defmfun1 $dtest6 (x &opt ($extra nil e-supplied-p) ($opt2 1 opt2-supplied-p))
  (format t "x is ~a, extra is ~a, opt2 is ~a~%" x $extra $opt2)
  (format t "Was `extra' passed as an argument ? : ~a~%" e-supplied-p)
  (format t "Was `opt2' passed as an argument ? : ~a~%" opt2-supplied-p)
  (cons '(mlist) (list x $extra e-supplied-p $opt2 opt2-supplied-p )))
