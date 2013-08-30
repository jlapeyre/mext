(defmfun1 $dtest1  ()
  t)

(defmfun1 ($dtest1a)  ()
  t)

(defmfun1 $dtest2  ( x )
  x)

(defmfun1 $dtest3  ( (x :non-neg-int) )
  x)

(defmfun1 $dtest4  ( (x (:int-range -1 1) ) )
  x)

(defmfun1::set-match-form `( $dtest1 $dtest1a $dtest2 $dtest3 $dtest4 ))

(defmfun1 ( $dtest5 ) ( x &opt ($extra nil e-supplied-p) )
  (format t "x is ~a, extra is ~a~%" x $extra)
  (format t "supplied ~a~%" e-supplied-p))
