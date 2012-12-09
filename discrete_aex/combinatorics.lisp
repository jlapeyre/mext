(in-package :maxima)
(mext:mext-optimize)
;(declaim (optimize (speed 3) (space 0) (safety 0) (debug 0)))
(use-package :gjl.lisp-util)

(max-doc:set-cur-sec 'max-doc::combinatorics-fandv)
(defmfun1:set-mext-package "discrete_aex")

;;; The use of (aex-get ...) and (setf (aex-get ...)) etc is a
;;; inefficient because the array is found in the aex struct each time. But it
;;; preserves some abstraction. In these routines, some quick tests showed that it
;;; does not slow them significantly.

;; using macros aex-get rather thant $aex_get  (and set) does not seem to
;; change speed noticably, with casual profiling

;; I also tried collecting the random numbers in an array or list without using
;; the code in $random that checks for argument type and bounds etc. with each call.
;; This is, however slower, I suppose because of allocating memory.

(defmfun-ae ($ae_random_permutation :doc) ( (a :non-atom)  )
  :desc ("returns " var "a" " with subexpressions permuted randomly.")
  (let* ( (a1 (aex-cp a)) ; aex-cp is like aex_cp, but no arg checks , etc.
          (n (aex-length a1)))
    (declare (fixnum n))
    (dotimes (i n)
      (declare (fixnum i))
      (let ( (j (+ i ($random (- n i))))
             (tmp (aex-get  a1 i)) )
        (declare (fixnum j))
        (setf (aex-get a1 i) (aex-get a1 j))
        (setf (aex-get a1 j) tmp)))
    (defmfun-final-to-ae a1)))

;;  Translated from matlab code by  Derek O'Connor 20 March 2011
;;  If ce(n) is the number of even-length cycles in a
;;  permutation p of length n, then one of the formulas for the
;;  sign of a permutation p is sgn(p)=(−1)ce(n).
;;  Here is an O(n) Matlab function that computes the sign of a
;;  permutation p by traversing each cycle of p and (implicitly)
;;  counting the number of even-length cycles.

(defmfun1 ($signature_permutation :doc) ( (ain :ae-list ))
  (let* ((a (aex-cp ain :adjustable nil :element-type 'fixnum))
         (n (aex-length a))
         (visited (make-array n :element-type 'bit  :initial-element 0))
         (knext 0)
         (L 0)
         (sgn 1))
    (declare (fixnum sgn L knext n)) ; improves speed by factor of 5 with sbcl
    (dotimes (k n)
      (declare (fixnum k)) ; does nothing
      (when (= 0 (bit visited k))
        (setf knext k L 0)
        (loop while (= 0 (bit visited knext)) do
             (incf L)
             (setf (bit visited knext)  1)
             (setf knext (1- (the fixnum (aex-get a knext)))))
        (when (evenp L) (setf sgn (- sgn)))))
    sgn))
             
(add-call-desc '( "signature_permutation" ("list")
 ("returns the sign, or signature, of the symmetric permutation " argcomma "list"
  " which must be represented by a permuation the integers from " math "1" 
  " through " math "n" ", where " math "n" " is  the length of the list.")))

(defmfun-ae ($perm_to_cycles :doc) ( (ain :ae-list ))
  :desc ("Returns a cycle decomposition of the input permutation " argdot "ain"
  " The input must be a permutation of " code "n" " integers from 1 through " code "n" ".")
  (let* ((a (aex-cp ain :adjustable nil :element-type 'fixnum))
         (arr (aex-arr a))
         (n (aex-length a))
         (visited (make-array n :element-type 'bit :initial-element 0) )
         (knext 0)
         (one-cycle nil)
         (all-cycles nil))
    (declare (fixnum knext n)) ; improves speed by factor of 5 with sbcl
    (dotimes (k n)
      (declare (fixnum k)) ; probably automatic because n is fixnum
      (when (= 0 (bit visited k))
        (when one-cycle (push (defmfun-final-to-ae (cons '(mlist simp) one-cycle)) all-cycles)
              (setf one-cycle nil))
        (setf knext k)
        (loop while (= 0 (bit visited knext)) do
             (push (the fixnum (1+ knext)) one-cycle)
             (setf (bit visited knext)  1)
             (setf knext (the fixnum (1- (the fixnum (aref arr knext))))))))
    (when one-cycle (push (defmfun-final-to-ae (cons '(mlist simp) one-cycle)) all-cycles))
    (defmfun-final-to-ae (cons '(mlist simp) all-cycles))))

(examples::clear-examples "perm_to_cycles")
(examples::add-example "perm_to_cycles" '( :code "perm_to_cycles([5,4,3,2,1,10,6,7,8,9])"))

;; again, 'the fixnum' does not seem to add efficiency in some quick tests.
;; Conversion with raex seems a bit slow. It takes about half the total time.
;; This could be made more efficient.
(defmfun-ae ($cycles_to_perm :doc) ((cycles :ae-list))
  :desc ("Returns a permutation from its cycle decomposition "
         arg "cycles" ", which is a list of lists. Here 'permutation' means
        a permutation of a list of the integers from " math "1" " to some number "
        math "n" ". The default output representation  is aex.")
  (let* ((ncycles (cdr ($lex (raex cycles '(2))))) ;convert only on level 2. ensure top is lex
         (n (loop for cyc in ncycles summing (aex-length cyc)))
         (perm ($aex_new n)))
    (declare (fixnum n))
    (loop for cyc in ncycles do
          (let ((ncyc (1- (aex-length cyc))))
            (declare (fixnum ncyc))
            (setf (aex-get perm (the fixnum (1- (aex-get cyc 0)))) 
                  (the fixnum (aex-get cyc ncyc)))
            (loop for i fixnum from 0 to (1- ncyc) do
                  (setf (aex-get perm 
                                 (the fixnum (1- (the fixnum (aex-get cyc (1+ i)))))) 
                        (the fixnum (aex-get cyc i))))))
    (defmfun-final-to-ae perm)))

;; could be more efficient
(defmfun-ae ($perm_to_transpositions :doc) ( (ain :ae-list ))
  :desc ("Returns a list representing the permutation " arg "ain"
         " as a product of transpositions. The output representation type
         is applied at both levels.")
  (let ((cycles ($perm_to_cycles ain (defmfun1:rule $ot '$ml))))
    (defmfun-final-to-ae
      (cons '(mlist simp)
            (if (eq o-type 'maxima::$ml)
                (loop for cyc in cycles append
                      (if (< (ilength cyc) 2) nil
                        (if (< (ilength cyc) 3)
                          (list cyc)
                          (loop for e in (cddr cyc) collect
                                (list '(mlist simp) (cadr cyc) e)))))
              (loop for cyc in cycles append
                    (if (< (ilength cyc) 2) nil
                      (if (< (ilength cyc) 3)
                          (list (aex-to cyc))
                        (loop for e in (cddr cyc) collect
                              (aex-to (list '(mlist simp) (cadr cyc) e)))))))))))

;; Need to implement routine for aex input      
(defmfun-ae ($transpositions_to_perm :doc) ((ain :ae-list ))
 :desc ("returns the permutation specified by the list of
         transpositions " argdot "ain")
  (setf ain (cdr (rlex ain 2))) ; convert top two levesl to lex
  (let* ((n (max (loop for pair in ain maximize (second pair))
                 (loop for pair in ain maximize (third pair))))
         (perm (aex-to ($lrange n))))
    (loop for pair in ain do
          (let* ((one (second pair))
                 (two (third pair))
                 (tmp (aex-get perm (1- one))))
            (setf (aex-get perm (1- one)) (aex-get perm (1- two)))
            (setf (aex-get perm (1- two)) tmp)))
    (defmfun-final-to-ae perm)))

(max-doc:implementation "transpositions_to_perm" 
 "Input is converted to lex on both levels. Default output is aex.")

;; testing using aex-get vs. directly using aref.
;; I see no speed difference with perm of length 5*10^6
(defmfun-ae ($inverse_permutation :doc) ((perm :ae-list))
  :desc ("returns the inverse permutation of " argdot "perm")
  (let* ((n (ilength perm))
         (iperm ($aex_new n))
         (aperm (aex-to perm))
         (iperm-arr (aex-arr iperm))
         (aperm-arr (aex-arr aperm)))
    (declare (fixnum n))
;    (loop for i fixnum from 0 to (1- n) do
;          (setf (aex-get iperm (1- (aex-get aperm i))) (1+ i)))
    (loop for i fixnum from 0 to (1- n) do
          (setf (aref iperm-arr (1- (aref aperm-arr i))) (1+ i)))
    (defmfun-final-to-ae iperm)))

(examples::clear-add-example "inverse_permutation" 
 '( :code ( "inverse_permutation([5,1,4,2,6,8,7,3,10,9])"
            "inverse_permutation(inverse_permutation([5,1,4,2,6,8,7,3,10,9]),ot->ml)")))

(defmfun1 ($permutation_p :doc) (ain)
  (if ($ae_listp ain)
      (let* ((n (ilength ain))
             (visited (make-array n :element-type 'bit  :initial-element 0)))
        (declare (fixnum n))
        (if (aex-p ain) 
            (let ((a (aex-arr ain))  ; ugly array stuff
                  (xa 0)
                  (flag nil))
              (declare (boolean flag))
              (loop for i fixnum from 0 below n do
                    (setf xa (aref a i))
                    when (or (not (typep xa 'fixnum)) (< (the fixnum xa) 1)
                             (> (the fixnum xa) n) (= 1 (bit visited (the fixnum (1- (the fixnum xa))))))
                    do (setf flag t)
                    do (if flag (return)) (setf (bit visited (the fixnum (1- (the fixnum xa)))) 1))
              (not flag))
          (do* ((x (cdr ain) (cdr x))
                (x1 (car x) (car x)))  ; "the fixnum" did not improve speed here much if at all.
              ((or (null x) (not (typep x1 'fixnum)) (< (the fixnum x1) 1) (> (the fixnum x1) n)
                   (= 1 (bit visited (the fixnum (1- (the fixnum x1)))))) (null x))
            (setf (bit visited (the fixnum (1- (the fixnum x1)))) 1))))
    '$false))
    
  
(add-call-desc '("permutation_p" ("list") ("Returns true if the list " arg "list" " of length "
 math "n" " is a permutation of the integers from " math "1" " through " math "n" ". Otherwise returns false.")))

(max-doc:implementation "permutation_p" 
 "Separate routines for aex and lex input are used.")

(defmfun1 ($permutation_p1 :doc) (ain)
 :desc ("This is the same as " mrefcomma "permutation_p" " but, if the input is a list,
   it assumes all elements in the input list are fixnum integers, while " mref "permutation_p" 
   " does not.")
  (if ($ae_listp ain)
      (let* ((n (ilength ain))
             (visited (make-array n :element-type 'bit  :initial-element 0)))
        (declare (fixnum n))
        (if (aex-p ain) 
            (let ((a (aex-arr ain))  ; ugly array stuff
                  (xa 0)
                  (flag nil))
              (declare (boolean flag) (fixnum xa))
              (loop for i fixnum from 0 below n do
                    (setf xa (aref a i))
                    when (or (< (the fixnum xa) 1)
                         (> (the fixnum xa) n) (= 1 (bit visited (the fixnum (1- (the fixnum xa))))))
                    do (setf flag t)
                    do (if flag (return)) (setf (bit visited (the fixnum (1- (the fixnum xa)))) 1))
              (not flag))
          (do* ((x (cdr ain) (cdr x))
                (x1 (car x) (car x)))  ; "the fixnum" did not improve speed here much if at all.
              ((or (null x) (< (the fixnum x1) 1) (> (the fixnum x1) n)
                   (= 1 (bit visited (the fixnum (1- (the fixnum x1)))))) (null x))
            (declare (fixnum x1))
            (setf (bit visited (the fixnum (1- (the fixnum x1)))) 1))))
    '$false))
  
(max-doc:implementation "permutation_p1" 
 "Some variables are declared fixnum, but this does not seem to
  improve performance with respect to permutationp.")

(defmacro rand-perm-sym-body ( cycle-p )
  "cycle-p true generates the body for a random cycle;
   nil for a random permutation."
  (flet ((rand-perm-sym-loop ( cycle-p )
           `(loop for i fixnum from 1 to (the fixnum (1- n)) do
                 (setf j ($random  ,(if cycle-p `i `(the fixnum (1+ i))  )))
                 (setf (aref a i) (aref a j))
                 (setf (aref a j) (the fixnum (1+ i)) ))))
    `(if (eq o-type '$ml )
         (let ((a (make-array n :element-type 'fixnum :adjustable nil :fill-pointer nil ))
               (j 0))
           (declare(fixnum j))
           (setf (aref a 0) 1)
           (cons '(mlist simp) (progn ,(rand-perm-sym-loop cycle-p)
                                      (coerce  a 'list ))))
         (let* ((oa (aex-make-n-head n :adjustable adj-type))
                (a (aex-arr oa))
                (j 0))
           (declare(fixnum j))
           (setf (aref a 0) 1)
           ,(rand-perm-sym-loop cycle-p)
           oa))))

(defmfun-ae ($random_permutation_sym :doc) ((n :pos-int))
  (rand-perm-sym-body nil))

(add-call-desc '("random_permutation_sym" ("n") 
  ("Returns a random permutation of the integers from " math "1" " through " argdot "n"
   " This represents a random element of the symmetric group " math "S_n" ".")))

(defmfun-ae ($random_cycle :doc) ((n :pos-int))
  (rand-perm-sym-body t))

(add-call-desc '("random_cycle" ("n") 
 ("Returns a random cycle of length " argdot "n" " The return value is a list
   of the integers from " math "1" " through " argcomma "n" " representing an
   element of the symmetric group " math "S_n" " that is a cycle.")))

(max-doc:implementation "random_cycle" "This function uses Sattolo's algorithm.")

(max-doc:see-also-group '( "random_cycle" "random_permutation_sym" "ae_random_permutation"
                            "signature_permutation" "perm_to_cycles" "cycles_to_perm"))
