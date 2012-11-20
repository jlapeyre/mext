(in-package :maxima)
(declaim (optimize (speed 3) (space 0) (safety 0) (debug 0)))
(use-package :gjl.lisp-util)

(max-doc::set-cur-sec 'max-doc::combinatorics-fandv)

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
  "Return expression a with random permutation of arguments."
  (let* ( (a1 (aex-cp a)) ; aex-cp is like aex_cp, but no arg checks , etc.
          (n   (aex-length a1)) )
    (declare (fixnum n))
    (dotimes (i n)
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
      (when (= 0 (bit visited k))
        (setf knext k L 0)
        (loop while (= 0 (bit visited knext)) do
             (incf L)
             (setf (bit visited knext)  1)
             (setf knext (1- (the fixnum (aex-get a knext)))))
        (when (evenp L) (setf sgn (- sgn)))))
    sgn))
             
(add-call-desc '( "signature_permutation" ("list")
                 ( "returns the sign or signature of the symmetric permutation " arg "list"
                  ", which must be represented by a permuation the integers from 1 through n,"
                  " with n the length of the list.")))

(defmfun-ae ($perm_cycles :doc) ( (ain :ae-list ))
  "Returns a cyclic decomposition of the input permutation."
  (let* (
         (a (aex-cp ain :adjustable nil :element-type 'fixnum))
         (arr (aex-arr a))
         (n (aex-length a))
         (visited (make-array n :element-type 'bit :initial-element 0) )
         (knext 0)
         (one-cycle nil)
         (all-cycles nil))
    (declare (fixnum knext n)) ; improves speed by factor of 5 with sbcl
    (dotimes (k n)
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

(examples::clear-examples "perm_cycles")
(examples::add-example "perm_cycles" '( :code "perm_cycles([5,4,3,2,1,10,6,7,8,9])"))

(defmfun1 ($permutation_p :doc) ( (ain :ae-list))
  (let* ((n (ilength ain))
         (visited (make-array n :element-type 'bit  :initial-element 0)))
    (declare (fixnum n))
    (if (aex-p ain) (let ((a (aex-arr ain))  ; ugly array stuff
                          (xa 0)
                          (flag nil))
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
          (setf (bit visited (the fixnum (1- (the fixnum x1)))) 1)))))

(add-call-desc '("permutation_p" ("list") ("Returns true if the list " arg "list" " of length n is a permutation "
 "of the integers from 1 through n. Otherwise returns false.")))                                          

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
(add-call-desc '("random_permutation_sym" ("n") ("Returns a random permutation of the integers from 1 through " arg "n"
                                                 ". This is a random permutation of the symmetric group S_n.")))

(defmfun-ae ($random_cycle :doc) ((n :pos-int))
  (rand-perm-sym-body t))
(add-call-desc '("random_cycle" ("n") ("Returns a random cycle of length " arg "n" ".")))

(max-doc::implementation "random_cycle" "This function uses Sattolo's algorithm.")

(max-doc::see-also-group '( "random_cycle" "random_permutation_sym" "ae_random_permutation"
                            "signature_permutation" "perm_cycles"))
