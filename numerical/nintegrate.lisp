(if (find-package :nintegrate ) t  
  (defpackage :nintegrate (:use common-lisp :gjl.lisp-util)
    (:nicknames :nint)))

;;; High-level interface to numerical integration routines.

;;; TODO
;;; Using qagi with qagp works, but it should be refactored and generalized.
;;; Everything else.

(in-package :nintegrate)

;; This should be moved to a more general location
;; This makes a an option expression that is common in Maxima:
;; e.g. epsrel=1e-5
(defmacro mkopt (opt)
  `(list '(maxima::mequal maxima::simp) ',opt ,opt))

;; If our option is named differently than option in
;; lower-level function.
(defmacro mkopt2 (opt val)
  `(list '(maxima::mequal maxima::simp) ',opt ,val))

(defparameter *quad-error-codes*
  `( "no problems" "too many sub-intervals" "excessive roundoff"
     "bad integrand behavior" "failed to converge" "probably divergent or slowly convergent"
     "invalid input"))

;; This should take a list of results
(defun combine-quad-results (r1 r2)
 "Add the results of integrating over two intervals. Make an attempt
  to write reasonable information fields. Really Should do sqrt of sqs of errors."
; (format t "~a~%" ($sconcat r1))
; (format t "~a~%" ($sconcat r2))
 (list '(maxima::mlist maxima::simp) (+ (second r1) (second r2))
       (+ (third r1) (third r2)) (+ (fourth r1) (fourth r2))
       (if (> (fifth r1) (fifth r2)) (fifth r1) (fifth r2))))

;; To reiterate: this could use refactoring!
(defun do-quad-pack (expr var lo hi singlist quad-ops)
  (cond ((and singlist (> (length singlist) 1))
         (cond ((and (eq 'maxima::$minf lo) (not (eq 'maxima::$inf hi)))
                (let* ((nsinglist (cdr singlist))
                       (nlo (first nsinglist))
                       (int1
                        (apply 'maxima::mfuncall (append `(maxima::$quad_qagi ,expr ,var maxima::$minf ,nlo) quad-ops)))
                       (int2
                        (apply 'maxima::mfuncall (append `(maxima::$quad_qagp ,expr ,var ,nlo ,hi 
                                                              ,(cons '(mlist simp) (cdr nsinglist))) quad-ops))))
                  (nint::combine-quad-results int1 int2)))
               ((and (not (eq 'maxima::$minf lo)) (eq 'maxima::$inf hi))
                (let* ((nsinglist (cdr singlist))
                       (rsinglist (reverse nsinglist))
                       (nhi (first rsinglist))
                       (n1singlist (reverse (cdr rsinglist)))
                       (int1
                        (apply 'maxima::mfuncall (append `(maxima::$quad_qagi ,expr ,var ,nhi maxima::$inf) quad-ops)))
                       (int2
                        (apply 'maxima::mfuncall (append `(maxima::$quad_qagp ,expr ,var ,lo ,nhi 
                                                              ,(cons '(mlist simp) n1singlist)) quad-ops))))
                  (nint::combine-quad-results int1 int2)))
               ((and (eq 'maxima::$minf lo) (eq 'maxima::$inf hi))
                (let* ((nsinglist (cdr singlist))
                       (nlo (first nsinglist))
                       (nnsinglist (cdr nsinglist))
                       (rsinglist (reverse nnsinglist))
                       (nhi (if (consp rsinglist) (first rsinglist) nlo))
                       (n1singlist (if (consp rsinglist) (reverse (cdr rsinglist)) nil))
                       (int1
                        (apply 'maxima::mfuncall (append `(maxima::$quad_qagi ,expr ,var ,lo ,nlo) quad-ops)))
                       (int2
                        (apply 'maxima::mfuncall (append `(maxima::$quad_qagi ,expr ,var ,nhi ,hi) quad-ops)))
                       (int3 (if (not (= nlo nhi))
                                 (apply 'maxima::mfuncall (append `(maxima::$quad_qagp ,expr ,var ,nlo ,nhi 
                                                                       ,(cons '(mlist simp) n1singlist)) quad-ops))
                               nil)))
                  (if (consp int3)
                      (nint::combine-quad-results (nint::combine-quad-results int1 int2) int3)
                    (nint::combine-quad-results int1 int2))))
               (t
                (apply 'maxima::mfuncall (append `(maxima::$quad_qagp ,expr ,var ,lo ,hi ,singlist) quad-ops)))))
        ((and (not (eq 'maxima::$minf lo)) (not (eq 'maxima::$inf hi)))
         (apply 'maxima::mfuncall (append `(maxima::$quad_qags ,expr ,var ,lo ,hi) quad-ops)))
        ((or (eq 'maxima::$minf lo) (eq 'maxima::$inf hi))
         (apply 'maxima::mfuncall (append `(maxima::$quad_qagi ,expr ,var ,lo ,hi) quad-ops)))
        (t (merror1 "nintegrate: cannot compute this integral."))))

(in-package :maxima)

(max-doc:set-cur-sec 'max-doc::numerical-fandv)
(defmfun1:set-mext-package "numerical")

(defmfun1 ($nintegrate :doc) ( expr (varspec :list) &optional (singlist :list) &opt 
          ($words t :bool) ($subint 200 :non-neg-int) ($epsabs 0 :non-neg-number)
          ($epsrel 1d-8 :non-neg-number)) ; ($method "automatic" :string))
  :desc ("Numerically integrate " :arg "expr" ", with the variable and limits supplied in "
  :argdot "varspec" " At present nintegrate is not very capable."
  " However, it does automatically choose and combine qags, qagp, and qagi. See the Maxima documentation for quadpack." )
  (let* ( (vp (rest varspec)) (var (first vp))
          (lo (second vp)) (hi (third vp))
          (quad-ops (list (nint::mkopt $epsrel) (nint::mkopt $epsabs)
                          (nint::mkopt2 $limit $subint)))
          (result (nint::do-quad-pack expr var lo hi singlist quad-ops)))
    (cond ((consp result)
           (when $words (setf (nth 4 result) (nth (nth 4 result) nint::*quad-error-codes*)))
           result)
          (t nil))))

(examples:clear-add-example "nintegrate"
                            '(:pretext "A list of intermediate points may be supplied. Compare the
following to the equivalent calls in the documentation for quadpack. Note that some of these
examples cannot be done with a single call to quadpack. These examples more or less exhaust
the current capabilities of the nintegrate interface. (These examples are only meant to show the interface;
there are no singularities here.)"
                              :code-res 
                              ( ("nintegrate(exp(-3*x^2),[x,minf,inf])" 
                                 "[1.023326707946489, 4.845541074534523e-12, 270, no problems]")
                                ("nintegrate(exp(-3*x^2),[x,-20,20])" 
                                 "[1.023326707946489, 1.049488242728112e-12, 399, no problems]")
                                ("nintegrate(exp(-3*x^2),[x,minf,inf] , [-1,0,1])"
                                 "[1.023326707946489, 4.758672504515692e-12, 252, no problems]")
                                ("nintegrate(exp(-3*x^2),[x,minf,20] , [-1,0,1])"
                                 "[1.023326707946489, 2.471535082631379e-10, 294, no problems]")
                                ("nintegrate(exp(-3*x^2),[x,-20,20] , [-1,0,1])"
                                 "[1.023326707946489, 4.895483415855454e-10, 336, no problems]"))))

