(if (find-package :nintegrate ) t  
  (defpackage :nintegrate (:use common-lisp :gjl.lisp-util)
    (:nicknames :nint)))

;;; High-level interface to numerical integration routines.

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

(in-package :maxima)

(defmfun1 ($nintegrate :doc) ( expr (varspec :list) &optional (singlist :list) &opt 
          ($subint 200 :non-neg-int) ($epsabs 0 :non-neg-number)
          ($epsrel 1d-8 :non-neg-number) ($method "automatic" :string))
  :desc ("Numerically integrate " :arg "expr" ", with the variable and limits supplied in "
  :argdot "varspec" " At present nintegrate is not very capable.")
  (let* ( (vp (rest varspec)) (var (first vp))
          (lo (second vp)) (hi (third vp))
          (quad-ops (list (nint::mkopt $epsrel) (nint::mkopt $epsabs)
                          (nint::mkopt2 $limit $subint)))
          (result
           (cond (singlist 
                  (apply 'mfuncall (append `($quad_qagp ,expr ,var ,lo ,hi ,singlist) quad-ops)))
                 ((and (numberp lo) (numberp hi))
                  (apply 'mfuncall (append `($quad_qags ,expr ,var ,lo ,hi) quad-ops)))
                 ((or (eq '$minf lo) (eq '$inf hi))
                  (apply 'mfuncall (append `($quad_qagi ,expr ,var ,lo ,hi) quad-ops)))
                 (t nil))))
    (cond ((listp result)
           (setf (nth 4 result) (nth (nth 4 result) nint::*quad-error-codes*))
           result)
          (t nil))))
