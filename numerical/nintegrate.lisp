(if (find-package :nintegrate ) t  
  (defpackage :nintegrate (:use common-lisp :gjl.lisp-util)
    (:nicknames :nint)))

;;; High-level interface to numerical integration routines.

;;; TODO
;;; * Using qagi with qagp works, but it should be refactored and generalized.
;;; * Make better combined error estimate.
;;; * store calls to quadpack for diagnostics
;;; * use assume(x>lo, x<hi) etc. when searching for roots.
;;; Everything else.

;;; Note what happens to sqrt(x). We do this before trying to integrate!
;;; (%i58) rectform(sqrt(x));
;;;              atan2(0, x)                     atan2(0, x)
;;;(%o58) %i sin(-----------) sqrt(abs(x)) + cos(-----------) sqrt(abs(x))
;;;                   2                               2

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

;; This should take a list of results, rather than just two
(defun combine-quad-results (r1 r2)
 "Add the results of integrating over two intervals. Make an attempt
  to write reasonable information fields. Really Should do sqrt of sqs of errors."
; (format t "~a~%" ($sconcat r1))
; (format t "~a~%" ($sconcat r2))
 (list '(maxima::mlist maxima::simp) (+ (second r1) (second r2))
       (+ (third r1) (third r2)) (+ (fourth r1) (fourth r2))
       (if (> (fifth r1) (fifth r2)) (fifth r1) (fifth r2))
       (append (sixth r1) (rest (sixth r2)))))

(defun combine-real-imag-results (r1 r2)
  (cond ((and (consp r1) (consp r2))
         (list '(maxima::mlist maxima::simp) (maxima::simplify `((maxima::mplus) ,(second r1) ,(second r2)))
               (+ (third r1) (third r2)) (+ (fourth r1) (fourth r2))
               (if (> (fifth r1) (fifth r2)) (fifth r1) (fifth r2))
               (append (sixth r1) (rest (sixth r2)))))
        ((consp r1) r1)
        (t r2)))

(defparameter *quad-names* '( :qagi maxima::$quad_qagi :qagp maxima::$quad_qagp :qags maxima::$quad_qags))

(defun quad-call (f expr var lo hi q-ops &optional slist)
  (let* ((fn (getf *quad-names* f))
         (call-list
          (if slist
              (append `(,fn ,expr ,var ,lo ,hi ,slist) q-ops)
            (append `(,fn ,expr ,var ,lo ,hi) q-ops)))
         (call-form (cons (list (car call-list) 'maxima::simp) (cdr call-list))))
;    (format t "~a~%" (maxima::$sconcat call-form))
    (append (apply 'maxima::mfuncall call-list) (list (list '(maxima::mlist maxima::simp) call-form)))))

;; To reiterate: this could use refactoring!
(defun do-quad-pack (expr var lo hi singlist quad-ops)
;  (format t "~a~%" (maxima::$sconcat expr))
  (when (not singlist)
    (let ((roots (apply 'maxima::mfuncall `(maxima::$solve ((maxima::mexpt maxima::simp) ,expr -1))))
          (nroots))
      (dolist (r (cdr roots))
        (let ((n (third r)))
          (when (maxima::$numberp n)
            (let ((nn (maxima::$float n)))
              (when (not
                     (or (and (maxima::$numberp hi) (> 1e-10 (abs (- nn hi))))
                         (and (maxima::$numberp lo) (> 1e-10 (abs (- nn lo))))))
                (push (maxima::$float n) nroots))))))
      (setf singlist (cons '(maxima::mlist maxima::simp) (sort nroots  #'<)))))
  (cond ((and singlist (> (length singlist) 1))
         (cond ((and (eq 'maxima::$minf lo) (not (eq 'maxima::$inf hi)))
                (let* ((nsinglist (cdr singlist))
                       (nlo (first nsinglist))
                       (int1
                        (quad-call :qagi expr var maxima::$minf nlo quad-ops))
                       (int2
                        (quad-call :qagp expr var nlo hi quad-ops (cons '(maxima::mlist simp) (cdr nsinglist)))))
                  (nint::combine-quad-results int1 int2)))
               ((and (not (eq 'maxima::$minf lo)) (eq 'maxima::$inf hi))
                (let* ((nsinglist (cdr singlist))
                       (rsinglist (reverse nsinglist))
                       (nhi (first rsinglist))
                       (n1singlist (reverse (cdr rsinglist)))
                       (int1 (quad-call :qagi expr var nhi 'maxima::$inf quad-ops))
                       (int2
                        (quad-call :qagp expr var lo nhi quad-ops (cons '(maxima::mlist simp) n1singlist))))
                  (nint::combine-quad-results int1 int2)))
               ((and (eq 'maxima::$minf lo) (eq 'maxima::$inf hi))
                (let* ((nsinglist (cdr singlist))
                       (nlo (first nsinglist))
                       (nnsinglist (cdr nsinglist))
                       (rsinglist (reverse nnsinglist))
                       (nhi (if (consp rsinglist) (first rsinglist) nlo))
                       (n1singlist (if (consp rsinglist) (reverse (cdr rsinglist)) nil))
                       (int1
                        (quad-call :qagi expr var lo nlo quad-ops))
                       (int2
                        (quad-call :quagi expr var nhi hi quad-ops))
                       (int3 (if (not (= nlo nhi))
                                 (quad-call :qagp expr var nlo nhi quad-ops (cons '(maxima::mlist simp) n1singlist))
                               nil)))
                  (if (consp int3)
                      (nint::combine-quad-results (nint::combine-quad-results int1 int2) int3)
                    (nint::combine-quad-results int1 int2))))
               (t
                (quad-call :qagp expr var lo hi quad-ops singlist))))
        ((and (not (eq 'maxima::$minf lo)) (not (eq 'maxima::$inf hi)))
         (quad-call :qags expr var lo hi quad-ops))
        ((or (eq 'maxima::$minf lo) (eq 'maxima::$inf hi))
         (quad-call :qagi expr var lo hi quad-ops))
        (t (maxima::merror1 "nintegrate: cannot compute this integral."))))

(in-package :maxima)

(max-doc:set-cur-sec 'max-doc::numerical-fandv)
(defmfun1:set-mext-package "numerical")

(defmfun1 ($nintegrate :doc) ( expr (varspec :list) &optional (singlist :list) &opt 
          ($words t :bool) ($subint 200 :non-neg-int) ($epsabs 0 :non-neg-number)
          ($epsrel 1d-8 :non-neg-number) ; ($method "automatic" :string))b
          ($calls nil :bool))
  :desc ("Numerically integrate " :arg "expr" ", with the variable and limits supplied in the list "
  :arg "varspec" " as ["  :argcomma "var" :argcomma "lo" :arg "hi" "]."
  " Only one-dimensional integrals are implemented."
  " " :mref "nintegrate" " automatically chooses and combines "
  :emrefcomma "qags" :emrefcomma "qagp" " and " :emrefdot "qagi"
  " Some support for complex numbers is implemented." " Some integrable singularities are found automatically."
  " See the Maxima documentation for quadpack." )
  (let* ((vp (rest varspec)) (var (first vp))
         (lo (second vp)) (hi (third vp))
         (quad-ops (list (nint::mkopt $epsrel) (nint::mkopt $epsabs)
                         (nint::mkopt2 $limit $subint)))
         (r-expr ($realpart expr))
         (i-expr ($imagpart expr)))
    (let ((r-res (if (eq 0 r-expr) nil (nint::do-quad-pack r-expr var lo hi singlist quad-ops)))
          (i-res (if (eq 0 i-expr) nil (nint::do-quad-pack i-expr var lo hi singlist quad-ops))))
      (when (consp i-res)
        (setf (second i-res) `((mtimes) $%i ,(second i-res))))
      (let ((res (nint::combine-real-imag-results r-res i-res)))
        (if (not $calls)
            (setf res (butlast res)))
        (cond ((consp res)
               (when $words (setf (nth 4 res) (nth (nth 4 res) nint::*quad-error-codes*)))
               res)
              (t nil))))))

(max-doc:see-also "nintegrate" '("quad_qags" "quad_qagi" "quad_qagp"))

(examples:clear-add-example 
 "nintegrate"
 '(:code-text
 (
  :ex ("nintegrate(sin(sin(x)), [x,0,2])" "[1.24706, 1.38451e-14, 21, no problems]")
  :text ("Integrate over a semi-infinite interval with an internal singularity. The location of the singularity is
supplied. This cannot be done with a single call to quadpack routines.")
  :ex ("nintegrate(1/sqrt(abs(1-x))*exp(-x),[x,0,inf], [1] )" "[1.72821, 1.87197e-10, 660, no problems]")
  :text ("If a list of possible singular points is not supplied, then they will be searched for using " :emrefdot "solve")
  :ex ("nintegrate(1/sqrt(abs(1-x))*exp(-x),[x,0,inf])" "[1.72821, 1.87197e-10, 660, no problems]")
  :text ("In some cases, complex numbers are treated correctly. (The simplifier replaces " 
       :code "cos(%i*x)" " with " :code "cosh(x)" " before the routine is called. So this works "
       " with " :emref "quad_qags" " as well.)")
  :ex ("nintegrate( cos(%i*x), [x,0,1])" "[1.1752, 1.30474e-14, 21, no problems]")
  :ex ("sinh(1.0)" "1.1752")
  :text ("But, the quadpack routines cannot handle the complex numbers in this example.")
  :ex ("nintegrate(exp(%i*x) * exp(-x*x), [x,0,inf])" "[.424436 %i + .690194, 4.988325e-9, 300, no problems]")
  :text ("Return quadpack error code rather than error text.")
  :ex ("nintegrate(sin(sin(x)), [x,0,2], words->false)" "[1.24706, 1.38451e-14, 21, 0]")
  :text ("Request a relative error.")
  :ex ("nintegrate(exp(%i*x) * exp(-x*x), [x,0,inf], epsrel -> 1e-12)"
       "[.424436 %i + .690194, 1.06796e-13, 480, no problems]")
  :text ("Trying to do the integral with too few sub-intervals fails.")
  :ex ("nintegrate(1/(1+x^2), [x, 0, inf], subint -> 2, epsrel -> 1e-10)"
       "[1.5708, 2.57779e-10, 45, too many sub-intervals]")
  :text ("This integral is not handled well. Giving limits of " :code "minf" " and " :code "inf" " fails.")
  :ex ("nintegrate(exp(%i*x*x),[x,-200,200],subint->10000)"
       "[1.25170114 %i + 1.25804682, 2.507635982e-8, 760578, no problems]")
  :ex ("integrate(exp(%i*x*x),x,minf,inf)" "sqrt(%pi)*(%i/sqrt(2)+1/sqrt(2))")
  :ex ("rectform(float(%))" "1.25331414*%i+1.25331414")
  :text "Return a list of calls made to quadpack."
  :ex ("nintegrate(1/sqrt(abs(1-x)) * exp(-x),[x,0,inf], calls->true)"
       "[1.72821,1.87197e-10,660, no problems,
 [quad_qagi(%e^-x/sqrt(abs(x-1)),x,1.0,inf,epsrel = 1.e-8,epsabs = 0,limit = 200),
  quad_qagp(%e^-x/sqrt(abs(x-1)),x,0,1.0,[],epsrel = 1.e-8,epsabs = 0,limit = 200)]]"))))
