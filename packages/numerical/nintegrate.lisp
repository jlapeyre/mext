;(if (find-package :nintegrate ) t  
;  (defpackage :nintegrate (:use common-lisp :gjl.lisp-util)
;    (:nicknames :nint)))

(defmfun1:set-file-and-package "nintegrate.lisp" "numerical")

;;; High-level interface to numerical integration routines.

;;; TODO
;;; * Using qagi with qagp works, but it should be refactored and generalized.
;;; * Make better combined error estimate.
;;; * use assume(x>lo, x<hi) etc. when searching for roots.
;;; * use to_poly_solve
;;; * use quad_qawo, etc. See notes in rtests.
;;; * implement, or interface with gsl cquad. Not sure what it is worth
;;; * find some code for adaptive meshes for multi-dimensional
;;;   I think some of this is written in plain fortran.
;;; Everything else.

;;; $trigreduce and $changevar
;;; e : 'integrate(cos(x^2),x,0,1);
;;; changevar(e,x^2-y,y,x);
;;; -('integrate(cos(y)/sqrt(y),y,0,1))/2
;;;

;;; maybe the code in sin.lisp and defint.lisp can be used to analyze
;;; the integrands.
;;; possibly of interest:
;;; sin.lisp:  (defun scep
;;;            (defun integrator
;;; csimp.lisp: (defun partition
;;;             (defun islinear

;;; (partition $e '$x 1), removes a constant factor from $e,
;;; returns a list (thefactor expression-with-factor-removed)
;;; m2 pattern matcher used in sin.lisp, defined in schatc.lisp

(in-package :nintegrate)

;; this should be moved to a more general location
;; this makes a an option expression that is common in maxima:
;; e.g. epsrel=1e-5
(defmacro mkopt (opt)
  `(list '(maxima::mequal maxima::simp) ',opt ,opt))

;; if our option is named differently than option in
;; lower-level function.
(defmacro mkopt2 (opt val)
  `(list '(maxima::mequal maxima::simp) ',opt ,val))

(defparameter *quad-error-codes*
  `( "no problems" "too many sub-intervals" "excessive roundoff"
     "bad integrand behavior" "failed to converge" "probably divergent or slowly convergent"
     "invalid input"))

;; res is a (maxima) list of lists
;; return list where first through third elements are summed.
;; fifth element is largest of fifth elements (better would be a list of all)
;; sixth is appending of all sixth elements (which are lists of calls to quadpack)
(defun combine-quad-results ( &rest res)
 "add the results of integrating over two intervals. make an attempt
  to write reasonable information fields. really should do sqrt of sqs of errors."
 (let* ((vals (apply #'+ (mapcar #'second res)))
        (errs (apply #'+ (mapcar #'third res)))
        (nit (apply #'+ (mapcar #'fourth res)))
        (calls (maxima::mk-mlist (apply #'append (mapcar #'(lambda (x) (cdr (sixth x))) res))))
        (max-err (reduce #'max (mapcar #'fifth res))))
   (maxima::make-mlist-simp vals errs nit max-err calls)))

(defun combine-real-imag-results (r1 r2)
  (cond ((and (consp r1) (consp r2))
         (maxima::make-mlist-simp (maxima::simplify `((maxima::mplus) ,(second r1) ,(second r2)))
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
    (append (apply 'maxima::mfuncall call-list) (list (maxima::make-mlist-simp call-form)))))

;; todo:
;; use to_poly_solve to get zeroes of sin, cos, between limits
;; look for logs and solve for arg = 0.
;; if float(root) is not a float, then the integrand is bad.
;; we need to signal an error that is handled in nintegrate itself which
;; can give a proper defmfun1 error.
(maxima::ddefun list-fp-singularities (expr var lo hi)
 "try to return a maxima list of floating point numbers representing
 the singularities between `lo' and `hi' (excluding these endpoints)
 in `expr'. this, of course, can't be done in general.  this routine
 just calls `solve' and has some bugs. if no satisfying numbers are
 found, return `nil'."
 (let ((roots (apply 'maxima::mfuncall `(maxima::$solve ((maxima::mexpt maxima::simp) ,expr -1) ,var)))
       (nroots))
   (dolist (r (cdr roots))
     (let ((n (third r)))
       (when (maxima::$numberp n)
         (let ((nn (maxima::$float n)))
;           (when (not (numberp nn))
;             (error 'integrand-not-numeric))
           (when (not
                  (or (and (maxima::$numberp hi) (> 1e-10 (- hi nn)))
                      (and (maxima::$numberp lo) (> 1e-10 (- nn lo)))))
             (push (maxima::$float n) nroots))))))
   (if (consp nroots)
       (maxima::mk-mlist (sort nroots  #'<))
     nil)))

;; to reiterate: this could use refactoring!
(defun do-quad-pack (expr var lo hi singlist quad-ops more-opts &aux sing)
  (setf sing (first more-opts))
  (when (and sing (not singlist))
    (setf singlist (list-fp-singularities expr var lo hi)))
  (cond ((and singlist (> (length singlist) 1))
         (cond ((and (eq 'maxima::$minf lo) (not (eq 'maxima::$inf hi)))
                (let* ((nsinglist (cdr singlist))
                       (nlo (first nsinglist))
                       (int1
                        (quad-call :qagi expr var 'maxima::$minf nlo quad-ops))
                       (int2
                        (quad-call :qagp expr var nlo hi quad-ops (maxima::mk-mlist (cdr nsinglist)))))
                  (nint::combine-quad-results int1 int2)))
               ((and (not (eq 'maxima::$minf lo)) (eq 'maxima::$inf hi))
                (let* ((nsinglist (cdr singlist))
                       (rsinglist (reverse nsinglist))
                       (nhi (first rsinglist))
                       (n1singlist (reverse (cdr rsinglist)))
                       (int1 (quad-call :qagi expr var nhi 'maxima::$inf quad-ops))
                       (int2
                        (quad-call :qagp expr var lo nhi quad-ops (maxima::mk-mlist n1singlist))))
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
                                 (quad-call :qagp expr var nlo nhi quad-ops (maxima::mk-mlist n1singlist))
                               nil)))
                  (if (consp int3)
                      (nint::combine-quad-results int1 int2 int3)
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

;; we had to call opt $domain $idomain
;; because when two-d integrals call nintegrate
;; they use $domain->... with $domain evaluated.
;; this will cause a problem if the $idomain is
;; bound.
(defmfun1 ($nintegrate :doc :match) 
  ((expr :thread) (varspec :list)
   &rest (varspecs :list)
   &aux old-domain
   &opt 
   (($idomain idomain) '$complex domain-supplied-p (:member '($real $complex)))
   ($find_sing :bool t) ($calls (:member '(nil t $short))) ($words t :bool) ($info :bool t) 
   ($subint 200 :non-neg-int) ($epsabs 0 :to-non-neg-float) ($epsrel 1d-8 :to-non-neg-float)
        ($points :to-float-listof))
           ; ($method "automatic" :string)) only doing automatic for now.
 :desc 
 ("numerically integrate " :arg "expr" ", with the variable and limits supplied in the list "
  :arg "varspec" " as ["  :argcomma "var" :argcomma "lo" :arg "hi" "]."
  " for higher dimensional integrals, supply further " :argdot "varspecs"
  :par ""
  " If the option " :opt "call" " is true, then calls made to quadpack are "
  " also returned in a list. if " :opt "call" " is " :varcomma "short" " then only the "
  " name of the quadpack routine is included."
  :par ""
  "by default, information on the integration is returned with the results. "
  "If the option " :opt "info" " is false, then only the result of the integration "
  "is returned." 
  :par ""
  "If the option " :opt "find_sing" " is false, then " :mref "nintegrate" " will not search "
  "for internal singularities, but user supplied singularities will still be used."
  :par ""
  " If the global variable " :emref "domain" " is " :codecomma "real" " then
   the integrand is assumed to be real. "
  " If " :emref "domain" " is " :codecomma "complex" " then
   the integrand may be complex. In particular roots of negative numbers may evaluate to complex numbers. "
  "The option " :opt "idomain" " overrides the global variable " :vardot "domain."
  :par ""
  " If an error occurs in multi-dimensional integrals, it may be possible to get 
  a result by giving a value " :code "real"
  " for option " :optdot "idomain"
  :par ""
  "this function is not well tested and may give incorrect results.")

  (when varspecs
    ; can't handle some complex results, maybe need to do real and imag here already
    ; also,   -> quotes lhs, but here the lhs is evaluated again somehow.
    ; this is bug.
    (let* ((call defmfun1-func-call)
           (new-call)
           (new-integrand)
           (opts (nthcdr (+ 3 (length varspecs)) call))
           (new-varspec (pop varspecs)))
      (setf new-integrand (append (list '(%nintegrate) expr varspec) varspecs opts
                                    (list (rule-opt '$info nil))))
      (setf new-call (append (list new-integrand new-varspec) opts))
      (return-from $nintegrate (apply '$nintegrate new-call))))
  (unwind-protect
      (progn
        (when domain-supplied-p
          (setf old-domain $domain)
          (mset '$domain idomain))
    (let* ((vp (rest varspec)) (var (first vp)) ; multi dimensional integrals
         (lo (second vp)) (hi (third vp))
         (quad-ops (list (nint::mkopt $epsrel) (nint::mkopt $epsabs)
                         (nint::mkopt2 $limit $subint)))
         (more-opts (list $find_sing))
         (r-expr (if (eq $domain '$real) expr ($realpart expr)))
         (i-expr (if (eq $domain '$real) 0 ($imagpart expr))))
    (echeck-arg $nintegrate :or-symbol-subvar var)
    (echeck-arg $nintegrate :to-or-float-minf lo)
    (echeck-arg $nintegrate :to-or-float-inf  hi)

; Following allows integrate(f,[x,0,1]), when f is a function.
; but not simplifying functions, like
; integrate(cos,[x,0,1]), but
; integrate(cos(x),[x,0,1]) is ok
    (when (and (not (numberp ($float expr))) (freeof var expr)
               (not (fboundp expr)) (not (mfboundp expr)))
      (defmfun1-error-return '$expr_freeof_var $nintegrate 
        "the integrand is not a number or a function and does not depend on the variable of integration" :match))
;    (handler-case
;     (let ((f (get-integrand expr var))))
;       (format t "Integrand ok~%"))
;     (error ()
;            (defmfun1-error-return '$nonnumeric_integrand $nintegrate 
;              "The integrand does not evaluate to a number")))
    (let ((r-res (if (equalp 0 r-expr) nil (nint::do-quad-pack r-expr var lo hi $points quad-ops more-opts)))
          (i-res (if (or (eq $domain '$real) (equalp 0 i-expr))
                     nil (nint::do-quad-pack i-expr var lo hi $points quad-ops more-opts))))
      (when (consp i-res)
        (setf (second i-res) `((mtimes) $%i ,(second i-res))))
      (let ((res (nint::combine-real-imag-results r-res i-res)))
        (cond ((eq $calls t) nil)
              ((eq $calls '$short)
               (let ((len (length res)))
                 (setf (nth (- len 1) res)
                       (mk-mlist (loop :for call :in (cdr (nth (- len 1) res)) :collect
                                       (caar call))))))
              (t (setf res (butlast res))))
        (cond ((consp res)
               (cond ($info
                      (when $words (setf (nth 4 res) (nth (nth 4 res) nint::*quad-error-codes*)))
                      res)
                     (t (second res))))
              (t nil))))))
    (when domain-supplied-p
      (mset '$domain old-domain))))

(max-doc:see-also "nintegrate" '("quad_qags" "quad_qagi" "quad_qagp"))

(max-doc:implementation "nintegrate"
 '(
  :mref "nintegrate" 
  " automatically chooses and combines " :emrefcomma "qags" :emrefcomma "qagp" 
  " and " :emrefdot "qagi"  " " :emref "realpart" " and " :emref "imagpart"
  " are used to get two real integrands, which are then integrated separately. " 
  :emref "solve" " is
  used to try to find singularities, and the results are either sent to "
  :emrefcomma "qagp" " or used to call quadpack routines on intervals. Multi-dimensional
  integrals are implemented simply by nesting calls to " :mrefcomma "nintegrate"
  " and thus, are not efficient."))
;  :par ""
;  "Things that could be added are: log, etc. singularities; to_poly_solve for
;   singularities; faster nesting for multi-dimensional integrals; better
;   routines for one and multi-dimensional integrals; symbolic manipulation of
;   integrand; use of remaining quadpack routines."))

(examples:clear-add-example 
 "nintegrate"
 '(:code-text
 (
  :text ("Compute a one-dimensional integral.")
  :ex ("nintegrate(sin(sin(x)), [x,0,2])" "[1.24706, 1.38451e-14, 21, no problems]")
  :text ("Integrate a list of integrands")
  :ex ("nintegrate([x,x^2,x^3], [x,0,1], info->false)" "[0.5,.3333333333333334,0.25]")
  :text ("Compute two- and three-dimensional integrals.")
  :ex ("nintegrate(x*y, [x,0,1], [y,0,1], info->false)" "0.25")
  :ex ("nintegrate(x*y*z, [x,0,1], [y,0,1], [z,0,1], info->false)" "0.125")
  :text ("Integrate over a semi-infinite interval with an internal singularity. 
     The location of the singularity is supplied. This cannot be done with a single call to quadpack routines.")
  :ex ("nintegrate(1/sqrt(abs(1-x))*exp(-x),[x,0,inf], points-> [1] )" "[1.72821, 1.87197e-10, 660, no problems]")
  :text ("If a list of possible singular points is not supplied, then they will be searched for using " :emrefdot "solve")
  :ex ("nintegrate(1/sqrt(abs(1-x))*exp(-x),[x,0,inf])" "[1.72821, 1.87197e-10, 660, no problems]")
;  :text ("In some cases, complex numbers are treated correctly. (The simplifier replaces " 
;       :code "cos(%i*x)" " with " :code "cosh(x)" " before the routine is called. So this works "
;       " with " :emref "quad_qags" " as well.)")
;  :ex ("nintegrate( cos(%i*x), [x,0,1])" "[1.1752, 1.30474e-14, 21, no problems]")
;  :ex ("sinh(1.0)" "1.1752")
  :text ("Four quadpack calls are needed to compute this example.")
;  :ex ("block([domain:'complex],nintegrate(exp(%i*x) * exp(-x*x), [x,0,inf]))" "[.424436 %i + .690194, 4.988325e-9, 300, no problems]")
  :ex ("block([domain:'complex],nintegrate(1/sqrt(1-x)*exp(-x),[x,0,inf], calls->short))"
       "[1.07616-.652049*%i,1.87197e-10,1026,\"no problems\",
        [quad_qagi,quad_qagp,quad_qagi,quad_qagp]]")
  :text ("Return quadpack error code rather than error text.")
  :ex ("nintegrate(sin(sin(x)), [x,0,2], words->false)" "[1.24706, 1.38451e-14, 21, 0]")
  :text ("Request a relative error.")
  :ex ("block([domain:'complex], nintegrate(exp(%i*x) * exp(-x*x), [x,0,inf], epsrel -> 1e-12))"
       "[.424436 %i + .690194, 1.06796e-13, 480, no problems]")
  :text ("Trying to do the integral with too few sub-intervals fails.")
  :ex ("nintegrate(1/(1+x^2), [x, 0, inf], subint -> 2, epsrel -> 1e-10)"
       "[1.5708, 2.57779e-10, 45, too many sub-intervals]")
;  :text ("This integral is not handled well. Giving limits of " :code "minf" " and " :code "inf" " fails.")
;  :ex ("nintegrate(exp(%i*x*x),[x,-200,200],subint->10000)"
;       "[1.25170114 %i + 1.25804682, 2.507635982e-8, 760578, no problems]")
;  :ex ("integrate(exp(%i*x*x),x,minf,inf)" "sqrt(%pi)*(%i/sqrt(2)+1/sqrt(2))")
;  :ex ("rectform(float(%))" "1.25331414*%i+1.25331414")
  :text "Return a list of calls made to quadpack."
  :ex ("nintegrate(1/sqrt(abs(1-x)) * exp(-x),[x,0,inf], calls->true)"
       "[1.72821,1.87197e-10,660, no problems,
 [quad_qagi(%e^-x/sqrt(abs(x-1)),x,1.0,inf,epsrel = 1.e-8,epsabs = 0,limit = 200),
  quad_qagp(%e^-x/sqrt(abs(x-1)),x,0,1.0,[],epsrel = 1.e-8,epsabs = 0,limit = 200)]]")
  :text ("Here we must supply the roots of " :code "sin(x)" " within the range of integration.")
  :ex ("block([domain:'complex], nintegrate(1/(sqrt(sin(x))),[x,0,10], points -> [%pi,2*%pi,3*%pi]))"
    "[10.4882-6.76947*%i,9.597497e-8,1596,\"no problems\"]"))))
