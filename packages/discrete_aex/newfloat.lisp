(in-package :maxima)
(mext:mext-optimize)
(max-doc:set-cur-sec 'max-doc::number-theory-fandv)

(defmfun1:set-file-and-package "newfloat.lisp" "discrete_aex")

;; modified to do float and bfloat on roots
;; All tests pass with these definitions

;; We still have to use cbfloat because there is a problem
;; with infinite recursion.
#|
(mext::no-warning
(defmfun $bfloat (x)
  (let (y)
    (cond ((bigfloatp x))
	  ((or (numberp x)
	       (member x '($%e $%pi $%gamma) :test #'eq))
	   (bcons (intofp x)))
	  ((or (atom x) (member 'array (cdar x) :test #'eq))
	   (if (eq x '$%phi)
	       ($bfloat '((mtimes simp)
			  ((rat simp) 1 2)
			  ((mplus simp) 1 ((mexpt simp) 5 ((rat simp) 1 2)))))
	       x))
	  ((eq (caar x) 'mexpt)
           (format t "EXHERP ~a~%" x)
           (if (equal (cadr x) '$%e)
               (*fpexp ($bfloat (caddr x)))
             (let* ((base ($bfloat (second x)))
                    (exp ($bfloat (third x))))
               (format t "base ~a exp ~a~%" base exp)
               (if
                   (and  (not (and ($numberp base) (mfuncall 'mgreaterp base 0) ($numberp exp)))
                        (complex-number-p base '$numberp) ; GJL 2013
                        (complex-number-p exp '$numberp))
                   (let*
                       ((form (list (car x) base exp))
                        (rform ($rectform form)))
                     (if (eq (caar rform) 'mexpt) ; will this happen ?
;                         (exptbigfloat base exp)
                       ($expand ($bfloat rform))
                       ($expand ($bfloat rform))))
                 (exptbigfloat base (caddr x))))))
           ((eq (caar x) 'mncexpt)
	   (list '(mncexpt) ($bfloat (cadr x)) (caddr x)))
	  ((eq (caar x) 'rat)
	   (ratbigfloat (cdr x)))
;	  ((and (not (eq (caar x) 'mtimes)) (setq y (safe-get (caar x) 'floatprog)))
;	   (funcall y (mapcar #'$bfloat (cdr x))))
	  ((setq y (safe-get (caar x) 'floatprog))
	   (funcall y (mapcar #'$bfloat (cdr x))))
	  ((or (trigp (caar x)) (arcp (caar x)) (eq (caar x) '$entier))
	   (setq y ($bfloat (cadr x)))
	   (if ($bfloatp y)
	       (cond ((eq (caar x) '$entier) ($entier y))
		     ((arcp (caar x))
		      (setq y ($bfloat (logarc (caar x) y)))
		      (if (free y '$%i)
			  y (let ($ratprint) (fparcsimp ($rectform y)))))
		     ((member (caar x) '(%cot %sec %csc) :test #'eq)
		      (invertbigfloat
		       ($bfloat (list (ncons (safe-get (caar x) 'recip)) y))))
		     (t ($bfloat (exponentialize (caar x) y))))
	       (subst0 (list (ncons (caar x)) y) x)))
;          ((eq (caar x) 'mtimes)
;           (recur-apply #'$bfloat x))
          ((eq (caar x) 'mtimesxx)
           (format t "GOT THIS~%")
           (let ((res (recur-apply #'$bfloat x)))
;           (let ((res x))
             ;; If we are multiplying complex numbers, expand result -- GJL 2013
             (if (and (consp res) (eq (caar res) 'mtimes)
                      (every #'(lambda (aa) (or (complex-number-p aa)
                                                (complex-number-p aa '$bfloatp)))
                                               (cdr res)))
;                      (not (every #'$numberp
;                                               (cdr res))))
                 (simplify res)
               res)))
;               (recur-apply #'$bfloat res))))
	  (t (recur-apply #'$bfloat x))))))
|#

;; Same as float() except
;; 1) do rectform on exponentials with base and exponent
;; both numbers.
;; 2) expand products of numbers and complex numbers
(mext::no-warning
(defmfun $float (e)
  (cond ((numberp e) (float e))
	((and (symbolp e) (mget e '$numer)))
        ((aex-p e)
         (let* ((a (aex-copy-new-n e))
                (ar (aex-arr a))
                (are (aex-arr e)))
           (dotimes (i (length ar))
             (setf (aref ar i) ($float (aref are i))))
           a))
	((or (atom e) (member 'array (cdar e) :test #'eq)) e)
	((eq (caar e) 'rat) (fpcofrat e))
	((eq (caar e) 'bigfloat) (fp2flo e))
        ((eq (caar e) 'mexpt)
         (let* ((base ($float (second e)))
                (exp ($float (third e)))
                (nform (list (car e) base exp)))
           (if (and
                (complex-number-p base '$numberp)
                (complex-number-p exp '$numberp))
               ($float ($rectform nform))
             (let ((res (recur-apply #'$float nform)))
               (if (floatp res) res
                 (list (ncons (caar e)) base (caddr e)))))))
;	((member (caar e) '(mexpt mncexpt) :test #'eq)
	((eq (caar e) 'mncexpt)
	 ;; avoid x^2 -> x^2.0, allow %e^%pi -> 23.14
	 (let ((res (recur-apply #'$float e)))
	   (if (floatp res)
	       res
	       (list (ncons (caar e)) ($float (cadr e)) (caddr e)))))
	((and (eq (caar e) '%log)
	      (complex-number-p (second e) '$ratnump))
	 ;; Basically we try to compute float(log(x)) as directly as
	 ;; possible, expecting Lisp to return some error if it can't.
	 ;; Then we do a more complicated approach to compute the
	 ;; result.  However, gcl and ecl don't signal errors in these
	 ;; cases, so we always use the complicated approach for these lisps.
	 (let ((n (second e)))
	   (cond ((integerp n)
		  ;; float(log(int)).  First try to compute (log
		  ;; (float n)).  If that works, we're done.
		  ;; Otherwise we need to do more.
		  (to (or (try-float-computation #'(lambda ()
						     (log (float n))))
			  (let ((m (integer-length n)))
			    ;; Write n as (n/2^m)*2^m where m is the number of
			    ;; bits in n.  Then log(n) = log(2^m) + log(n/2^m).
			    ;; n/2^m is approximately 1, so converting that to a
			    ;; float is no problem.  log(2^m) = m * log(2).
			    (+ (* m (log 2e0))
			       (log (float (/ n (ash 1 m)))))))))
		 (($ratnump n)
		  ;; float(log(n/m)) where n and m are integers.  Try computing
		  ;; it first.  If it fails, compute as log(n) - log(m).
		  (let ((try (try-float-computation #'(lambda()
							(log (fpcofrat n))))))
		    (if try
			(to try)
			(sub  ($float `((%log) ,(second n)))
			      ($float `((%log) ,(third n)))))))
		 ((complex-number-p n 'integerp)
		  ;; float(log(n+m*%i)).
		  (let ((re ($realpart n))
			(im ($imagpart n)))
		    (to (or (try-float-computation #'(lambda ()
						       (log (complex (float re)
								     (float im)))))
			    (let* ((size (max (integer-length re)
					      (integer-length im)))
				   (scale (ash 1 size)))
			      (+ (* size (log 2e0))
				 (log (complex (float (/ re scale))
					       (float (/ im scale))))))))))
		 (t
		  ;; log(n1/d1 + n2/d2*%i) =
		  ;;   log(s*(n+m*%i)) = log(s) + log(n+m*%i)
		  ;;
		  ;; where s = lcm(d1, d2), n and m are integers
		  ;;
		  (let* ((s (lcm ($denom ($realpart n))
				 ($denom ($imagpart n))))
			 (p ($expand (mul s n))))
		    (add ($float `((%log) ,s))
			 ($float `((%log) ,p))))))))
	((and (eq (caar e) '%erf)
	      (eq (second e) '$%i))
	 ;; Handle like erf(%i).  float(%i) (via recur-apply below)
	 ;; just returns %i, so we never numerically evaluate it.
	 (complexify (complex-erf (complex 0 1d0))))
        ;; If we are multiplying complex numbers, expand result -- GJL 2013
        ((eq (caar e) 'mtimes)
         (let ((res (recur-apply #'$float e)))
           (if (and (consp res) (eq (caar res) 'mtimes)
                    (every #'complex-number-p (cdr res)))
                 ($expand res)
             res)))
	(t (recur-apply #'$float e)))))

;; from src/nparse.lisp
;; GJL 2013 construct bf with fpprec corresponding to number of input
;; digits
(mext::no-warning
(defun make-number (data)
  (setq data (nreverse data))
  ;; Maxima really wants to read in any number as a flonum
  ;; (except when we have a bigfloat, of course!).  So convert exponent
  ;; markers to the flonum-exponent-marker.
  (let ((marker (car (nth 3 data))))
    (unless (eql marker flonum-exponent-marker)
      (when (member marker '(#\E #\F #\S #\D #\L #+cmu #\W))
        (setf (nth 3 data) (list flonum-exponent-marker)))))
  (if (not (equal (nth 3 data) '(#\B)))
      (readlist (apply #'append data))
      (let*
	   ((*read-base* 10.)
	    (int-part (readlist (or (first data) '(#\0))))
	    (frac-part (readlist (or (third data) '(#\0))))
	    (frac-len (length (third data)))
	    (exp-sign (first (fifth data)))
	    (exp (readlist (sixth data)))
            (old-$fpprec $fpprec)
            (trial-fpprec (+ frac-len (length (first data)))))
;        (format t "fraclen ~a~%" frac-len)
;        (format t "intlen ~a~%" (length (first data)))
        (when (> trial-fpprec old-$fpprec)
          (mset '$fpprec trial-fpprec))
        (unwind-protect
            (if (and $fast_bfloat_conversion
                     (> (abs exp) $fast_bfloat_threshold))
                   ;; Exponent is large enough that we don't want to do exact
                   ;; rational arithmetic.  Instead we do bfloat arithmetic.
	    ;; For example, 1.234b1000 is converted by computing
                   ;; bfloat(1234)*10b0^(1000-3).  Extra precision is used
	    ;; during the bfloat computations.
                   (let* ((extra-prec (+ *fast-bfloat-extra-bits* (ceiling (log exp 2e0))))
                          (fpprec (+ fpprec extra-prec))
                          (mant (+ (* int-part (expt 10 frac-len)) frac-part))
                          (bf-mant (bcons (intofp mant)))
                          (p (power (bcons (intofp 10))
                                    (- (if (char= exp-sign #\-)
                                           (- exp)
                                         exp)
				frac-len)))
                          ;; Compute the product using extra precision.  This
                          ;; helps to get the last bit correct (but not
                          ;; always).  If we didn't do this, then bf-mant and
                          ;; p would be rounded to the target precision and
                          ;; then the product is rounded again.  Doing it
                          ;; this way, we still have 3 roundings, but bf-mant
                          ;; and p aren't rounded too soon.
                          (result (mul bf-mant p)))
                     (let ((fpprec (- fpprec extra-prec)))
                       ;; Now round the product back to the desired precision.
                       (bigfloatp result)))
	    ;; For bigfloats, turn them into rational numbers then
                 ;; convert to bigfloat.  Fix for the 0.25b0 # 2.5b-1 bug.
                 ;; Richard J. Fateman posted this fix to the Maxima list
                 ;; on 10 October 2005.  Without this fix, some tests in
	    ;; rtestrationalize will fail.  Used with permission.
                 (let ((ratio (* (+ int-part (* frac-part (expt 10 (- frac-len))))
                                 (expt 10 (if (char= exp-sign #\-)
                                              (- exp)
                                            exp)))))
                   ($bfloat (cl-rat-to-maxima ratio))))
        (when (> trial-fpprec old-$fpprec)
          (mset '$fpprec old-$fpprec)))))))


(defmfun1 ($precision :doc) ((num :number-max-lisp))
  :desc
  ("Returns the number of digits of precision in " :argdot "num"
   :par ""
   "This does not directly correspond to the accuracy of of the
    calculation that resulted in " :argdot "num")
  (cond ((atom num)
         (when (complexp num) (setf num (realpart num)))
         (cond ((integerp num) '$inf)
               ((floatp num) '$machine_precision)
               ((rationalp num) '$inf)))
        (($bfloatp num)
         (floor (* (car (last (car num))) (log 2.0 10.0))))
        ((eq (caar num) 'rat) '$inf)
        (t (merror "Bug in precision with number ~M" num))))

;; Copied from src/float.lisp
;; Number of bits of precision in the mantissa of newly created bigfloats.
;; FPPREC = ($FPPREC+1)*(Log base 2 of 10)
