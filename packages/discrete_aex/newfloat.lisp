;; modified to do float and bfloat on roots
;; All tests pass with these definitions

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


