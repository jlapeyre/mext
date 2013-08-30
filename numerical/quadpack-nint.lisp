;;; Maxima interface to quadpack integration modified for nintegrate
;;; The routines quad_qags, quad_qagi, and quad_qagp are rewritten
;;; as defmfun1 functions. This was not strictly necessary, but it makes
;;; them a bit easier to work with.
(in-package :maxima)

(defmfun1:set-file-and-package "quadpack-nint.lisp" "numerical")

(defmfun1 ($mquad_qags :doc) (fun (var :or-symbol-subvar) (a :to-float) (b :to-float) &opt
                                  (($epsrel epsrel) 1e-8 :to-float) 
                                  (($limit limit) 200 :non-neg-int) (($epsabs epsabs) 0.0 :to-float))
  :desc ("This is an interface to qags that is modified from " :emrefdot "quad_qags")
  (quad_argument_check fun var a b) 
  (let* ((lenw (* 4 limit))
	 (work (make-array lenw :element-type 'flonum))
	 (iwork (make-array limit :element-type 'f2cl-lib:integer4))
	 (f (get-integrand fun var)))
    (handler-case
	(multiple-value-bind (junk z-a z-b z-epsabs z-epsrel result abserr neval ier
				   z-limit z-lenw last)
	    (slatec:dqags #'(lambda (x) (float (funcall f x)))
			  a b epsabs epsrel 0.0 0.0 0 0
			  limit lenw 0 iwork work)
	  (declare (ignore junk z-a z-b z-epsabs z-epsrel z-limit z-lenw last))
	  (list '(mlist) result abserr neval ier))
      (error () (defmfun1-error-final "Exception. Probably a floating-point exception")))))

(defmfun1 ($mquad_qagi :doc) (fun (var :or-symbol-subvar) a b &opt
		      (($epsrel epsrel) 1e-8 :to-float) (($limit limit) 200 :non-neg-int) 
                      (($epsabs epsabs) 0.0 :to-float))
  :desc ("This is an interface to qagi that is modified from " :emrefdot "quad_qagi")
  (quad_argument_check fun var a b)
  ;; Massage the limits a and b into what Quadpack QAGI wants.
  (flet ((fixup (low high)
	   (let (bnd inf)
	     ;; Cases to handle: (minf, x), (x, inf), (minf, inf).
	     ;; Everything else is an error.
	     (cond ((eq low '$minf)
		    (cond ((eq high '$inf)
			   (setf bnd 0)
			   (setf inf 2))
			  (t
			   (setq bnd ($float high))
			   (setq inf low))))
		   ((eq high '$inf)
		    (setq bnd ($float low))
		    (setq inf high))
		   (t (defmfun1-error-return $mquad_qagi "Neither of the limits are `minf' or `inf'")))
	     (values bnd inf))))

    (multiple-value-bind (bound inf-type)
	(fixup a b)
      (let* ((lenw (* 4 limit))
	     (work (make-array lenw :element-type 'flonum))
	     (iwork (make-array limit :element-type 'f2cl-lib:integer4))
	     (f (get-integrand fun var))
	     (infinity (ecase inf-type
			 ((1 $inf)
			  ;; Interval is [bound, infinity]
			  1)
			 ((-1 $minf)
			  ;; Interval is [-infinity, bound]
			  -1)
			 ((2 $both)
			  ;; Interval is [-infinity, infinity]
			  2))))
	(handler-case
	    (multiple-value-bind (junk z-bound z-inf z-epsabs z-epsrel
				       result abserr neval ier
				       z-limit z-lenw last)
		(slatec:dqagi #'(lambda (x)
				  (float (funcall f x)))
			      (float-or-lose bound)
			      infinity epsabs epsrel
			      0.0 0.0 0 0
			      limit lenw 0 iwork work)
	      (declare (ignore junk z-bound z-inf z-epsabs z-epsrel
			       z-limit z-lenw last))
	      (list '(mlist) result abserr neval ier))
	  (error () (defmfun1-error-final "Exception. Probably a floating-point exception.")))))))

(defmfun1 ($mquad_qagp :doc) (fun (var :or-symbol-subvar) (a :to-float) (b :to-float) points
		     &opt (($epsrel epsrel) 1e-8 :to-float) (($limit limit) 200 :non-neg-int) 
                     (($epsabs epsabs) 0.0 :to-float))
  :desc ("This is an interface to qagp that is modified from " :emrefdot "quad_qagp")
  (quad_argument_check fun var a b)
  (let* ((npts2 (+ 2 (length (cdr points))))
	 (p (make-array npts2 :element-type 'flonum))
	 (leniw (max limit (- (* 3 npts2) 2)))
	 (lenw (- (* 2 leniw) npts2))
	 (work (make-array lenw :element-type 'flonum))
	 (iwork (make-array limit :element-type 'f2cl-lib:integer4))
	 (f (get-integrand fun var)))
    (map-into p #'float-or-lose (cdr points))
    (handler-case
	(multiple-value-bind (junk z-a z-b z-npts z-points z-epsabs z-epsrel
				   result abserr neval ier
				   z-leniw z-lenw last)
	    (slatec:dqagp #'(lambda (x)
			      (float (funcall f x)))
			  a b npts2 p epsabs epsrel
			  0.0 0.0 0 0
			  leniw lenw 0 iwork work)
	  (declare (ignore junk z-a z-b z-npts z-points z-epsabs z-epsrel
			   z-leniw z-lenw last))
	  (list '(mlist) result abserr neval ier))
      (error () (defmfun1-error-final "Exception. Probably a floating-point exception.")))))
