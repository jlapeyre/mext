;;; Maxima interface to quadpack integration modified for nintegrate
;;; The routines quad_qags, quad_qagi, and quad_qagp are rewritten
;;; as defmfun1 functions. This was not strictly necessary, but it makes
;;; them a bit easier to work with.
(in-package :maxima)

(defmfun1:set-file-and-package "quadpack-nint.lisp" "numerical")

(defun mquad-error (c)
  (format nil "Integration failed with error: ~a" c))

(defparameter mquad-error-string
  "Inegration failed. Probably a floating-point error")

(defmfun1-opt defmfun-quad ((($limit limit) 200 :non-neg-int)
                            (($epsabs epsabs) 0.0 :to-float) 
                            (($epsrel epsrel) 1e-8 :to-float)))

(defmfun-quad ($mquad_qag :doc :match) (fun (var :or-symbol-subvar) (a :to-float) (b :to-float) 
                     (key (:int-range 1 6)))
  :desc ("This is an interface to qag that is modified from " :emrefdot "quad_qag")
  (let* ((lenw (* 4 limit))
	 (work (make-array lenw :element-type 'flonum))
	 (iwork (make-array limit :element-type 'f2cl-lib:integer4))
	 (f (get-integrand fun var)))
    (handler-case
	(multiple-value-bind (junk z-a z-b z-epsabs z-epsrel z-key result abserr neval ier
				   z-limit z-lenw last)
	    (slatec:dqag #'(lambda (x)
			     (float (funcall f x)))
                         a b epsabs epsrel 
			 key  0.0 0.0 0 0
			 limit lenw 0 iwork work)
	  (declare (ignore junk z-a z-b z-epsabs z-epsrel z-key z-limit z-lenw last))
	  (list '(mlist) result abserr neval ier))
        (error (c) (defmfun1-error-final '$qag (mquad-error c) :match)))))

(defmfun-quad ($mquad_qags :doc :match) (fun (var :or-symbol-subvar) (a :to-float) (b :to-float))
  :desc ("This is an interface to qags that is modified from " :emrefdot "quad_qags")
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
        (error (c) (defmfun1-error-final '$qags (mquad-error c) :match)))))

(defmfun-quad ($mquad_qagi :doc :match) (fun (var :or-symbol-subvar) (a :to-or-float-inf) (b :to-or-float-inf))
  :desc ("This is an interface to qagi that is modified from " :emrefdot "quad_qagi")
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
		   (t (defmfun1-error-return '$qagi_minf_inf 
                        $mquad_qagi "Neither of the limits are `minf' or `inf'")))
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
            (error (c) (defmfun1-error-final '$qagi (mquad-error c) :match)))))))

(defmfun-quad ($mquad_qagp :doc :match) (fun (var :or-symbol-subvar) (a :to-float) (b :to-float) (points :list))
  :desc ("This is an interface to qagp that is modified from " :emrefdot "quad_qagp")
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
        (error (c) (defmfun1-error-final '$qagp (mquad-error c) :match)))))


(defmfun-quad ($mquad_qawc :doc :match) (fun (var :or-symbol-subvar) (c :to-float) (a :to-float) (b :to-float))
  :desc ("This is an interface to qawc that is modified from " :emrefdot "quad_qawc")
  (let* ((lenw (* 4 limit))
	 (work (make-array lenw :element-type 'flonum))
	 (iwork (make-array limit :element-type 'f2cl-lib:integer4))
	 (f (get-integrand fun var)))
    (handler-case
	(multiple-value-bind (junk z-a z-b z-c z-epsabs z-epsrel result abserr neval ier
				   z-limit z-lenw last)
	    (slatec:dqawc #'(lambda (x)
			      (float (funcall f x)))
			  a b c epsabs epsrel 0.0 0.0 0 0                 
			  limit lenw 0 iwork work)
	  (declare (ignore junk z-a z-b z-c z-epsabs z-epsrel z-limit z-lenw last))
	  (list '(mlist) result abserr neval ier))
        (error (c) (defmfun1-error-final '$qawc (mquad-error c) :match)))))

(defmfun1 ($mquad_qawf :doc :match) (fun (var :or-symbol-subvar) (a :to-float) 
                          (omega :to-float) (trig (:member '($cos 1 %cos $sin %sin 2)))
                                   &opt  (($limit limit) 200 :non-neg-int)
                  (($epsabs epsabs) 1e-10 :to-float) 
		  (($maxp1 maxp1) 100 :non-neg-int) (($limlst limlst) (:int-gte 3) 10))
  :desc ("This is an interface to qawf that is modified from " :emrefdot "quad_qawf")
  (let* ((leniw limit)
	 (lenw (+ (* 2 leniw) (* 25 maxp1)))
	 (work (make-array lenw :element-type 'flonum))
	 (iwork (make-array leniw :element-type 'f2cl-lib:integer4))
	 (f (get-integrand fun var))
	 (integr (ecase trig
		   ((1 %cos $cos) 1)
		   ((2 %sin $sin) 2))))
    (handler-case
	(multiple-value-bind (junk z-a z-omega z-integr
				   epsabs result abserr neval ier
				   z-limlst z-lst
				   z-leniw z-maxp1 z-lenw)
	    (slatec:dqawf #'(lambda (x)
			      (float (funcall f x)))
			  (float-or-lose a)
			  (float-or-lose omega)
			  integr
			  (float-or-lose epsabs)
			  0.0 0.0 0 0
			  limlst 0 leniw maxp1 lenw iwork work)
	  (declare (ignore junk z-a z-omega z-integr epsabs z-limlst z-lst
			   z-leniw z-maxp1 z-lenw))
	  (list '(mlist) result abserr neval ier))
        (error (c) (defmfun1-error-final '$qawf (mquad-error c) :match)))))


(defmfun-quad ($mquad_qawo :doc :match)  (fun (var :or-symbol-subvar) (a :to-float) (b :to-float) 
                                             (omega :to-float) (trig (:member '($cos 1 %cos $sin %sin 2)))
                                             &opt (($maxp1 maxp1) 100 :non-neg-int))
  :desc ("This is an interface to qawo that is modified from " :emrefdot "quad_qawo")
  (let* ((leniw limit)
	 (lenw (+ (* 2 leniw) (* 25 maxp1)))
	 (work (make-array lenw :element-type 'flonum))
	 (iwork (make-array leniw :element-type 'f2cl-lib:integer4))
	 (f (get-integrand fun var))
	 (integr (ecase trig
		   ((1 %cos $cos) 1)
		   ((2 %sin $sin) 2))))
    (handler-case
	(multiple-value-bind (junk z-a z-b z-omega z-integr z-epsabs z-epsrel
				   result abserr neval ier
				   z-leniw z-maxp1 z-lenw z-lst)
	    (slatec:dqawo #'(lambda (x)
			      (float (funcall f x)))
                          a b omega integr epsabs epsrel
			  0.0 0.0 0 0
			  leniw maxp1 lenw 0 iwork work)
	  (declare (ignore junk z-a z-b z-omega z-integr z-epsabs z-epsrel
			   z-lst z-leniw z-maxp1 z-lenw))
	  (list '(mlist) result abserr neval ier))
        (error (c) (defmfun1-error-final '$qawo (mquad-error c) :match)))))


(defmfun-quad ($mquad_qaws :doc :match)  (fun (var :or-symbol-subvar) (a :to-float) (b :to-float) 
                                             (alfa :to-float) (beta :to-float) (wfun (:int-range 1 4)))
  :desc ("This is an interface to qaws that is modified from " :emrefdot "quad_qaws")
  (let* ((lenw (* 4 limit))
	 (work (make-array lenw :element-type 'flonum))
	 (iwork (make-array limit :element-type 'f2cl-lib:integer4))
	 (f (get-integrand fun var)))
    (handler-case
	(multiple-value-bind (junk z-a z-b z-alfa z-beta z-int z-epsabs z-epsrel
				   result abserr neval ier
				   z-limit z-lenw last)
	    (slatec:dqaws #'(lambda (x)
			      (float (funcall f x)))
                          a b alfa beta wfun epsabs epsrel
			  0.0 0.0 0 0
			  limit lenw 0 iwork work)
	  (declare (ignore junk z-a z-b z-alfa z-beta z-int z-epsabs z-epsrel
			   z-limit z-lenw last))
	  (list '(mlist) result abserr neval ier))
        (error (c) (defmfun1-error-final '$qaws (mquad-error c) :match)))))
