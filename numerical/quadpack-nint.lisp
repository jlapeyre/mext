;;; Maxima interface to quadpack integration

(in-package :maxima)

#-(or gcl ecl)
(defmacro get-integrand (fun var)
  `(compile nil (coerce-float-fun ,fun `((mlist) ,,var))))

#+(or gcl ecl)
(defmacro get-integrand (fun var)
  `(coerce-float-fun ,fun `((mlist) ,,var)))

(defun float-or-lose (val)
  (let ((v ($float val)))
    (if (numberp v)
	v
	(error (intl:gettext "~A is not a real floating-point number") val))))

(defun quad-qag (fun var a b key &key
		 (epsrel 1e-8)
		 (limit 200)
		 (epsabs 0.0))
  (quad_argument_check fun var a b) 
  (let* ((lenw (* 4 limit))
	 (work (make-array lenw :element-type 'flonum))
	 (iwork (make-array limit :element-type 'f2cl-lib:integer4))
	 (f (get-integrand fun var)))
    (handler-case
	(multiple-value-bind (junk z-a z-b z-epsabs z-epsrel z-key result abserr neval ier
				   z-limit z-lenw last)
	    (slatec:dqag #'(lambda (x)
			     (float (funcall f x)))
			 (float-or-lose a)
			 (float-or-lose b)
			 (float-or-lose epsabs)
			 (float-or-lose epsrel)
			 key
			 0.0 0.0 0 0
			 limit lenw 0 iwork work)
	  (declare (ignore junk z-a z-b z-epsabs z-epsrel z-key z-limit z-lenw last))
	  (list '(mlist) result abserr neval ier))
      (error ()
	`(($quad_qag) ,fun ,var ,a ,b ,key
	  ((mequal) $epsrel ,epsrel)
	  ((mequal) $epsabs ,epsabs)
	  ((mequal) $limit ,limit))))))


(defun quad-qags (fun var a b &key
		  (epsrel 1e-8)
		  (limit 200)
		  (epsabs 0.0))
  (quad_argument_check fun var a b) 
  (let* ((lenw (* 4 limit))
	 (work (make-array lenw :element-type 'flonum))
	 (iwork (make-array limit :element-type 'f2cl-lib:integer4))
	 (f (get-integrand fun var)))
    (handler-case
	(multiple-value-bind (junk z-a z-b z-epsabs z-epsrel result abserr neval ier
				   z-limit z-lenw last)
	    (slatec:dqags #'(lambda (x)
			      (float (funcall f x)))
			  (float-or-lose a)
			  (float-or-lose b)
			  (float-or-lose epsabs)
			  (float-or-lose epsrel)
			  0.0 0.0 0 0
			  limit lenw 0 iwork work)
	  (declare (ignore junk z-a z-b z-epsabs z-epsrel z-limit z-lenw last))
	  (list '(mlist) result abserr neval ier))
      (error ()
	`(($quad_qags) ,fun ,var ,a ,b
	  ((mequal) $epsrel ,epsrel)
	  ((mequal) $epsabs ,epsabs)
	  ((mequal) $limit ,limit))))))

(defmfun1 ($mquad_qags :doc) (fun (var :or-symbol-subvar) (a :to-float) (b :to-float) &opt
                                  ($epsrel 1e-8 :to-float) ($limit 200 :non-neg-int) ($epsabs 0.0 :to-float))
  :desc ("This is an interface to qags that is modified from " :emrefdot "quad_qags")
  (quad_argument_check fun var a b) 
  (let* ((lenw (* 4 $limit))
	 (work (make-array lenw :element-type 'flonum))
	 (iwork (make-array $limit :element-type 'f2cl-lib:integer4))
	 (f (get-integrand fun var)))
    (handler-case
	(multiple-value-bind (junk z-a z-b z-epsabs z-epsrel result abserr neval ier
				   z-limit z-lenw last)
	    (slatec:dqags #'(lambda (x) (float (funcall f x)))
			  a b $epsabs $epsrel
			  0.0 0.0 0 0
			  $limit lenw 0 iwork work)
	  (declare (ignore junk z-a z-b z-epsabs z-epsrel z-limit z-lenw last))
	  (list '(mlist) result abserr neval ier))
      (error () (defmfun1-error-final "Exception. Probably a floating-point error")))))


(defmfun1 ($mquad_qagi :doc) (fun (var :or-symbol-subvar) a b &opt
		      ($epsrel 1e-8 :to-float)
		      ($limit 200 :non-neg-int)
		      ($epsabs 0.0 :to-float))
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
      (let* ((lenw (* 4 $limit))
	     (work (make-array lenw :element-type 'flonum))
	     (iwork (make-array $limit :element-type 'f2cl-lib:integer4))
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
			      infinity
			      $epsabs
			      $epsrel
			      0.0 0.0 0 0
			      $limit lenw 0 iwork work)
	      (declare (ignore junk z-bound z-inf z-epsabs z-epsrel
			       z-limit z-lenw last))
	      (list '(mlist) result abserr neval ier))
	  (error () (defmfun1-error-final "Exception. Probably a floating-point error.")))))))

(defun quad-qawc (fun var c a b &key
		  (epsrel 1e-8)
		  (limit 200)
		  (epsabs 0.0))
  (quad_argument_check fun var a b) 
  (let* ((lenw (* 4 limit))
	 (work (make-array lenw :element-type 'flonum))
	 (iwork (make-array limit :element-type 'f2cl-lib:integer4))
	 (f (get-integrand fun var)))
    (handler-case
	(multiple-value-bind (junk z-a z-b z-c z-epsabs z-epsrel result abserr neval ier
				   z-limit z-lenw last)
	    (slatec:dqawc #'(lambda (x)
			      (float (funcall f x)))
			  (float-or-lose a)
			  (float-or-lose b)
			  (float-or-lose c)
			  (float-or-lose epsabs)
			  (float-or-lose epsrel)
			  0.0 0.0 0 0
			  limit lenw 0 iwork work)
	  (declare (ignore junk z-a z-b z-c z-epsabs z-epsrel z-limit z-lenw last))
	  (list '(mlist) result abserr neval ier))
      (error ()
	`(($quad_qawc) ,fun ,var ,c ,a ,b
	  ((mequal) $epsrel ,epsrel)
	  ((mequal) $epsabs ,epsabs)
	  ((mequal) $limit ,limit))))))

(defun quad-qawf (fun var a omega trig &key
		  (epsabs 1e-10)
		  (limit 200)
		  (maxp1 100)
		  (limlst 10))
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
      (error ()
	`(($quad_qawf) ,fun ,var ,a ,omega ,trig
	  ((mequal) $epsabs ,epsabs)
	  ((mequal) $limit ,limit)
	  ((mequal) $maxp1 ,maxp1)
	  ((mequal) $limlst ,limlst))))))

(defun quad-qawo (fun var a b omega trig &key
		  (epsrel 1e-8)
		  (limit 200)
		  (maxp1 100)
		  (epsabs 0.0))
  (quad_argument_check fun var a b) 
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
			  (float-or-lose a)
			  (float-or-lose b)
			  (float-or-lose omega)
			  integr
			  (float-or-lose epsabs)
			  (float-or-lose epsrel)
			  0.0 0.0 0 0
			  leniw maxp1 lenw 0 iwork work)
	  (declare (ignore junk z-a z-b z-omega z-integr z-epsabs z-epsrel
			   z-lst z-leniw z-maxp1 z-lenw))
	  (list '(mlist) result abserr neval ier))
      (error ()
	`(($quad_qawo) ,fun ,var ,a ,b ,omega ,trig
	  ((mequal) $epsrel ,epsrel)
	  ((mequal) $epsabs ,epsabs)
	  ((mequal) $limit ,limit)
	  ((mequal) $maxp1 ,maxp1))))))

(defun quad-qaws (fun var a b alfa beta wfun &key
		  (epsrel 1e-8)
		  (limit 200)
		  (epsabs 0.0))
  (quad_argument_check fun var a b) 
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
			  (float-or-lose a)
			  (float-or-lose b)
			  (float-or-lose alfa)
			  (float-or-lose beta)
			  wfun
			  (float-or-lose epsabs)
			  (float-or-lose epsrel)
			  0.0 0.0 0 0
			  limit lenw 0 iwork work)
	  (declare (ignore junk z-a z-b z-alfa z-beta z-int z-epsabs z-epsrel
			   z-limit z-lenw last))
	  (list '(mlist) result abserr neval ier))
      (error ()
	`(($quad_qaws) ,fun ,var ,a ,b ,alfa ,beta ,wfun
	  ((mequal) $epsrel ,epsrel)
	  ((mequal) $epsabs ,epsabs)
	  ((mequal) $limit ,limit))))))

(defun quad-qagp (fun var a b points
		  &key (epsrel 1e-8) (epsabs 0.0) (limit 200))
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
			  (float-or-lose a)
			  (float-or-lose b)
			  npts2
			  p
			  (float-or-lose epsabs)
			  (float-or-lose epsrel)
			  0.0 0.0 0 0
			  leniw lenw 0 iwork work)
	  (declare (ignore junk z-a z-b z-npts z-points z-epsabs z-epsrel
			   z-leniw z-lenw last))
	  (list '(mlist) result abserr neval ier))
      (error ()
	`(($quad_qagp) ,fun ,var ,a ,b ,points
	  ((mequal) $epsrel ,epsrel)
	  ((mequal) $epsabs ,epsabs)
	  ((mequal) $limit ,limit))))))

					
;; error checking similar to that done by $defint
#|
(defun quad_argument_check (exp var ll ul) 
  (setq exp (ratdisrep exp))
  (setq var (ratdisrep var))
  (setq ll (ratdisrep ll))
  (setq ul (ratdisrep ul))
  (cond (($constantp var)
	 (merror "Variable of integration not a variable: ~M"
		 var)))
  (cond ((not (or ($subvarp var) (atom var)))
	 (merror "Improper variable of integration: ~M" var))
	((or (among var ul)
	     (among var ll))
	 (merror "Limit contains variable of integration: ~M" var))))
|#
