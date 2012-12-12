(in-package :maxima)

(mext:mext-optimize)

(max-doc::set-cur-sec 'max-doc::equations-fandv)
(defmfun1:set-mext-package "nelder_mead")

(defmfun1 ($nelder_mead :doc) (expr (vars :symbol-listof) (init :number-listof) )
  "The Nelder-Mead optimization algorithm."
  (let* ((fun (coerce-float-fun expr vars))
	 (fun1 (lambda (arr)
		 (mfuncall '$apply fun `((mlist simp) ,@(loop for i across arr collect i)))))
	 (init (make-array ($length init) :initial-contents (cdr ($float init)))))
    (multiple-value-bind
	  (xk fk fv) (neldermead:grnm-optimize fun1 init :verbose nil)
      (declare (ignore fk fv))
      `((mlist simp) ,@(mapcar #'(lambda (x y) `((mequal simp) ,x ,y))
			       (cdr vars)
			       (loop for i across xk collect i))))))

(examples::clear-add-example "nelder_mead"
                       '( :pretext "Find the minimum of a function at a non-analytic point."
                         :vars "[x]"
                         :code "nelder_mead(if x<0 then -x else x^2, [x], [4])")
                       '(:code-res 
                         ( ("f(x) := if x<0 then -x else x^2$" nil)
                           ("nelder_mead(f, [x], [4])" "[x = 9.536387892694628e-11]")
                           ("nelder_mead(f(x), [x], [4])" "[x = 9.536387892694628e-11]")))
                       '( :vars "[x,y]"
                          :code "nelder_mead(x^4+y^4-2*x*y-4*x-3*y, [x,y], [2,2])"))

(max-doc::author "nelder_mead" "Mario S. Mommer")
(max-doc::copyright "nelder_mead" '( 2006 "Mario S. Mommer"))
