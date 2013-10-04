(in-package :maxima)

(mext:mext-optimize)

(max-doc::set-cur-sec 'max-doc::equations-fandv)
(defmfun1:set-file-and-package "nm-maxima.lisp" "nminimize")

(defmfun1 ($nminimize :doc) (expr (vars :symbol-listof) (init :number-listof)
                                    &opt ($method $grnm (:member '($grnm $nm)))
                                    ($verbose nil :bool))
;                                    ($info t :bool))
  :desc
  ("Minimizes " :var "expr" " with respect to " :vardot "vars"
   " The minimizer, minimum and number of function evaluations are returned."
   " The method is either the default " :code "grnm" " grid optimized "
   " Nelder-Mead algorithm, or " :codecomma "nm" " for the original "
   "Nelder-Mead algorithm.")
  (let* ((fun (coerce-float-fun expr vars))
	 (fun1 (lambda (arr)
		 (mfuncall '$apply fun `((mlist simp) ,@(loop :for i :across arr :collect i)))))
	 (init (make-array ($length init) :initial-contents (cdr ($float init)))))
    (multiple-value-bind
	  (xk fk simplex fv) 
        (if (eq $method '$grnm)
            (neldermead:grnm-optimize fun1 init :verbose $verbose)
          (neldermead:nm-optimize fun1 init :verbose $verbose))
      (declare (ignorable fk simplex fv))
      (let ((xks
             `((mlist simp) ,@(mapcar #'(lambda (x y) `((mequal simp) ,x ,y))
                                      (cdr vars)
                                      (loop :for i :across xk :collect i)))))
;             `((mlist simp)  ,@(loop :for i :across xk :collect i))))
        (mk-mlist (list fk xks fv))))))
        
            


(examples::clear-add-example 
 "nminimize"
 '( :pretext "Find the minimum of a function at a non-analytic point."
             :vars "[x]"
             :code "nminimize(if x<0 then -x else x^2, [x], [4])")
 '(:code-res 
   ( ("f(x) := if x<0 then -x else x^2$" nil)
     ("nminimize(f, [x], [4])" "[9.09427e-21,[x = 9.53639e-11],9.09427e-21,84]")
     ("nminimize(f(x), [x], [4])" "[9.09427e-21,[x = 9.53639e-11],9.09427e-21,84]")))
 '( :vars "[x,y]"
          :code "nminimize(x^4+y^4-2*x*y-4*x-3*y, [x,y], [2,2])"))

(max-doc::author "nminimize" "Mario S. Mommer")
(max-doc::copyright "nminimize" '( 2006 "Mario S. Mommer"))
