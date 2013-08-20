(in-package :maxima)
(mext:mext-optimize)
(max-doc:set-cur-sec 'max-doc::numerics-fandv)
(defmfun1:set-mext-package "aex")

;;; Checking number of arguments appears not to work. Reviewing
;;; this, I can't see what the advantage is of n_cos over ?cos,
;;; especially since arg checking is broken.

(defmacro mk-numeric-lisp-function ( (lisp-name &optional (n 1)) )
  (let* ((max-name-string (concatenate 'string "$N_" (symbol-name lisp-name)))
         (max-name (intern max-name-string))
         (max-print-name ($sconcat max-name))
         (arg-code
          (cond ((eq n 'opt2) `((x &optional y) (x y)))
                ((= n 1) `((x)))
                ((= n 2) `((x y)))
                ((= n 3) `((x y z)))
                (t (merror "mk-numeric-lisp-functions: too many arguments to numeric lisp function.")))))
    (when (= 1 (length arg-code)) (setf arg-code (cons (car arg-code) arg-code)))
;;    (format t "print name ~a~%" max-print-name)
    `(progn
       (defmfun ,max-name (,@(car arg-code))
         (unless (numberp x)
           (merror1 "~a: Argument ~a not a number." ($sconcat ',max-name) ($sconcat x)))
         ,(if (eq n 'opt2)
              `(if y (,lisp-name x y) (,lisp-name x))
              `(,lisp-name ,@(cadr arg-code))))
       (putprop ',max-name  '$float 'function-mode) ; what is this for ?
       (max-doc::add-doc-entry (list  :name ,max-print-name
   :contents ,`(format nil "~a calls the lisp numeric function ~a. This function
  accepts only float or integer arguments from maxima (lisp complex and rationals, as well.). ~a
  may be considerably faster in some code, particularly untranslated code." ,max-print-name ,($sconcat lisp-name) ,max-print-name))))))

(loop for x in '((cl::cos) (cl::sin) (cl::tan) (cl::log opt2) (cl::sqrt) (cl::expt 2)
                 (cl::atan) (cl::acos) (cl::asin) (cl::exp) (cl::abs) (cl::sinh)
                 (cl::cosh) (cl::tanh) (cl::acosh) (cl::asinh) (cl::atanh))
   do (eval `(mk-numeric-lisp-function ,x)))
