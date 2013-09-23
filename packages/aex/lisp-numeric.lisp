;;;  Copyright (C) (2012,2013) John Lapeyre. Licensed under GPL, v3 or greater. See the file
;;;  `LICENSE' in this directory.

(in-package :maxima)
(mext:mext-optimize)
(max-doc:set-cur-sec 'max-doc::numerical-fandv)
(defmfun1:set-mext-package "aex")
(defmfun1:set-file-and-package "lisp-numeric.lisp" "aex")

;;; These are faster in
;;; timing(imap(n_cos,lrange(10^6)),0) than
;;; ?cos. Maybe because ?cos is interpreted each time it is
;;; called on the list. Same holds for map.
;;; The following is even faster!
;;; imap(lambda([x],n_cos(x)),lrange(10^6))
;;; It appears these functions are not compiled! compiling on the command
;;; line speeds them up!
;;; Arg checking works somewhat.
;;; 
;;; This could be rewritten without using eval, and
;;; the functions would probably be compiled

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
       (putprop ',max-name  '$float 'function-mode) ; tells translator what arg type this takes.
       (max-doc::add-doc-entry (list  :name ,max-print-name
   :contents ,`(format nil "~a calls the lisp numeric function ~a. This function
   accepts only float or integer arguments from maxima (lisp complex and rationals, as well.). ~a
   may be considerably faster in some code, particularly untranslated code." 
   ,max-print-name ,($sconcat lisp-name) ,max-print-name))))))

(loop for x in '((cl::cos) (cl::sin) (cl::tan) (cl::log opt2) (cl::sqrt) (cl::expt 2)
                 (cl::atan) (cl::acos) (cl::asin) (cl::exp) (cl::abs) (cl::sinh)
                 (cl::cosh) (cl::tanh) (cl::acosh) (cl::asinh) (cl::atanh))
   do (eval `(mk-numeric-lisp-function ,x)))
