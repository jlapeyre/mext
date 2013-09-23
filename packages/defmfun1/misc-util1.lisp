;;;  Copyright (C) (2012,2013) John Lapeyre. Licensed under GPL, v3 or greater. See the file
;;;  `LICENSE' in this directory.

;;; Some of the functions in here are documented in  max-doc-entries.lisp.

;;; Some of this stuff should be moved to aex, because some of it relies on aex
;;; The rest does not rely on defmfun1 and should be moved to mext.

(in-package :maxima)
(mext:mext-optimize)
(use-package :gjl.lisp-util)

(defvar *merror1-last-message* nil)
(defvar *merror1-last-code* nil)

;;  Following is used by 'take' and 'partition_list'.
;;  This should be documented well! This is a potentially important function.
;;  Suppose we have a function called like (inner e a1 a2 ...)
;;  where e is an expression and a1 a2, etc tell how to process the args.
;;  We would like to have (inner e (a11 a12 ...) (a21 a22 ..)) apply inner
;;  with a11 a21 at the top level, a21 a22 at second level, etc.
;;  Write this function with mk-level-func.
;;  But perhaps we want a different function than inner at a lower level depending on the
;;  element of e. So we allow name-outer and inner-map to be different.
;;  Otherwise call (mk-level-func name-outer inner name-outer).
;;  This calls name-outer recursivley until the arglists are of length 1.
;;  Write a defun for a function that takes first arg e plus more arguments
;;  which are lists, each element of which applies at a deeper level.
;;  Each of the additional args must have the same length when called.
;;  name-outer : the name of the function defined with defun
;;  inner : the name of the function called if each additional arg list is length 1
;;     It is called on e along with the car of each arglist
;;  inner-map : the name of the function called on each elemnt of e along with the
;;     cdr of each arglist in case the length of the arglists is > 1
;;  The function inner should be written to be called like
;;     (inner e a1 a2 ... ) where e is the same as e passed to name-outer and
;;  a1 a2 ... are single elements in from each of the arglists.

(defun mk-level-list-inner (inner-map level-args)
  `(cons (car e) (mapcar (lambda (x) (,inner-map x ,@(loop for arg in level-args collect `(cdr ,arg))))
                         (cdr e))))

(defun mk-level-array-inner (inner-map level-args)
  `(let* ((ne (maxima::aex-copy-new-n e))
                          (a  (maxima::aex-arr ne))
                          (ea (maxima::aex-arr e)))
                     (loop for i from 0 to (1- (length a)) do
                          (setf  (aref a i) (,inner-map (aref ea i) ,@(loop for arg in level-args collect `(cdr ,arg)))))
                     ne))
(macrolet 
    ((mk-mk-level-func (mk-name body-func)
         `(defmacro ,mk-name (name-outer inner inner-map nargs)
            (let ((level-args (loop for i below nargs collect (gensym)))
                  (body-func ',body-func))
              `(defun ,name-outer (e ,@level-args) 
                 (,inner (if (gjl.lisp-util:length1p ,(first level-args)) e
                           ,(funcall body-func inner-map level-args))
                         ,@(loop for arg in level-args collect `(car ,arg))))))))
  (mk-mk-level-func mk-level-func-array mk-level-array-inner)
  (mk-mk-level-func mk-level-func-list  mk-level-list-inner))


(defun read-file-with-pager (filename &optional (pager-command nil) )
  (unless pager-command (setf pager-command $pager_command))
  ($system (concatenate 'string pager-command " " filename)))

(defmacro read-output-with-pager (&body body)
  "Execute forms in body, capturing any output in a file.
   Then call a pager on the file."
  `(let* ((pfile "dataforpager")
          (ofile (plot-temp-file pfile)))
     (with-open-file (*standard-output* ofile :direction :output
                                        :if-exists :supersede)
                     ,@body)
     (read-file-with-pager ofile)))

(defun merror1 (sstring &rest l &aux code)
  "This is like merror, but don't tell the user to do something that
   will allow them to \"Print a backtrace of the stack frames.\""
  (declare (special errcatch *mdebug*))
  (unless (stringp sstring)
    (setf code sstring)
    (setf sstring (pop l)))
  (when code (setf $error_code code)) ; if a symbol was passed first
  (setq $error `((mlist) ,sstring ,@ l))
  (and $errormsg ($errormsg))
  (cond (*mdebug*
	 (let ((dispflag t) ret)
	   (declare (special $help dispflag))
	   (format t (intl:gettext " -- an error.  Entering the Maxima debugger.~%~
                       Enter ':h' for help.~%"))
	   (progn
	     (setq ret (break-dbm-loop nil))
	     (cond ((eql ret :resume)
		    (break-quit))))))
	(errcatch  (error 'maxima-$error))
	(t
	 (fresh-line *standard-output*)
	 ($backtrace 3)
         (format t "~%")
;;	 (format t (intl:gettext "~& -- an error. To debug this tery: debugmode(true);~%"))
	 (throw 'macsyma-quit 'maxima-error))))
