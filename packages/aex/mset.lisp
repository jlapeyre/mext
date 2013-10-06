;;;  Copyright (C) (2012,2013) John Lapeyre. Licensed under GPL, v3 or greater. See the file
;;;  `LICENSE' in this directory.

;; modified for ipart(var,ind1,ind2,...) : val;

;; Search for ipart below to find changes from stock Maxima
;; This should be documented

(in-package :maxima)
(mext:mext-optimize)
;(declaim (optimize (speed 3) (space 0) (safety 0) (debug 0)))

(defmfun1:set-file-and-package "mset.lisp" "aex")
;(doc-system::set-source-file-name "src/mset.lisp")
;(doc-system::set-source-package "maxima")

(maxima-dev-doc::add-item "mset"
                  "defun mset x y"
"mset assigns y to x. This has been redefined by aex-maxima so that ipart can be
 used as an lvalue.")

(mext::no-warning
(defun mset (x y)
  (prog ()
     (cond ((or (null $setcheck)
		(eq $setcheck '$setcheck)))
	   ((and (or (atom $setcheck)
		     (memalike x (cdr $setcheck))
		     (and (not (atom x))
			  (memalike (caar x) (cdr $setcheck))))
		 (not (eq x y)))
	    (displa (list '(mtext) (disp2 x) '| set to | y))
	    (if (and $setcheckbreak (not (eq x '$setval)))
		(let (($setval y))
		  (merrbreak t)
		  (setq y $setval)))))
     (cond ((atom x)
	    (when (or (not (symbolp x))
		      (member x '(t nil) :test #'eq)
                      (mget x '$numer)
                      (get x 'sysconst))
	      (if munbindp (return nil))
	      (if (mget x '$numer)
		  (merror (intl:gettext "assignment: cannot assign to ~M; it is a declared numeric quantity.") x)
		  (merror (intl:gettext "assignment: cannot assign to ~M") x)))
	    (let ((f (get x 'assign)))
	      (if (and f (or (not (eq x y))
			     (member f '(neverset read-only-assign) :test #'eq)))
		  (if (eq (funcall f x y) 'munbindp) (return nil))))
	    (cond ((and (not (boundp x))
			(not dsksetp))
		   (add2lnc x $values))
		  ((and (not (eq x y))
			(optionp x))
		   (if $optionset (mtell (intl:gettext "assignment: assigning to option ~M") x))
		   (if (not (eq x '$linenum)) (add2lnc x $myoptions))))
	    (return (setf (symbol-value x) y)))
           ((let*
                ((x-value (if (boundp (caar x)) (symbol-value (caar x))))
                 (mset-extension-op
                  (cond
                    ((get (caar x) 'mset_extension_operator))
                    ((and
                      (not (atom x-value))
                      (get (caar x-value) 'defstruct-template)
                      (get (caar x-value) 'mset_extension_operator))))))
              (if mset-extension-op
                  (return-from mset (funcall mset-extension-op x y)))))
           ((eq '$ipart (caar x))
            (when (not (symbolp (cadr x)))
              (format t "ipart: Can't set part of literal expression.~%")
              (return-from mset `((msetq simp) ,x ,y)))
            (return-from mset
              (let ((res
                     (i-part-set (eval (cadr x)) y (mapcar #'meval (cddr x)))))
;                (format t "cadr ~a~%" (cadr x))
;                (format t "cadr val ~a~%" (eval (cadr x)))
                (set (cadr x) (meval (eval (cadr x))))
;                (format t "cadr after ~a~%" (cadr x))
;                (format t "cadr val after ~a~%" (eval (cadr x)))
                res)))
	   ((member 'array (cdar x) :test #'eq)
	    (return (arrstore x y)))
	   (t (merror (intl:gettext "assignment: cannot assign to ~M") x))))))

;; This is just modified for i-part-set. But it will probably fail when subparts
;; are not translated properly. Don't know how translation works.
;; For mode, I put '$any. Don't know how to compute it properly.

(maxima-dev-doc::add-item "msetq"
                  "def%tr msetq form"
"msetq translates maxima mset calls to lisp. msetq is modified 
  for aex-maxima to translate $ipart to i-part-set. But it will probably fail when subparts
 are not translated properly. Don't know how translation works.
 For mode, I put '$any. Don't know how to compute it properly.")

(def%tr msetq (form)
  (let ((var (cadr form))
	(val (caddr form))
	assign
	mode)
    (cond ((atom var)
	   (setq mode (value-mode var) val (translate val))
	   (cond ((not (tboundp var))
		  (pushnew var specials :test #'eq)))
	   (warn-mode var mode (car val))
	   (if (eq '$any mode)
	       (setq mode (car val) val (cdr val))
	       (setq val (dconv val mode)))
	   (cons mode
		 (if (setq assign (get var 'assign))
		     (let ((tn (tr-gensym)))
		       (lambda-wrap1 tn val `(progn (,assign ',var ,tn)
                                                    (setq ,(teval var) ,tn))))
                     `(progn
                        (if (not (boundp ',(teval var)))
                            (add2lnc ',(teval var) $values))
                        (,(if *macexpr-top-level-form-p*
                              'defparameter
                              'setq)
                          ,(teval var) ,val)))))
	  ((member 'array (car var) :test #'eq)
	   (tr-arraysetq var val))
          ((eq '$ipart (caar var))
           `($any i-part-set ,@(tr-args (list (cadr var))) ,@(tr-args (list val)) (list ,@(tr-args (cddr var)))))
	  (t
	   (unless (safe-get (caar var) 'mset_extension_operator)
             (tr-format (intl:gettext "warning: no assignment operator known for ~:M~%") var)
             (tr-format (intl:gettext "note: just keep going and hope for the best.~%")))
	   (setq val (translate val))
	   `(,(car val) mset ',(translate-atoms var) ,(cdr val))))))
