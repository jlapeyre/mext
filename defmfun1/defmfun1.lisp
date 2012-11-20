;;; Copyright (C) 2012 John Lapeyre
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(if (find-package :defmfun1 ) t (defpackage :defmfun1 (:use :common-lisp :gjl.lisp-util )))
(in-package :defmfun1)
(declaim (optimize (speed 3) (space 0) (safety 0) (debug 0)))

(use-package :gjl.lisp-util :maxima)
(use-package :maxima-dev-doc)
(import 'maxima::ddefun)
(import 'maxima::ddefvar)
(import 'maxima::ddefparameter)

(doc-system::set-source-file-name "defmfun1.lisp")
(doc-system::set-source-package "defmfun1")

(ddefvar *arg-check-func-table* (make-hash-table)
 "Table of argument checking functions for defmfun1. These are
  lists defining lambda functions. They are written literally into
  the functions at macro expansion time.
  This is populated by mk-arg-check.")

(ddefvar *arg-check-mssg-table* (make-hash-table)
 "This table holds message information (what form?)
  corresponding to argument checks. The information
  is used for printing error messages as well  as
  documentations.")

(defvar *arg-spec-to-english-table* (make-hash-table :test 'equal))
(defvar *option-arg-check-mssg-table* (make-hash-table))
(defvar *option-arg-spec-to-english-table* (make-hash-table :test 'equal))

(ddefvar *arg-preprocess-table* (make-hash-table)
         "This hash table stores structs specifying how to preprocess arguments
          to defmfun1 functions.")

(defstruct (pre-proc)
  (doc-string "" :type string)
  (func nil))

(defparameter *option-table*  (make-hash-table :test 'equal)
           "This hash-table stores option defintions for functions defined via defmfun1.
   The data in the table is only used for online documentation.")

(ddefvar *attributes-table* (make-hash-table :test 'equal)
  "This hash-table stores 'attribute' definitons for functions defined via defmfun1.
   Currently, information on when to evaluate the arguments is stored here. 
   I suppose ths could be specified in the lambda list, but it would probably be rather
   cluttered.")

(maxima::ddefun set-hold-all (name)
  "Set the $hold_all attribute for a defmfun1 function. The macro will expand to
   a defmspec, so that when the maxima function is called, the args are not evaluated.
   We convert name from a symbol to a string. This will cause conflict problems eventually,
   but using symbols now is not practical."
  (setf name ($sconcat name))
  (let ((nhash (get-or-make-subhash name *attributes-table*)))
    (setf (gethash '$hold_all nhash) t)))


(defmacro mk-attribute (attribute max-attribute)
 "Make accessor and query functions for an attribute. set- , unset- , is- ."
 (if (symbolp attribute) (setf attribute (symbol-name attribute)))
 (setf attribute (string-upcase attribute))
  `(progn 
     (defun ,(intern (concatenate 'string "SET-" attribute)) (name)
       (setf name ($sconcat name))
       (let ((nhash (get-or-make-subhash name *attributes-table*)))
         (setf (gethash ',max-attribute nhash) t)))
     (defun ,(intern (concatenate 'string "UNSET-" attribute)) (name)
       (setf name ($sconcat name))
       (let ((nhash (get-or-make-subhash name *attributes-table*)))
         (setf (gethash ',max-attribute nhash) nil)))
     (defun ,(intern (concatenate 'string "IS-" attribute)) (name)
       (setf name ($sconcat name))
       (let ((nhash (get-or-make-subhash name *attributes-table*)))
         (gethash ',max-attribute nhash)))))

(mk-attribute nowarn $nowarn)
(mk-attribute match-form $match_form)

;(defun set-match-form (name)
;  (setf name ($sconcat name))
;  (let ((nhash (get-or-make-subhash name *attributes-table*)))
;    (setf (gethash '$match_form nhash) t)))
;(defun unset-match-form (name)
; (setf name ($sconcat name))
; (let ((nhash (get-or-make-subhash name *attributes-table*)))
;   (setf (gethash '$match_form nhash) nil)))
;(defun is-match-form (name)
;  (setf name ($sconcat name))
;  (let ((nhash (get-or-make-subhash name *attributes-table*)))
;    (gethash '$match_form nhash)))

(defun are-some-args-held (name)
  (setf name ($sconcat name))
  (let ((nhash (get-or-make-subhash name *attributes-table*)))
    (gethash '$hold_all nhash)))

(ddefvar *arg-type-list*
  '(:req :optional :aux :rest :opt)
  "A list of allowed keys in the lambda list of a defmfun1 definition.")

;; for writing lambda list of defmfun when expanding defmfun1
(defparameter *arg-type-param-spec* '(:req nil :optional &optional :aux &aux :rest &rest :opt nil))

(defvar *arg-doc-string-table* (make-hash-table :test 'equal))
(defvar *pp-doc-string-table* (make-hash-table :test 'equal))

(defun save-lambda-list-etc (&rest info)
  "While expanding a defmfun1 definition, save some lambda list information for documentation."
  (setf (gethash (maxima::maybe-invert-string-case (symbol-name (car info))) *arg-doc-string-table*) info))

(maxima::ddefun save-preprocess-specs (&rest info)
  "While expanding a defmfun1 definition, save preprocessing info for documentation."
  (setf (gethash (maxima::maybe-invert-string-case (symbol-name (car info))) *pp-doc-string-table*) info))

;; This does not work if compiled code is loaded. The information is lost.
(defun get-lambda-list-etc (name)
  (gethash name *arg-doc-string-table*))

(defun get-pp-specs (name)
  (gethash name *pp-doc-string-table*))

(defun format-arg-list (args)
  (format nil "~{~a~^, ~}"
         (loop for arg in args collect
              (progn 
                (if (listp arg) (setf arg (car arg)))
                (maxima::maybe-invert-string-case (format nil "<~a>" arg))))))

(defun error-or-message (name mssg)
  "name is function name. mssg is error message. print message,
   but do not signal an error if match_form is set."
  (if (is-match-form name) (unless (is-nowarn name) (format t (concatenate 'string "Warning: " mssg)))
      (maxima::merror1 mssg)))

(maxima::ddefun get-check-func (spec-name)
  "Return list defining a lambda function for argument check
   specified by spec-name. This is called by defmfun1 at macro expansion time."
  (cond ((listp spec-name)
         (dbind (name &rest args) spec-name
                (let ((code (gethash name *arg-check-func-table*)))
                  (if code
                      `(lambda (e) ,(apply code args))                      
                      (maxima::merror "defmfun1::get-check-func: No code registered for check type ~a~%" name)))))
        ((keyword-p spec-name)
         (let ((code (gethash spec-name *arg-check-func-table*)))
           (if code code
               (maxima::merror "defmfun1::get-check-func: No code registered for check type ~a~%" spec-name))))
        (t (maxima::merror (intl:gettext "defmfun1::get-check-func: spec-name ~a is not a list or keyword") spec-name))))

(defun check-and-error (test arg name args)
  `(unless (funcall ,(defmfun1::get-check-func test) ,arg)
     (defmfun1::signal-arg-error ',test (list ,arg) ',name ,args)
     (return-from ,name (cons (list ',name) ,args))))

;; Extraordinarily hard to debug. It seems like a textual substititution, but
;; in fact, we have to qualify val as maxima::val. Nothing I can do with macroexpand or format
;; will show the qualification, so there is no way to get a clue about what is wrong.

;; get-check-func will signal an error at expansion time if the check code does not exist.
;; but the code for opts and regular args is shared. So signal-option-arg-error may not
;; find the error message if the author of the defmfun1 inadvertently used a test for a
;; normal arg with an option (very easy to do). All you get is a cryptic error at runtime.
;; This needs to be fixed.

(defun check-and-error-option (tst name opt-name  args)
  `(unless (funcall ,(defmfun1::get-check-func (car tst)) maxima::val)
     (defmfun1::signal-option-arg-error
      ',(car tst) (list maxima::val ',opt-name) ',name ,args)
     (return-from ,name (cons (list ',name) ,args))))

(defun narg-error-or-message (name args restarg nargs nreq nreqo rest)
  `(progn (defmfun1::narg-error-message  ',name ,restarg
                                 ,nargs ,nreq ,nreqo ,(not (null rest)))
          (return-from ,name (cons (list ',name) ,args))))

(maxima::ddefun format-protocol (sname req optional rest)
  "This formats the protocol (lambda list) of a defmfun1 form as a string for printing
   documentation." 
  (let ((sarg (format-arg-list req)))
    (when (not (null optional))
      (setf sarg (concatenate 'string sarg " :optional "
                              (format-arg-list (rest optional)))))
    (when (not (null rest))
      (setf sarg (concatenate 'string sarg " :rest "
                              (format-arg-list (rest rest)))))
  (format nil "~a(~a)" sname sarg)))


(defun mk-pre-proc (spec-name doc-string body)
  (let ((pp (make-pre-proc :doc-string doc-string
                           :func `(lambda (e) ,body))))
    (setf (gethash spec-name *arg-preprocess-table*) pp)))

;; Specify the preprocessing directive here.
(dolist (one-pp '(
                  (:ensure-lex "is converted to a lisp list expression before processing"
                   (maxima::$lex e))
                  (:ensure-list "is ensured to be lisp list before processing"
                   (setf e (if  (listp e) (cdr e) (list e))))))
  (apply #'mk-pre-proc one-pp))



(ddefparameter *pp-spec-types*
  (get-hash-keys *arg-preprocess-table*))

(defun get-arg-spec-to-english (spec-name)
  (let (args)
    (when (listp spec-name) (setf args (cdr spec-name)) (setf spec-name (car spec-name)))
    (let ((txt (gethash spec-name *arg-spec-to-english-table*)))
      (unless txt (maxima::merror1 '$defmfun1_no_spec_mssg "No argument message for ~a." spec-name))
      (if args (apply #'format (append (list nil txt) args))
          txt))))
    

(defun mk-arg-check1 (arg-class spec-name err-mssg-spec body)
  (let* ((emsg (rest err-mssg-spec)))
    (setf (gethash spec-name *arg-check-func-table*)
          `(lambda (e) ,body))
    (cond ( (eq 'option arg-class)
            (setf (gethash spec-name *option-arg-spec-to-english-table*)
                  (or-comma-separated-english emsg))
             (setf (gethash spec-name *option-arg-check-mssg-table*) (list (car err-mssg-spec) (rest err-mssg-spec))))
          ( (eq 'arg arg-class)
            (setf (gethash spec-name *arg-spec-to-english-table*)
                  (or-comma-separated-english emsg))
            (setf (gethash spec-name *arg-check-mssg-table*) (list (car err-mssg-spec) (rest err-mssg-spec))))
          (t (merror "defmfun1::mk-arg-check: Unrecognized arg-class ~M" arg-class)))))


(defun mk-arg-check2 (arg-class spec-name-and-args err-mssg-spec body)
  (let* ((emsg (rest err-mssg-spec)))
    (let  ((spec-name (first spec-name-and-args))) ;; throw away the args, but we should use then for checking
           (setf (gethash spec-name *arg-check-func-table*)
                 body)
           (cond ( (eq 'option arg-class)
                  (setf (gethash spec-name *option-arg-spec-to-english-table*)
                        (or-comma-separated-english emsg))
                   (setf (gethash spec-name *option-arg-check-mssg-table*) (list (car err-mssg-spec) (rest err-mssg-spec))))
                 ( (eq 'arg arg-class)
                  (setf (gethash spec-name *arg-spec-to-english-table*)
                        (or-comma-separated-english emsg))
                   (setf (gethash spec-name *arg-check-mssg-table*) (list (car err-mssg-spec) (rest err-mssg-spec))))
                 (t (maxima::merror "defmfun1::mk-arg-check: Unrecognized arg-class ~M" arg-class))))))

(defun mk-arg-check (spec-name err-mssg-spec body)
  (unless (listp err-mssg-spec) (setf err-mssg-spec (list err-mssg-spec)))
  (if (listp spec-name)
      (progn 
        (mk-arg-check2 'arg spec-name (cons "Argument '~a'" err-mssg-spec) body))
      (progn
        (mk-arg-check1 'arg spec-name (cons "Argument '~a'" err-mssg-spec) body))))

(defun mk-opt-check (spec-name err-mssg-spec body)
  (unless (listp err-mssg-spec) (setf err-mssg-spec (list err-mssg-spec)))
  (if (listp spec-name)
      (progn 
        (mk-arg-check2 'option spec-name (cons "Value '~a' for option '~a'" err-mssg-spec) body))
      (progn 
        (mk-arg-check1 'option spec-name (cons "Value '~a' for option '~a'" err-mssg-spec) body))))

(defun get-pp-func (spec-name)
  (pre-proc-func (gethash spec-name *arg-preprocess-table*)))

;;  These format- functions exist to stop printing huge error messages.

(defun format-call (name call)
  (let ((str ($sconcat (cons (list name) call))))
    (if (> (length str) 80)
        (concatenate 'string (subseq str 0 60) " ... ") str)))

(defun format-rest-args (args)
  (let* ((strlist (mapcar #'$sconcat args))
         (str (format nil "[~{~a~^,~}]" strlist)))
    (if (> (length str) 80)
        (concatenate 'string (subseq str 0 60) " ... ") str)))

(defun format-one-arg (arg)
  (let ((str ($sconcat arg)))
    (if (> (length str) 80)
        (concatenate 'string (subseq str 0 60) " ... ") str)))

;; This is not good enough if there are many args with long lengths

(defun format-args (args)
  (mapcar #'format-one-arg  args))

(defun signal-rest-arg-error (spec-name arg-list name call)
  (let* ((espec (gethash spec-name *arg-check-mssg-table*))
;;         (arg-list1 (list (format nil "[~{~a~^,~}]" (mapcar #'$sconcat (car arg-list)))))
;;         (arg-list1 (list (format nil "[~{~a~^,~}]" (format-args (car arglist)))))
         (arg-list1 (list (format-rest-args (car arg-list))))
         (specl-str (not-comma-separated-english (cadr espec))))
    (maxima::merror1 (format nil "~a One of ~? is ~a in ~a." (err-prefix name) (car espec)
                            arg-list1 specl-str (format-call name call)))))

;; yes, we might as well preserve the call and not have two of these.

(defmacro mk-signal-error (name hash)
  "Prints one of two error messages depending on whether it is called
 in a defmfun1 expansion. If not, the list of call args is lost. But, I
 suppose we could preserve them in a lexical variable. Call this with
 call nil to get the second message."
  `(defun ,name  (spec-name arg-list name call)
   (let ((spec-args (if (listp spec-name) (rest spec-name) nil)))
    (if (listp spec-name) (setf spec-name (car spec-name)))
    (let* ((espec (gethash spec-name ,hash))
           (arg-list1 (format-args arg-list))
           (specl-str (not-comma-separated-english (cadr espec)))
           (pre-name (err-prefix name))
           (spstr (if spec-args (apply #'format (append (list nil specl-str) spec-args))
                                          specl-str))
           (call-str (format-call name call)))
      (if call
          (error-or-message name (format nil "~a ~? is ~a in ~a.~%" pre-name (car espec)
                                       arg-list1 spstr  call-str))
          (maxima::merror1 (format nil "~a ~? is ~a." pre-name (car espec)  arg-list1 spstr)))))))
          

(mk-signal-error signal-arg-error *arg-check-mssg-table*)
(mk-signal-error signal-option-arg-error *option-arg-check-mssg-table*)

(defun int-range-check (n1 n2)
  `(and (integerp e) (>= e ,n1) (<= e ,n2)))

(dolist (one-check `(
                     (:function "a function"
                                (not (numberp e))) ; a bit fuzzy what this would mean
                     (:string  "a string" (stringp e))
                     (:string-or-listof ("an string" "a list of strings")
                                         (or (stringp e)
                                             (and (maxima::$listp e)
                                                  (every #'stringp (cdr e)))))
                     (:or-pathname-string ("a string" "a lisp pathname")
                                          (or (stringp e) (pathnamep e)))
                     (:list    "a list"   (maxima::$listp e))
                     (:ae-list "a list (lex or aex)"
                      (maxima::$ae_listp e))
                     (:or-ae-list-string ("a list (lex or aex)" "a string")
                               (or (maxima::$ae_listp e) (stringp e)))
                     (:integer "an integer" (integerp e))
                     (:integer-or-listof ("an integer" "a list of integers")
                                         (or (integerp e)
                                             (and (maxima::$listp e)
                                                  (every #'integerp (cdr e)))))
                     (:number  "a number" (numberp e))
                     (:number-listof ("a list of numbers")
                                     (and (maxima::$listp e)
                                          (every #'(lambda (x) (numberp x)) (cdr e))))
                     (:complex-number  "a complex number" (complex_number_p e)) ; from ellipt.lisp
                     (:non-neg-number "a non-negative number"
                      (and (numberp e) (>= e 0)))
                     (:non-neg-int "a non-negative integer"
                      (and (integerp e) (>= e 0)))
                     (:pos-int "a positive integer"
                         (and (integerp e) (> e 0)))
                     ((:int-range 2) "an integer between ~a and ~a"
                       int-range-check)
                     (:uint-64 ,(format nil "equivalent to an unsigned 64 bit integer~%    (ie an integer between 0 and 2^64)")
                      (and (integerp e) (>= e 0) (< e 18446744073709551616)))
                     (:not-zero "an expression that is not zero."
                      (not (maxima::zerop1 e)))
                     (:roman-integer "an integer between 2 and 3999 (Required for roman form.)"
                                     (and (integerp e) (> e 0) (< e 4000)))
                     (:radix "a valid radix (an integer between 2 and 36)"
                             (and (integerp e) (> e 1) (< e 37)))
                     (:or-radix-string ("a valid radix (an integer between 2 and 36)" "a string")
                             (or (stringp e) (and (integerp e) (> e 1) (< e 37))))
                     (:or-string-symbol  ("a string" "a symbol")
                      (or (symbolp e) (stringp e)))
                     (:or-string-symbol-or-listof ("an string" "a symbol" "a list of strings or symbols")
                                         (or (stringp e) (symbolp e)
                                             (and (maxima::$listp e)
                                                  (every #'(lambda (x) (or (stringp x) (symbolp x))) (cdr e)))))
                     (:symbol-listof ("a list of symbols")
                                     (and (maxima::$listp e)
                                          (every #'(lambda (x) (symbolp x)) (cdr e))))
                     (:or-string-non-atom  ("a string" "non-atomic")
                      (or (stringp e) (not (maxima::$mapatom e))))
                     (:non-atom "non-atomic"
                                (not (maxima::$mapatom e)))
                     (:non-atom-list "non-atomic and represented by a lisp list"
                                (and (not (maxima::$mapatom e)) (listp e)))
                     (:or-pos-int-string ("a string" "a positive integer")
                                             (or (stringp e) (and (integerp e) (> e 0))))
                     (:or-non-atom-subvar  ("a subscripted variable" "non-atomic")
                      (or (not (maxima::$mapatom e)) (maxima::$subvarp e)))
                     (:or-non-atom-subvar-string   ("a string" "a subscripted variable" "non-atomic")
                      (or (stringp e) (not (maxima::$mapatom e)) (maxima::$subvarp e)))
                     (:aex "an array-representation expression"
                      (maxima::aex-p e))
                     (:aex_adj  "an adjustable array expression"
                                (and (maxima::aex-p e) (maxima::aex-adjustable e)))
                     (:seq-spec "a sequence specification"  ; need to check better than this
                                (or (integerp e)
                                    (member e '( maxima::$all maxima::$none maxima::$reverse))
                                    (and (maxima::$listp e)
                                         (every #'integerp (cdr e)))))))
  (apply #'mk-arg-check one-check))


;; Careful! code for test is overwritten if already defined above. this should be fixed somehow.
(dolist (one-check '(
                     (:out-rep  "a valid expression representation.~%   (It must be either 'ml' or 'ar')"
                      (member e '(maxima::$ml maxima::$ar)))
                     (:bool  "a boolean value. It must be true or false."
                      (member e '(t nil)))
                     ( (:int-range 2) "an integer between ~a and ~a"
                      int-range-check)
                     (:non-neg-int "a non-negative integer."
                      (and (integerp e) (>= e 0)))))
  (apply #'mk-opt-check one-check))

(ddefparameter *arg-spec-keywords*
  (get-hash-keys *arg-check-func-table*)
  "This is a list of the valid keywords specifying argument checks for the lambda list
   of a defmfun1 function. It must be set after all of the tests are defined.
   During expansion of a defmfun1 form, elements in an argument spec in
   the lambda list are searched for in this list.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-default-options (name opts)
   "Set  possible option hash table from spec in defmfun1 param list.
  This hash table is only used to provide information to the user
  via foptions()."
    (let ((sname ($sconcat name)))
;      (format t "Setting default options for function ~s~%" sname)
      (unless (gethash sname *option-table*) ; put this here to avoid possible overwrite. but no difference
        (let ((optt (make-hash-table)))
          (fill-hash-from-list optt opts)
          (setf (gethash ($sconcat name) *option-table*) optt)))))

;; Note that in type-tab &opt appears twice with different package qualification.
;; I never know which way is up with this. lose hours trying to track down bugs.
(maxima::ddefun group-args (args)
   "At macro expansion time of defmfun1, Collect args in list for each arg type."
  (let* ((arglist) (argt ':req))
    (dolist (arg args)
      (gaif (getf '(&opt :opt maxima::&opt :opt  &req :req maxima::&req :req
               maxima::&optional :optional maxima::&aux :aux maxima::&rest :rest)
                 arg) (setf argt anit)
               (push (if (symbolp arg) (list arg) arg) (getf arglist argt))))
    (dolist (argt *arg-type-list*)
      (setf (getf arglist argt) (nreverse (getf arglist argt))))
    arglist))

(maxima::ddefun parse-args (name arglist)
  "At macro expansion time of defmfun1, Return four lists constrcucted from arglist.
  First list will be directly converted to the lambdalist argument  spec for defmfun definition.
    This list only has the test   specs :list, etc. stripped out.
  Second list is the same,  but for each argument spec, only the argname and the
    tests are retained (eg default values are removed).
  Third list is pre-processing specs.
  Fourth list is supplied-p args. "
  (let ( (arglist1) (arg-specs) (pp-specs) (supplied-p-hash (make-hash-table)))
    (dolist (argt *arg-type-list*)
      (let ((argt1) (argt-spec) (pp-spec) )
        (dolist (arg (getf arglist argt)) ; arg is complete specification of a single argument
          (let
              ((nospecs (remove-if #'(lambda(x) (or (member x *arg-spec-keywords*) (member x *pp-spec-types*)
                                                    (and (listp x) (keyword-p (first x))
                                                         (member (first x) *arg-spec-keywords*) )))   arg))
               (wspecs (cons (car arg) (remove-if #'(lambda(x) (not (or (member x *arg-spec-keywords*)
                                                                (and (listp x) (keyword-p (first x))
                                                                     (member (first x) *arg-spec-keywords*)))))  arg)))
               (wppspecs (cons (car arg) (remove-if #'(lambda(x) (not (member x *pp-spec-types*))) arg))))
            (when (some (lambda (e)
                          (or (keyword-p e) (and (listp e) (keyword-p (car e)) )))  nospecs)
              (maxima::merror1 'maxima::$defmfun1_unknown_directive "defmfun1: Error expanding function definition ~s"
                                (format nil "~s. Unknown argument directive ~s.~%In source file ~a."
                                        name nospecs (doc-system::get-source-file-name))))
;;                                        name nospecs  maxima::$load_pathname))) ; does not work for more than one reason
            (push (if (length1p nospecs) nospecs (list (first nospecs) `(quote ,(second nospecs))) ) argt1) ; quote default values
            (if (length-eq nospecs 3) (setf (gethash (first nospecs) supplied-p-hash) (third nospecs)))
            (push wspecs argt-spec)
            (push wppspecs pp-spec)))
      (setf (getf arg-specs argt) (nreverse argt-spec))
      (setf (getf pp-specs argt) (nreverse pp-spec))
      (let ( (v (getf *arg-type-param-spec* argt)))
        (setf (getf arglist1 argt) (if v (if argt1 (cons v (nreverse argt1)) nil)
                                     (nreverse argt1))))))
    (list arglist1 arg-specs pp-specs supplied-p-hash)))

(defun rem-keys-arg-list (arg-list)
  (mapcar #'(lambda (x) (getf arg-list x)) defmfun1::*arg-type-list*))

(defun group-and-parse-args (name args)
  (defmfun1::parse-args name (defmfun1::group-args args)))

(defun make-arg-spec-hash (specs)
  "Collect argument (parameter) check specification into a hash keyed by parameter name.
   This hash is build during expansion of a defmfun1 and it is checked when writing the code
   that does assignment."
  (let ((h (make-hash-table)))
    (dolist (e specs)
      (setf (gethash (if (listp (car e)) (caar e) (car e)) h) (cdr e)))
    h))

#|
(defun make-pp-spec-hash (specs)
  "blindly copied. could clean this up."
  (let ((h (make-hash-table)))
    (dolist (e specs)
      (setf (gethash (if (listp (car e)) (caar e) (car e)) h) (cdr e)))
    h))
|#

(defun rule-expr-p (e)
  (and (listp e) (listp (car e))
       (or (eq 'maxima::|$Rule| (caar e))
           (eq 'maxima::$-> (caar e)))))

(defun listof-rule-expr-p (e)
  "Check if first elmt of mlist is Rule."
  (and (maxima::$listp e) (rule-expr-p (cadr e))))


(maxima::ddefun collect-opt-args (rarg nmin)
  "At run time, filter parameters passed to a defmfun1 definition into option
  params and non-option params. Skipping searching through required arguments
  is done here, but it is probably not most efficient."
  (declare (fixnum nmin))
  (do* ((rarg1 rarg (cdr rarg1))
        (a (car rarg) (car rarg1))
        (i 0 (1+ i))
        (opts)
        (nonopts))
       ((null rarg1) (list (nreverse opts) (nreverse nonopts)))
    (declare (fixnum i))
    (cond
      ((< i nmin) (push a nonopts)) ; for a bit of efficiency
      ((rule-expr-p a) (push (cdr a) opts))
      ((listof-rule-expr-p a) (setf opts (collect-opt-args-lev-1 (cdr a) opts)))
      (t (push a nonopts)))))

(defun collect-opt-args-lev-1 (rarg opts)
  "Collect options that were supplied in an mlist."
  (do* ((rarg1 rarg (cdr rarg1))
        (a (car rarg) (car rarg1)))
      ((null rarg1) opts)
    (if (rule-expr-p a) (push (cdr a) opts)
      (maxima::merror1 "put name of func here: Found a non-option argument '~a' in a list of options." a))))


(defun err-prefix (name)
  (unless (stringp name) (setf name ($sconcat name)))
  (format nil "~a:" name))

(defparameter *print-error-integers-as-words* t)

(defun err-itostr (i)
  (err-itostr0 i "~[no~:;~:*~r~]"))
(defun err-itostr1 (i)
  (err-itostr0 i "~r"))
(defun err-itostr0 (i zfmt)
    (let ((f (if *print-error-integers-as-words* zfmt "~d")))
    (format nil f i)))

; no localization here.
(defun format-nargs-expected (nmin nmax restp terminal-verb-p)
  "This function is also called within :max-doc."
  (let ((pw *print-error-integers-as-words*)
        (are-word (if terminal-verb-p " are" "")))
    (cond ( restp
           (if terminal-verb-p 
               (format nil "~a or more arguments are" (err-itostr nmin))
               (format nil "~a or more arguments" (err-itostr nmin))))
          ( (= nmin nmax)
           (if terminal-verb-p 
               (format nil "~a argument~p ~:[are~;is~]" (err-itostr nmin) (if (and pw (= nmin 0)) 1 nmin)
                       (or (= 1 nmin) (and pw (= 0 nmin))))
               (format nil "~a argument~p" (err-itostr nmin) nmin )))
          ( (= 1 (- nmax nmin))
           (format nil "either ~d or ~d arguments~a" (err-itostr1 nmin) (err-itostr nmax) are-word))
          ( t
           (format nil "between ~d and ~d arguments~a" (err-itostr1 nmin) (err-itostr nmax) are-word)))))


(defun compute-narg-error-code (restarg nargs nmin nmax restp)
  (declare (ignore restp restarg))
  (cond ((< nargs nmin)
         'maxima::$args_too_few)
        ((> nargs nmax)
         'maxima::$args_too_many)))

(defun narg-error-message (name restarg nargs nmin nmax restp)
  "Call merror1 with message about incorrect number of arguments."
  (declare (fixnum nargs nmin nmax))
  (setf nargs (the fixnum (+ nargs (length restarg))))
  (let* ((sname ($sconcat name))
         (str-narg (format nil "called with ~d argument~p" (err-itostr nargs) nargs))
         (str-expected
          (format-nargs-expected nmin nmax restp t)))
;;         (err-code (compute-narg-error-code restarg nargs nmin nmax restp))) maybe restore this later
    (error-or-message name (format nil "~a ~a ~a; ~a expected.~%" (err-prefix sname) sname str-narg str-expected))))
;;    (if (is-match-form name) (progn
;;                               (format t "~a ~a ~a; ~a expected.~%" (err-prefix sname) sname str-narg str-expected)
;;                               (cons `(,name) call))
;;        (maxima::merror1 err-code  "~a ~a ~a; ~a expected." (err-prefix name) sname str-narg str-expected))))
