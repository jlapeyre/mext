;;;  defmfun1 is a function definition macro for relatively high-level maxima
;;;  functions.
;;;  Copyright (C) (2012) John Lapeyre. Licensed under GPL, v3 or greater. See the file
;;;  `LICENSE' in this directory.

(if (find-package :defmfun1 ) t (defpackage :defmfun1 (:use :common-lisp :gjl.lisp-util )))
(in-package :defmfun1)

(mext:mext-optimize)

(use-package :gjl.lisp-util :maxima)
(use-package :maxima-dev-doc)
(import 'maxima::ddefun)
(import 'maxima::ddefvar)
(import 'maxima::ddefparameter)

(defvar *mext-package* nil)

(defun set-mext-package (name)
  (setf *mext-package* name))

;; There are a couple of competing ways to set these.
;; So, we will try to use this function only.
(defun set-file-and-package (file-name package-name)
  (doc-system:set-source-file-name file-name)
  (doc-system:set-source-package package-name)
  (set-mext-package package-name))

(set-file-and-package "defmfun1.lisp" "defmfun1")

;(doc-system:set-source-file-name "defmfun1.lisp")
;(doc-system:set-source-package "defmfun1")

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

(ddefvar *arg-check-err-code-table* (make-hash-table)
 "This table holds the symbol (in maxima package) representing the
  error code associated with an argument check.")


(defvar *arg-check-preprocess-table* (make-hash-table))
(defvar *opt-check-preprocess-table* (make-hash-table))

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

;;; ATTRIBUTES ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Two levels of hash tables maybe not best implementation.

(ddefvar *attributes-table* (make-hash-table :test 'equal)
"This hash-table stores 'attribute' definitons for functions defined via defmfun1.
Keys are stringified function names; values are hash tables with: keys (maxima
symbols) being attribute names; and values being the value of the attribute, typically
`t' or `nil'.")

(defun set-attribute (names att)
  (ensure-list names)
  (dolist (name names)
    (setf name ($sconcat name))
    (let ((nhash (get-or-make-subhash name *attributes-table*)))
    (setf (gethash att nhash) t))))

(defun unset-attribute (name att)
  (setf name ($sconcat name))
  (let ((nhash (get-or-make-subhash name *attributes-table*)))
    (setf (gethash att nhash) nil)))

;; does not yet distinguish between no attribute and nil
(defun get-attribute (name att)
  (setf name ($sconcat name))
  (let ((nhash (gethash name *attributes-table*)))
    (if nhash
        (gethash att nhash)
      nil)))

(defun get-attributes (name)
 "Return a lisp list of all attributes set for `name'."
 (maxima::maxima-symbol-to-string name)
 (let ((oh (gethash name defmfun1::*attributes-table*)))
   (when oh (let (ol)  ; (cadr v) to get rid of quote
              (maphash (lambda (k v)
;                         (declare (ignore v))
                         (when v (push k ol ))) oh)
              ol))))

(defun get-funcs-with-attribute (attr)
 "Return a list of all functions with attribute `attr'."
  (let ((all-funcs (get-hash-keys defmfun1::*attributes-table*))
        (good-funcs))
    (dolist (f all-funcs)
      (when (member attr (get-attributes f))
        (push f good-funcs)))
    good-funcs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Some of this should be separated and moved to mext package.

(ddefvar *mext-functions-table* (make-hash-table :test 'equal)
  "This hash-table stores a list of the user functions defined 
in a  mext package.")

(ddefvar *mext-package-table* (make-hash-table :test 'equal)
 "This hash-table stores the name of the mext package in which a function
   is defined.")

(ddefvar *mext-filename-table* (make-hash-table :test 'equal)
 "This hash-table stores the name of the source file in which a function
  is defined.")

;; Need a maxima interface to this, for eg alt_eigen.
;; Is there a maxima interface for setting the source file ?
;; Maybe should allow optionally passing source filename
(defun record-mext-package (names package &optional source-filename)
 "Record fact that string `name' is in mext packge `package',
  and is defined in source-filename.
  This may be called with the current package *mext-package*,
  which may not be set, in which case, we do nothing. At the
  same time, push `name' onto a list keyed by `package' in
  the hash table *mext-functions-table*."
 (when package
   (dolist (name (maxima::ensure-lisp-list names))
     (setf name (maxima::$sconcat name))
     (setf (gethash name *mext-package-table*) package)
     (setf (gethash name *mext-filename-table*) 
           (if source-filename source-filename (doc-system:get-source-file-name)))
     (push name (gethash package *mext-functions-table*)))))

(defun get-mext-package-for-function (name)
  (setf name (maxima::$sconcat name))
  (let ((pack (gethash name *mext-package-table*)))
    (if pack pack "none")))

(defun get-filename-for-function (name)
  (setf name (maxima::$sconcat name))
  (let ((fname (gethash name *mext-filename-table*)))
    (if fname fname "none")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(maxima::ddefun set-hold-all (name)
  "Set the $hold_all attribute for a defmfun1 function. The macro will expand to
   a defmspec, so that when the maxima function is called, the args are not evaluated.
   We convert name from a symbol to a string. This will cause conflict problems eventually,
   but using symbols now is not practical."
  (set-attribute name 'maxima::$hold_all))

(defmacro mk-attribute (attribute max-attribute)
 "Make accessor and query functions for an attribute. set- , unset- , is- ."
 (when (symbolp attribute) (setf attribute (symbol-name attribute)))
 (setf attribute (string-upcase attribute))
  `(progn 
     (defun ,(intern (concatenate 'string "SET-" attribute)) (name)
       (set-attribute name ',max-attribute))
     (defun ,(intern (concatenate 'string "UNSET-" attribute)) (name)
       (unset-attribute name ',max-attribute))
     (defun ,(intern (concatenate 'string "IS-" attribute)) (name)
       (get-attribute name ',max-attribute))))

;; Why do we not do this for set-hold-all ?
;; I think because that must be set before defmfun1 expansion.
;; Changing it later will have no effect.
(mk-attribute nowarn maxima::$nowarn)
(mk-attribute match-form maxima::$match_form)

(defun are-some-args-held (name)
  (get-attribute name 'maxima::$hold_all))

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

(defun get-lambda-list-etc (name)
  (gethash name *arg-doc-string-table*))

(defun get-pp-specs (name)
  (gethash name *pp-doc-string-table*))

(defun format-arg-list (args)
  (format nil "~{~a~^, ~}"
          (loop for arg in args collect
                (progn 
                  (when (listp arg) (setf arg (car arg)))
                  (maxima::maybe-invert-string-case (format nil "<~a>" arg))))))

;; Print a warning or signal an error.
;; is-match-form is only called here.
;; error-or-message is called by
;;    signal-arg-error, signal-option-arg-error, narg-error-message (via narg-error-or-message),
;;    defmfun1-error-final, defmfun1-error-return
(defun error-or-message (name mssg force-match match-val &optional err-code)
  "name is function name. mssg is error message. print message,
   but do not signal an error if match_form is set."
  (when err-code (setf maxima::$error_code err-code)); dont pass to merror1
  (cond ((or (and force-match match-val) (and (not force-match) (is-match-form name)))
         (unless (is-nowarn name) (format t (concatenate 'string "Warning: " mssg)))
         t)  ; return true here so that the calling function knows not to exit. Umm not sure its used.
        (t
         (maxima::merror1 mssg)
         nil)))

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


(defun write-force-match-code (have-match)
  (if have-match `(maxima::match-supplied-p maxima::match-opt)
                             `(nil nil)))

;; This function generates a bit of code that is inserted into the
;; body of a defmfun1 function.  The code tests the value of an
;; argument at run-time. The test spec may also specifiy that the argument
;; is `preprocessed' in some way. So we check for this.
;; check-and-error is called by: defmfun1-write-assignments, 
;; defmfun1-write-rest-assignments, 
(defun check-and-error (test arg fname args have-match)
  (let* ((fc `(funcall ,(defmfun1::get-check-func test) ,arg))
         (force-match-code (write-force-match-code have-match))
         (sa1 `(defmfun1::signal-arg-error ',test (list ,arg) ',fname ,args ,@force-match-code))
         (sa `(,sa1 (return-from ,fname (cons (list ',fname) ,args)))))
    (if (gethash test *arg-check-preprocess-table*)
        `(let ((res ,fc))
           (if (not (first res))
               (progn ,@sa)
             (setf ,arg (second res))))
      `(unless ,fc ,@sa))))
  
;; We have to qualify val as maxima::val. Nothing I can do with macroexpand or format
;; will show the qualification.
;; get-check-func will signal an error at expansion time if the check code does not exist.
;; but the code for opts and regular args is shared. So signal-option-arg-error may not
;; find the error message if the author of the defmfun1 inadvertently used a test for a
;; normal arg with an option (very easy to do). All you get is a cryptic error at runtime.
;; This needs to be fixed.
;; check-and-error-option is called by defmfun1-write-opt-assignments.
(defun check-and-error-option (tst fname opt-name opt-var args have-match)
  (let* ((fc `(funcall ,(defmfun1::get-check-func (car tst)) maxima::val))
        (force-match-code (write-force-match-code have-match))
        (sc `((defmfun1::signal-option-arg-error
                ',(car tst) (list maxima::val ',opt-name) ',fname ,args ,@force-match-code)
              (return-from ,fname (cons (list ',fname) ,args)))))
  (if (gethash (car tst) *opt-check-preprocess-table*)
      `(let ((res ,fc))
         (if (not (first res))
           (progn ,@sc)
           (setf ,opt-var (second res))))
    `(unless ,fc ,@sc))))

;; Write a code snippet to insert in body of defmfun1 function
(defun narg-error-or-message (fname args restarg nargs nreq nreqo rest have-match)
  (let ((force-match-code (write-force-match-code have-match)))
    `(progn (defmfun1::narg-error-message  ',fname ,restarg
              ,nargs ,nreq ,nreqo ,(not (null rest)) ,@force-match-code)
            (return-from ,fname (cons (list ',fname) ,args)))))

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

;; Specify the preprocessing directives here.
;; Note that preprocessing can be done together with checking
;; with the argument checking directives, eg in tables *arg-check... .
;; This may make the preprocessing directives here obsolete ?
(dolist (one-pp '(
                  (:ensure-lex "is converted to a lisp list expression before processing"
                   (maxima::$lex e))
                  (:ensure-list "is ensured to be lisp list before processing"
                   (setf e (if (listp e) (cdr e) (list e))))))
  (apply #'mk-pre-proc one-pp))

(ddefparameter *pp-spec-types*
  (get-hash-keys *arg-preprocess-table*))

(defun get-arg-spec-to-english (spec-name)
  (let (args)
    (when (listp spec-name) (setf args (cdr spec-name)) (setf spec-name (car spec-name)))
    (let ((txt (gethash spec-name *arg-spec-to-english-table*)))
      (unless txt 
;        (defmfun1-expand-error '$defmfun1_no_spec_mssg "unknown function"
;          (format nil "No argument message for test ~a." (keyword-etc-to-string spec-name))))
        (maxima::merror1 'maxima::$defmfun1_no_spec_mssg 
          "defmfun1: Error printing argument test description for test `~a'. No test description found." 
          (maxima::sym-to-string spec-name)))
      (if args (apply #'format (append (list nil txt) args))
          txt))))

;; The 'arg, etc. should be :arg
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

;; For tests that take additional argumens. eg (:int-range 2)
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

;; arg spec-name that is list is actually (name number-of-parameters-passed-to-check-func)
;; eg. see :int-range
(defun mk-arg-check (spec-name err-mssg-spec body)
  (unless (listp err-mssg-spec) (setf err-mssg-spec (list err-mssg-spec)))
  (let* 
      ((rspec-name (if (consp spec-name) (car spec-name) spec-name))
       (err-mssg-spec-1 (cons "Argument '~a'" err-mssg-spec))
       (code (intern (sconcat "$CHK_" 
           (coerce (loop for char across (symbol-name rspec-name)
                        collect (if (eq char #\-) #\_ char)) 'string)) "MAXIMA")))
    (setf (gethash rspec-name *arg-check-err-code-table*) code)
    (if (consp spec-name)
        (mk-arg-check2 'arg spec-name err-mssg-spec-1 body)
      (mk-arg-check1 'arg spec-name err-mssg-spec-1 body))))

; took this out of the symbol making form above.
; (maxima::maybe-invert-string-case  ; looks downcase in maxima, upcase in lisp

(defun mk-opt-check (spec-name err-mssg-spec body)
  (unless (listp err-mssg-spec) (setf err-mssg-spec (list err-mssg-spec)))
  (if (listp spec-name)
;      (progn 
        (mk-arg-check2 'option spec-name (cons "Value '~a' for option '~a'" err-mssg-spec) body)
;      (progn 
        (mk-arg-check1 'option spec-name (cons "Value '~a' for option '~a'" err-mssg-spec) body)))

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

;; This is never called ?!
(defun signal-rest-arg-error (spec-name arg-list name call)
  (let* ((espec (gethash spec-name *arg-check-mssg-table*))
         (arg-list1 (list (format-rest-args (car arg-list))))
         (specl-str (not-comma-separated-english (cadr espec))))
    (maxima::merror1 (format nil "~a One of ~? is ~a in ~a." (err-prefix name) (car espec)
                            arg-list1 specl-str (format-call name call)))))

;; These are parameters, eg (1 3)  in (:int-range 1 3)
;; Ugh, this is all about removing quotes. And maybe it is not even a good
;; idea, because ($COS %COS) becomes (cos cos)
(defun format-arg-spec-params (fmt p)
  (apply #'format (append (list nil fmt)
                          (mapcar 
                           #'(lambda (x)
                               (if (and (listp x) (eq 'quote (car x)))
                                   (maxima::$sconcat (second x))
                                 (maxima::$sconcat x)))
                                          p))))

;; Yes, we might as well preserve the call and not have two of these.
;; NOTE: The call is now preserved. So the second form with (eq call nil)
;; is never used. We can get rid of the second form.
;; Generate two functions. These functions are called at runtime.
(defmacro mk-signal-error (name hash)
  "Prints one of two error messages depending on whether it is called
   in a defmfun1 expansion. If not, the list of call args is lost. But, I
   suppose we could preserve them in a lexical variable. Call this with
   call nil to get the second message."
  `(defun ,name (spec-name arg-list name call force-match match-val)
     (let ((spec-args (if (listp spec-name) (rest spec-name) nil))) ; s.a. (:int-range 1 3)
       (when (listp spec-name) (setf spec-name (car spec-name)))
       (let* ((espec (gethash spec-name ,hash))
              (arg-list1 (format-args arg-list))
              (specl-str (not-comma-separated-english (cadr espec)))
              (pre-name (err-prefix name))
              (spstr (if spec-args
                         (progn 
                           (format t "~a~%" spec-args)
                           (format-arg-spec-params specl-str spec-args))
                       specl-str))
              (call-str (format-call name call))
              (err-code (gethash spec-name *arg-check-err-code-table*)))
         (setf maxima::$error_code err-code) ; ought to pass to merror1, but same as putting it in two calls below
         (cond (call
                (error-or-message name (format nil "~a ~? is ~a in ~a.~%" pre-name (car espec)
                                               arg-list1 spstr  call-str) force-match match-val)
                nil)
               (t
                (maxima::merror1 (format nil "~a ~? is ~a." pre-name (car espec)  arg-list1 spstr))))))))

;;; signal-arg-error called by echeck-arg and check-and-error
(mk-signal-error signal-arg-error *arg-check-mssg-table*)
;;; signal-option-arg-error called by check-and-error-option
(mk-signal-error signal-option-arg-error *option-arg-check-mssg-table*)

(defun int-range-check (n1 n2)
  `(and (integerp e) (>= e ,n1) (<= e ,n2)))

(defun int-gt-check (n)
  `(and (integerp e) (> e ,n)))

(defun int-gte-check (n)
  `(and (integerp e) (>= e ,n)))

(defun arg-member-check (list)
  `(member e ,list :test #'equal))

;; two cases here: just a test, or test and conversion (preprocessing) of input data.
(dolist (one-check *arg-spec-definitions*)
  (when (eq :pp (car one-check))
            (setf one-check (cdr one-check))
            (setf (gethash (car one-check) *arg-check-preprocess-table*) t))
  (apply #'mk-arg-check one-check))

;; Careful! code for test is overwritten if already defined above. this should be fixed somehow.
;; What ?
(dolist (one-check *opt-spec-definitions*)
  (when (eq :pp (car one-check))
            (setf one-check (cdr one-check))
            (setf (gethash (car one-check) *opt-check-preprocess-table*) t))
  (apply #'mk-opt-check one-check))

(ddefparameter *arg-spec-keywords*
  (get-hash-keys *arg-check-func-table*)
  "This is a list of the valid keywords specifying argument checks for the lambda list
   of a defmfun1 function. It must be set after all of the tests are defined.
   During expansion of a defmfun1 form, elements in an argument spec in
   the lambda list are searched for in this list. The list of keywords is built
   simply by getting the hash keys from *arg-check-func-table* ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-default-options (name opts)
   "Set  possible option hash table from spec in defmfun1 param list.
  This hash table is only used to provide information to the user
  via foptions()."
    (let ((sname ($sconcat name)))
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

(defun expansion-error-fname (fname)
  (format nil "defmfun1: Error expanding function definition for ~a."
          (maxima::sym-to-string fname)))
;          (maxima::maybe-invert-string-case (format nil "~a" fname))))

(defun file-package-string ()
  (let ((sf (doc-system:get-source-file-name))
        (pk (doc-system:get-source-package)))
    (if sf
        (if pk
            (format nil "In source file ~a, package ~a" sf pk)
          (format nil "In source file ~a" sf))
      (if pk
          (format nil "In package ~a" pk)
        ""))))

(defun defmfun1-expand-error (errcode fname mssg)
  (maxima::merror1 errcode (format nil "~a ~a~%~a"
    (expansion-error-fname fname) mssg (file-package-string))))

(defun parse-args-err (errcode fname mssg arg)
  (defmfun1-expand-error errcode fname
    (format nil "Error in argument spec ~s.~%~a"
            (maxima::sym-to-string arg) mssg)))

(defun check-directives (name directives)
  (dolist (d directives)
    (when (not (member d '( :doc :no-nargs :fast-opt :match )))
      (defmfun1::defmfun1-expand-error 'maxima::$defmfun1_unknown_directive
        name (format nil "Unknown directive ~a." (maxima::sym-to-string d))))))

(maxima::ddefun parse-args (name arglist)
 "At macro expansion time of defmfun1, this function is called and
  returns four lists constructed from arglist.  The first list will be
  directly converted to the lambdalist argument spec for the
  defmfun (defmspec) definition. This list has only the test
  specs :list, etc. stripped out. The second list is the same, but for
  each argument spec, only the argname and the tests are retained (eg
  default values are removed). The third list contains the pre-processing specs.
  The fourth list contains the supplied-p args."
  (let ( (arglist1) (arg-specs) (pp-specs) (supplied-p-hash (make-hash-table)))
    (dolist (argt *arg-type-list*)
      (let ((argt1) (argt-spec) (pp-spec) )
        (dolist (arg (getf arglist argt)) ; arg is complete specification of a single argument
          (when (keyword-p (first arg))
            (parse-args-err 'maxima::$defmfun1_malformed_argspec name 
                            "First element is a keyword, variable name expected." arg))
;          (when (consp (first arg)) ; can't do this. gives false negatives.
;            (parse-args-err 'maxima::$defmfun1_malformed_argspec name 
;                            "First element is a list, variable name expected." arg))
          (let
              ((nospecs (remove-if #'(lambda(x) (or (member x *arg-spec-keywords*) (member x *pp-spec-types*)
                                                    (and (listp x) (keyword-p (first x))
                                                         (member (first x) *arg-spec-keywords*) )))   arg))
               (wspecs (cons (car arg) (remove-if #'(lambda(x) (not (or (member x *arg-spec-keywords*)
                                                                (and (listp x) (keyword-p (first x))
                                                                     (member (first x) *arg-spec-keywords*)))))  arg)))
               (wppspecs (cons (car arg) (remove-if #'(lambda(x) (not (member x *pp-spec-types*))) arg))))
            (let ((kres (find-if #'(lambda (e) (keyword-p e)) nospecs)))
              (when kres
                (parse-args-err 'maxima::$defmfun1_unknown_directive name 
                  (format nil "Found keyword ~s in unexpected position. Probably an unknown directive."
                          (maxima::sym-to-string kres)) nospecs)))
            (when (some (lambda (e)(and (listp e) (keyword-p (car e)))) nospecs)
              (parse-args-err 'maxima::$defmfun1_unknown_directive name 
                              "Found list beginning with keyword in unexpected position. Probably an unknown directive." nospecs))
            (push (if (length1p nospecs) nospecs (list (first nospecs) `(quote ,(second nospecs))) ) argt1) ; quote default values
            (when (length-eq nospecs 3) (setf (gethash (first nospecs) supplied-p-hash) (third nospecs)))
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

(defmacro rule (rule val)
 "For passing rules from lisp code."
  `(list '(maxima::|$Rule| maxima::simp) ',rule ,val))

;; At run time, filter parameters passed to a defmfun1 definition into option
;;  params and non-option params. Skipping searching through required arguments
;;  is done with the fast version. But this will cause an error if the user
;;  makes the error of omitting required arguments. 
(macrolet ((mk-coll (fname fast-flag)
                    (let ((arg-list (if fast-flag '(rarg nmin) '(rarg))))
                      `(defun ,fname ,arg-list
                         ,@(when fast-flag '((declare (fixnum nmin))))
                         (do* ((rarg1 rarg (cdr rarg1))
                               (a (car rarg) (car rarg1))
                               ,@(when fast-flag '((i 0 (1+ i))))
                               (opts)
                               (nonopts))
                             ((null rarg1) (list opts (nreverse nonopts)))
                           ,@(when fast-flag '((declare (fixnum i))))
                           (cond
                            ,@(when fast-flag `(((< i nmin) (push a nonopts))))
                            ((rule-expr-p a) (push (cdr a) opts))
                            ((listof-rule-expr-p a) (setf opts (collect-opt-args-lev-1 (cdr a) opts)))
                            (t (push a nonopts))))))))
  (mk-coll collect-opt-args-fast t)
  (mk-coll collect-opt-args-slow nil))

;; removed nreverse before returning opts, because
;; match-opt is the last opt, but we want it processed first.
;; (maxima::ddefun collect-opt-args-fast (rarg nmin)
;;   "At run time, filter parameters passed to a defmfun1 definition into option
;;   params and non-option params. Skipping searching through required arguments
;;   is done here, but it is probably not most efficient."
;;   (declare (fixnum nmin))
;;   (do* ((rarg1 rarg (cdr rarg1))
;;         (a (car rarg) (car rarg1))
;;         (i 0 (1+ i))
;;         (opts)
;;         (nonopts))
;;       ((null rarg1) (list opts (nreverse nonopts)))
;;     (declare (fixnum i))
;;     (cond
;;      ((< i nmin) (push a nonopts)) ; for a bit of efficiency
;;      ((rule-expr-p a) (push (cdr a) opts))
;;      ((listof-rule-expr-p a) (setf opts (collect-opt-args-lev-1 (cdr a) opts)))
;;      (t (push a nonopts)))))

;; (defun collect-opt-args-slow (rarg)
;;   "Same as collect-opt-args, but search all args for Rule.
;;  This may be slower, but catches errors when calling the
;;  defmfun1-defined function."
;;   (do* ((rarg1 rarg (cdr rarg1))
;;         (a (car rarg) (car rarg1))
;;         (i 0 (1+ i))
;;         (opts)
;;         (nonopts))
;;       ((null rarg1) (list opts (nreverse nonopts)))
;;     (declare (fixnum i))
;;     (cond
;; ;     ((< i nmin) (push a nonopts)) ; for a bit of efficiency
;;      ((rule-expr-p a) (push (cdr a) opts))
;;      ((listof-rule-expr-p a) (setf opts (collect-opt-args-lev-1 (cdr a) opts)))
;;      (t (push a nonopts)))))


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

;; I think 'zero' is better than 'no'.
;; 'no' might imply that arguments are optional. Also, we must
;; use 'zero' for 'zero or more'.
(defun err-itostr (i)
  (err-itostr0 i "~[zero~:;~:*~r~]"))
;  (err-itostr0 i "~[no~:;~:*~r~]"))
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

(defun narg-error-message (name restarg nargs nmin nmax restp force-match match-val)
  "Call merror1 with message about incorrect number of arguments."
  (declare (fixnum nargs nmin nmax))
  (setf nargs (the fixnum (+ nargs (length restarg))))
  (let* ((sname ($sconcat name))
         (str-narg (format nil "called with ~d argument~p" (err-itostr nargs) nargs))
         (str-expected
          (format-nargs-expected nmin nmax restp t))
         (err-code (compute-narg-error-code restarg nargs nmin nmax restp))); maybe restore this later; done!
    (error-or-message name (format nil "~a ~a ~a; ~a expected.~%" (err-prefix sname) sname str-narg str-expected) 
                      force-match match-val err-code)))
