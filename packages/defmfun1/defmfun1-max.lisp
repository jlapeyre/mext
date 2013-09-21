;;;  defmfun1 is a function definition macro for relatively high-level maxima
;;;  functions.
;;;  Copyright (C) (2012) John Lapeyre. Licensed under GPL, v3 or greater. See the file
;;;  `LICENSE' in this directory.

(in-package :maxima)
(use-package :gjl.lisp-util)
(mext:mext-optimize)

(max-doc:set-cur-sec 'max-doc::misc-fandv)

(doc-system:set-source-file-name "defmfun1-max.lisp")
(doc-system:set-source-package "maxima")
(defmfun1:set-file-and-package "defmfun1-max.lisp" "defmfun1")

;;; "Define a function to be called from Maxima. Expands into a `defmfun' or `defmspec' macro.
;;;  Provides:
;;;  * optional argument (keyword) support at the maxima level.
;;;  * automated argument checking.
;;;  * more detailed, consistent error messages.
;;;  `defmfun1' takes a lambda list that is very similar to `defun'.
;;;  `&optional' `&rest' `&aux' are more or less the same. `&req' can be given
;;;  for required arguments. These keywords can each occur mulitiple times
;;;  in the lambda list."

;(format t ">>args ~s~%>>argl ~s~%>>specl ~s~%" args arg-list arg-specs)
; (format t ">>args ~s~%>>argl ~s~%>>specl ~s~%" args arg-list arg-specs)
;(format t  ">> grouped args ~s~%" (defmfun1::group-args args))
; Could have used destructuring bind for parts of the parameter specification. I can't
; recall why I didn't.

;; Here we write the bindings in the main `let*' wrapping the body of the function
;; defmfun1-func-name and defmfun1-func-name are available to use in the body of the function.
;; defmfun1-func-name is the name of the function
;; defmfun1-func-call is the noun form of the function call
;; defmfun1-func-call-args is the same, but just args
;; For efficiency, these should perhaps only be optionally saved, if requested
;; !!! yes only if neccessary
;; Or better make a macro that only writes them if needed.
;; Also, we don't need all three of these. It could be rewritten.
;;
;; name -- the function name (symbol)
;; nargs -- a symbol (via gensym) that will count the number of non-option arguments
;; args -- a symbol (via gensym) that contains a list of *all* args(parameters) passed at run time
;; all-args -- a list of all argument names for this defmfun1 function (except &rest args)
;; supplied-p-hash -- 
;; rest -- a list containing the name of the rest arg (or nil)
;; count-args -- a flag specifying whether the function is to verify the number of args.
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun defmfun1-write-let-bindings (name nargs args all-args supplied-p-hash rest count-args)
  `(,@(when count-args `((,nargs 0))) ; count args passed when calling
   (defmfun1-func-name ',name) ; save this name for echeck-arg macro below. Wasteful as most funcs never use it.
   (defmfun1-func-call (cons (list ',name) ,args)) ; the input form
   (defmfun1-func-call-args ,args)
;   (defmfun1-opts) don't need these yet, but maybe later
   ,@(loop for n in all-args collect ; write default bindings for req and &optional
           (if (null (car n))
               (merror1 '$defmfun1_null_arg_name (intl:gettext "defmfun1: null argument name in definition of ~a.
 Did you omit a pair of parens?~%") name)
          (if (listp (car n)) `,(cons (cadar n) (cdr n)) (if (length1p n) `,(car n) `,n))))
   ,@(loop for n in (get-hash-keys supplied-p-hash) collect `,(gethash n supplied-p-hash))
   ,@(when rest (cdr rest))))) ; binding for &rest arg

;; Threading, or distributing, over lists of arguments. This is similar
;; to stock maxima distributing over bags. But presently, stock maxima
;; either threads over all or none of the arguments. Here, we allow
;; threading over only specified arguments, which is useful. We
;; also allow threading over aex (vector) representation.
;; We could look at efficiency:
;; for instance return before we enter the block (defmspec, defmfun)
;; IMPORTANT: functions using thread like this cannot put opts before
;; the threading args. If we want to allow this, we need to walk through
;; and look for Rule an skip over them. We could make this routine respect
;; fast-opt and slow-opt.
;;
;; The present setup calls the function repeatedly, parses all args and opts
;; every time (after all thread loops are started.) For functions that take
;; a long time to execute compared to calling, this is ok.
;; It would be good to have another directive that loops over argments within
;; the first call. this is more efficient, but restricts what the writer of the
;; function can do. i.e. the body may not be in the same state each time through
;; the loop, which is probably not desireable.
;; After checking threading with string_reverse, I see that threading is
;; very fast, so I won't be implementing another scheme soon.
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun defmfun1-write-threading (name args arg-directives)
  (let ((arg-d (append (getf arg-directives :req ) (getf arg-directives :optional )))
                       (thread-forms '()) (i 0) )
    (dolist (req-arg-d arg-d)
      (when (member :thread req-arg-d)
        (push `(let ((ith-arg (nth ,i ,args)))
                 (when ($listp ith-arg)
                   (let ((newargs (copy-list ,args)))
                     (return-from ,name 
                       (cons '(mlist) (loop :for ith-arg-1 :in (cdr ith-arg) :collect
                         (progn                                            
                           (setf (nth ,i newargs) ith-arg-1)                                    
                           (apply ',name newargs)))))))
                 (when (and (aex-p ith-arg) (eq (car (aex-head ith-arg)) 'mlist))
                   (let* ((newargs (copy-list ,args))
                          (n (aex-length ith-arg))
                          (retvec (aex-make-n-head n))
                          (retarr (aex-arr retvec))
                          (argarr (aex-arr ith-arg)))
                     (dotimes (j n)
                       (setf (nth ,i newargs) (aref argarr j))
                       (setf (aref retarr j) (apply ',name newargs)))
                     (return-from ,name retvec))))
              thread-forms))
      (incf i))
    (nreverse thread-forms))))

;; name -- name of function (a symbol)
;; args -- a symbol (via gensym) that contains a list of *all* args(parameters) passed at run time
;; reqo -- a list of the names of each of the required and optional arguments (each wrapped in a list!)
;; nargs -- a symbol (via gensym) that will count the number of non-option arguments
;; supplied-p-hash -- hash
;; reqo-spec -- hash
;; pp-spec -- hash
;; have-match -- flag for :match specified with function name in defmfun1 definition.
;; count-args -- a flag specifying whether the function is to verify the number of args.
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun defmfun1-write-assignments (name args reqo restarg nargs 
                                        supplied-p-hash reqo-spec pp-spec-h
                                        have-match count-args)
 "Write code to set required and &optional args to values supplied by call."
  `(tagbody
     ,@(do* ((reqo1 reqo (cdr reqo1))
             (targ (caar reqo) (caar reqo1))
             (res))
            ((null reqo1)  (nreverse res))
            (push `(if (endp ,restarg) (go out)) res)
            (when count-args (push `(incf ,nargs) res))
            (push `(setf ,targ (pop ,restarg)) res)
            (when (gethash targ supplied-p-hash) (push `(setf ,(gethash targ supplied-p-hash) t) res))
            (dolist (tst (gethash targ reqo-spec))
              (push (defmfun1::check-and-error tst targ name args nargs have-match) res))
            (dolist (pp (gethash targ pp-spec-h))
               (push `(setf ,targ (funcall ,(defmfun1::get-pp-func pp) ,targ)) res)))
   out)))

;; name -- name of function (a symbol)
;; args -- a symbol (via gensym) that contains a list of *all* args(parameters) passed at run time
;; opt-args -- a symbol ... list of opt-args passed at run time
;;   each element is a dyad: (optname val) 
;; opt -- a list of option names (symbols) each wrapped in a list
;; supplied-p-hash -- hash
;; reqo-spec -- hash
;; have-match -- flag for :match specified with function name in defmfun1 definition.
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun defmfun1-write-opt-assignments (name args opt-args opt supplied-p-hash reqo-spec have-match)
  "Write code to set option variables to supplied values."
  (when opt `((dolist (ospec ,opt-args) 
                (dbind (var val) ospec
                       (cond ,@(do* ( (optl opt (cdr optl))
                                      (topt (car opt) (car optl))
                                      (optns  (car topt) (car topt))
                                      (opt-name)(opt-var)
                                      (res1))
                                    ((null optl) (nreverse res1))
                                    (setf opt-name (if (listp optns) (car optns) optns))
                                    (setf opt-var  (if (listp optns) (cadr optns) optns))
                                    (push
                                     `((eq ',opt-name var)
                                       (setf ,opt-var  val)
                                       ,@(when (gethash optns supplied-p-hash)
                                             `((setf ,(gethash optns supplied-p-hash) t)))
                                       ,@(do ((tst (gethash opt-name reqo-spec) (cdr tst))
                                              (res))
                                             ((null tst) (nreverse res))
                                             (push
                                              (defmfun1::check-and-error-option tst name opt-name opt-var args have-match)
                                              res))) res1))
                             (t
                              (defmfun1-error-return '$defmfun1_invalid_opt_name ,name
                                (format nil (intl:gettext "~a does not accept the option `~a'")
                                        ($sconcat ',name) ($sconcat var)) ,have-match)))))))))
;                              (merror1 '$defmfun1_invalid_opt_name (intl:gettext "~a ~a does not accept the option `~a'.~%")
;                                         (defmfun1::err-prefix ',name) ($sconcat ',name) ($sconcat var) ))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun defmfun1-write-rest-assignments (name args rest reqo-spec nargs have-match)
  (if rest
      (let ((res)
            (rest-name (caadr rest)))
        (dolist (tst (gethash rest-name reqo-spec))
          (push `(mapc
                  #'(lambda (a) (incf ,nargs)
                      ,(defmfun1::check-and-error tst 'a name args nargs have-match))
                  ,rest-name)  res))
        (nreverse res))
      nil)))

;; NOTE: We moved set-default-options, save-lambda-list-etc, into the expansion part of
;; the macro. Thus, the documentation is generated, and is available, at run-time.
;; Another option would be to move it outside the backquote, so that it is generated at
;; compile-time, and save it somehow to disk. But that seems much more complicated, and I
;; can't see a benefit now. Time required to load does not seem to be affected at all.
(defmacro defmfun1 (name args &body body &aux directives have-match count-args)
  (when (listp name) 
    (setf directives (cdr name)) (setf name (car name)))
  (when (or (not (symbolp name)) (null name))
    (defmfun1::defmfun1-expand-error 'maxima::$defmfun1_name_not_symbol
      name "The first argument, the function name, is not a non-null symbol."))
  (let ((d1 (car body))) ; these checks are getting long. should move to function.
    (when (and (keyword-p d1) (not (eq :desc d1)))
      (defmfun1::defmfun1-expand-error 'maxima::$defmfun1_unknown_body_directive
      name (format nil (sconcat "The first form in the body ~a is a keyword, but is not `:desc'.~%"
            "This check can be removed if it is legal/useful for the body to begin with a keyword.")
                   (sym-to-string d1)))))
  (when (not (listp args))
    (defmfun1::defmfun1-expand-error 'maxima::$defmfun1_missing_arg_list
      name "No argument list found."))
  (defmfun1::check-directives name directives)
  (when (member :match directives)
    (setf have-match t)
    (setf args (append args `(&opt (($match match-opt) nil match-supplied-p) ))))
  (when (not (member :no-nargs directives)) (setf count-args t))
  (dbind (arg-list arg-specs pp-specs arg-directives supplied-p-hash) (defmfun1::group-and-parse-args name args)
   (dbind (req optional aux rest opt) (defmfun1::rem-keys-arg-list arg-list)
    (let* ((args (gensym "args-"))
           (restarg (gensym "restarg-")) ; Initialize to non-option arguments,
                                         ; then pop values when assigned. remaining vals are &rest
           (opt-args (gensym "opt-args-")) ; Option arguments filtered from args
           (nargs (gensym "nargs-")) ; Count non-option arguments as they are assigned
           (reqo (append req (cdr optional))) ; required and optional args
           (reqo-spec (defmfun1::make-arg-spec-hash (append (getf arg-specs :req) (getf arg-specs :optional)
                                                            (getf arg-specs :opt)
                                                            (getf arg-specs :rest)))) ; fix this. its ugly
           (pp-spec-h (defmfun1::make-arg-spec-hash (append (getf pp-specs :req) (getf pp-specs :optional)
                                                            (getf pp-specs :opt)))) ; fix this. its ugly
           (nreq (length req)) ; minimum number of non-option args.
           (nreqo (length reqo)) ; maximum nuumber of non-option args. (required plus optional)
           (all-args (append req (cdr optional) opt)) ; all args except &rest
           (sname ($sconcat name))  (declare-form nil)  (doc-string nil) (doc-content nil) (defun-type 'defmfun))
      (declare (fixnum nreq nreqo))
      (when (stringp (car body))
         (setf doc-string (list (car body))) (setf body (cdr body)))
      (when (eq :desc (car body))
        (setf doc-content (second body)) (setf body (cddr body)))
      (loop :while (and (listp (car body)) (eq 'declare (caar body))) :do
           (push (car body) declare-form) (setf body (cdr body)))
       (when (defmfun1::are-some-args-held name) (setf defun-type 'defmspec))
       `(progn
          ,(when (member :doc directives)
                 `(max-doc::add-doc-entry1 :e 
                '( :name ,sname :protocol ,(defmfun1::format-protocol sname req optional rest)
                         :protocol-list ,(list sname req optional rest)
                         :contents ,(if doc-content doc-content
                                      (if doc-string (sconcat "   " (first doc-string)) "")))))
          (defmfun1::set-default-options ',name ',opt) ; only for user, not used in macro or function body
          (defmfun1::save-lambda-list-etc ',name ',arg-specs)
          (defmfun1::save-preprocess-specs ',name ',pp-specs)
          (defmfun1::save-arg-directives ',name ',arg-directives)
;          (defmfun1:record-mext-package ',name defmfun1::*mext-package*) ; move to add-doc-entry
          ; Here is the function definition.
          (,defun-type ,name ( ,@(if (eq defun-type 'defmspec) nil `(&rest)) ,args ,@aux) 
            ,@doc-string
            ,@(when (eq defun-type 'defmspec) `((setf ,args (cdr ,args))))
            (let* ,(defmfun1-write-let-bindings name nargs args all-args supplied-p-hash rest count-args)
              (declare (ignorable defmfun1-func-name defmfun1-func-call defmfun1-func-call-args ))
              ,@(when count-args `((declare (fixnum ,nargs))))
              ,@declare-form ; moved out of body, because it must occur after parameter list
              (,@(if opt `(dbind (,opt-args ,restarg) 
                                 ,(if (member :fast-opt directives)
                                      `(defmfun1::collect-opt-args-fast ,args ,nreq)
                                      `(defmfun1::collect-opt-args-slow ,args)))
                      `(let ((,restarg ,args)))) ; filter options from other args
               (,@(if (eq defun-type 'defmspec ) `(block ,name)  `(progn)) ; make a block for return-from
                ,@(defmfun1-write-threading name args arg-directives)
        ; write-opt-assignments first so have-match in effect during write-assignments
        ;        (setf defmfun1-opts ,opt-args) these have lost the Rule, don't need them yet
                ,@(defmfun1-write-opt-assignments name args opt-args opt supplied-p-hash reqo-spec have-match)
                ,(defmfun1-write-assignments name args reqo restarg nargs ; assign required args
                   supplied-p-hash reqo-spec pp-spec-h have-match count-args)
                ,@(when rest `((setf ,(caadr rest) ,restarg))) ; remaining args go to &rest if it was specified
                ,(when count-args
                   `(when (< ,nargs ,nreq) ,(defmfun1::narg-error-or-message name args restarg nargs nreq nreqo rest have-match)))
                ,@(when (and (null rest) count-args)
                    `((if ,restarg ,(defmfun1::narg-error-or-message 
                                      name args restarg nargs nreq nreqo rest have-match))))
                ,@(defmfun1-write-rest-assignments name args rest reqo-spec nargs have-match)
                ,@body)))))))))

;; Not using this
;; (defmacro dcheck-arg (spec-name arg)
;; " Check arg <arg> with <spec-name>. For use within the body of a function. It need not be
;;  a function defined with defmfun1."
;;   `(funcall ,(defmfun1::get-check-func spec-name) ,arg))

;; We need to write one of these has the function name supplied explicitly for
;; use outside of a defmfun1 function.
;; return-from does not work here because the block name is not evaluated.
;; I can't think of a way to make this work without parsing the body of the
;; function defined via defmfun1 and rewriting code. As of yet, I have not
;; done any of this. A workaround is to write the return-from by hand
;; in each function body.
;; Hmmm, maybe a macrolet would work, but that is a lot of trouble.
#|
(ddefmacro echeck-arg-old (spec-name arg)
 "Check arg <arg> with <spec-name> and signal error with pretty message if check fails.
  For use within the body of a defmfun1 function."
  `(unless (funcall ,(defmfun1::get-check-func spec-name) ,arg)
;     (defmfun1::signal-arg-error ',spec-name (list ,arg) defmfun1-func-name nil)
     (progn
       (defmfun1::signal-arg-error ',spec-name (list ,arg) defmfun1-func-name defmfun1-func-call-args)
       t))) ; return true if no error was signaled.
;;     (return-from defmfun1-func-name defmfun1-func-call)))
|#

;; Prefer this macro now, as it allows returning the noun form.
;; Use within a defmfun1 body like this:
;; (echeck-arg $funcname :testname var)
;; This checks the variable `var' with the test :testname
;; The $funcname is supplied for return-from, which needs a
;; lexically scoped name. This makes defmfun1-func-name superfluous, so we should
;; get rid of it.

;; The following three macros take an argument have-match. It must
;; only be true when they are called from within a defmfun1 function
;; that had the directive :match. Otherwise the variables referenced
;; will not be bound. If have-match is true then the match_form attribute
;; may be overridden.

;; In signal-arg-error below, we put arg `nil', which represents
;; that we don't know the argument position. check-and-error sends nargs
;; instead of nil
(ddefmacro echeck-arg (fname test arg &optional (have-match nil))
 "check arg <arg> with <test> and signal error with pretty message if check fails.
  for use within the body of a defmfun1 function."
 (let* ((fc `(funcall ,(defmfun1::get-check-func test) ,arg))
        (force-match-code (defmfun1::write-force-match-code have-match))
        (sa1 `(defmfun1::signal-arg-error ',test (list ,arg)
                defmfun1-func-name defmfun1-func-call-args nil ,@force-match-code))
        (sa `(,sa1 (return-from ,fname defmfun1-func-call))))
   (if (gethash test defmfun1::*arg-check-preprocess-table*)
       `(let ((res ,fc))
          (if (not (first res))
              (progn ,@sa)
           (setf ,arg (second res))))
     `(unless ,fc ,@sa))))

; old version without ability to 'preprocess'
; (ddefmacro echeck-arg (fname test arg &optional have-match)
;  "check arg <arg> with <test> and signal error with pretty message if check fails.
;   for use within the body of a defmfun1 function."
;   `(unless (funcall ,(defmfun1::get-check-func test) ,arg)
;      (progn
;        (defmfun1::signal-arg-error ',test (list ,arg) defmfun1-func-name defmfun1-func-call-args
;          ,@(defmfun1::write-force-match-code have-match))
;        (return-from ,fname defmfun1-func-call))))

; following two macros for use inside defmfun1-defined funcs
; first writes a form at an exit point.
; second return-from's  the block
; if have-match is true (I suggest using :match for value), then
; we respect runtime opt match->true or false
(defmacro defmfun1-error-final (err-code mssg &optional have-match)
 "used at an exit point of a defmfun1 body. does not call return-from"
  `(progn (defmfun1::error-or-message defmfun1-func-name 
            (format nil "~a: ~a; in ~a" ($sconcat defmfun1-func-name) ,mssg
              ($sconcat defmfun1-func-call)) ,@(defmfun1::write-force-match-code have-match)
              ,err-code)
          defmfun1-func-call))

(defmacro defmfun1-error-return (err-code funcname mssg &optional have-match)
 "used to return from defmfun1 body with error message and return-from"
  `(progn (defmfun1::error-or-message defmfun1-func-name 
            (format nil "~a: ~a; in ~a" ($sconcat defmfun1-func-name) ,mssg
                    ($sconcat defmfun1-func-call))
            ,@(defmfun1::write-force-match-code have-match) ,err-code)
          (return-from ,funcname defmfun1-func-call)))

(defun mk-defmfun1-form (name args body)
  "Helper function for defmfun1-opt."
  `(defmfun1 ,name ,args ,@body))

;; defmfun1-opt takes : name, list of opts, code to be inserted before body.
;; For an example of how  to use, see the top of aex/aex-core.lisp
(ddefmacro defmfun1-opt (name opts &rest code)
  "Define a macro based on defmfun1 that already has the options <opts> defined.
   <name> is the name of the macro. <opts> is a list of &opt specifications for defmfun1.
   <code> is forms to be inserted before the body when expanding calls to macro <name>.
   Notice we check for a doc string in body, and move the <code> to the other side"
  `(defmacro ,name (name1 args &body body)
     ,@(if code
           `((if (stringp (first body))
                 (setf body (cons (car body) (append ',code (rest body))))
               (if (eq :desc (first body))
                   (setf body (cons (car body) (cons (second body) (append ',code (cddr body)))))
                 (setf body (append ',code body)))))
         nil)
     (progn
       (setf args (append args
                          (if (not (member '&opt args)) (cons '&opt ',opts)
                            ',opts)))
       (mk-defmfun1-form name1 args body))))

(max-doc::set-cur-sec 'max-doc::options)

(defmfun1 ($foptions :doc) ( (name :or-string-symbol) )
 :desc ("Return a list of allowed options to " :code "defmfun1"
  " function " :argdot "name"
  " I would prefer to call this " :code "options" ", but that name is taken
 by an unused, undocumented function.")
 (maxima-symbol-to-string name)
 (let ((oh (gethash name defmfun1::*option-table*)))
   (mk-mlist
         (when oh (let (ol)  ; (cadr v) to get rid of quote
                  (maphash (lambda (k v)
                             (push ($sconcat `((|$Rule| simp) ,(if (listp k) (car k) k) ,(cadr v))) ol)) oh)
                  ol)))))

(max-doc::set-cur-sec 'max-doc::attributes)

(defmfun1 ($attributes :doc) ((name :or-string-symbol) )
  :desc ("Returns a list of the `attributes' of function " :argdot "name")
  (mk-mlist (defmfun1::get-attributes name)))

(defmfun1 ($attributes_find :doc) ( &optional (attribute :or-string-symbol))
  :desc (
 "Return a list of all functions for which the attribute "
 :arg "attribute" " is set."
 " Some attributes are " :varcomma "match_form" :varcomma "hold_all"
 " and " :vardot "nowarn" )
  (mk-mlist (sort (defmfun1::get-funcs-with-attribute attribute) #'string-lessp)))

(defmacro mk-maxima-attribute (max-attribute attribute doc-string)
 (when (symbolp attribute) (setf attribute (symbol-name attribute)))
 (setf attribute (string-upcase attribute))
 (when (symbolp max-attribute) (setf max-attribute (symbol-name max-attribute)))
 (setf max-attribute (string-upcase max-attribute))
 (let ((set-doc-str 
         (list (format nil "Set the `~a' attribute for function(s) " (string-downcase max-attribute))
               :arg "names" ". " doc-string))
       (unset-doc-str 
        (list (format nil "Unset the `~a' attribute for function(s) " (string-downcase max-attribute))
              :arg "names" ". " doc-string)))
   `(progn 
      (defmfun1 (,(intern (sconcat "$SET_" max-attribute)) :doc) ((names :or-string-symbol-or-listof :ensure-list))
        :desc ,set-doc-str
        (loop :for name :in names :do
              (maxima-symbol-to-string name)
              (,(find-symbol (sconcat "SET-" attribute) 'defmfun1) name))
        '$done)
      (defmfun1 (,(intern (sconcat "$UNSET_" max-attribute)) :doc) ((names :or-string-symbol-or-listof :ensure-list))
        :desc ,unset-doc-str
        (loop :for name :in names :do
              (maxima-symbol-to-string name)
              (,(find-symbol (sconcat "UNSET-" attribute) 'defmfun1) name))
        '$done))))
      
(mk-maxima-attribute match_form match-form "If the argument checks for a function call fail,
 and the attribute `match_form' is set, then rather than signaling an error, the unevaluated form
 is returned. Furthemore, if the attribute `nowarn' is not set, then a warning message is printed.
 Currently, only automatic argument checks generated from the defmfun1 protocol are controlled.
 Argument checks and errors written within the body of the functions occur regardless of function attributes.")

(mk-maxima-attribute nowarn nowarn "If the argument checks for a function call fail,
 and the attribute `match_form' is set, and the attribute `nowarn' is set, then rather than signaling an error, the unevaluated form
 is returned and no warning message is printed.")

(max-doc::see-also-group '( "unset_match_form" "set_match_form" "set_nowarn" "unset_nowarn" "attributes"))

(max-doc:add-doc-entry '(:name "hold_all" :type "Attribute"
    :contents 
    ("A function with the attribute " :code "hold_all"
     " evaluates none of its arguments.")))

;; copied from translation of maxima code
;; Use of Rule is just to be compatible with mixima.
;; But, we could change this.
;;infix("->")$
;;"->"(a,b) ::= buildq([a:a,b:b],Rule('a,b));
;; Note, this macro is deleted by kill(all), but it does not seem to affect
;; anything
(meval '(($infix simp) "->"))
(meval '((MDEFMACRO SIMP) (($->) $A $B)
 (($BUILDQ) ((MLIST) ((MSETQ) $A $A) ((MSETQ) $B $B))
  ((|$Rule|) ((MQUOTE) $A) $B))))

;; prevent operator "->" from being clobbered by kill(all)
(push "->" *mopl*)

(mext::no-warning
(ddefun rule-opt (opt-name val)
 "Make an option specification as a Rule. For use when calling from lisp code.
  ***!! Note. We need to learn how to set the precedence."
 `((|$Rule| simp) ,opt-name ,val)))

(defprop |$Rule| msize-infix grind)
(defprop |$Rule| (#\- #\>) strsym)
