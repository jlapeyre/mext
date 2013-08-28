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

(defun defmfun1-write-let-bindings (name nargs all-args supplied-p-hash rest)
  `((,nargs 0) ; count args passed when calling
   (defmfun1-func-name ',name) ; save this name for echeck-arg macro below. Wasteful as most funcs never use it.
   ,@(loop for n in all-args collect ; write default bindings for req and &optional
           (if (null (car n))
               (merror1 (intl:gettext "defmfun1: null argument name in definition of ~a.
 Did you omit a pair of parens?~%") name)
          (if (listp (car n)) `,(cons (cadar n) (cdr n)) (if (length1p n) `,(car n) `,n))))
   ,@(loop for n in (get-hash-keys supplied-p-hash) collect `,(gethash n supplied-p-hash))
   ,@(when rest (cdr rest)))) ; binding for &rest arg

(defun defmfun1-write-assignments (name args reqo restarg nargs supplied-p-hash reqo-spec pp-spec-h)
 "Write code to set required and &optional args to values supplied by call."
  `(tagbody 
     ,@(do* ((reqo1 reqo (cdr reqo1))
             (targ (caar reqo) (caar reqo1))
             (res))
            ((null reqo1)  (nreverse res))
            (push `(if (endp ,restarg) (go out)) res)
            (push `(incf ,nargs) res)
            (push `(setf ,targ (pop ,restarg)) res)
            (when (gethash targ supplied-p-hash) (push `(setf ,(gethash targ supplied-p-hash) t) res))
            (dolist (tst (gethash targ reqo-spec))
              (push (defmfun1::check-and-error tst targ name args) res))
            (dolist (pp (gethash targ pp-spec-h))
              (push `(setf ,targ (funcall ,(defmfun1::get-pp-func pp) ,targ)) res)))
   out))

(defun defmfun1-write-opt-assignments (name args opt-args opt supplied-p-hash reqo-spec)
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
                                              (defmfun1::check-and-error-option tst name opt-name  args)
                                              res))) res1))
                             (t (merror1 (intl:gettext "~a ~a does not accept the option `~a'.~%")
                                         (defmfun1::err-prefix ',name) ($sconcat ',name) ($sconcat var) ))))))))

(defun defmfun1-write-rest-assignments (name args rest reqo-spec)
  (if rest
      (let ((res)
            (rest-name (caadr rest)))
        (dolist (tst (gethash rest-name reqo-spec))
          (push `(mapc
                  #'(lambda (a)
                      ,(defmfun1::check-and-error tst 'a name args))
                  ,rest-name)  res))
        (nreverse res))
      nil))

;; NOTE: We moved set-default-options, save-lambda-list-etc, into the expansion part of
;; the macro. Thus, the documentation is generated, and is available, at run-time.
;; Another option would be to move it outside the backquote, so that it is generated at
;; compile-time, and save it somehow to disk. But that seems much more complicated, and I
;; can't see a benefit now. Time required to load does not seem to be affected at all.
(defmacro defmfun1 (name args &body body &aux directives)
  (when (listp name)
    (setf directives (cdr name))   (setf name (car name)))
  (dbind (arg-list arg-specs pp-specs supplied-p-hash) (defmfun1::group-and-parse-args name args)
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
      (loop while (and (listp (car body)) (eq 'declare (caar body))) do
           (push (car body) declare-form) (setf body (cdr body)))
       (when (defmfun1::are-some-args-held name) (setf defun-type 'defmspec))
       `(progn
          ,(when (member :doc directives)
                 `(max-doc::add-doc-entry1 :e 
                '( :name ,sname :protocol ,(defmfun1::format-protocol sname req optional rest)
                         :protocol-list ,(list sname req optional rest)
                         :contents ,(if doc-content doc-content
                                      (if doc-string (concatenate 'string "   " (first doc-string)) "")))))
          (defmfun1::set-default-options ',name ',opt) ; only for user, not used in macro or function body
          (defmfun1::save-lambda-list-etc ',name ',arg-specs)
          (defmfun1::save-preprocess-specs ',name ',pp-specs)
;          (defmfun1:record-mext-package ',name defmfun1::*mext-package*) ; move to add-doc-entry
          (,defun-type ,name ( ,@(if (eq defun-type 'defmspec) nil `(&rest)) ,args ,@aux) ; Here is the function definition
            ,@doc-string
            ,@(when (eq defun-type 'defmspec) `((setf ,args (cdr ,args))))
            (let* ,(defmfun1-write-let-bindings name nargs all-args supplied-p-hash rest)
              (declare (ignorable defmfun1-func-name))
              (declare (fixnum ,nargs))
              ,@declare-form ; moved out of body, because it must occur after parameter list
              (,@(if opt `(dbind (,opt-args ,restarg) (defmfun1::collect-opt-args ,args ,nreq))
                      `(let ((,restarg ,args))))
;;              (dbind ,(if opt `(,opt-args ,restarg) `(,restarg))
;;                ,(if opt `(defmfun1::collect-opt-args ,args ,nreq) `(list ,args)) ; filter options from other args                
                (,@(if (eq defun-type 'defmspec ) `(block ,name)  `(progn)) ; make a block for return-from
                   ,(defmfun1-write-assignments name args reqo restarg nargs supplied-p-hash reqo-spec pp-spec-h)
                   ,@(when rest `((setf ,(caadr rest) ,restarg))) ; remaining args go to &rest if it was specified
                   (when (< ,nargs ,nreq) ,(defmfun1::narg-error-or-message name args restarg nargs nreq nreqo rest))
                   ,@(when (null rest) `((if ,restarg ,(defmfun1::narg-error-or-message 
                                                       name args restarg nargs nreq nreqo rest))))
                   ,@(defmfun1-write-rest-assignments name args rest reqo-spec)
                   ,@(defmfun1-write-opt-assignments name args opt-args opt supplied-p-hash reqo-spec)
                   ,@body)))))))))



(defmacro dcheck-arg (spec-name arg)
" Check arg <arg> with <spec-name>. For use within the body of a function. It need not be
 a function defined with defmfun1."
  `(funcall ,(defmfun1::get-check-func spec-name) ,arg))

;; we need to write one of these has the function name supplied explicitly for
;; use outside of a defmfun1 function.
(ddefmacro echeck-arg (spec-name arg)
 "Check arg <arg> with <spec-name> and signal error with pretty message if check fails.
  For use within the body of a defmfun1 function."
  `(unless (funcall ,(defmfun1::get-check-func spec-name) ,arg)
     (defmfun1::signal-arg-error ',spec-name (list ,arg) defmfun1-func-name nil)))

(defun mk-defmfun1-form (name args body)
  "Helper function for defmfun1-opt."
  `(defmfun1 ,name ,args ,@body))

;; defmfun1-opt takes : name, list of opts, code to be inserted before body.
(defmacro defmfun1-opt (name opts &rest code)
  "Define a macro like defmfun1 that already has the options <opts> defined.
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
   (cons '(mlist simp)
         (when oh (let (ol)  ; (cadr v) to get rid of quote
                  (maphash (lambda (k v)
                             (push ($sconcat `((|$Rule| simp) ,(if (listp k) (car k) k) ,(cadr v))) ol)) oh)
                  ol)))))

(max-doc::set-cur-sec 'max-doc::attributes)

(defmfun1 ($attributes :doc) ((name :or-string-symbol) )
  :desc ("Returns a list of the `attributes' of function " :argdot "name")
 (maxima-symbol-to-string name)
 (let ((oh (gethash name defmfun1::*attributes-table*)))
   (cons '(mlist simp)
         (when oh (let (ol)  ; (cadr v) to get rid of quote
                  (maphash (lambda (k v)
                             (declare (ignore v))
                             (push k ol )) oh)
                  ol)))))

;; why do functions defined by this macro return false ?
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
      (defmfun1 (,(intern (concatenate 'string "$SET_" max-attribute)) :doc) ((names :or-string-symbol-or-listof :ensure-list))
        :desc ,set-doc-str
        (loop for name in names do
              (maxima-symbol-to-string name)
              (,(find-symbol (concatenate 'string "SET-" attribute) 'defmfun1) name)))
      (defmfun1 (,(intern (concatenate 'string "$UNSET_" max-attribute)) :doc) ((names :or-string-symbol-or-listof :ensure-list))
        :desc ,unset-doc-str
        (loop for name in names do
              (maxima-symbol-to-string name)
              (,(find-symbol (concatenate 'string "UNSET-" attribute) 'defmfun1) name))))))
      
(mk-maxima-attribute match_form match-form "If the argument checks for a function call fail,
 and the attribute `match_form' is set, then rather than signaling an error, the unevaluated form
 is returned. Furthemore, if the attribute `nowarn' is not set, then a warning message is printed.
 Currently, only automatic argument checks generated from the defmfun1 protocol are controlled.
 Argument checks and errors written within the body of the functions occur regardless of function attributes.")

(mk-maxima-attribute nowarn nowarn "If the argument checks for a function call fail,
 and the attribute `match_form' is set, and the attribute `nowarn' is set, then rather than signaling an error, the unevaluated form
 is returned and no warning message is printed.")

;; we can get rid  of these
;; (defmfun1 ($set_match_form :doc) ((names :or-string-symbol-or-listof :ensure-list))
;;   "Set 'match_form' attribute for function with symbol <name>. This
;;    prints a warning message if the argument check fails, but returns the unevaluated form."
;;   (loop for name in names do
;;         (maxima-symbol-to-string name)
;;         (defmfun1::set-match-form name)))

;; (defmfun1 ($unset_match_form :doc) ((names :or-string-symbol-or-listof :ensure-list ))
;;   "Unset 'match_form' attribute so that failed arg checks signal an error. This
;;    attribute can be changed at runtime."
;;   (loop for name in names do
;;         (maxima-symbol-to-string name)
;;         (defmfun1::unset-match-form name)))

(max-doc::see-also-group '( "unset_match_form" "set_match_form" "set_nowarn" "unset_nowarn" "attributes"))

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
