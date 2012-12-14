(in-package :examples)
(mext:mext-optimize)

(doc-system:set-source-file-name "aex-examples.lisp")
(doc-system:set-source-package "aex-maxima")
(defmfun1:set-mext-package "defmfun1")

;;; Database of examples for functions. Add an example for a function with (add-example "funcname" ...)
;;;  format-examples returns a string "Examples: ...." with input output pairs of examples (all as one string)
;;; The output is computed each time format-examples is called. (could add other routines to do it differently.
;;; There is a line in max-doc::format-doc-entry that calls format-examples.
;;; See example entries for maxima function "first" at the bottom of this file.

;;; This differs from the maxima examples() facility
;;;  1. the examples are not extracted from a texinfo file
;;;  2. they can be added with lisp code from anywhere.
;;;  3. a maxima interface for adding examples is easy to write.
;;;  4. explanatory text pretext and posttext is available.
;;;  5. format-examples prints text strings, but does not set the line number variables
;;;  6. current maxima example() implemetation does not protect or rebind symbols used in the examples.

(defstruct (example)
  (pretext "") ; can be string or list of maxdoc formatted text. print this before the input output pair
  (posttext "" :type string)  ; print this after the input output pair. Maybe we won't use this.
  (vars "[]" :type string)    ; list of vars that will go in "block( vars, code )" to keep example symbols separate from session.
  (code '())  ; string or list strings of code lines for example. each will be wrapped in block with vars, but block not printed.
  (code-res '()) ; code and results in a list of length two (or list of these). Code is not evaluated in this case.
  (clean-code "" :type string)) ; cleanup code. not used now.

;; Note that :vars "[f]" does not make a local function. I don't know how to do this

(defvar *examples-hash* (make-hash-table :test 'equal))

(defun format-one-input (e n)
  (let* ((len (length e)) (lastchar (subseq e (1- len) len)))
    (if (or (equal lastchar ";") (equal lastchar "$"))
        (format nil "(%i~d) ~a~%" n e)
      (format nil "(%i~d) ~a;~%" n e))))

(defun format-one-output (e n)
  (format nil "(%o~d) ~a~%" n e))

(defun eval-one-input (s vars)
  "wrap code string in block with vars to make symbols local."
  (let ((bs (format nil "block( ~a , ~a);" vars s)))
    (mfuncall 'maxima::$eval_string bs)))

(defun eval-and-string-one-input (s vars)
  ($sconcat (eval-one-input s vars)))

(defun format-input-output (s vars n)
  (concatenate 'string (format-one-input s n)
               (format-one-output (eval-and-string-one-input s vars) n)))

;; evaluate all input, but only print the last one
(defun format-input-output-trick (all-input one-input vars n)
  (concatenate 'string (format-one-input one-input n)
               (format-one-output (eval-and-string-one-input all-input vars) n)))

(defun format-input-output-dead (code-res n)
  (concatenate 'string (format-one-input (first code-res) n)
               (when (second code-res) (format-one-output (second code-res) n))))

;; we can dump this soon. keep just in case.
(defun old-format-input-output-list (es vars)
  (when (stringp es) (setf es (list es)))
  (format nil "~{~A~}"
          (loop for e in es for i from 1 to (length es) collect
               (format-input-output e vars i))))

;; evaluate code with vars in block and format results
(defun format-input-output-list (es vars)
  (when (stringp es) (setf es (list es)))
  (let* ( (cum-e (first es)) (es (cdr es)))
    (format nil "~{~A~}"
            (cons (format-input-output cum-e vars 1)
                  (loop for e in es for i from 1 to (length es) collect
                       (progn (setf cum-e (concatenate 'string cum-e "," e))
                              (format-input-output-trick cum-e e vars i)))))))

;; format code and results without any evaluation
(defun format-input-output-list-dead (ecode)
  (when (stringp (first ecode)) (setf ecode (list ecode)))
  (format nil "~{~A~}"
          (loop for ec in ecode for i from 1 to (length ecode) collect
               (format-input-output-dead ec i))))

(defmacro form-ent (slot &body body)
  `(let ((x (,slot e)))
     (if (and x (or (stringp x) (listp x)) (> (length x) 0))
       (format nil ,@body)  "")))

(maxima::ddefun format-example (e)
 " Format a single example. The example can be 'live', that
 is, it is executed when it is printed, or it is dead, that
 is, both the input and output are specified as strings.
 The example is specified by a struct. If the slot :vars is
 present, then the specified variables are made local in a
 block when the example is evaluated.  If the slot :code is
 present, it must consist of a string or list of strings
 which are interpreted as maxima input. Each input string is
 evaluated and the input and output are formatted to mimic
 typing at the maxima command line. In order to use local
 values of the variables specified in :vars, the following
 trick is used if the input is a list of strings. The first
 input is evaluated in a block with the local variables and
 the input and output are printed. Then the first two inputs
 are evaluated inside a block with the same local vars. Only
 the second input is printed, along with the output. Then
 the first three inputs are evaluated in a block, etc. For
 many examples specifications, this transparantly mimics
 typing them at the command line, with no localization of
 variables.
 
 An alternative is to use a single input string with
 commands separated by commas. Another alternative is to
 use :code-res, rather than :code. In the later
 case :code-res is a list of two strings specifiying input
 and output strings, or a list of these lists. No evaluation
 is done, the strings are simply formatted."
  (unless (example-p e)
    (merror (intl:gettext "max-doc::format-example: Not an example ~a") e))
  (format nil "~a" (concatenate 'string 
                    (form-ent example-pretext "~%~a~%" 
                              (wrap-text :text (max-doc::format-doc-text x) :width 80 :indent 3 ))
                                (if (example-code-res e)
                                    (format-input-output-list-dead (example-code-res e))
                                  (format-input-output-list (example-code e) (example-vars e)))
                                (form-ent example-posttext "~a~%" x))))

(maxima::ddefun format-examples (name)
  "Format as a string the list of examples for topic 'name'."
  (let* ((el (gethash name *examples-hash*)))
    (if el (concatenate 'string 
                        (format nil "~%Examples:~%~{~A~}~%"
                                (loop for e in (reverse el) collect (format nil (format-example e)))))
        "")))

(defun format-example-latex (e)
  (unless (example-p e)
    (merror (intl:gettext "max-doc::format-example: Not an example ~a") e))
  (format nil "~a" (concatenate 'string 
        (if (example-pretext e)
            (form-ent example-pretext "~%~a~%"  ; "~%\\end{Verbatim}~%~a~%\\begin{Verbatim}[frame=single]~%"
                      (wrap-text :text (max-doc::format-doc-text-latex x) :width 80 :indent 3 ))
          "")
        "~%\\begin{Verbatim}[frame=single]~%"
       (if (example-code-res e)
           (format-input-output-list-dead (example-code-res e))
         (format-input-output-list (example-code e) (example-vars e)))
       "\\end{Verbatim}~%"       
       (form-ent example-posttext "~a~%" x))))

(maxima::ddefun format-examples-latex (name)
  "Format as a string the list of examples for topic 'name'."
  (let* ((el (gethash name *examples-hash*)))
    (when el (concatenate 'string 
                        (format nil (concatenate 'string
  "\\noindent{\\bf Examples}~%~{~A~}~%")
         (loop for e in (reverse el) collect (format nil (format-example-latex e))))
        ""))))

(defun add-example (name &rest examples)
  (let ((el (gethash name *examples-hash*)))
    (loop for example in examples do
         (push (apply #'make-example example) el))
    (setf (gethash name *examples-hash*) el)))

(defun clear-examples (name)
  "Remove all examples associated with 'name'."
  (setf (gethash name *examples-hash*) nil))

(defun clear-add-example (name &rest examples)
  (clear-examples name)
  (apply #'add-example (cons name examples)))

;; using examples:: is not necessary in this file, but is in other files, unless these functions are exported.

(examples::clear-examples "first")

(examples::add-example "first" '( :pretext "Get the first element of a list."
                       :vars "[x,y,z]"
                       :code  "first([x,y,z])" ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Maxima functions

;;; See other examples in tyep_of in misc-util.lisp 

(max-doc:set-cur-sec 'max-doc::misc-fandv)

(maxima::defmfun1 (maxima::$examples :doc) ( (item :or-string-symbol ) )
  (maxima::maxima-symbol-to-string item)
  (format t "~a" (format-examples item))
  'maxima::$done)

(max-doc:clear-call-desc "examples")
(max-doc:add-call-desc '( "examples" ("item") ("Print examples for the topic " arg "item"
                              ". Note these examples are different from those extracted from the
            maxima manual with the command " code "example" ".")))

(maxima::defmfun1 (maxima::$examples_add :doc) ( (item :or-string-symbol)
                                                 (text :string)
                                                 (protected-var-list :string)
                                                 (code :string-or-listof))
  (add-example item (list :pretext text :vars protected-var-list :code code))
    'maxima::$done)

(max-doc::clear-call-desc "examples_add")
(max-doc::add-call-desc '( "examples_add" ("item" "text" "protected-var-list" "code")
                          ("Add an example for item " arg "item" ". " arg "text" " will be printed
 before the example is displayed. " arg "protected-var-list" " is string giving a list of variables
 such as \"[x,y]\" that appear in the example code. The example code will be wrapped in a block
 that makes "  arg "protected-var-list" " local. " arg "code" " may be a string or list of strings
 that is/are the example code.")))

(add-example "examples_add" '( :pretext "Add an example for the function 'last'."
                       :vars "[]"
    :code "examples_add(\"last\", \"Return the last item in a  list.\", \"[a,b,c,d]\", \"last([a,b,c,d])\") "))
