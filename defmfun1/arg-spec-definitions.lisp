;;;  Argument specifications for functions defined via defmfun1.
;;;  Copyright (2012) John Lapeyre. Licensed under GPL, v3 or greater.

(if (find-package :defmfun1 ) t (defpackage :defmfun1 (:use :common-lisp :gjl.lisp-util )))
(in-package :defmfun1)

;;; Argument specs defined here may be placed in the lambda list of
;;; `defmfun1' function definitions. For example
;;; `(defmfun1 $f1 ((str :string) (n :integer)) ...)'
;;; When the defmfun1 function is called, the arguments are tested
;;; with the code in these specs. If the test fails, one of the
;;; following happens: 1) An error is signaled with `merror1', which
;;; is very similar to `merror'. 2) The error message is printed, but
;;; no error is signaled and the input form is returned. 3) No error
;;; message is printed and the input form is returned. Which of these
;;; happens may be controlled for each function at run time by setting
;;; attributes.

;;; TODO add check during macro expansion for the error
;;; (:test var), when we should have (var :test)

;;; The keyword :pp signifies a combined test and preprocessing.
;;;   In this case, the code snippet must return a list of two elements. 
;;;   The first is the result of the test, the second is the processed
;;;   value, which will be assigned to the input variable.

(defvar *arg-spec-definitions* `(
                     (:function "a function"
                                (not (numberp e))) ; a bit fuzzy what this would mean
                     (:string  "a string" (stringp e))
                     (:string-or-listof ("a string" "a list of strings")
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
                     (:pp :to-float "an expression that can be converted to a float"
                          (let ((v (maxima::$float e))) (list (numberp v) v)))
                     (:complex-number  "a complex number" (complex_number_p e)) ; from ellipt.lisp
                     (:non-neg-number "a non-negative number"
                      (and (numberp e) (>= e 0)))
                     (:non-neg-int "a non-negative integer"
                      (and (integerp e) (>= e 0)))
                     (:pos-int "a positive integer"
                         (and (integerp e) (> e 0)))
                     (:pos-int-or-listof ("a positive integer" "a list of positive integers")
                                         (or (and (integerp e) (> e 0))
                                             (and (maxima::$listp e)
                                                  (every #'(lambda (x) (and (integerp x) (> x 0)))
                                                         (cdr e)))))
                     ((:int-range 2) "an integer between ~a and ~a"
                       int-range-check)
                     (:uint-64 ,(format nil (concatenate 'string 
                         "equivalent to an unsigned 64 bit integer~%"
                      " (that is, an integer between 0 and 2 to the power 64)~%"
            "(We need to modify the doc system so we can use notation for powers in arg check strings.~%"))
                      (and (integerp e) (>= e 0) (< e 18446744073709551616)))
                     (:not-zero "an expression that is not zero"
                      (not (maxima::zerop1 e)))
                     (:roman-integer "an integer between 2 and 3999 (Required for roman form.)"
                                     (and (integerp e) (> e 0) (< e 4000)))
                     (:radix "a valid radix (an integer between 2 and 36)"
                             (and (integerp e) (> e 1) (< e 37)))
                     (:or-radix-string ("a valid radix (an integer between 2 and 36)" "a string")
                             (or (stringp e) (and (integerp e) (> e 1) (< e 37))))
                     (:or-string-symbol  ("a string" "a symbol")
                      (or (symbolp e) (stringp e)))
                     (:or-string-symbol-or-listof ("a string" "a symbol" "a list of strings or symbols")
                                         (or (stringp e) (symbolp e)
                                             (and (maxima::$listp e)
                                                  (every #'(lambda (x) (or (stringp x) (symbolp x))) (cdr e)))))
                     (:symbol-listof ("a list of symbols")
                                     (and (maxima::$listp e)
                                          (every #'(lambda (x) (symbolp x)) (cdr e))))
                     (:or-string-non-atom  ("a string" "non-atomic")
                      (or (stringp e) (not (maxima::$mapatom e))))
                     (:or-symbol-subvar ("a symbol" "a subscripted variable")
                       (or (symbolp e) (maxima::$subvarp e)))
                     (:non-atom "non-atomic"
                                (not (maxima::$mapatom e)))
                     (:non-atom-list "non-atomic and represented by a lisp list"
                                (and (not (maxima::$mapatom e)) (listp e)))
                     (:non-atom-ae-list "non-atomic and either aex or represented by a lisp list"
                                (and (not (maxima::$mapatom e)) (or (maxima::aex-p e) (listp e))))
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

(defvar *opt-spec-definitions* '(
                     (:symbol "a symbol"
                              (symbolp e))
                     (:out-rep  "a valid expression representation.~%   (It must be either 'ml' or 'ar')"
                      (member e '(maxima::$ml maxima::$ar)))
                     (:bool  "a Boolean value. It must be true or false."
                      (member e '(t nil)))
                     ((:int-range 2) "an integer between ~a and ~a"
                      int-range-check)
                     (:non-neg-int "a non-negative integer."
                      (and (integerp e) (>= e 0)))
                     (:non-neg-number "a non-negative number"
                      (and (numberp e) (>= e 0)))
                     (:pp :to-float "an expression that can be converted to a float"
                          (let ((v (maxima::$float e))) (list (numberp v) v)))))



;;  LocalWords:  defmfun Lapeyre GPL defpackage gjl util str merror
;;  LocalWords:  defvar arg numberp stringp listof maxima listp cdr
;;  LocalWords:  pathname pathnamep ae lex aex integerp ellipt pos ar
;;  LocalWords:  uint radix symbolp mapatom subvar subscripted bool
;;  LocalWords:  subvarp
