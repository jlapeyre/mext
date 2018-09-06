;;; Copyright (C) 2012 John Lapeyre
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;; These are some max-doc entries that must occur after objects they
;; describe because the objects are part of the documentation system,
;; which is not yet working when they are defined

;; Why is this necessary ?
(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :max-doc))

(max-doc::set-cur-sec 'max-doc::options)
(defmfun1:set-file-and-package "maxdoc-entries.lisp" "maxdoc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-doc-entry '( :name "compile" :type "Option"
    :contents "If this option is true, then lambda functions passed as arguments to a function will
 be automatically translated or compiled. If it is false they will used as interpreted maxima code. Compiling lambda
 functions usually greatly deceases the execution time of the function if the lambda function
 is called many times."))

(add-doc-entry '( :name "ot" :type "Option"
                 :contents
 ( "With a value " :code "ar" " this option causes the function to return an array-representation expression.
  With a value " :code "ml" " a standard lisp list representation is returned.
  The array-representation is not a maxima array, but rather a more-or-less arbitrary maxima expression that is stored
  internally  as an array. For certain operations, such as random access to elements of the expression,
  an array representation  is faster than the standard list representation. One disadvantange of the array
  representations is that creating an array is relatively slow. For instance, execution time may be large
  if a function returns an expression with many small subexpressions
  that are in the array-representation.  The majority of the maxima system does not understand array-representation, so
  conversion back to list-representation at may be necessary.")))

(add-doc-entry '( :name "adj" :type "Option" :contents
 ( "This option takes values of " :code "true" " or " :code " false. If " :codecomma "true"
 " then the output aex expression is  adjustable, that is, the underlying array
  can be extended in size. If " :codecomma "false" " then the output aex expression is not
  adjustable. The non-adjustable array may have some advantanges in efficiency, but I have
  not observed them, and this may be lisp-implementation dependent.")))
