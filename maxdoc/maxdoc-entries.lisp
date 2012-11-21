;;; Copyright (C) 2012 John Lapeyre
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;; These are some max-doc entries that must occur after objects they
;; describe because the objects are part of the documentation system,
;; which is not yet working when they are defined

(use-package :max-doc)

(add-doc-entry1  :section 'max-doc::doc-fandv 
                 :e '(
                    :name "read_docs_with_pager"
                    :type "Option variable"
                    :default-value "true"
                     :contents
                     "If read_docs_with_pager is true then documentation printed
by describe() or ? or ?? is read with a pager. This will most
likely only work with a command line interface under linux/unix
with certain lisp implementations."))

;; from misc-util1.lisp
(max-doc::set-cur-sec 'max-doc::io-fandv)
(add-doc-entry1 :section 'max-doc::io-fandv
 :e '(
      :name "pager_command"
      :type "Option variable"
      :default-value "/usr/bin/less"
      :see-also ("read_docs_with_pager")
      :contents
"The pathname to the pager program used for reading paged output,
 eg for documentation."))

#|
(add-doc-entry1 :section 'max-doc::strings-fandv
 :e '(
      :name "with_output_to_string"
      :protocol "with_output_to_string(<expr_1>,<expr_2>,...)"
      :see-also ("with_stdout")
      :contents
"Evaluates <expr_1>, <expr_2>, <expr_3>, ... and writes any output
 thus generated to a string, which is returned."))

(examples::clear-examples "with_output_to_string")
(examples::add-example "with_output_to_string" 
     '( :code "sreverse(with_output_to_string(for i:5 thru 10 do print(\"i! for i=\",i,i!)))"))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(max-doc::set-cur-sec 'max-doc::options)

(add-doc-entry '( :name "compile" :type "Option"
                 :contents "If this option is true, then lambda functions passed as arguments to a function will
 be automatically translated or compiled. If it is false they will used as interpreted maxima code. Compiling lambda
 functions usually greatly deceases the execution time of the function if the lambda function
 is called many times."))

(add-doc-entry '( :name "ot" :type "Option"
                 :contents "With a value <ar> this option causes the function to return an array-representation expression.
 With a value <ml> a standard lisp list representation is returned.
 The array-representation is not a maxima array, but rather a more-or-less arbitrary maxima expression that is stored internally
 as an array. For certain operations, such as random access to elements of the expression, an array representation
 is faster than the standard list representation. One disadvantange to array representations is that creating
 an array is relatively slow. For instance, execution time may be larg if a function returns an expression with many small subexpressions
 that are in the array-representation.  The majority of the maxima system does not understand array-representation, so
 conversion back to list-representation at may be necessary."))

(add-doc-entry '( :name "adj" :type "Option"
                 :contents "This option takes values of true or false. If true, then the output aex expression is
 adjustable, that is, the underlying array can be extended in size. If false, then the output aex expression is not
 adjustable. The non-adjustable array may have some advantanges in efficiency."))

