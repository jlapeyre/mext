;;; Copyright (C) 2012 John Lapeyre
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;; This file contains code that is not specific to maxima. Some is taken or modified from other
;; libraries. It would be better to learn an organized way to use those libraries
(in-package "COMMON-LISP-USER")

(in-package :gjl.lisp-util)

(defmacro gaif (test-form then-form &optional else-form)
  "Paul Graham macro"
  `(let ((anit ,test-form))
     (if anit ,then-form ,else-form)))

(defmacro length1p (e)
  "Return true if length of list is 1. Traverses at most 1
element of e."
  `(null (cdr ,e)))

(defun cmp-length (e n)
"Return the smaller of n and the length of e.
Traverses at most n elements of e."
  (declare (fixnum n))
  (if (arrayp e)
      (let ((len (length e)))
        (if (> len n) n len))
    (do ( (e e (cdr e))
          (i 0 (1+ i) ))
;;        ((or (= n i) (null e)) (if (null e)  i i))  ; why did I have this ??
        ((or (= n i) (null e)) i)
      (declare (fixnum i)))))

(defun length-eq (e n)
  "Return true if length of list or array e is n. Traverses at most n
elements of e."
  (declare (fixnum n))
  (= (cmp-length e  (1+ n)) n))

;; no gensym.
(defmacro ensure-list (e)
  "Singleton to list maybe. Return e if e is a
   list and (list e) otherwise."
  `(if (listp ,e) t (setf ,e (list ,e))))

;;  (let ((e1 (gensym)))
;;    `(let ((,e1 e))
;;       (setf (if (listp ,e1) ,e1 (list ,e1)))))

(defmacro dbind (&body body)
  `(destructuring-bind ,@body))

(defun fill-hash-from-list ( hash-table element-list) 
  "Set several hash elements at once from a list of pairs"
  (mapcar (lambda (pair) (setf (gethash (first pair) hash-table) (second pair))) element-list)
  hash-table)

(defun get-hash-keys (hash)
  "Return a list of keys of hash-table hash."
  (loop :for k :being :the :hash-keys :of hash
     :collect k))
;; following code is ok, but may be slower. Then  again, may be faster
;  (let (klist)
;    (maphash (lambda (x y) (declare (ignore y)) (push x klist)) hash)
;    klist))

(defun get-hash-vals (hash)
  "Return a list of values of hash-table hash."
  (loop :for v :being :the :hash-values :of hash
     :collect v))

(defun print-hash-entry (key value)
 (format t "key:~S, value:~S~%" key value))

(defun dump-hash (hash-table)
  (maphash #'gjl.lisp-util::print-hash-entry hash-table))

(defun get-or-make-subhash (key top-hash &optional (test-type #'eq) )
  (let ((sub-hash (gethash key top-hash)))
    (if sub-hash sub-hash (setf (gethash key top-hash) (make-hash-table :test test-type)))))

(defun string-ends-with-pos (string substr)
 "Takes two arguments string and substr. If string ends with substr,
return the position in string at which the final ocurrence of substr begins.
Otherwise return nil."
  (let ((pos (search substr string :from-end t)))
        (if (null pos) nil
          (let ((len (length string)))
            (if (not (= (- len (length substr)) pos)) nil
              pos)))))

(defun remove-terminal-substring (string substr)
 "Takes two arguments string and substr. If string ends with
substr, return string with substr stripped from the end. Otherwise
return nil."  
  (let ((pos (string-ends-with-pos string substr)))
    (if pos (subseq string 0 pos)
      nil)))

;; where did I find this ?
(defun keyword-p (sym)
  (and (symbolp sym)
       (eq #\: (elt  (format nil "~s" sym) 0))))

;; copied from alexandria (in-package :alexandria)
(defun copy-array (array &key
                   (element-type (array-element-type array))
                   (fill-pointer (and (array-has-fill-pointer-p array)
                                      (fill-pointer array)))
                   (adjustable (adjustable-array-p array)))
  "Returns an undisplaced copy of ARRAY, with same fill-pointer
and adjustability (if any) as the original, unless overridden by
the keyword arguments."
  (let ((dims (array-dimensions array)))
    ;; Dictionary entry for ADJUST-ARRAY requires adjusting a
    ;; displaced array to a non-displaced one to make a copy.
    (adjust-array
     (make-array dims
                 :element-type element-type :fill-pointer fill-pointer
                 :adjustable adjustable :displaced-to array)
     dims)))

(defun copy-array-type (array &key
                   (element-type (array-element-type array))
                   (fill-pointer (and (array-has-fill-pointer-p array)
                                      (fill-pointer array)))
                   (adjustable (adjustable-array-p array)))
  "Returns an array with the same length,  fill-pointer
and adjustability (if any) of ARRAY, unless overridden by
the keyword arguments. This is like copy-array,
but the contents are not copied."
  (let ((dims (array-dimensions array)))
     (make-array dims
                 :element-type element-type :fill-pointer fill-pointer
                 :adjustable adjustable )))


;; Adapted from Gene Michael Stover. One change is that he had ~(~a~) in the default case,
;; which lowercases the text.
(defmacro mk-comm-sep-eng ( name (one (two-a two-b) (three-start three-last)))
  `(defun ,name (lst)
     (let ((n (cmp-length lst 3)))
       (cond ((= 0 n) "")
             ((= 1 n) (format nil "~a~a" ,one (car lst)))
             ((= 2 n) (format nil "~a~a ~a ~a" ,two-a (car lst) ,two-b (cadr lst)))
             (t
              (format nil (concatenate 'string ,three-start "~{ ~a~#[~;, " ,three-last
                                       "~:;,~]~}")   lst))))))

(mk-comm-sep-eng comma-separated-english ("" ("" "and") ("" "and")))
(mk-comm-sep-eng or-comma-separated-english ("" ("" "or") ("" "or")))
(mk-comm-sep-eng not-comma-separated-english ("not " ("neither " "nor") ("not one of: " "or")))

;; taken from common lisp cookbook. By whom ?
(defun split-by-one-space (string)
 "Returns a list of substrings of string
  divided by ONE space each.
  Note: Two consecutive spaces will be seen as
  if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

;; (make-sequence 'string 4 :initial-element #\Space)
(defun wrap-text (&key text (width 70) (indent 0))
  (setf text (remove #\Newline text))
  (cond ( (stringp text) (setf text (remove "" (split-by-one-space text) :test #'equal )))
        ( (listp text) t)
        (t (error "lisp-utils::wrap-text: argument must be a string or a list.")))
  (let*
      ((spaces (make-sequence 'string indent :initial-element #\Space))
       (str (concatenate 'string spaces "~{~<~%" spaces "~1,"  (format nil "~d" width)
                          ":;~A~> ~}")))
    (format nil str text)))

;; from common lisp cookbook
(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
 are replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))
