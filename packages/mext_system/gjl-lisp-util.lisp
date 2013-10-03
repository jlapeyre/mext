;;;  Copyright (C) (2012,2013) John Lapeyre. Licensed under GPL, v3 or greater. See the file
;;;  `LICENSE' in this directory.

;;; Utility macros.
;;; This file contains code that is not specific to maxima. Some is taken or modified from other
;;; libraries. Using other libraries would be nice, but we want this to work with many
;;; lisp implementations, which may not support a packaging system.

(in-package "COMMON-LISP-USER")
(in-package :gjl.lisp-util)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Anaphoric
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro gaif (test-form then-form &optional else-form)
  "Paul Graham macro"
  `(let ((anit ,test-form))
     (if anit ,then-form ,else-form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Short cuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro dbind (&body body)
  `(destructuring-bind ,@body))

(defmacro sconcat (&body body)
  `(concatenate 'string ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Predicates and comparisons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro length1p (e)
  "Return true if length of list is 1. Traverses at most 1
   element of e."
  `(null (cdr ,e)))

;; where did I find this ?
(defun keyword-p (sym)
  (and (symbolp sym)
       (eq #\: (elt  (format nil "~s" sym) 0))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hashes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Arrays
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note: gensym here.
(defmacro ensure-list (e)
  "Singleton to list maybe. Return e if e is a
   list and (list e) otherwise."
  `(if (listp ,e) t (setf ,e (list ,e))))

(defun nthcdr-check (n list)
  "Return the nth cdr with a number consed to the front.
   If the number is greater than zero, then list
   was of length < n."
  (do ((i n (1- i))
       (result list (cdr result)))
      ((or (null result) (not (plusp i))) (cons i result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; Splits s
(defun split-by-one-space (string)
 "Returns a list of substrings of string
  divided by ONE space each.
  Note: Two consecutive spaces will be seen as
  if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

;; gjl adapted above to get this.
(defun split-by-one-newline (string)
 "Returns a list of substrings of string
  divided by ONE newline each.
  Note: Two consecutive newlines will be seen as
  if there were a single newline between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Newline string :start i)
          collect (let ((res (subseq string i j)))
                    (if (string= "" res) (format nil "~%") res))
          while j))

(defun split-by-space-and-newline (string)
  (loop for str in (split-by-one-newline string) append
        (remove "" (split-by-one-space str) :test #'equal )))

;; (make-sequence 'string 4 :initial-element #\Space)
;; Don't know where I got this. It is very similar to Gene Michael Stover's code,
;; which also leaves a spurious space at the end.
(defun wrap-text1 (&key text (width 70) (indent 0))
  (setf text (remove #\Newline text))
  (cond ( (stringp text) (setf text (remove "" (split-by-one-space text) :test #'equal )))
        ( (listp text) t)
        (t (error "gjl:wrap-text: argument must be a string or a list.")))
  (let*
      ((spaces (make-sequence 'string indent :initial-element #\Space))
       (str (concatenate 'string spaces "~{~<~%" spaces "~1,"  (format nil "~d" width)
                          ":;~A~> ~}")))
    (format nil str text)))

;; adapted from above. This one removes single newlines. n newlines
;; are converted to n-1 newlines.
;; gjl dec 2012, added ~^ to format
;; Final loop puts indent spaces before a requested newline.
;; 
(defun wrap-text (&key text (width 70) (indent 0))
  (cond ( (stringp text) (setf text (split-by-space-and-newline text)))
        ( (listp text) t)
        (t (error "gjl:wrap-text: argument must be a string or a list.")))
  (let*
      ((spaces (make-sequence 'string indent :initial-element #\Space))
       (spaces-1 (make-sequence 'string (if (> indent 0) (1- indent) 0) :initial-element #\Space))
       (fmt (concatenate 'string spaces "~{~<~%" spaces "~1,"  (format nil "~d" width)
                          ":;~A~>~^ ~}"))
       (nlstr (format nil "~%"))
       (indent-nl (format nil "~%~a" spaces-1))
       (text1 (loop for e in text collect (if (string= nlstr e) indent-nl e))))
    (format nil fmt text1)))

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

;; For keywords
;; eg :key -> ":key"
(defun to-string-lc (kw)
  "Convert symbol kw to string, including package qualifications, and
  convert to lower case"
  (format nil "~(~s~)" kw))
