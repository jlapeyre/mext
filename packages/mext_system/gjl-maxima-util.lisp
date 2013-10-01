;;;  Copyright (C) (2012,2013) John Lapeyre. Licensed under GPL, v3 or greater. See the file
;;;  `LICENSE' in this directory.

(in-package :maxima)
;(mext:mext-optimize)
(use-package :gjl.lisp-util)


(defun lisp-sym-to-max (s)
 "This certainly exists somewhere in the maxima source."
  (intern (concatenate 'string "$"
                       (coerce (loop for char across (symbol-name s)
                                     collect (if (eq char #\-) #\_ char)) 'string)) "MAXIMA"))
; (symbol-name s)) "MAXIMA"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hashes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; should perhaps sort, but how ?
(defun hash-to-maxima-list (hash)
  "Convert a hash to a maxima list of lists of key val pairs."
  (cons '(mlist simp)
        (loop :for k :being :the :hash-keys :of hash
              :collect `((maxima::mlist simp) ,k ,(gethash k hash)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; multiple evaluation!
;; Looks like alters the input to be a lisp list (or makes a new object)
;; If input is an atom, then (atom),
;;  if a lisp list, falls through,
;;  maxima list, then remove '(mlist)
(defmacro s-or-mlist-to-list (e)
  "Convert single element of mlist to a lisp list.
   ie, make singleton out of element."
  `(cond ((maxima::$listp ,e)
          (pop ,e))
         ((listp ,e))
         (t
          (setf ,e (list ,e)))))

(defun ensure-lisp-list (e)
  "Input is a maxima list, or a lisp list (not a maxima expression)
   or anything else. Convert maxima list to lisp list.
   Lisp list falls through. Everything else is put into a lisp list."
  (cond (($listp e) (cdr e))
        ((consp e) e)
        (t (list e))))

;; Very similar to macros in src/strmac.lisp
;; But this one is not there, and I want a short name.
(defmacro mk-mlist (e) `(cons '(mlist simp) ,e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; maybe allow string to fall through.
(defun sym-to-string (s)
  (maybe-invert-string-case (format nil "~s" s)))

;; Why is this a macro?,.. i guess to avoid let or setf
;; uh this one is probably leaky, but it should not
;; be called with complicated expressions, i hope.
(defmacro maxima-symbol-to-string (or-sym-str)
  "Convert symbol to string. String falls through."
  (let ((s (gensym)))
    `(let ((,s ,or-sym-str))
       (unless (stringp ,s) (setf ,or-sym-str ($sconcat ,s))))))

;; function version for mapping, etc.
(defun fmaxima-symbol-to-string (s)
  (if (stringp s) s ($sconcat s)))
