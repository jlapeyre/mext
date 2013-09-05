(in-package :maxima)
;(mext:mext-optimize)
(use-package :gjl.lisp-util)

(defun lisp-sym-to-max (s)
 "This certainly exists somewhere in the maxima source."
  (intern (concatenate 'string "$"
                       (coerce (loop for char across (symbol-name s)
                                     collect (if (eq char #\-) #\_ char)) 'string)) "MAXIMA"))
; (symbol-name s)) "MAXIMA"))

;; maybe allow string to fall through.
(defun sym-to-string (s)
  (maybe-invert-string-case (format nil "~s" s)))

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
