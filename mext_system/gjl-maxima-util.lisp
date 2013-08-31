(in-package :maxima)
;(mext:mext-optimize)
(use-package :gjl.lisp-util)

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


