(if (find-package :maxima-take ) t (defpackage :maxima-take (:use :common-lisp )))
(in-package :maxima-take)
(mext:mext-optimize)
;(declaim (optimize (speed 3) (space 0) (safety 0) (debug 0)))

(use-package :gjl.lisp-util)
(use-package :max-doc)

(set-cur-sec 'max-doc::lists-fandv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro mk-array-sub (name sign)
  (let ((np (if (eq 'neg sign) t nil)))
    `(defun ,name (e i1 i2 i3)
       ,@(if np `((setf i3 (- i3))))
       (let* ((a (maxima::aex-arr e))
              (n1 (truncate (1+ (- ,@(if np `(i1 i2) `(i2 i1)))) i3))
              (s1 (maxima::aex-copy-only-array e n1)))
         (loop for i from ,@(if np `((1- i1) downto i2) `(i1 to i2)) by i3
            for j from 0 to (1- n1)  do
            (setf (aref s1 j) (aref a i)))
         s1))))

(mk-array-sub array-sub-pos pos)
(mk-array-sub array-sub-neg neg)

(defun take-1-array  ( e inds )
  (let ((eout (maxima::aex-copy-no-data e)))
    (setf (maxima::aex-arr eout)
          (dbind (i1 i2 i3 n) (maxima-take::process-seq-spec-0 (maxima::aex-arr e) inds)
                 (declare (ignore n))
                 (let ((a (maxima::aex-arr e)))
                   (cond ((= 1 i3)
                          (subseq  a i1 i2))
                         ((= -1 i3)
                          (reverse (subseq a i2 i1)))
                         ((< i3 0)
                          (array-sub-neg e i1 i2 i3))
                         (t             ; i3 is positive
                          (array-sub-pos e i1 i2 i3))))))
    eout))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro mk-take-1-pos (name i3 inc1 inc2)
  `(defun ,name (e i1 i2 ,@i3)
     (declare (fixnum i1 i2 ,@i3))
     (do* ((e (nthcdr i1 e) ,inc1)
           (count (1+ i1) (+ count ,inc2))
           (res))
          ((or (null e) (> count i2)) (nreverse res))
       (declare (fixnum count))
       (push (car e) res))))

(mk-take-1-pos take-1-pos (i3) (nthcdr i3 e) i3)
(mk-take-1-pos take-1-pos-i3-1 nil (cdr e) 1)

;;  step from i1 downto i2 by i3 ,  ( i3 > 0)
;;  returns number of iterations n, and starting cdr i2a

(defun take-comp-neg-inds (i1 i2 i3)
  (let ((n (truncate (1+ (- i1 i2)) i3) ))
    (list (1+ n) (- i1 (* i3 n)))))

;; e is a lisp  list. i1 >= i2 are indices ,  i3 < 0 is negative step.
;; Return elements from i1 down to not less than i2 in steps of i3
(defmacro mk-take-1-neg (name i3 inc1)
  `(defun ,name (e i1 i2 ,@i3)
     (declare (fixnum i1 i2 ,@i3))
     ,(if i3 `(setf i3 (- i3)))
     (dbind (n i2a) (take-comp-neg-inds i1 i2 ,(if i3 `i3 `1))
            (declare (fixnum n i2a))
;;            (format t "n ~a, i2a ~a~%" n i2a)
            (do* ((e (nthcdr i2a e) ,inc1)
                  (count 1 (1+ count))
                  (res))
                 ((or (null e) (> count n)) res)
              (declare (fixnum count))
              (push (car e) res)))))

(mk-take-1-neg take-1-neg (i3) (nthcdr i3 e) )
(mk-take-1-neg take-1-neg-i3-1 nil (cdr e) )

;; Process a sequence specification, returning start stop and increment
;; indices, as well as the length of the expression e. The specification
;; is either a number or a list of 1, 2, or 3 numbers
;; This is used by take and string_take, and could be used by other functions, as well

(defun process-seq-spec (e inds)
  (let ((i1 1) i2 (i3 1) (n -1))
    (cond ((listp inds)
           (pop inds)
           (let ((ni (length inds)))
             (setf i1 (first inds))
             (cond ((= 3 ni)
                    (setf i2 (second inds))
                    (setf i3 (third inds)))
                   ((= 2 ni)
                    (setf i2 (second inds)))
                   ((= 1 ni)
                    (setf i2 i1)))))
          ((eq inds 'maxima::$all)
           (setf i2 -1))
          ((eq inds 'maxima::$none)
           (setf i1 2)
           (setf i2 1))
          ((eq inds 'maxima::$reverse)
           (setf i1 -1)
           (setf i2 1)
           (setf i3 -1))
          ((symbolp inds)  ; never get here if the defmfun1 filter looks for the three symbols
           (maxima::merror1 (intl:gettext "Unknown symbolic sequence specifier ~a~%") inds))
          (t
           (if (< inds 0)
               (let ((n (length e)))
                 (setf i1 (+ n inds 1))
                 (setf i2 n))
               (setf i2 inds))))
    (when (or (< i1 0) (< i2 0))
        (setf n (length e))
        (when (< i1 0) (setf i1 (+ n i1 1)))
        (when (< i2 0) (setf i2 (+ n i2 1))))
    (list (1- i1) i2 i3 n)))

(max-doc::add-doc-entry '( :name "sequence specifier" :type "Argument type"
                          :contents
" A sequence specification specifies a subsequence of the elements in an expression.
 A single positive number n means the first n elements. -n means the
 last n elements. A list of three numbers [i1,i2,i3] means the i1 th through the
 i2 th stepping by i3. If i1 or i2 are negative, they count from the end. If i3 is
 negative, stepping is down and i1 must be greater than or equal to i2. If i3 is omitted,
 it is taken to be 1.  A sequence specifiier can also be one of 'all 'none or
 'reverse, which mean all elements, no  elements or all elements in
 reverse order respectively." ))

(max-doc::see-also "sequence specifier" '("take" "string_take"))

;; suitable for strings and arrays

(defun process-seq-spec-0 (e inds)
  (dbind (i1 i2 i3 n) (process-seq-spec e inds)
         (if (< i3 0)
             (list  (1+ i1) (1- i2) i3 n)
             (list i1 i2 i3 n))))

;; take-1 handles list representation only, with no conversion

(defun take-1 (e inds)
  "e is maxima expression. inds is a list giving user-level sequence spec.
   separate (car e) and choose routine to take from (cdr e), then cons
   (car e) to the front again. This is called recursively from take-do-list."
  (cons (car e)
        (dbind (i1 i2 i3 n) (progn (pop e) (process-seq-spec e inds))
               (cond ((= 1 i3)
                      (if (= n i2)
                          (copy-list (nthcdr i1 e))
                          (take-1-pos-i3-1 e i1 i2)))
                     ((> i3 0)
                      (take-1-pos e i1 i2 i3))
                     ((= -1 i3)
                      (if (= n i2)
                          (reverse (nthcdr i1 e))
                          (take-1-neg-i3-1 e i1 i2)))
                     (t
                      (take-1-neg e i1 i2 i3))))))

;; take-do-list and take-do-array recursively call take-do because subexpression can
;; be represented differently (array , list) than parent expression.

;; mk-level-fun-list writes a depth-first function.
;; This way of calling at levels is not efficient for some ways of calling take.
;; The way it is now:
;; Suppose we want to take at nested levels. We first take according the
;; specs for all elements at the deeper levels. Then we take at top level,
;; only keeping some results. So the work of taking at some of the lower levels
;; was wasted. In addition, if the shape of the lower levels is wrong for the spec
;; but we will discard it at a higher level, we will get an unecesary error.

;; So, this should be changed to loop from the top level down rather than recurse.

;; Hopefully, we don't need to change the call mk-level-func at this level of abstraction

;; define (defun take-do-list e v). It calls take-1 on e and (car v) if v is of length 1
;;  or else calls take-1 on result of  take-do recursively on parts of e and (cdr v)
(maxima::mk-level-func-list take-do-list take-1 take-do 1)

(maxima::mk-level-func-array take-do-array take-1-array take-do 1)

(defun take-do (e v)
  (if (maxima::aex-p e)
      (take-do-array e v)
      (take-do-list e v)))

(maxima::defmfun1 (maxima::$take :doc) ((e :non-atom)  &rest (v :seq-spec))
  "The first argument to take can have mixed lex and aex expressions on different levels.
   If more sequence specifications are given, they apply to sucessively deeper levels in <e>."
  (if (null v) (list (car e))
      (take-do e v)))

(max-doc::add-call-desc     ; more calls are missing
 '("take" ("e" "n") ("returns a list of the first " arg "n"
                     " elements of list or expression " arg "e" "."))
 '("take" ("e" ("list" "n1" "n2")) ("returns a list of the " arg "n1" "th through " arg "n2" "th "
                      " elements of list or expression " arg "e" "."))

 '("take" ("e" ("list" "n1" "n2" "step")) ("returns a list of the " arg "n1" "th through " arg "n2" "th "
                                     " elements stepping by " arg "step" " of list or expression " arg "e" "."))

 '("take" ("e" ("lit" "-<n>" )) ("returns the last " arg "n" " elements."))

 '("take" ("e" "spec1" "spec2" "...") ("applies the sequence specifications at sucessively deeper levels in " arg "e" ".")))

(examples::clear-examples "take") ;;

(examples::add-example "take"
          '( :pretext "Take the first 3 elements of a list."
             :vars "[a,b,c,d,e]"
             :code ( "take([a,b,c,d,e],3)" ))
          '( :pretext "Take the last 3 elements of a list."
            :vars "[a,b,c,d,e]"
            :code ( "take([a,b,c,d,e],-3)" ))
          '( :pretext "Take the second through third elements of a list."
            :vars "[a,b,c,d,e]"
            :code ( "take([a,b,c,d,e],[2,3])" ))
          '( :pretext "Take the second through tenth elements of a list counting by two."
            :code ( "take([1,2,3,4,5,6,7,8,9,10],[2,10,2])" ))
          '( :pretext "Take the last through first elements of a list counting backwards by one."
            :vars "[a,b,c,d]"
            :code ( "take([a,b,c,d],[-1,1,-1])" ))
          '( :pretext "Shorthand for the previous example is 'reverse."
            :vars "[a,b,c,d]"
            :code ( "take([a,b,c,d],'reverse)" ))
          '( :pretext "Take the second through third elements at the first level and  the last 2 elements at the second level."
            :vars "[a,b,c,d,e,f,g,h,i]"
            :code "take([[a,b,c], [d,e,f], [g,h,i]], [2,3],-2)" ))

