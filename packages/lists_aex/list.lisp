(in-package :max-list)
(declaim (optimize (compilation-speed 0) (speed 3) (space 0) (safety 0) #-gcl (debug 0)))
;(mext:mext-optimize)

(use-package :max-doc)

(max-doc:set-cur-sec 'max-doc::lists-fandv)
(defmfun1:set-file-and-package "list.lisp" "lists_aex")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; lrange
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun num-range (imin imax incr )
  "Range with numeric elements."
  (if (and (typep imin 'fixnum) (typep imax 'fixnum) (typep incr 'fixnum))
      (num-range-int imin imax incr)
    (num-range-float imin imax incr)))

;; fixnum declarations don't seem to do much
(defmacro def-num-range-type ( name type )
  (dbind (decl loop-decl)
         (if (eq type 'fixnum) '( ((declare (fixnum imin imax incr))) (fixnum))
             '( nil nil ))
         `(defun ,name (imin imax incr )
            ,@decl
            (cond ( (< (/ (- imax imin) incr) 0)
                   '( (maxima::mlist) ))
                  (t
                   (cond ( (>= imax imin)
                          (maxima::mk-mlist
                                (loop for i ,@loop-decl  from imin to imax by incr collect i)))
                         (t
                          (setf incr (- incr))
                          (maxima::mk-mlist
                                (loop for i ,@loop-decl from imin downto imax by incr collect i)))))))))

(def-num-range-type num-range-int fixnum)
(def-num-range-type num-range-float float)

(defun ar-num-range (adj-type imin imax incr )
  (if (and (typep imin 'fixnum) (typep imax 'fixnum) (typep incr 'fixnum))
      (ar-num-range-int adj-type imin imax incr)
    (ar-num-range-float adj-type imin imax incr)))

;; acl gives error on svref as well. just remove it.
(defmacro our-aref (&rest args) `(aref ,@args))
;;;    #+sbcl (defmacro our-aref (&rest args) `(aref ,@args))
;;;#-sbcl (defmacro our-aref (&rest args) `(svref ,@args))

;; fixed bug by removing 'n' from first fixnum declaration.  if incr
;; is not 1 then division is causing n to take rational value and
;; floor does not fix this.  Tried a fix, which is apparantly working
;; using floor with 2 arguments
(defmacro def-num-range-ar-type ( name type )
  (dbind (dec1 dec2)
         (if (eq type 'fixnum) '( ((declare (fixnum imin imax incr n))) ((declare (fixnum val))))
           '( nil nil ))
         (progn
           `(defun ,name (adj-type imin imax incr &aux oar (n 0))
              ,@dec1
;           (setf n (/ (- imax imin) incr))
;           (if (< n 0) (setf n 0))
;           (setf n (floor n))
           (setf n (- imax imin))
           (if (< n 0) (setf n 0))
           (setf n (floor n incr))
           (if (= 0 n)
               (setf oar (make-array 0 :adjustable adj-type :fill-pointer adj-type))
             (setf oar (make-array (+ n 1) :element-type t :adjustable adj-type :fill-pointer adj-type)))
           (cond ( (<= n 0) oar)
                 (t (let ( (val imin) )
                      ,@dec2
                      (incf n)
                      (loop for i fixnum from 0 to (1- n)
                            do (setf (our-aref oar i) val)
                            (setf val (+ val incr))))))
           oar))))

(def-num-range-ar-type ar-num-range-int fixnum )
(def-num-range-ar-type ar-num-range-float float )

(max-doc::add-doc-entry '( :name "lrange" :type "Function"
                       :see-also ("makelist" "table" "constant_list")
                       :contents
 (" lrange is much more efficient than makelist for creating ranges, particularly for large lists
   (e.g. " :math "10^5" " or more items.) Functions for creating a list of numbers, in order "
   " of decreasing speed, are: " :mrefcomma "lrange" " " :mrefcomma "table" " " :emrefcomma
   "create_list" :emref "makelist" "." )))

(max-doc::add-call-desc '( "lrange" ("stop") ("returns a list of numbers from 1 through " :argdot "stop"))
               '( "lrange" ("start" "stop")
                  ("returns a list of expressions from " :arg "start" " through " :argdot "stop"))
               '( "lrange" ("start" "stop" "incr")
         ("returns a list of expressions from " :arg "start" " through " :arg "stop" " in steps of " :argdot "incr")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :maxima)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmfun-ae $lrange ((arg1 :thread) &optional (imax :thread) (incr 1 :not-zero :thread)  &aux (imin 1) d lst)
  (if (null imax) (setf imax arg1) (setf imin arg1)) ; different number of args changes semantics
  (cond ( (and (numberp imin) (numberp imax) (numberp incr))
         (if (eq o-type '$ar) (make-aex :head '(mlist simp) :arr (max-list::ar-num-range adj-type imin imax incr)
                                        :adjustable adj-type)
             (max-list::num-range imin imax incr)))
        (t (setf d ($float (meval `((mtimes) ((mplus) ,imax ((mtimes) ,imin -1)) ((mexpt) ,incr -1)))))
           (if (not (numberp d))
               (defmfun1-error-return '$lrange_invalid_inc
                        $lrange (format nil "lrange: ~a is not a number. The ratio of ~
                         the difference~%    between the second and first argument to ~
                                          third argument must be a number" ($sconcat d)))
; (maxima::merror1 (format nil "lrange: ~a is not a number. The ratio of ~
;                         the difference~%    between the second and first argument to ~
;                                          third argument must be a number." ($sconcat d)))
               (progn (setf d (floor d))
                      (if (eq o-type '$ar)
                          (let ((oar (make-array (1+ d) :adjustable adj-type :fill-pointer adj-type)))
                            (do ( (i 0 (1+ i))) ((> i d) )
                              (declare (fixnum i d))
                              (setf (aref oar i) (simplify `( (mplus) ,imin  ((mtimes) ,i ,incr)))))
                            (make-aex :head '(mlist simp) :arr oar :adjustable adj-type))
                          (progn (do ( (i 0 (1+ i))) ((> i d) )
                                   (declare (fixnum i d))
                                   (setf lst (cons (simplify `( (mplus) ,imin  ((mtimes) ,i ,incr))) lst)))
                                 (mk-mlist (nreverse lst)))))))))

(examples::clear-examples "lrange")
(examples::add-example "lrange"
                       '(:code  ("lrange(6)" "lrange(2,6)" "lrange(2,6,2)"
                                 "lrange(6,1,-1)" "lrange(6,1,-2)" 
                                    "lrange(6,ot->ar)"))
                       '(:pretext "The type of the first element and increment determine the type of the elements."
                         :code ("lrange(1.0,6)" "lrange(1.0b0,6)" "lrange(1/2,6)"
                         "lrange(6.0,1,-1)"))
                       '(:pretext "Symbols can be used for limits or increments."
                         :vars "[x,a]"
                         :code ("lrange(x,x+4)" "lrange(x,x+4*a,a)")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; tuples
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; efficiency could be improved, but it's not bad
(defun max-list::tuples-lists ( o-type-p o-type  lists)
  (let* ((n (length lists))
         (b (make-array n :element-type 'fixnum))
         (a (make-array n :element-type 'fixnum :initial-element 0))
         (earr (make-array n ))
         (res nil)
         (bres '()))
    (declare (fixnum n))
    (dotimes (i n)
      (let* ((one-list (pop lists))
             (m (progn (pop one-list) (length one-list))))
        (declare (fixnum m))
        (setf (aref b i) m)
        (setf (aref earr i) (make-array m :initial-contents (reverse one-list)))))
    (let ((n-tuples 1))
      (declare (fixnum n-tuples))
      (dotimes (i n) (setf n-tuples (* n-tuples (length (aref earr i)))))
      (dotimes (i n-tuples)
        (setf res '())
        (dotimes (k n)
          (push (aref (aref earr k) (aref a k)) res))
        (push (defmfun-final-to-ae (mk-mlist res)) bres)
        (dotimes (j n)
          (if  (= (the fixnum (aref b j)) (the fixnum (incf (aref a j))))
               (setf (aref a j) 0)
               (return))))
      (defmfun-final-to-ae (mk-mlist bres)))))

;; value of n 0 is default just because defmfun1 needs a val.
(defmfun-ae ($tuples :doc) ( (list-or-lists :non-atom-list ) &optional (n 0 n-supplied-p :non-neg-int)  )
  (declare (fixnum n))
  (if n-supplied-p
      (let* ((head  (pop list-or-lists))
             (b (length list-or-lists))
             (a (make-array n :element-type 'fixnum :initial-element 0))
             (earr (make-array b :initial-contents (reverse list-or-lists)))
             (res nil)
             (bres '()))
        (declare (fixnum b))
        (dotimes (i (expt b n))
          (setf res '())
          (dotimes (k n)
            (push (aref earr (aref a k)) res))
          (push (defmfun-final-to-ae (cons head res)) bres)
          (dotimes (j n)
            (if  (= b  (incf  (aref a j)))
                 (setf (aref a j) 0)
                 (return))))
        (defmfun-final-to-ae (mk-mlist bres)))
      (progn (pop list-or-lists)
             (max-list::tuples-lists o-type-p o-type (reverse list-or-lists)))))

(max-doc::add-call-desc 
 '( "tuples" ("list" "n")
    ("Return a list of all lists of length " :arg "n" " whose elements are chosen from " :arg "list" "."))
 '( "tuples" (("list" "list1" "list2" "..."))
    ("Return a list of all lists whose " :math "i" "_th element is chosen from " :arg "listi" ".")))

(examples::clear-examples "tuples")

(examples::add-example "tuples"
                       '( :pretext "Make all three letter words in the alphabet `a,b'."
                         :vars "[a,b]"           
                         :code "tuples([a,b],3)")
                       '( :pretext "Take all pairs chosen from two lists."
                         :vars "[x,y,z]"           
                         :code "tuples([ [0,1] , [x,y,z] ])")
                       '( :pretext "tuples works for expressions other than lists."
                         :vars "[f]"
                         :code "tuples(f(0,1),3)" ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; constant_list
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun max-list::canon-depth-spec (spec)
  (if (atom spec) (list spec) (rest spec)))

(defun max-list::canon-head-spec (spec head)
  "Check if the number of levels and heads agree. If there is
   only one atomic head, make a repeated list with an element for each level."
  (cond ((maxima::$listp head) ; listp should fail here, not qualified!, qualified it anyway
         (if (= (1+ (length spec)) (length head)) (list t (reverse (cdr head)))
           '(nil nil)))
        (t 
         (let ((res (list head)))
           (dotimes (i (1- (length spec)))
             (setf res (cons head res)))
           (list t (nreverse res))))))

(defun max-list::const-list0 (c n head)
 (cons (list head 'maxima::simp) (make-list n :initial-element c)))

(defun max-list::const-list1 (c n head)
  (declare (fixnum n))
  (let ( (res (list (if (listp c) (copy-tree c) c) )))
    (setf n (1- n))
    (dotimes (i n)
      (setf res (cons (copy-tree c)  res)))
    (cons (list head 'maxima::simp) res)))
                       
(defmfun-ae ($constant_list :doc :match) (c (spec :pos-int-or-listof) &optional (head mlist))
  :desc 
  ("Returns a list of " :math "n" " elements, each of which is "
   "an independent copy of expr. "
   :code "constant_list(expr,[n,m,..])" " returns a nested list of dimensions "
   :argcomma "n" :argcomma "m" :dots ""
   " where each leaf is an independent copy of expr and the copies of each
   list at each level are independent. If a third argument is given, then it
   is used as the op, rather than `list', at every level. The third argument
   may also be a list of heads to apply, one at each leve.")
  (setf spec (max-list::canon-depth-spec spec))
;  (setf head (reverse (max-list::canon-head-spec spec head))) ; easier to work in reverse order
  (setf spec (reverse spec))
  (dbind (head-err head1)
         (max-list::canon-head-spec spec head)
         (unless head-err
           (defmfun1-error-return '$nheads_neq_nlevels $constant_list 
                   "Number of heads not equal to number of levels." :match))
         (let ((lev
                (if (or (symbolp c) (numberp c))
                    (max-list::const-list0 c (pop spec) (pop head1))
                  c)))
           (dolist (n spec)
             (setf lev (max-list::const-list1 lev n (pop head1))))
           (if (eq o-type '$ar) ($faex lev) lev))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; nest_list, nest_while
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-doc-entry '( :name "nest" :type "Function"
                       :protocol "nest(f,x,n)"
                       :contents
" nest(f,x,n) returns  f(...f(f(f(x)))...) where there are n nested calls of f."))

(defmacro max-list::nest-call (call-type)
  `(defmfun-final-to-ae
      (let ( (res (,call-type f  x)))
        (dotimes (i (1- n))
          (setf res (,call-type f res)))
        res)))
  
(defmfun-ae $nest ((f :map-function :thread) (x :ensure-lex) (n :non-neg-int :thread) &opt ($compile t :bool))
  (option-compile-lambda f)
  (defmfun-final-to-ae
      (if (functionp f) (max-list::nest-call funcall) (max-list::nest-call mfuncall))))

(defmacro max-list::nest-list-call (call-type)
  `(let ( (res (list (,call-type f  x))))
    (dotimes (i (1- n))
      (push (,call-type f  (car res)) res))
    (defmfun-final-to-ae (mk-mlist (nreverse res)))))

(defmfun-ae ($nest_list :doc)  ((f :thread) x (n :non-neg-int :thread) &opt ($compile t :bool))
  (option-compile-lambda f)
  (if (functionp f) (max-list::nest-list-call funcall) (max-list::nest-list-call mfuncall)))

(examples::add-example "nest_list"
                       '( :pretext "Find the first 10 primes after 100."
                         :code "nest_list(next_prime,100,10)"))
                       
(max-doc::see-also "nest_list" '("nest" "fold" "fold_list"))

;; try making fixnum declarations here. probably makes no difference at all.
(defmfun-ae ($nest_while :doc) ((f :thread) x (test :thread) &optional (min 1 supplied-min-p :non-neg-int)
                                (max 1 supplied-max-p :non-neg-int) &opt ($compile t :bool)  )
  (option-compile-lambda test)
  (option-compile-lambda f)
  (let ((res  x))
    (if supplied-min-p
        (dotimes (i (- min 2))
          (setf res (mfuncall f res))))
    (if supplied-max-p (progn
                         (setf max (- max min))
                         (dotimes (i (1+ max))
                           (unless (mfuncall test  res) (return))
                           (setf res (mfuncall f res))))
        (loop  do
             (unless (mfuncall test res) (loop-finish))
             (setf res (mfuncall f res))))
    (defmfun-final-to-ae res)))

(add-call-desc '( "nest_while" ("f" "x" "test") ("applies " :arg "f" " to " :arg "x" " until " :arg "test"
                   " fails to return true when called on the nested result."))
               '( "nest_while" ("f" "x" "test" "min") ("applies " :arg "f" " at least " :arg "min" " times."))
               '( "nest_while" ("f" "x" "test" "min" "max") ("applies " :arg "f" " not more than " :arg "max" " times.")))
(max-doc::implementation "nest_while" "This should be modified to allow applying test to more than just the most recent
    result.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; take_while
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro max-list::take-while (call-type)
  `(let* ((head (car expr))
         (expr (cdr expr))
         (res))
    (do* ((e expr (cdr e))
         (el (car e) (car e)))
        ((or (null e) (not (,call-type test el))) (cons head (nreverse res)))
      (push el res))))

(defmfun-ae ($take_while :doc) (( expr :non-atom-list) (test :thread)  &opt ($compile t :bool))
  (option-compile-lambda test)
  (defmfun-final-to-ae
      (if (functionp test) (max-list::take-while funcall) (max-list::take-while mfuncall))))

(add-call-desc '("take_while" ("expr" "test") ("collects the elements in " :arg "expr" " until " :arg "test"
   " fails on one of them. The op of the returned expression is the same as the op of " :arg "expr" ".")))
(examples::clear-examples "take_while")
(examples::add-example "take_while" '( :pretext "Take elements as long as they are negative."
                                      :code "take_while([-3,-10,-1,3,6,7,-4], lambda([x], is(x<0)))"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; drop_while
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro max-list::drop-while (call-type)
  `(let* ((head (car expr))
         (expr (cdr expr)))
    (do* ((e expr (cdr e))
         (el (car e) (car e)))
        ((or (null e) (not (,call-type test el))) (cons head (copy-list e))))))

(defmfun-ae ($drop_while :doc) (( expr :non-atom-list) (test :thread) &opt ($compile t :bool)  )
  (option-compile-lambda test)
  (defmfun-final-to-ae
      (if (functionp test) (max-list::drop-while funcall) (max-list::drop-while mfuncall))))

(add-call-desc '("drop_while" ("expr" "test") ("Tests the elements of " :arg "expr" " in order, dropping them until "
                                               :arg "test" " fails."
   " The remaining elements are returned in an expression with the same op as that " :arg "expr" ".")))

(examples:clear-add-example "drop_while" '( :pretext "Drop elements as long as they are negative."
                                      :code "drop_while([-3,-10,-1,3,6,7,-4], lambda([x], is(x<0)))"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; length_while
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro max-list::length-while (call-type)
  `(let ((expr (cdr expr)))
    (do* ((e expr (cdr e))
          (el (car e) (car e))
          (count 0))
        ((or (null e) (not (,call-type test el))) count)
      (declare (fixnum count))
      (incf count))))

(defmfun1 ($length_while :doc) (( expr :non-atom-list) (test :thread) &opt ($compile t :bool)  )
  :desc ("Computes the length of " :arg "expr" " while " :arg "test" " is true.")
  (option-compile-lambda test)
  (if (functionp test) (max-list::length-while funcall) (max-list::length-while mfuncall)))

(examples:clear-add-example "length_while" 
     '(:code "length_while([-3,-10,-1,3,6,7,-4], lambda([x], is(x<0)))"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; every1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro max-list::every1 (call-type)
  `(let ((expr (cdr expr)))
    (do* ((e expr (cdr e))
          (el (car e) (car e)))
        ((or (null e) (not (,call-type test el))) (null e)))))

(defmfun1 ($every1 :doc) (( expr :non-atom-list) (test :thread) &opt ($compile t :bool)  )
  (option-compile-lambda test)
  (if (functionp test) (max-list::every1 funcall) (max-list::every1 mfuncall)))

(add-call-desc '("every1" ("expr" "test") 
    ("Returns true if " :arg "test" " is true for each element in " :arg "expr" "."
     " Otherwise, false is returned. This is like " :code "every" " but allow a test that "
     "takes only one argument. For some inputs, every1 is much faster than every.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; imap
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro max-list-imap-call ( call-type)
;;  (when (eq 'direct (car call-type)) (setf call-type nil))
  `(let* ((head (car expr))
         (expr (cdr expr))
         (res))
    (do* ((e expr (cdr e))
         (el (car e) (car e)))
        ((null e) (cons head (nreverse res)))
      (push (,call-type f el) res))))

(defmacro max-list-imap-aex-call (call-type)
  `(let* ((a (aex-copy-new-n expr))
          (ar (aex-arr a))
          (are (aex-arr expr)))
;;     (format t "aex call type ~a~%" ',call-type)
     (dotimes (i (length ar))
       (setf (aref ar i) (,call-type f (aref are i))))
     a))

; this is the definition of format1
; There is a defmfun1 interface in mext_basic
;(defmfun format1 (e)
;  (cond (($listp e) e)
;	($inflag (specrepcheck e))
;	(t (nformat e))))
;; Note: stuck format1 in here in order to pass rtests. There is
;; no way from within maxima to see that the expression is not
;; what is printed.
(defmfun1 ($imap :doc) (f (expr :non-atom) &opt ($compile t :bool))
  :desc 
  ("Maps functions of a single argument. I guess that " :emref "map" " handles more
   types of input without error. But " :mref "imap" " is much faster for many inputs."
   " This is especially true if a lambda function is passed to imap, as it can be compiled.")
  (option-compile-lambda f)
  (let (($distribute_over nil))
    (if (aex-p expr)
        (if (functionp f) (max-list-imap-aex-call funcall) (max-list-imap-aex-call mfuncall))
      (if (and (symbolp f) (eq 'direct (get f 'call-mode)))
          (max-list-imap-call funcall)
            (if (functionp f)
                (format1 (max-list-imap-call funcall))
              (format1 (max-list-imap-call mfuncall)))))))

(examples::clear-examples "imap")
(examples::add-example "imap"
                       '( :pretext "Map sqrt efficiently over a list of floats"
                         :vars "[a]"
                         :code "(a : lrange(1.0,4),
        imap(lambda([x],modedeclare(x,float),sqrt(x)),a))")
                       '( :pretext "With aex expression, no conversions to lex are done."
                         :vars "[a]"
                         :code "(a : lrange(1.0,4,ot->ar),
          imap(lambda([x],modedeclare(x,float),sqrt(x)),a))"))
;;                         :code ("a : lrange(1.0,10)" "imap(lambda([x],modedeclare(x,float),sqrt(x)),[1.0,2.0])")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mapall
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is slow. mapcar would be faster
;; (defun max-list::do-map-all-old (f expr)
;;   (if ($mapatom expr)
;;       (mfuncall f expr)
;;     (mfuncall f
;;               (cons (car expr)
;;                     (loop :for subexpr :in (cdr expr) :collect
;;                           (max-list::do-map-all-old f subexpr))))))

#|
;; this if funcer
(defmfun funcer (fn args)
  (cond ((member fn '(mplus mtimes mexpt mnctimes) :test #'eq)
	 (simplify (cons (ncons fn) args)))
	((or (member fn '(outermap2 constfun) :test #'eq)
	     (and $transrun (symbolp fn) (get fn 'translated)
		  (not (mget fn 'local-fun)) (fboundp fn)))
	 (apply fn (mapcar #'simplify args)))
	(t (mapply1 fn (mapcar #'simplify args) fn
		    nil	;; try to get more info to pass
		    ))))

|#

(defun max-list::do-map-all-1 (f expr)
  (if (mapatom expr) ; mapatom instead of $mapatom does not help
      (funcer f (list expr))
    (funcer f
            (list (cons (car expr)
                  (mapcar #'(lambda (x) (max-list::do-map-all f x)) (cdr expr)))))))


;; Using funcer instead of mfuncall is much faster.
;; simple constructing the list, eg for f(a) is lightening
;; fast, but it fails to apply real functions, such as expand.
;; This is now faster by a bit than scanmap, but it will fail
;; on mrats and so forth.
(defun max-list::do-map-all (f expr)
  (if (mapatom expr) ; mapatom instead of $mapatom does not help
      (funcer f (list expr))
    (funcer f
            (list (cons (car expr)
                  (mapcar #'(lambda (x) (max-list::do-map-all f x)) (cdr expr)))))))

;; We should be able to check the function f just once to find out what it is
;; and then either do the fast or slow version!
;; Look at the code in funcer to figure this out!
(defun max-list::do-map-all-fast (f expr)
  (if (mapatom expr)
      (cons (list f) (list expr))
    (cons (list f)    
          (list (cons (car expr)
                      (mapcar #'(lambda (x) (max-list::do-map-all-fast f x)) (cdr expr)))))))

;; Can't map on ops too. This works in Mma, but not here.
;; There may be a way to encode mapping onto ops in the car of
;; the maxima expression, but I don't know what its worth.
;; ... I bet there is already some support for this.
;; Note that this is different than fullmap.
; distribute_over nil  seems to disable distributing without 
; changing global flag, but the rtest is not working.

;; Disable this until it is improved.
#|
(defmfun1 ($mapall :doc) ((f :map-function) expr &opt ($fast nil :bool))
  :desc ("Apply " :arg "f" " at all levels of " :argdot "expr"
         " It appears that this is the same as " :emrefdot "scanmap"
         :par ""
         " If the option " :opt "fast" " is true, then " :arg "f" " is not "
         "called, but is rather applied as an op. In this case, " :mref "mapall"
         " runs much faster.")
  (let (($distribute_over nil))
    (if $fast 
        (max-list::do-map-all-fast f expr)
      (max-list::do-map-all f expr))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; fold
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-doc-entry '( :name "fold" :type "Function"
                       :protocol "fold(f,x,v)"
                       :see-also ("fold_list" "nest")
                       :contents
 ( :code "fold(f,x,[a,b,c])" " returns " :code " f(f(f(x,a),b),c).")))

(defmacro max-list::fold-call (call-type)
 `(let ((res (,call-type f x (car v))))
    (when ar-p (setf res ($aex res)))
    (do ((v1 (cdr v) (cdr v1)))
        ((null v1) res)
      (setf res (,call-type f res (car v1)))
      (when ar-p (setf res ($aex res))))))

(defmfun-ae $fold ( (f :thread) x (v :non-atom) &opt ($compile t :bool))
  (option-compile-lambda f)
  (setf v (cdr v))
  (let ((ar-p (if (eq o-type '$ar) t)))
    (if (length1p v) (if ar-p ($aex x) x)
        (if (functionp f) (max-list::fold-call funcall) (max-list::fold-call mfuncall)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; fold_list
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-doc-entry1 :e '( :name "fold_list" :type "Function"
                       :protocol "fold_list(f,x,v)"
                       :see-also ("fold" "nest")
                       :contents
" fold_list(f,x,[a,b,c]) returns [f(x,a),f(f(x,a),b),f(f(f(x,a),b),c)]."))

(defmacro max-list::fold-list-call (call-type)
  `(let ((res (,call-type f x (car v))))
    (do ((v1 (cdr v) (cdr v1))
         (res-all (list res)))
        ((null v1) (defmfun-final-to-ae (mk-mlist (nreverse res-all))))
      (push (setf res (defmfun-final-to-ae (,call-type f res (car v1)))) res-all))))

(defmfun-ae $fold_list ((f :thread) x (v :non-atom)  &opt ($compile t :bool))
  (option-compile-lambda f)
  (setf v (cdr v))
  (if (length1p v) x
      (if (functionp f) (max-list::fold-list-call funcall) (max-list::fold-list-call mfuncall))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; select
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro max-list::select-call (call-type)
  `(let* ((head (car expr))
          (expr (cdr expr))
          (res))
     (if supplied-n-p
         (do* ((e expr (cdr e))
               (el (car e) (car e))
               (count 0))
              ((or (null e) (>= count n))  (cons head (nreverse res)))
           (declare (fixnum count))
           (incf count)
           (when (,call-type test el) (push el res)))
         (do* ((e expr (cdr e))
               (el (car e) (car e)))
              ((null e) (cons head (nreverse res)))
           (when (,call-type test el) (push el res))))))
         

(defmfun-ae ($select :doc) (( expr :non-atom-list) (test :thread)
                            &optional (n 0 supplied-n-p :pos-int) &opt ($compile t :bool))
  :desc (
  "Returns a list of all elements of " :arg "expr" 
  " for which " :arg "test" " is true. " :arg "expr"
  " may have any op. If " :arg "n" " is supplied, then at most " :arg "n"
  " elements are examined. " :mref "select" " is much faster than "
  :emrefcomma "sublist" " but may be less generally applicable.")
  (declare (fixnum n))
  (option-compile-lambda test)
  (defmfun-final-to-ae
      (if (functionp test) (max-list::select-call funcall) (max-list::select-call mfuncall))))

(examples::clear-examples "select")
(examples::add-example "select"
                       '( :pretext "Select elements less than 3"
                          :code "select([1,2,3,4,5,6,7], lambda([x], is(x<3)))"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; aelistp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Does this really belong here ?
(add-doc-entry '("aelistp"
               :contents ( "Returns true if " :arg "e" " is a list, either ml or ar representation.")))
(defmfun $aelistp (e)
         (or (and (aex-p e)
                  (eq (car (aex-head e)) 'mlist))
             (and (not (atom e))
                  (not (atom (car e)))
                  (eq (caar e) 'mlist))))

(examples::clear-examples "aelistp")
(examples::add-example "aelistp"
                       '( 
                          :vars "[x,y]"         
                          :code ("aelistp([1,2,3])" "aelistp( aex([1,2,3]))" "aelistp(3)" "aelistp(x)"
                                 "x:lrange(10),aelistp(x)" "aelistp(%%f(y))" "aelistp( aex( %%f(y) ))")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; nreverse
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmfun1  ($nreverse :doc)((e :non-atom))
  :desc ("Destructively reverse the arguments of expression " :argdot "e" 
    " This is more efficient than using reverse.")
  (if-aex e
          (progn
            (setf (aex-arr e) (nreverse (aex-arr e)))
            e)
          (progn
            (cons (car e) (nreverse (cdr e))))))

(max-doc:see-also "nreverse" "reverse")
(examples:clear-add-example "nreverse"
 '( :pretext ("Be careful not to use " :arg "a" " after applying nreverse. Do assign the result to another variable.")
    :vars "[a,b]"         
    :code ("a : lrange(10), b : nreverse(a)" "a : lrange(10,ot->ar), b : nreverse(a)")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; count
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This does not recognize lambda functions unless compile is t
(defmfun1 ($count :doc) ( (expr :non-atom-ae-list) (item :thread)  &opt ($compile t :bool))
  :desc 
  ("Counts the number of items in " :arg "expr" " matching " :argdot "item" 
   " If " :arg "item" " is a lambda function then " :arg "compile" " must be true.")
  (setf expr (if (aex-p expr) (aex-arr expr)
               (cdr expr)))
  (option-compile-lambda item)
  (if (functionp item)
      (count-if item expr)
    (count item expr :test 'equal)))

(examples::clear-add-example "count"
                  '( :code ("count([1,2,\"dog\"], 'numberp)" "count([1,2,\"dog\"], \"dog\")"
                            "count(lrange(10^4), lambda([x], is(mod(x,3) = 0)))"
                            "count( %%ff(1,2,\"dog\"), \"dog\")"
                            "count(lrange(100,ot->ar), 'evenp)"
                            "count(lrange(10^5), 'primep)")))
