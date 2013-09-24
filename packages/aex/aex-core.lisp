;;;  Copyright (C) (2012,2013) John Lapeyre. Licensed under GPL, v3 or greater. See the file
;;;  `LICENSE' in this directory.

;; TODO: Move some of this to a separate package !!
(in-package :maxima)

(mext:mext-optimize)
(use-package :gjl.lisp-util)
(use-package :max-doc)

(max-doc:set-cur-sec 'max-doc::aex-fandv)
(defmfun1:set-file-and-package "aex-core.lisp" "aex")

; need to integrate this with mext somehow.
($put '$aex_package 0.1 '$version)

#|

Moved to defmfun1/aex-core.lisp 

(defvar *aex-core-dummy-vector* (make-array 0)
  "This is only here so that we can automatically check the type.")

(defstruct (aex
            (:print-function
             $print_aex))
  (head '(mlist simp) :type list)
  (arr *aex-core-dummy-vector* :type vector)
  (adjustable nil :type boolean))

(defun aex-op (e)
  (getop (car (aex-head e))))

;; is used
(defun aex-make-n-head (n &key (adjustable t)  (head '(mlist simp)))
  (make-aex :head head :arr (make-array n :adjustable adjustable)
              :adjustable adjustable))
|#

;; Define a macro for defining functions that takes the options:
;; output representation and adjustability.
;; defmfun1-opt takes : name, list of opts, code to be inserted before body. the ignorable is to quiet
;; the compilers.
(defmfun1-opt defmfun-ae ( (($ot o-type) $ml o-type-p :out-rep)
  (($adj adj-type) t adj-type-p :bool) )
  (declare (ignorable adj-type-p o-type-p adj-type )))

;; Define a macro for defining functions that take an option for output adjustability.
;; Normally used with routines that only output array rep expression.
(defmfun1-opt defmfun-aeo ((($adj adj-type) t adj-type-p :bool))
  (declare (ignorable adj-type-p adj-type )))

(defmfun1-opt defmfun-aeo-adj-false ((($adj adj-type) nil adj-type-p :bool))
  (declare (ignorable adj-type-p adj-type )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; accesors and setters
;; $aex_get is faster than $aexg because it does not allow getting the head
;; same for $aex_set and $aexs 

(add-doc-entry1 :e  '( :name "aexg"
                      :protocol "aexg(e,n)"
                      :contents
                      " aexg(e,n) returns the nth part of aexpr e. If n is 0, the head
  of e is returned. No argument checking is performed."))

;; only used in one rtest
(defmfun $aexg (e n)
  (if (= n 0)
      (aex-head e)
  (aref (aex-arr e) (1- n))))

(max-doc::see-also "aexg" '("aex_get" "ipart" "inpart" "part"))

;; not used anywhere
(defun aeref (e n)
 "Get reference to element of array expression. n = 0
refers to the head."   
  (if (= n 0) (aex-head e)
    (aref (aex-arr e) (1- n))))

;; not used anywhere
(defun aeref1 (e n)
  "Get reference to element of array expression. But don't allow getting head.
   This should be more efficient."
  (aref (aex-arr e) (1- n)))

;; not used anywhere
(defsetf aeref (e n) (y) `(progn (if (= 0 ,n) (setf (aex-head ,e) ,y)
                                     (setf (aref (aex-arr ,e) (1- ,n)) ,y)) ,y))

;; not used anywhere
(defsetf aeref1 (e n) (y) `(progn (setf (aref (aex-arr ,e) (1- ,n)) ,y) ,y))

;; this way of entering doc is ugly
(add-doc-entry1 :e '( :name "aex_get"
                      :protocol "aex_get(e,n)"
                      :contents
  ("Returns the " :arg "n-1" "th part of aexpr " :argdot "e"
    " A value of " :arg "n" " less than 0 is not allowed. This is more efficient than "
    :mrefcomma "aexg" " which returns the head of the expression when " 
    :arg "n" " is equal to zero.")))

(defmfun $aex_get (e n)
  (aref (aex-arr e) n))

(examples::clear-examples "aex_get")
(examples::add-example "aex_get"
                       '( 
                         :vars "[a]"
                         :code "a : aex([5,6,7]), aex_get(a,2)"))

(add-doc-entry1 :e  '( :name "aex_set"
                      :protocol "aex_set(e,n,v)"
                      :contents
 ("Destructively sets the " :arg "n" "th part of aexpr " :arg "e" " to value "
   :argdot "v" " A value of " :math "0" " for " :arg "n" " is not allowed. 
 This is more efficient than " :mrefdot "aexs" " No argument checking is done.")))

(defmfun $aex_set (e n v)
  (setf (aref (aex-arr e) n) v))

(examples::clear-examples "aex_set")
(examples::add-example "aex_set"
                       '( :pretext "Destructively assign to a part of an expression."
                         :vars "[a,x]"
                         :code "a : aex([1,2,3]), aex_set(a,1,x), a"))

(max-doc::see-also "aex_set" '("aexs" "ipart"))

;; is used
(defmacro aex-get (e n)
  `(aref (aex-arr ,e) ,n))

;; is used
(defmacro aex-set (e n v)
  `(setf (aref (aex-arr ,e) ,n) ,v))

(defmacro aex-get1 (e n)
  `(aref (aex-arr ,e) (1- ,n)))

(defmacro aex-set1 (e n v)
  `(setf (aref (aex-arr ,e) (1- ,n)) ,v))

(defsetf aex-get (e n) (y) `(progn (setf (aref (aex-arr ,e)  ,n) ,y) ,y))
(defsetf aex-get1 (e n) (y) `(progn (setf (aref (aex-arr ,e) (1- ,n)) ,y) ,y))

(add-doc-entry1 :e '( :name "aexs"
                      :protocol "aexs(e,n,v)"
                      :contents
 ("destructively sets the " :arg "n" "th part of aexpr " :arg "e" " to value " :argdot "v"
 " A value of " :math "0" " for " :arg "n" " returns the head (or op) of " :argdot "e")))

;; used in rtest
(defmfun $aexs (e n v)
  (if (= n 0)
      (progn 
      (setf (aex-head e) (list v))
      v)
  (setf (aref (aex-arr e) (1- n)) v)))

;; This would be faster if it were not defmfun1
(defmfun1 ($aex_unshift :doc) ( v (e :aex_adj) )
  :desc ("Destructively pushes an element " :arg "v" " onto the end
 of " :argdot "e" " The return value is " :argdot "v" 
 " For array representation of expressions we use the words "
  " `push' and `pop' for the beginning of and expression, and "
 "`shift' and `unshift' for the end of an expression, whether the "
 "representation is an array or a list. This is consistent with "
 " maxima, but the reverse of the meaning of the terms in perl.")
  (vector-push-extend  v (aex-arr e)))

(examples::clear-examples "aex_unshift")
(examples::add-example "aex_unshift"
                       '( 
                         :vars "[a]"
                         :code "a : lrange(10,ot->ar), aex_unshift(\"dog\",a), a"))

(defmfun1 ($aex_shift :doc) ( (e :aex_adj) )
   :desc ("destructively removes an element from the end
 of " :argdot "e"  
  " For array representation of expressions we use the words "
  " `push' and `pop' for the beginning of and expression, and "
 "`shift' and `unshift' for the end of an expression, whether the "
 "representation is an array or a list. This is consistent with "
 " maxima, but the reverse of the meaning of the terms in perl.")
  (vector-pop (aex-arr e)))

(examples::clear-examples "aex_shift")
(examples::add-example "aex_shift"
                       '(:vars "[a,b]"
                         :code ( "a : lrange(10,ot->ar)"
                                 "b : aex_shift(a)"
                                 "a")))

;; moved to aex-base.lisp
;; this is used
;;(defun aex-length (e)
;;  (length (aex-arr e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get and set  parts of nested structures
;; that mix lexpr and aexpr.

;; is used
(defmacro aexg-int (e n)
  `(if (= ,n 0)
      (aex-head ,e)
      (if (> ,n (length (aex-arr ,e)))
          (merror1 "ipart: part ~a does not exist~%" ,n)
          (aref (aex-arr ,e) (1- ,n)))))

;; is used
(ddefun i-part (e inds)
    "ipart returns the part of <e> specified by the list <inds>.
    <e> is a mixed representation expression."
        (let ((i (first inds)))
;;          (declare (fixnum i)) why remove this ?
;; f+ is a maxima function or macro that uses the fixnum, I believe
          (when (< i 0) (setf i (f+ (ilength e) i 1)))
          (let ((opart
                  (cond ((aex-p e) (aexg-int e i))
                        ((consp e)  (nth i e ))
                        (t 
                         (throw 'ipart-error
                                (list 'ipart-error
                                      (format nil "Can't find part ~a of ~a" i ($sconcat e))))))))
;                         (merror1 "ipart: Can't find part ~a of ~a~%" i ($sconcat e))))))
                (if (null (cdr inds)) (if (= 0 i) (getop (car opart)) opart)
                    (i-part opart (cdr inds))))))

;(defmspec $ipart2 (x)
;  (setf x (cdr x))
;  (dbind (e &rest inds) x
;         (i-part (meval e) (mapcar #'meval  inds))))

;; is used
;; this is faster, why were we doing the above ?
;; Note, use as lvalue is implemented in aex/mset.lisp
(defmfun1 $ipart (e &rest inds)
  (let ((res (catch 'ipart-error
               (i-part e inds))))
    (if (and (consp res) (eq (car res) 'ipart-error))
        (defmfun1-error-final '$no_part (second res))
      res)))

(add-doc-entry "ipart" )
(add-call-desc '("ipart" ("e" "ind1" "ind2" "..." )
 ("Returns the part of expression " :arg "e" " specified by indices. "
  :arg "e" " may be a mixed (lex and aex) representation expression. When used as an lvalue, ipart
  can be used to assign to a part of an expression. If an index is negative, then
  it counts from the end of the list. If " :arg "e" " is an ordinary maxima list (lex),
  then using a negative index is potentially slower than using a positive index because
  the entire list must first be traversed in order to determine it's length. If "
  :arg "e" " is in aex representation, then this inefficiency is not present.")))

(max-doc:implementation "ipart" '( 
 "Some tests were performed with large lists of numbers. If
  we set " :codecomma "a:lrange(10^7)" " then the times required for "
 :codecomma "ipart(a,10^7)" " " :codecomma "ipart(a,-1)" " "
 :codecomma "inpart(a,10^7)" " and " :code "part(a,10^7)" 
 " were " :math "30" ", " :math "60" ", " :math "90" ", and "
 :math "90" " ms."))

(examples:clear-add-example "ipart"
                       '( :pretext "Destructively assign to a part of an exression."
                          :vars "[a]"
                          :code "(a : [1,2,3], ipart(a,1) : 7, a)"))

;; is used
(defun ilength (e)
  (if (aex-p e) (length (aex-arr e)) (length (cdr e))))

;; is used
(defmfun1 ($ilength :doc)  ((e :or-non-mapatom-subvar))
 :desc ("Returns the length of the expression " :argdot "e" " This is like maxima "
 :emref "length" ", but here, " :arg "e" " can be either an aex or a lex.")
  (if (aex-p e) (length (aex-arr e))
     (progn (setf e (if ($listp e) e
                           (specrepcheck e)))
             (length (margs e)))))

;; this is broken sometimes. dont use it i think
;; esp when compiled. hmm maybe it was only order of appearance
;; is used
(defmacro if-aex (x c1 &optional c2)
  `(if (aex-p ,x) ,c1 ,c2))

;; is used
(ddefun i-part-set (e val inds)
 "Set part of <e> specified by the list <inds> to <val>.
 <e> is a mixed representation expression. i-part-set is
  call by mset or msetq."
  (let ((i (car inds)))
    (when (< i 0) (setf i (f+ (ilength e) i 1)))
;;    (declare (fixnum i))
    (if (null (cdr inds))
        (if (aex-p e) ($aexs e i val)
          (setf (elt e i) val))
      (let ( (opart
              (if-aex e ($aexg e i )
                        (nth i e ))))
        (i-part-set opart val (cdr inds))))))

;; is used
(defmfun $ipart_set (e val &rest inds)
  (i-part-set e val inds))
(add-doc-entry "ipart_set")
(add-call-desc '("ipart_set" ("e" "val" "ind1" "ind2" "...")
     ("Set part of " :arg "e" " specified by the final arguments to " :arg "val" ". "
 :arg "e" " is a mixed representation expression.")))

; should add error checking, etc. In fact this should
; be written as a more general macro

;; not used anywhere
;; note there is multiple evaluation here.
(defmacro defmfun-cond-to-ae (e)
 "Conditionally convert output to array expression.
  For use with defmfun-ae, etc. This evaluates
  its arg mulitple times."
  `(if o-type-p
       (if (eq o-type '$ar)
           (setf ,e ($aex ,e))
           (setf ,e ($lex ,e)))))

;; is used
(defmacro defmfun-final-to-ae (e)
 "Conditionally convert output to array expression.
  For use with defmfun-ae, etc. Single evaluation of e."
  `(if o-type-p
       (if (eq o-type '$ar)
           ($aex ,e)
           ($lex ,e))
       ,e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; following three macros work together
;; These are older code. should not be used.

;; basically not used
;; used only in $apply, which is currently commented out
(defmacro defaesimp (name args &rest body)
  "We want to get rid of these. functions"
  `(defmfun ,name (,@args &rest ropts &aux o-type i-type )
     (if ropts
         (ecase (car ropts)
           ($oar
            (setf o-type 'ar))
           ('$oml
            (setf o-type 'ml))))
     ,@body))

;; used only in apply_a in system-essential.lisp, which is not used
;; This looks slick, but it's a bad idea to automatically
;; output the input form
(defmacro aesimp-out (oe)
    `(cond ((eq o-type 'ar)
            ($aex ,oe))
           ((eq o-type 'ml)
            ($lex ,oe))
           ((eq i-type 'ar)
            ($aex ,oe))
           (t ,oe)))

;; used only in apply_a in system-essential.lisp, which is not used
(defmacro aesimp-in-to-ml (ein)
  `(progn (if (aex-p ,ein) (setf i-type 'ar))
          (setf ,ein ($lex ,ein ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; need to push to &aux, not set it!!
;; used only in apply_a in system-essential.lisp, which is not used
(defmacro defmfun-ae-in (name args &rest body)
  `(defmfun-ae ,name (,@args &aux i-type )
     ,@body))

;; used only in apply_a in system-essential.lisp, which is not used
(defmacro aesimp-out-1 (oe)
    `(cond ((eq o-type '$ar)
            ($aex ,oe))
           ((eq o-type '$ml)
            ($lex ,oe))
           ((eq i-type '$ar)
            ($aex ,oe))
           (t ,oe)))

;; used only in apply_a in system-essential.lisp, which is not used
(defmacro aesimp-in-to-ml-1 (ein)
  `(progn (if (aex-p ,ein) (setf i-type '$ar))
          (setf ,ein ($lex ,ein ))))

;; is used
;; not sure why these are defined here. Existing functions were not
;; adequate ?
;; These should be moved or not used or something!!
;; probably to save a function call or two ???
(defun lbfloatp (x)
  (eq (caar x) 'bigfloat))

;; is used
(defun lratp (x)
  (eq (caar x) 'mrat))

;; is used
(defun lspecrepp (x)
  (or (lbfloatp x) (lratp x)))

;; is used
(defmfun-aeo ($aex :doc) (&optional x )
  (cond ((null x) (make-aex))
        ((and (listp x) (not (lspecrepp x)))
          (make-aex :head (car x) :adjustable adj-type
                                :arr (aex-copy-to-vector (cdr x) (if adj-type
                   (make-array (length (cdr x)) :adjustable t :fill-pointer t)
                   (make-array (length (cdr x)))))))
          (t x)))
(add-call-desc '("aex" ("e")
   ("Converts expression " :arg "e" " to an array representation."
    " The input expression " :arg "e" " is returned unchanged if it is already an "
    "array expression or is a symbol or number or specially represented maxima expression.
    This function converts only at the first level.")))

;; is used
;; non-maxima version of above
(defun aex-to  (x &key (adjustable t) (element-type t))
;;  (format t "elements ~a~%"  element-type)
  (cond 
    ((and (listp x) (not (lspecrepp x)))
     (make-aex :head (car x) :adjustable adjustable
        :arr (aex-copy-to-vector (cdr x)
            (if adjustable
                (make-array (length (cdr x)) :adjustable t :fill-pointer t :element-type element-type)
                (make-array (length (cdr x)) :element-type element-type)))))
    (t x)))


;; not used anywhere
;; just for convenience
(defmfun-aeo-adj-false $aex_adj_false (&optional x )
  (cond ((null x) (make-aex))
        ((and (listp x) (not (lspecrepp x)))
          (make-aex :head (car x) :adjustable adj-type
                                :arr (aex-copy-to-vector (cdr x) (if adj-type
                   (make-array (length (cdr x)) :adjustable t :fill-pointer t)
                   (make-array (length (cdr x)))))))
          (t x)))

;; copy list to a vector.
;; use this because initializing array with intial contents is so slow in gcl
;; assume 'to' is a sequence already large enough to hold 'from'
;; Maybe gcl has fixed this and we can use it optionally
(defun aex-copy-to-vector (from to)
  (when (listp from)
      (do* ( (e from (cdr e))
             (i 0 (1+ i))
            (x (car from) (car e)))
          ( (null e) to)
        (declare (fixnum i))
        (setf (elt to i) x))))

;; is used
(defmfun1 ($aex_new :doc) ((n :non-neg-int) &optional (head mlist)
                           &opt ($type t) ($init nil init-supplied-p ))
  :desc
  ("makes a new aex object. The default op is list. Default type is any (true).
    Default initial element is the lisp default.")
  (aex-make-n-head n :head `(,head simp) :element-type $type
                   :initial-element $init :supply-init-element init-supplied-p ))

;; is used. 
(defmfun1 ($copy_aex_type :doc) ((ein :aex))
  "Create a new aex with same head,length,adjustability,etc.
   but contents of expression are not copied."
  (make-aex :head (aex-head ein) :arr (copy-array-type (aex-arr ein))
            :adjustable (aex-adjustable ein)))

(defun copy-aex-type (ein)
  "Same as $copy_aex_type, but to be called from lisp."
  (make-aex :head (aex-head ein) :arr (copy-array-type (aex-arr ein))
            :adjustable (aex-adjustable ein)))

;; is used
(defun aex-copy-new-n (e &optional (n (length (aex-arr e))) )
  "Copy e, but with uninitialized data array of length n."
  (make-aex :head (aex-head e) :adjustable (aex-adjustable e)
            :arr (make-array n :adjustable (aex-adjustable e))))

;; is used
(defun aex-copy-no-data (e)
  "Copy e, but with no data array set"
  (make-aex :head (aex-head e) :adjustable (aex-adjustable e)))

;; is used
(defun aex-copy-only-array (e &optional (n (length (aex-arr e)))  )
  "Note: this returns a lisp array, not an aex. But it is the same
  adjustability (maybe fill pointer or whatever later) as e, with
   the array having length n."
  (make-array n :adjustable (aex-adjustable e)))

;; is used
(defun canon-level-spec (s)
 "Convert level specification to a canonical form.
  Input is either nil, or a number, or an mlist.
  If argumet is 
  nil -> (1 1) = convert at top lev
  one number -> (1 s) = convert from levels 1 through s
  a list of one number -> (s s) convert only on level s
  a list of two numbers -> (1 s) convert on levels 1 through s."
  (if (null s) `(1 1)
    (if ($listp s)
        (if (null (cddr s))
            (list (second s) (second s))
          (rest s))
      (list 1 s))))

(defun canon-level-spec-lisp (s)
 "Same as above, but use lisp lists rather than mlists."
  (if (null s) `(1 1)
    (if (listp s)
        (if (null (cdr s))
            (list (first s) (first s))
          s)
      (list 1 s))))

;; is used
(defmfun1 ($raex :doc) (e &optional spec )
  :desc ("Convert " :arg "e" " to aex representation at the specified levels.")
  (setf spec (canon-level-spec spec))
  (raex1 e 1 (first spec) (second spec)))

(defun raex (x &optional spec )
  "Same as above, but call from lisp code."
  (setf spec (canon-level-spec-lisp spec))
  (raex1 x 1 (first spec) (second spec)))

;; is used
;; convert to aexpr at all levels
(defmfun1 ($faex :doc) (e)
 :desc ("deep copy " :arg "e" " converting to aex representation at all levels.")
  (raex1 e 1 1 '$inf))

;; is used
(defun raex1 (x d d1 d2)
  (declare (fixnum d d1))
  (if (and (not (eq d2 '$inf)) (> d d2)) x
      (cond (($mapatom x) x)
            ((aex-p x)
             (if (and (not (eq d2 '$inf)) (= d d2)) x
                 (let* ((len (aex-length x))
                        (xout (copy-aex-type x))
                        (arin (aex-arr x))
                        (arout (aex-arr xout)))
                   (declare (fixnum len))
                   (incf d)
                (dotimes (i len)
                  (declare (fixnum i))
                  (setf (aref arout i) (raex1 (elt arin i) d d1 d2 )))
                xout)))
            ((and (not (eq d2 '$inf)) (= d d2)) (aex-to x))
            (t
             (if (< d d1)
                  (do* ((xc (cdr x) (cdr xc))
                        (item (car xc) (car xc))
                        (xout))
                       ( (null xc) (cons (car x) (nreverse xout)))
                    (setf xout (cons (raex1 item (1+ d) d1 d2 ) xout)))
                  (let* ((xin (cdr x))
                         (len (length xin)) ; not best way maybe
                         (xout (aex-make-n-head len :head (car x)))
                         (ar (aex-arr xout))
                         (xin (cdr x)))
                    (declare (fixnum len))
                    (incf d)
                    (dotimes (i len)
                      (declare (fixnum i))
                      (setf (aref ar i) (raex1 (car xin) d d1 d2 ))
                      (setf xin (cdr xin)))
                    xout))))))

;; is used
(defmfun1 ($rlex :doc) (e &optional spec )
  :desc ("Deep copy, converting " :arg "e" " to lex representation at the specified levels.") 
  (setf spec (canon-level-spec spec))
  (rlex1 e 1 (first spec) (second spec)))

(defun rlex (x &optional spec )
  "Same as above, but call from lisp code."
  (setf spec (canon-level-spec-lisp spec))
  (rlex1 x 1 (first spec) (second spec)))

;; is used
(defmfun1 ($flex :doc) (e)
 :desc ("deep copy " :arg "e" " converting to lex representation at all levels.")
  (rlex1 e 1 1 '$inf))

;; is used
(defun rlex1 (x d d1 d2)
  (if (and (not (eq d2 '$inf)) (> d d2))  x
    (if ($mapatom x) x
      (let* ( (h (op-a x))
              (args (if (aex-p x)
                        (aex-arr x) (cdr x)))
              (n (length args)))
        (if (or (>= d d1) (not (aex-p x)))
            (let ((ar nil))
              (dotimes (i n)
                (setf ar (cons (rlex1 (elt args i) (1+ d) d1 d2 ) ar)))
              (cons h (nreverse ar)))
          (let ((ar (make-array n :adjustable t )))
            (dotimes (i n)
              (setf (aref ar i) (rlex1 (elt args i) (1+ d) d1 d2 )))
            (aex-mk-head-args h ar)))))))

(add-doc-entry "lex")
(defmfun $lex ( &optional x)
  (cond ((aex-p  x)
         (cons (aex-head x) (coerce (aex-arr x) 'list)))
        (t x)))
(add-call-desc '("lex" ("e") ("converts the aex expression " :arg "e" " to lex.
 If " :arg "e" " is not an aex expression, " :arg "e" " is returned.
 Conversion is only done on the first level.")))

;; is used
(defun aex-lex  (&optional x)
  "This is only because there is another function called $lex in package sym."
  (cond ((aex-p  x)
         (cons (aex-head x) (coerce (aex-arr x) 'list)))
        (t x)))

(meval* '( ($matchfix) "<<" ">>" ))

(defmfun $<< (&rest e)
  (if (length-eq e 1)
      ($aex (first e))
    ($aex (mk-mlist e))))

;; prevent these operators from being clobbered by kill(all)
(push "<<" *mopl*)
(push ">>" *mopl*)

(mext:add-to-dont-kill "<<" ">>")

;; Yes, this is used when display2d:true
;; This converts the expression to normal maxima
;; expression and just puts a tilde in front
(defmfun $print_aex (x strm depth)
  "This only marks the outermost expression with a tilde."
;;  (declare (ignore depth)) ;  eh ccl doesn't like this
  (format strm "~~~a" ($sconcat ($flex x))))

;; is used
;; same as redefinition of $op, which we have disabled
(defmfun1 ($aeop :doc) ( (expr :non-mapatom) )
 :desc (  "op function for aex. returns op if " :arg "e" " is not an aex.")
  (if-aex expr (aex-op expr)
            ($part expr 0)))

;; used only in iargs, which is used in rtests
(defun aex-cp-args (x)
  "return lisp array copy of aex expr"
  (when (aex-p x) (gjl::copy-array (aex-arr x))))

;; is used
(defun aex-mk-head-args (h args)
  (make-aex :head h :arr args ))

;; is used
(defmfun-aeo ($aex_cp :doc) ( (e :non-mapatom)  &optional head)
  (when (and head (atom head)) (setf head (list head 'msimp)))
  (cond ((aex-p e)
         (make-aex :head (if head head (aex-head e)) :arr (gjl::copy-array (aex-arr e))
                   :adjustable (if adj-type-p adj-type (aex-adjustable e))))
        (t
         (when head (setf e (cons head (cdr e))))
         ($aex e (rule-opt '$adj adj-type)))))

(add-call-desc '("aex_cp" ("e")
    ("Returns an aex form copy of " :arg "e" ". " :arg "e" " may be in either lex or aex form."
      " Conversion to aex representation occurs only on the first level." )))

;; create aex expression by copying or converting. if head, adjustability , are specified
;;  then change these, otherwise copy them
;; is used
(defun aex-cp (e &key (adjustable t adj-type-p) (head nil) (element-type t))
  (cond ((aex-p e)
         (make-aex :head (if head head (aex-head e)) :arr (gjl::copy-array (aex-arr e))
                   :adjustable (if adj-type-p adjustable (aex-adjustable e)) ))
        (t
         (when head (setf e (cons head (cdr e))))
         (aex-to e :adjustable adjustable :element-type element-type))))

;; not used anywhere
;; copy only aex type
(defmfun $aex_copy_tree (e)
  (if-aex e
            (let* ((ar (aex-arr e))
                   (n (length ar))
                   (e1 (copy-aex-type e))
                   (ar1 (aex-arr e1))
                   (x))
              (dotimes (i n)
                (setf x (elt ar i))
                (setf (elt ar1 i)
                      (if-aex x ($aex_copy_tree x) x)))
              e1)
            e))

;; used in rtest
;; This would probably be more efficient if the recursively called function
;; were just a defun, so that argument checking is not done.
;; !!! It seems that CL builtin copy-tree already handles all data types,
;; struct's, arrays, etc. No need for all this.
(defmfun1 ($deep_copy :doc) (expr)
  :desc ( 
    "Note: it appears that the core maxima function " :emref "copy" " acheives the same "
    "result as " :mrefcomma "deep_copy" " so that the latter is redundant. It will probably be removed. "
    :mref  "deep_copy"
    " returns a copy of expression " :arg "expr" " which may be of mixed lex/aex representation. "
    "An exact copy is made; that is, the representation is preserved at all levels. "
    :mref "deep_copy" " is similar to " :emrefcomma "copylist" " except that it can copy "
    " some expressions that " :emref "copylist" " cannot. For instance, if "
    :arg "expr" " is of aex representation at the top level.")
  (cond ((aex-p expr)
         (let* ((ar (aex-arr expr))
                (n (length ar))
                (e1 (copy-aex-type expr))
                (ar1 (aex-arr e1))
                (x))
           (dotimes (i n)
             (setf x (elt ar i))
             (setf (elt ar1 i)
                   (if (or (listp x) (aex-p x)) ($deep_copy x) x )))
           e1))
        ((listp expr)
         (do* ((e1 expr (cdr e1))
               (res)
               (x (car expr) (car e1)))
             ((null e1) (nreverse res))
           (setf res (cons (if (or (listp x) (aex-p x)) ($deep_copy  x) x  ) res))))
        (t expr)))

;; not used
(defmfun $aeargs (x)
  (if (aex-p x) (aex-mk-head-args '(mlist simp) (aex-cp-args x))
    ($args x)))

;; not used
(defmfun $aelength (x)
  (if-aex x (length (aex-arr x))
            ($length x)))

;; is used
(defmfun $ae_listp (e)
  (if (aex-p e) (eq (car (aex-head e)) 'mlist)
      ($listp e)))

;; not used
;; adjustable-array-p works as expected in sbcl, fails for gcl
;; But it appears the standard allows this, so it's rather useless.
;; So we store it in the struct and hope that we can keep track of it.
(defmfun $aex_adj_p (x)
  (aex-adjustable x))
;;  (adjustable-array-p (aex-arr x)))

;; is used
(defun op-a (e)
  (if (aex-p e) (aex-head e)
            (car e)))

;; not used
(defun op-name-a (e)
  (if (aex-p e) (car (aex-head e))
            (caar e)))

;; Moved these from elsewhere.
;; is used in rtests
(defmfun1 ($iop :doc) ((expr :or-non-mapatom-subvar))
  :desc ("returns the main operator of the expression " :arg "expr"
  ", which may be either a lex or aex expression. This works like `op', but is more general.")
  (if-aex expr (getop (car (aex-head expr)))
            ($part expr 0)))

;; is used in rtests
(defmfun-ae ($iargs :doc) ((e :or-non-mapatom-subvar))
  :desc 
  ("returns a list of the arguments of " :argcomma "e"
   " which may be either a lex or aex expression. This works "
   " like " :emrefcomma "args" " but is more general.")
  (defmfun-final-to-ae
      (if-aex e (aex-mk-head-args '(mlist simp) (aex-cp-args e))
              (progn (atomchk (setq e (format1 e)) '$args nil)
                     (mk-mlist (margs e))))))

;; in progress. add list of lists which can be of mixed representation.
;; Avoid using the simplifer with aex expressions.
;; (defun iadd (argsin)
;;   (let* ((args
;;          (loop for arg in argsin collect
;;                (if (aex-p arg) (aex-arr arg) (cdr arg))))
;;         (n (loop for arg in args 
;;                   (if (aex-p arg) (aex-arr arg) (cdr arg))))
;;     (cons '(mlist simp)
;;           (loop 
