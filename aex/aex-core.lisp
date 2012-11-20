;;; Copyright (C) 2012 John Lapeyre
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(in-package :maxima)
(declaim (optimize (speed 3) (space 0) (safety 0) (debug 0)))
(use-package :gjl.lisp-util)
(use-package :max-doc)

(max-doc::set-cur-sec 'max-doc::aex-fandv)

($put '$aex_package 0.1 '$version)

;;(defmfun1::set-source-file-name "aex-core.lisp")

(defvar *aex-core-dummy-vector* (make-array 0)
  "This is only here so that we can automatically check the type.")

(defstruct (aex
            (:print-function
             $print_aex))
  (head '(mlist simp) :type list)
  (arr *aex-core-dummy-vector* :type vector)
  (adjustable nil :type boolean))

;; Define a macro for defining functions that take options:
;; output representation and adjustability
;; defmfun1-opt takes : name, list of opts, code to be inserted before body. the ignorable is to quiet
;; the compilers.
(defmfun1-opt defmfun-ae ( (($ot o-type) $ml o-type-p :out-rep)
  (($adj adj-type) t adj-type-p :bool) )
  (declare (ignorable adj-type-p o-type-p adj-type )))


;; option for output adjustability
;; Normally used with routine that only outputs array rep expression
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
  of e is returned. No argument checking is peformed."))

(defmfun $aexg (e n)
  (if (= n 0)
      (aex-head e)
  (aref (aex-arr e) (1- n))))

(max-doc::see-also "aexg" '("aex_get" "ipart" "inpart" "part"))


(defun aeref (e n)
 "Get reference to element of array expression. n = 0
refers to the head."   
  (if (= n 0) (aex-head e)
    (aref (aex-arr e) (1- n))))

(defun aeref1 (e n)
  "Get reference to element of array expression. But don't allow getting head.
   This should be more efficient."
  (aref (aex-arr e) (1- n)))

(defsetf aeref (e n) (y) `(progn (if (= 0 ,n) (setf (aex-head ,e) ,y)
                                     (setf (aref (aex-arr ,e) (1- ,n)) ,y)) ,y))

(defsetf aeref1 (e n) (y) `(progn (setf (aref (aex-arr ,e) (1- ,n)) ,y) ,y))

;; this way of entering doc is ugly
(add-doc-entry1 :e  '( :name "aex_get"
                      :protocol "aex_get(e,n)"
                      :contents
                      " aex_get(e,n) returns the nth part of aexpr e. A value
 of 0 for n is not allowed. This is more efficient than aexg, which allows 
 n=0."))

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
                      " aex_set(e,n,v) destructively sets the nth part of aexpr e to value v. A value
of 0 for n is not allowed. This is more efficient than aexs. No argument checking is done."))

(defmfun $aex_set (e n v)
  (setf (aref (aex-arr e) n) v))

(examples::clear-examples "aex_set")
(examples::add-example "aex_set"
                       '( :pretext "Destructively assign to a part of an expression."
                         :vars "[a,x]"
                         :code "a : aex([1,2,3]), aex_set(a,1,x), a"))

(max-doc::see-also "aex_set" '("aexs" "ipart"))

(defmacro aex-get (e n)
  `(aref (aex-arr ,e) ,n))

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
                      " aex_set(e,n,v) destructively sets the nth part of aexpr e to value v. A value
of 0 for n returns the head (or op) of e."))

(defmfun $aexs (e n v)
  (if (= n 0)
      (progn 
      (setf (aex-head e) (list v))
      v)
  (setf (aref (aex-arr e) (1- n)) v)))

(defmfun1 ($aex_unshift :doc) ( v (e :aex_adj) )
  "aex_unshift destructively pushes an element <v> onto the end
 of <e>. The return value is <v>. For array representation of expressions we use
 push, pop for the beginning of and expression, and
 shift, unshift for the end of an expression, whether the
 representation is an array or a list. This is consistent with
 maxima, but the reverse of the meaning of the terms in perl."
  (vector-push-extend  v (aex-arr e)))

(examples::clear-examples "aex_unshift")
(examples::add-example "aex_unshift"
                       '( 
                         :vars "[a]"
                         :code "a : lrange(10,ot->ar), aex_unshift(\"dog\",a), a"))

(defmfun1 ($aex_shift :doc) ( (e :aex_adj) )
    "aex_shift destructively removes an element from the end
 of <e>. For array representation of expressions we use
 push, pop for the beginning of and expression, and
 shift, unshift for the end of an expression, whether the
 representation is an array or a list. This is the reverse
 of the meaning of the terms in perl."
  (vector-pop (aex-arr e)))

(examples::clear-examples "aex_shift")
(examples::add-example "aex_shift"
                       '(:vars "[a,b]"
                         :code ( "a : lrange(10,ot->ar)"
                                 "b : aex_shift(a)"
                                 "a")))


;; hmm , ever use this ?
(defun aex-length (e)
  (length (aex-arr e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get and set  parts of nested structures
;; that mix lexpr and aexpr.

(defmacro aexg-int (e n)
  `(if (= ,n 0)
      (aex-head ,e)
      (if (> ,n (length (aex-arr ,e)))
          (merror1 "ipart: part ~a does not exist~%" ,n)
          (aref (aex-arr ,e) (1- ,n)))))


(ddefun i-part (e inds)
        "ipart returns the part of <e> specified by the list <inds>.
<e> is a mixed representation expression."
        (let ((i (first inds)))
;;          (declare (fixnum i))
          (when (< i 0) (setf i (f+ (ilength e) i 1)))
          (let ( (opart
                  (cond ((aex-p e) (aexg-int e i))
                        ((consp e)  (nth i e ))
                        (t (merror1 "ipart: Can't find part of ~a~%" ($sconcat e))))))
                (if (null (cdr inds)) (if (= 0 i) (getop (car opart)) opart)
                    (i-part opart (cdr inds))))))

(defmspec $ipart (x)
  (setf x (cdr x))
  (dbind (e &rest inds) x
         (i-part (meval e) (mapcar #'meval  inds))))

  (add-doc-entry "ipart" )
(add-call-desc '("ipart" ("ind1" "ind2" "..." )
                ("Returns the part of expression " arg "e" " specified by indices. "
  arg "e" " is a mixed representation expression. When used as an lvalue, ipart
 can be used to assign to a part of an expression.")))

(examples::clear-examples "ipart")
(examples::add-example "ipart"
                       '( :pretext "Destructively assign to a part of an exression."
                         :vars "[a]"
                         :code "(a : [1,2,3], ipart(a,1) : 7, a)"))


(defun ilength (e)
  (if (aex-p e) (length (aex-arr e)) (length (cdr e))))

(defmfun1 ($ilength :doc)  ((e :or-non-atom-subvar))
 "Returns the length of the expression <e>. This is like maxima 'length',
 but here,  <e> can be either an aex or a lex."
  (if (aex-p e) (length (aex-arr e))
     (progn (setf e (if ($listp e) e
                           (specrepcheck e)))
             (length (margs e)))))

#|
          (progn
            (setq e (cond (($listp e) e)
                          ((or $inflag (not ($ratp e))) (specrepcheck e))
                          (t ($ratdisrep e))))
            (cond 
                  ((or (numberp e) (eq (caar e) 'bigfloat))
                   (if (and (not $inflag) (mnegp e))
                       1
                       (merror1 (intl:gettext "length: argument cannot be a number; found ~:M") e)))
                  ((or $inflag (not (member (caar e) '(mtimes mexpt) :test #'eq))) (length (margs e)))
                  ((eq (caar e) 'mexpt)
                   (if (and (alike1 (caddr e) '((rat simp) 1 2)) $sqrtdispflag) 1 2))
                  (t (length (cdr (nformat e))))))))
|#
;; this is broken sometimes. dont use it i think
;; esp when compiled. hmm maybe it was only order of appearance
(defmacro if-aex (x c1 &optional c2)
  `(if (aex-p ,x) ,c1 ,c2))

(ddefun i-part-set (e val inds)
 "Set part of <e> specified by the list <inds> to <val>.
 <e> is a mixed representation expression. i-part-set is
  call by mset or msetq."
  (let ((i (car inds)))
;;    (declare (fixnum i))
    (if (null (cdr inds))
        (if (aex-p e) ($aexs e i val)
          (setf (elt e i) val))
      (let ( (opart
              (if-aex e ($aexg e i )
                        (nth i e ))))
        (i-part-set opart val (cdr inds))))))


(defmfun $ipart_set (e val &rest inds)
  (i-part-set e val inds))
(add-doc-entry "ipart_set")
(add-call-desc '("ipart_set" ("e" "val" "ind1" "ind2" "...")
     ("Set part of " arg "e" " specified by the final arguments to " arg "val" ". "
 arg "e" " is a mixed representation expression.")))


; should add error checking, etc. In fact this should
; be written as a more general macro

#|
(defmacro old-defmfun-ae (name args &body body)
  (setf args (append args '(&opt (($ot o-type) $ml o-type-p :opt-out-rep)
                                 (($adj adj-type) t :opt-bool))))
  `(defmfun1 ,name ,args
     ,@body))
|#

;; note there is multiple evaluation here.
(defmacro defmfun-cond-to-ae (e)
 "Conditionally convert output to array expression.
  For use with defmfun-ae, etc. This evaluates
  its arg mulitple times."
  `(if o-type-p
       (if (eq o-type '$ar)
           (setf ,e ($aex ,e))
           (setf ,e ($lex ,e)))))


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

(defmacro defaesimp (name args &rest body)
  "We want to get rid of these. functions"
  `(defmfun ,name (,@args &rest ropts &aux o-type i-type )
     (if ropts
         (cond ( (eq (car ropts) '$oar)
                 (setf o-type 'ar))
               ( (eq (car ropts) '$oml)
                 (setf o-type 'ml))))
     ,@body))

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

(defmacro aesimp-in-to-ml (ein)
  `(progn (if (aex-p ,ein) (setf i-type 'ar))
          (setf ,ein ($lex ,ein ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; need to push to &aux, not set it!!
(defmacro defmfun-ae-in (name args &rest body)
  `(defmfun-ae ,name (,@args &aux i-type )
     ,@body))

(defmacro aesimp-out-1 (oe)
    `(cond ((eq o-type '$ar)
           ($aex ,oe))
          ((eq o-type '$ml)
           ($lex ,oe))
          ((eq i-type '$ar)
           ($aex ,oe))
          (t ,oe)))

(defmacro aesimp-in-to-ml-1 (ein)
  `(progn (if (aex-p ,ein) (setf i-type '$ar))
          (setf ,ein ($lex ,ein ))))

(defmfun-aeo ($aex :doc) (&optional x )
  (cond ((null x) (make-aex))
        ((and (listp x) (not ($lspecrepp x)))
          (make-aex :head (car x) :adjustable adj-type
                                :arr (aex-copy-to-vector (cdr x) (if adj-type
                   (make-array (length (cdr x)) :adjustable t :fill-pointer t)
                   (make-array (length (cdr x)))))))
          (t x)))
(add-call-desc '("aex" ("e")
                  ("converts expression " arg "e" " to an array representation."
" The input expression " arg "e" " is returned unchanged if it is already an "
"array expression or is a symbol or number or specially represented maxima expression.")))

;; non-maxima version of above
(defun aex-to  (x &key (adjustable t) (element-type 'any))
;;  (format t "elements ~a~%"  element-type)
  (cond 
    ((and (listp x) (not ($lspecrepp x)))
     (make-aex :head (car x) :adjustable adjustable
        :arr (aex-copy-to-vector (cdr x)
            (if adjustable
                (make-array (length (cdr x)) :adjustable t :fill-pointer t :element-type element-type)
                (make-array (length (cdr x)) :element-type element-type)))))
    (t x)))


;; just for convenience
(defmfun-aeo-adj-false $aex_adj_false (&optional x )
  (cond ((null x) (make-aex))
        ((and (listp x) (not ($lspecrepp x)))
          (make-aex :head (car x) :adjustable adj-type
                                :arr (aex-copy-to-vector (cdr x) (if adj-type
                   (make-array (length (cdr x)) :adjustable t :fill-pointer t)
                   (make-array (length (cdr x)))))))
          (t x)))

;; copy list to a vector.
;; use this because initializing array with intial contents is so slow in gcl
;; assume 'to' is a sequence already large enough to hold 'from'
(defun aex-copy-to-vector (from to)
  (if (listp from)
      (do* ( (e from (cdr e))
             (i 0 (1+ i))
            (x (car from) (car e)))
          ( (null e) to)
        (declare (fixnum i))
        (setf (elt to i) x))
    nil))


(defmfun1 ($aex_new :doc) ((n :non-neg-int) &optional (head mlist))
  (aex-make-n-head n :head `(,head simp)))

(defmfun1 ($copy_aex_type :doc) ((ein :aex))
  "Create a new aex with same head,length,adjustability,etc.
   but contents of expression are not copied."
  (make-aex :head (aex-head ein) :arr (copy-array-type (aex-arr ein))
            :adjustable (aex-adjustable ein)))

;;(defmfun-aeo ($aex_make_n_head :doc) (n head)
;;  (make-aex :head head :arr (make-array n :adjustable adj-type)
;;            :adjustable adj-type))

(defun aex-make-n-head (n &key (adjustable t)  (head '(mlist simp)))
  (make-aex :head head :arr (make-array n :adjustable adjustable)
              :adjustable adjustable))

(defun aex-copy-new-n (e &optional (n (length (aex-arr e))) )  ; lisp version of copy_aex_type above
  "Copy e, but with uninitialized data array of length n."
  (make-aex :head (aex-head e) :adjustable (aex-adjustable e)
            :arr (make-array n :adjustable (aex-adjustable e))))

(defun aex-copy-no-data (e)
  "Copy e, but with no data array set"
  (make-aex :head (aex-head e) :adjustable (aex-adjustable e)))

(defun aex-copy-only-array (e &optional (n (length (aex-arr e)))  )
  "Note: this returns a lisp array, not an aex. But it is the same
  adjustability (maybe fill pointer or whatever later) as e, with
   the array having length n."
  (make-array n :adjustable (aex-adjustable e)))

;; convert level specification to a canonical form
(defun canon-level-spec (s)
  (if (null s) `( 1 1)
    (if ($listp s)
        (if (null (cddr s))
            (list (second s) (second s))
          (rest s))
      (list 1 s))))

(defmfun $raex (x &optional spec )
  "Convert to aexpr at specified levels"
  (setf spec (canon-level-spec spec))
  ($raex1 x 1 (first spec) (second spec)))

;; convert to aexpr at all levels
(defmfun $faex (x)
  ($raex1 x 1 1 '$inf))

(defmfun $raex1 (x d d1 d2)
  (declare (fixnum d d1))
;;  (format t "****** ~a ~a ~a~%" d d1 d2)
  (if (and (not (eq d2 '$inf)) (> d d2)) x
      (cond (($mapatom x) x)
            ((aex-p x)
             (if (and (not (eq d2 '$inf)) (= d d2)) x
                 (let* ((len (aex-length x))
                        (xout ($copy_aex_type x))
                        (arin (aex-arr x))
                        (arout (aex-arr xout)))
                   (incf d)
                (dotimes (i len)
                  (setf (aref arout i) ($raex1 (elt arin i) d d1 d2 )))
                xout)))
            ((and (not (eq d2 '$inf)) (= d d2)) (aex-to x))
            (t
             (if (< d d1)
                  (do* ((xc (cdr x) (cdr xc))
                        (item (car xc) (car xc))
                        (xout))
                       ( (null xc) (cons (car x) (nreverse xout)))
                    (setf xout (cons ($raex1 item (1+ d) d1 d2 ) xout)))
                  (let* ((xin (cdr x))
                         (len (length xin)) ; not best way maybe
                         (xout (aex-make-n-head len :head (car x)))
                         (ar (aex-arr xout))
                         (xin (cdr x)))
                    (incf d)
                    (dotimes (i len)
                      (setf (aref ar i) ($raex1 (car xin) d d1 d2 ))
                      (setf xin (cdr xin)))
                    xout))))))

(defmfun $rlex (x &optional spec )
  (setf spec (canon-level-spec spec))
  ($rlex1 x 1 (first spec) (second spec)))

(defmfun $flex (x )
  ($rlex1 x 1 1 '$inf))

(defmfun $rlex1 (x d d1 d2)
  (if (and (not (eq d2 '$inf)) (> d d2))  x
    (if ($mapatom x) x
      (let* ( (h (op-a x))
              (args (if (aex-p x)
                        (aex-arr x) (cdr x)))
              (n (length args)))
        (if (or (>= d d1) (not (aex-p x)))
            (let ((ar nil))
              (dotimes (i n)
                (setf ar (cons ($rlex1 (elt args i) (1+ d) d1 d2 ) ar)))
              (cons h (nreverse ar)))
          (let ((ar (make-array n :adjustable t )))
            (dotimes (i n)
              (setf (aref ar i) ($rlex1 (elt args i) (1+ d) d1 d2 )))
            (aex-mk-head-args h ar)))))))

#|> Function lex
lex(<e>) Convert expression from aex to lex, ie standard maxima lisp list
expression. lex is the identity if <e> is not an aex.
|#
(add-doc-entry "lex")
(defmfun $lex ( &optional x)
  (cond ((aex-p  x)
         (cons (aex-head x) (coerce (aex-arr x) 'list)))
        (t x)))
(add-call-desc '("lex" ("e") ("converts the aex expression " arg "e" " to lex.
 If " arg "e" " is not an aex expression, " arg "e" " is returned.")))

(defun aex-lex  (&optional x)
  "This is only because there is another function called $lex in package sym."
  (cond ((aex-p  x)
         (cons (aex-head x) (coerce (aex-arr x) 'list)))
        (t x)))

(meval* '( ($matchfix) "<<" ">>" ))

(defmfun $<< (&rest e)
  (if (length-eq e 1)
      ($aex (first e))
    ($aex (cons '(mlist simp) e))))


(defmfun $print_aex (x strm depth)
  "This only marks the outermost expression with a tilde."
;;  (declare (ignore depth)) ;  eh ccl doesn't like this
  (format strm "~~~a" ($sconcat ($flex x))))

;; same as redefinition of $op, which we have disabled
(defmfun1 ($aeop :doc) ( (expr :non-atom) )
  "aeop(<e>) op function for aex. returns op if <e> is not an aex."
  (if-aex expr (getop (car (aex-head expr)))
            ($part expr 0)))

(defun aex-cp-args (x)
  "return lisp array copy of aex expr"
  (if (aex-p x) (copy-array (aex-arr x)) nil))

(defun aex-mk-head-args (h args)
  (make-aex :head h :arr args ))

(defmfun-aeo ($aex_cp :doc) ( (e :non-atom)  &optional head)
  (if (and head (atom head)) (setf head (list head 'msimp)))
  (cond ( (aex-p e)
          (make-aex :head (if head head (aex-head e)) :arr (copy-array (aex-arr e))
                    :adjustable (if adj-type-p adj-type (aex-adjustable e))))
        ( t
          (if head (setf e (cons head (cdr e))))
          ($aex e (rule-opt '$adj adj-type)))))

(add-call-desc '("aex_cp" ("e")
                 ("returns an aex form copy of " arg "e" ". " arg "e" " may be in either lex or aex form." )))

;; create aex expression by copying or converting. if head, adjutability , are specified
;;  then change these, otherwise copy them

(defun aex-cp (e &key (adjustable t adj-type-p) (head nil) (element-type 'any))
  (cond ( (aex-p e)
         (make-aex :head (if head head (aex-head e)) :arr (copy-array (aex-arr e))
                   :adjustable (if adj-type-p adjustable (aex-adjustable e)) ))
        ( t
         (if head (setf e (cons head (cdr e))))
         (aex-to e :adjustable adjustable :element-type element-type))))

;; copy only aex type
(defmfun $aex_copy_tree (e)
  (if-aex e
            (let* ((ar (aex-arr e))
                   (n (length ar))
                   (e1 ($copy_aex_type e))
                   (ar1 (aex-arr e1))
                   (x))
              (dotimes (i n)
                (setf x (elt ar i))
                (setf (elt ar1 i)
                      (if-aex x ($aex_copy_tree x) x)))
              e1)
            e))

(defmfun $alex_copy_tree (e)
   "Copy tree for mixed ml and ar expressions."
  (cond ( (aex-p e)
          (let* ((ar (aex-arr e))
                 (n (length ar))
                 (e1 ($copy_aex_type e))
                 (ar1 (aex-arr e1))
                 (x))
            (dotimes (i n)
              (setf x (elt ar i))
                (setf (elt ar1 i)
                      (if (or (listp x) (aex-p x)) ($alex_copy_tree x) x )))
            e1))
        ( (listp e)
          (do* ( (e1 e (cdr e1))
                (res)
                (x (car e) (car e1)))
              ( (null e1) (nreverse res))
            (setf res (cons (if (or (listp x) (aex-p x)) ($alex_copy_tree  x) x  ) res))))
        (t e)))
  
(defmfun $aeargs (x)
  (if (aex-p x) (aex-mk-head-args '(mlist simp) (aex-cp-args x))
    ($args x)))

(defmfun $aelength (x)
  (if-aex x (length (aex-arr x))
            ($length x)))

(defmfun $aex_p (x)
  (aex-p x))

(defmfun $ae_listp (e)
  (if (aex-p e) (eq (car (aex-head e)) 'mlist)
      ($listp e)))

;; adjustable-array-p works as expected in sbcl, fails for gcl
;; But it appears the standard allows this, so it's rather useless.
;; So we store it in the struct and hope that we can keep track of it.
(defmfun $aex_adj_p (x)
  (aex-adjustable x))
;;  (adjustable-array-p (aex-arr x)))

#| Function lbfloatp
lbfloatp(<e>) tests if <e> is a bigfloat assuming
that <e> is represented by a list.
|#
(defun $lbfloatp (x)
  (eq (caar x) 'bigfloat))

(defun $lratp (x)
  (eq (caar x) 'mrat))

(defun $lspecrepp (x)
  (or ($lbfloatp x) ($lratp x)))

(defun op-a (e)
  (if (aex-p e) (aex-head e)
            (car e)))

(defun op-name-a (e)
  (if (aex-p e) (car (aex-head e))
            (caar e)))


;; Moved these from elsewhere.
(defmfun1 $iop ((expr :or-non-atom-subvar))
  (if-aex expr (getop (car (aex-head expr)))
            ($part expr 0)))

(defmfun-ae $iargs (e)
  (defmfun-final-to-ae
      (if-aex e (aex-mk-head-args '(mlist simp) (aex-cp-args e))
              (progn (atomchk (setq e (format1 e)) '$args nil)
                     (cons '(mlist) (margs e))))))
; caught STYLE-WARNING:
;   The return value of NREVERSE should not be discarded.



;; this does not work with aex at deeper level
;;(defmfun $print_aex (x strm depth &aux arr h)
;;  (setf arr (aex-arr x))
;;  (setf h (aex-head x))
;;  (format strm "<<~a>>" ($sconcat (cons h (coerce arr 'list)))))

;; broke in maxima 5.26.0

;;(defmfun $print_aex (x strm depth)
;;  (declare (ignore strm depth))
;;  (displa ($flex x)))

