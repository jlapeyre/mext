;;;  Copyright (C) (2012,2013) John Lapeyre. Licensed under GPL, v3 or greater. See the file
;;;  `LICENSE' in this directory.

(in-package :maxima)
(mext:mext-optimize)

(max-doc:set-cur-sec 'max-doc::predicates-fandv)
(defmfun1:set-file-and-package "mext-predicates.lisp" "aex")

;; used in rtests
(defmfun1 ($aex_p :doc) (e)
 :desc ("Returns true if " :arg "e" " is an aex expression, otherwise false.")
  (aex-p e))

(defmfun1 ($cmplength :doc) (e (n 0 :non-neg-int)) ; 0 to quiet compiler
  :desc ( "return the smaller of " :arg "n" " and " :codedot "length(e)"
  " This is useful if " :arg "e" " is very large and " :arg "n" " is small, so that
   computing the entire length of " :arg "e" " is inefficient. Expression "
  :arg "e" " can be either a lex of aex expression.")
  (declare (fixnum n))
  (if (aex-p e)
      (let ((len (length (aex-arr e))))
        (if (> len n) n len))
    (do ( (e e (cdr e))
          (i 0 (1+ i) ))
        ( (or (= n i) (null e)) (if (null e) (1- i) i))
      (declare (fixnum i)))))

(maxdoc:implementation "cmplength" "cmplength is implemented with defmfun1, which slows
  things down a bit. So be cautious using it in a tight loop.")

(defmfun1 ($length_eq :doc) ( (e :or-string-non-mapatom) (n 0 :non-neg-int)) ; 0 to quiet compiler
  :desc ("Returns true if " :arg "e" " is of length " :argcomma "n" 
  " false otherwise. This implementation traverses no more
 elements of " :arg "e" " than necessary to return the result.")
  (declare (fixnum n))
  (cond ((listp e)
         (do ((e e (cdr e))
              (i 0 (1+ i) )
              (n1 (1+ n)))
             ((or (= n1 i) (null e)) (and (= n1 i) (null e)))
           (declare (fixnum i n1))))
        ((aex-p e) (= n (length (aex-arr e))))
        (t (= n (length e)))))

(maxdoc:implementation "length_eq" "length_eq is implemented with defmfun1, which slows
  things down a bit. So be cautious using it in a tight loop.")
  
(defmfun1 ($length1p :doc) ( (e :or-string-non-mapatom))
  :desc ("Returns true if " :arg "e" " is of length 1, false otherwise. This implementation traverse no more
 elements of " :arg "e" " than necessary to return the result.")
  (cond ((listp e)
         (and (not (null (cdr e))) (null (cddr e))))
        ((aex-p e)
         (= 1 (length (aex-arr e))))
        (t  ; try it anyway
         (= 1 (length e)))))

(maxdoc:implementation "length1p" "length1p is implemented with defmfun1, which slows
  things down a bit. So be cautious using it in a tight loop.")

(defmfun1 ($length0p :doc) ( (e :or-string-non-mapatom))
  "Returns true if <e> is of length 0, false otherwise. This implementation traverse no more
  elements of <e> than necessary to return the result."
  (cond ((listp e)
         (null (cdr e)))
        ((aex-p e) (= 0 (length (aex-arr e))))
        (t  (= 0 (length e)))))

(maxdoc:implementation "length0p" "length0p is implemented with defmfun1, which slows
  things down a bit. So be cautious using it in a tight loop.")

(maxdoc:see-also-group '( "length0p" "cmplength" "length_eq" "length1p"))

(defun lisp-lisp-sym-to-max (s)
 "This certainly exists somewhere in the maxima source."
  (intern (concatenate 'string "$LISP_"
                       (coerce (loop for char across (symbol-name s)
                                     collect (if (eq char #\-) #\_ char)) 'string)) "MAXIMA"))

(defun lisp-type-of (e)
  (if (symbolp e)
      '$symbol
    (lisp-lisp-sym-to-max
     (let ((et (type-of e)))
            (if (consp et) (car et) et)))))

;; constantp seems a bit buggy
;; it seems to obvious that numbers and strings are constants.
;; we are interested in maxima constant symbols. maybe this
;; is not the right way.
(defun type-of-constant-p (e)
  (let ((res 
         (and (not (or (numberp e) (stringp e))) (mfuncall '$errcatch 
                   `($constantp ,e)))))
    (if (consp res) (cadr res) res)))

(defun type-of-rat-p (e)
  (let ((res 
         (mfuncall '$errcatch 
                   `($ratp ,e))))
    (if (consp res) (cadr res) res)))

;; should we report lisp's type-of for strings ?
(defun info-type-of (e type info-flag &optional already-lisp-type)
  (if info-flag
      (let ((res '()))
        (when (type-of-constant-p e)
          (push '$constant res))
        (when (type-of-rat-p e)
          (push '$mrat res))
;        (when (and (not (stringp e)) (atom e))
        (when (atom e)
          (when (not already-lisp-type)
            (let ((lt (lisp-type-of e)))
              (when (not (eq lt '$symbol))
                (push (lisp-type-of e) res)))))
        (if res
            (mk-mlist (cons type res))
          type))
    type))

;;; The idea is give general maxima types, unless info is given,
;;; in which case we give more.
;;; info mixes both properties and representations, which should
;;; perhaps be separate.
;;; eg fixnum, aex, mrat, are representations,
;;; 'constant is a property

(defmfun1 ($type_of :doc) (e &opt (($info info) nil :bool))
 :desc 
 ("Return something like the `type' of a maxima expression. "
  :mref "type_of" " uses the lisp function " 
 :codedot "type-of" " Currently, " :mref "type_of" " is a bit ill-defined; "
 " it may be better called `what-is-this?'."
 :par ""
 "If the option " :opt "info" " is true, then more information is returned on "
 "properties and the underlying maxima and lisp representations."
 " Lisp types are returned with " :code "lisp_" " prepended to the lisp symbol
 representing the type.")
 (cond ((aex-p e)
        (if info (make-mlist-simp (aex-op e) '$aex)
          (aex-op e)))
       ; this misses ratp strings
       #+(or sbcl ecl) ((stringp e)
                        (info-type-of e '$string info))
;        (cond ((symbolp e)
;               (if (and ($constantp e) info)
;                   (make-mlist-simp '$symbol '$constant)
;                 '$symbol))
;              (t (lisp-type-of e))))
;        (cond ((symbolp e)
;               (if (and ($constantp e) info)
;                   (make-mlist-simp '$symbol '$constant)
;                 '$symbol))
;              (t (lisp-type-of e))))
       (($mapatom e)
        (cond (($bfloatp e) '$bfloat)
              ((stringp e) (info-type-of e '$string info))
              ((floatp e) (info-type-of e '$float info))
              (($integerp e) (info-type-of e '$integer info))
;; rat(2) is both integerp and numberp
;; intopois(2) is neither
;; in fact, I can't find any maxima function to tell whether a number
;; is in pois representation.
;              (($numberp e) (info-type-of e '$number info))
              ((atom e)
               (info-type-of e (lisp-type-of e) info t))
              ((eq (caar e) 'mpois)
               '$mpois)
              (t
               (let ((res ($op e)))
                 (if (and info (not (eq (caar e) res)))
                     (make-mlist-simp res (lisp-sym-to-max (caar e)))
                   ($op e))))))
       ((atom e)
        (info-type-of e (lisp-type-of e) info))
       (($ratp e)
        '$mrat)
       (t
        (let (( res ($op e)))
          (if (and info (not (eq (caar e) res)))
              (make-mlist-simp res (lisp-sym-to-max (caar e)))
            ($op e))))))

(examples::clear-examples "type_of")
(examples::add-example "type_of" 
                       '( :code ("type_of(1)" "type_of(1, info->true)" "type_of(1.0)" 
                                 "type_of(1.0b0)" "type_of(1/3)" "type_of(1/3, info->true)"
                                 "type_of(\"dog\")" "type_of([1,2,3])" "type_of(aex([1,2,3]))"
                                 "type_of(%e)"  "type_of(%i)" "type_of(%i+1)"))
                       '( :pretext "type_of returns the type of the lisp struct corresponding to a maxima object."
                            :code-res ( ("load(graphs)$" nil)
                                        ("type_of(new_graph())" "  graph"))))
