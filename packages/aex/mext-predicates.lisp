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

;; convert lisp symbol to maxima
;;  - --> _
;; prepend $LISP_
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
          (push (maxima-op-to-sym 'mrat) res))
;        (when (and (not (stringp e)) (atom e))
        (when (atom e)
          (when (not already-lisp-type)
            (let ((lt (lisp-type-of e)))
              (when (not (eq lt '$symbol))
                (push (lisp-type-of e) res)))))
        (if res
            (cons type res)
          type))
    type))

;;; The idea is give general maxima types, unless info is given,
;;; in which case we give more.
;;; info mixes both properties and representations, which should
;;; perhaps be separate.
;;; eg fixnum, aex, mrat, are representations,
;;; 'constant is a property

(defun poisp (e)
  (and (consp e) (eq (caar e) 'mpois)))

(defmfun1 ($poisp :doc) (e)
  :desc
  ("returns " :var "true" " if " :arg "e" " is in Poisson encoding, and "
   :varcomma "false" " otherwise.")
  (and (consp e) (eq (caar e) 'mpois)))

(defmfun1 ($rationalp :doc) (e)
  :desc
  (
   "Note: this is a duplicate of " :emrefcomma "ratnump" " probably. "
   "returns " :var "true" " if " :arg "e" " is (encoded as) a rational number, and "
   :varcomma "false" " otherwise."
   " Rational number encoding is distinct from canonical ration encoding (CRE).")
  (and (consp e) (eq (caar e) 'rat)))

(defun maxima-op-to-sym (h)
  (cond 
   ((eq h 'mlist) '$list)
   ((eq h 'mplus) '$plus)
   ((eq h 'mtimes) '$times)
   ((eq h 'mexpt) '$expt)
   ((eq h 'rat)   '$ratio)
   ((eq h 'mrat)  '$cre)
   ((eq h 'mpois)  '$pois)
   (t (lisp-sym-to-max h))))

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

 (let* ((add-info '())
        (e (cond ((poisp e) 
                  (push (maxima-op-to-sym 'mpois) add-info)
                  ($outofpois e))
                 (($ratp e) 
                  (push (maxima-op-to-sym 'mrat) add-info)
                  ($ratdisrep e))
                 ((aex-p e)
                  (push '$aex add-info)
                  ($lex e))
                 (t e)))
        (type-res
         (cond ((aex-p e) ; should never happen
               (if info (make-mlist-simp (aex-op e) '$aex)
                 (aex-op e)))
                                        ; this misses ratp strings
              #+(or sbcl ecl) ((stringp e)
                               (info-type-of e '$string info))
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
                      ; following should never happen
                     ((eq (caar e) 'mpois)
                      '$mpois)
                     (t
                      (let* ((the-op ($op e))
                             (real-op (caar e)))
                        (if (not (eq the-op real-op)) (maxima-op-to-sym real-op) the-op)))))
              ((atom e)
               (info-type-of e (lisp-type-of e) info))
              ; following should never happen
              (($ratp e) (info-type-of ($ratdisrep e) (maxima-op-to-sym (caar e)) info))
              (t
               (let* ((the-op ($op e))
                      (real-op (caar e)))
                 (if (not (eq the-op real-op)) (maxima-op-to-sym real-op) the-op))))))
   (if (consp type-res)
       (progn
         (when add-info
           (setf type-res (cons (car type-res) (append add-info (cdr type-res)))))
         (mk-mlist type-res))
     (if (and info add-info)
         (mk-mlist (cons type-res add-info))
       type-res))))
       
(examples::clear-examples "type_of")
(examples::add-example "type_of" 
                       '( :code ("type_of(1)" "type_of(1, info->true)" "type_of(1.0)" 
                                 "type_of(1.0b0)" "type_of(1/3)" "type_of(1/3, info->true)"
                                 "type_of(\"dog\")" "type_of([1,2,3])" "type_of(aex([1,2,3]))"
                                 "type_of(%e)"  "type_of(%i)" "type_of(%i+1)"))
                       '( :pretext "type_of returns the type of the lisp struct corresponding to a maxima object."
                            :code-res ( ("load(graphs)$" nil)
                                        ("type_of(new_graph())" "  graph"))))

(defmfun1 ($lratio :doc) ((expr :thread))
  :desc
  ("Return " :arg "expr" " converted, if possible, to lisp rational type."
   "Compare this to " :emrefcomma "rationalize" " which converts "
   "expressions to maxima rational types... Yes it is confusing.")
  (let ((re ($rationalize expr)))
    (if (and (consp re) (eq (caar re) 'rat))
        (/ (second re) (third re))
      re)))
  
(defmfun1 ($lcomplex :doc) ((expr :thread))
  :desc
  ("Return  maxima complex number " :arg "expr" " converted to a lisp complex number.")
  (let ((rp ($realpart expr))
        (ip ($imagpart expr)))
    (when (ratnump rp) (setf rp ($lratio rp)))
    (when (ratnump ip) (setf ip ($lratio ip)))
    (if (and (numberp rp) (numberp ip))
        (complex rp ip)
      expr)))

(defmfun1 ($mcomplex :doc) ((cnum :lisp-maxima-number :thread))
  :desc
  ("Return lisp complex number " :arg "cnum" " converted to a maxima complex number.")
  (let ((rp) (ip))
    (if (complexp cnum)
        (setf rp (realpart cnum) ip (imagpart cnum))
      (setf rp ($realpart cnum) ip ($imagpart cnum)))
    (meval `((mplus simp) ,rp ((mtimes simp) $%i ,ip)))))

(defmfun1 ($mratio :doc) ((expr :thread))
  :desc
  ("Convert " :arg "expr" " from lisp rational to maxima rational type.")
  (if (and (rationalp expr) (not (= 1 (denominator expr))))
     (meval `((rat simp) ,(numerator expr) ,(denominator expr)))
    ($rationalize expr)))

(defmfun1 ($meval :doc) (expr)
  :desc
  ("Evaluates the expression " :argdot "expr"
   " This runs the maxima evaluator " :code "meval"
   " once. This is after a suggestion by S. Macrakis")
  (meval expr))

;; Note, the following is almost the same as this
;; cpow2(x,n):=cabs(x)^n*exp(%i*carg(x)*n)
;; But, the former is can be used in more situations.
;; It works somehow better with the simplifier.
;; It looks like the maxima version is more useful
;; In fact, this should not be enabled until it
;; works better.

#|

(defmfun1 ($cpow :doc) (x pow)
  :desc
  ("Returns " :arg "x" " raised to the " :arg "pow"
   " power. This tries to assume complex domain and range.")
  (let ((aas (absarg x))) ; ($%emode nil)) do we want this or not ?
    (mul (powers (car aas) pow)
                (powers '$%e (mul '$%i (cdr aas) pow)))))

|#

; modeled on polar form
;(defmfun $polarform (xx)
;  (cond ((and (not (atom xx)) (member (caar xx) '(mequal mlist $matrix) :test #'eq))
;	 (cons (car xx) (mapcar #'$polarform (cdr xx))))
;	(t
;	 (let ((aas (absarg xx)) ($%emode nil))
;	   (mul (car aas) (powers '$%e (mul '$%i (cdr aas))))))))
