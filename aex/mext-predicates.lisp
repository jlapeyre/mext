(in-package :maxima)
(mext:mext-optimize)

(max-doc:set-cur-sec 'max-doc::predicates-fandv)
(defmfun1:set-mext-package "aex")

(defmfun1 ($cmplength :doc) (e (n 0 :non-neg-int)) ; 0 to quiet compiler
  :desc ( "return the smaller of " arg "n" " and " codedot ("length(" arg "e" ")")
  " This is useful if " arg "e" " is very large and " arg "n" " is small, so that
   computing the entire length of " arg "e" " is inefficient. Expression "
  arg "e" " can be either a list or an array.")
  (declare (fixnum n))
  (if (aex-p e)
      (let ((len (length (aex-arr e))))
        (if (> len n) n len))
    (do ( (e e (cdr e))
          (i 0 (1+ i) ))
        ( (or (= n i) (null e)) (if (null e) (1- i) i))
      (declare (fixnum i)))))

(max-doc::implementation "cmplength" "cmplength is implemented with defmfun1, which slows
  things down a bit. So be cautious using it in a tight loop.")

(defmfun1 ($length_eq :doc) ( (e :or-string-non-atom) (n 0 :non-neg-int)) ; 0 to quiet compiler
  :desc ("Returns true if " arg "e" " is of length " argcomma "n" 
  " false otherwise. This implementation traverses no more
 elements of " arg "e" " than necessary to return the result.")
  (declare (fixnum n))
  (cond ((listp e)
         (do ((e e (cdr e))
              (i 0 (1+ i) )
              (n1 (1+ n)))
             ((or (= n1 i) (null e)) (and (= n1 i) (null e)))
           (declare (fixnum i n1))))
        ((aex-p e) (= n (length (aex-arr e))))
        (t (= n (length e)))))

(max-doc::implementation "length_eq" "length_eq is implemented with defmfun1, which slows
  things down a bit. So be cautious using it in a tight loop.")
  
(defmfun1 ($length1p :doc) ( (e :or-string-non-atom))
  :desc ("Returns true if " arg "e" " is of length 1, false otherwise. This implementation traverse no more
 elements of " arg "e" " than necessary to return the result.")
  (cond ((listp e)
         (and (not (null (cdr e))) (null (cddr e))))
        ((aex-p e)
         (= 1 (length (aex-arr e))))
        (t  ; try it anyway
         (= 1 (length e)))))

(max-doc::implementation "length1p" "length1p is implemented with defmfun1, which slows
  things down a bit. So be cautious using it in a tight loop.")

(defmfun1 ($length0p :doc) ( (e :or-string-non-atom))
  "Returns true if <e> is of length 0, false otherwise. This implementation traverse no more
  elements of <e> than necessary to return the result."
  (cond ((listp e)
         (null (cdr e)))
        ((aex-p e) (= 0 (length (aex-arr e))))
        (t  (= 0 (length e)))))

(max-doc::implementation "length0p" "length0p is implemented with defmfun1, which slows
  things down a bit. So be cautious using it in a tight loop.")

(max-doc::see-also-group '( "length0p" "cmplength" "length_eq" "length1p"))

(defmfun1 ($type_of :doc) (e &optional verbose)
 :desc ("Return something like the 'type' of a maxima expression. This
 is a bit ill defined currently. " mref "type_of" " uses the lisp function type-of.")
  (cond ( (aex-p e)
          (if verbose (list '(mlist simp) ($op e) 'aex)
            ($aeop e)))
#+(or sbcl ecl)       ( (stringp e) 'string)
        ( (atom e)
          (type-of e))
        (($mapatom e)
         (cond (($bfloatp e)
                'bfloat)
               (t
                (let (( res ($op e)))
                  (if (and verbose (not (eq (caar e) res)))
                      (list '(mlist simp) res (caar e))
                    ($op e))))))
        ( ($ratp e)
                '$cre)
        (t
         (let (( res ($op e)))
           (if (and verbose (not (eq (caar e) res)))
               (list '(mlist simp) res (caar e))
             ($op e))))))

(examples::clear-examples "type_of")
(examples::add-example "type_of" 
                       '( :code ("type_of(1)" "type_of(1.0)" "type_of(1.0b0)" "type_of(1/3)"
                                 "type_of(\"dog\")" "type_of([1,2,3])" "type_of(aex([1,2,3]))"
                                 "type_of(%e)"  "type_of(%i)" "type_of(%i+1)"))
                       '( :pretext "type_of returns the type of the lisp struct corresponding to a maxima object."
                            :code-res ( ("load(graphs)$" nil)
                                        ("type_of(new_graph())" "  graph"))))

