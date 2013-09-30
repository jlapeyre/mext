;;;  Copyright (C) (2012,2013) John Lapeyre. Licensed under GPL, v3 or greater. See the file
;;;  `LICENSE' in this directory.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modified maxima core functions. These can
;; potentially break maxima, but test suite passes till now.

;;; The formatting code is modified/modelled on code in grind.lisp

(in-package :maxima)
(mext:mext-optimize)

(defmfun1:set-file-and-package "system-essential.lisp" "aex")

;; [1,2] not alike <<1,2>>, but it probably should be

;; src/simp.lisp
;; last synced with maxima source between 5.31.0 and 5.31.1
;; Sept 2013
(mext::no-warning
(defmfun alike1 (x y)
  (cond ((eq x y))
	((atom x)
     (cond
       ((arrayp x)
	(and (arrayp y) (lisp-array-alike1 x y)))
       ((aex-p x)              ;  NEW FOR AEX
         (and (aex-p y) (aex-alike1 x y)))  ;  NEW FOR AEX

    ;; NOT SURE IF WE WANT TO ENABLE COMPARISON OF MAXIMA ARRAYS
    ;; ASIDE FROM THAT, ADD2LNC CALLS ALIKE1 (VIA MEMALIKE) AND THAT CAUSES TROUBLE
    ;; ((maxima-declared-arrayp x)
    ;;  (and (maxima-declared-arrayp y) (maxima-declared-array-alike1 x y)))
    ;; ((maxima-undeclared-arrayp x)
    ;;  (and (maxima-undeclared-arrayp y) (maxima-undeclared-array-alike1 x y)))

       (t (equal x y))))
	((atom y) nil)
	((and
	  (not (atom (car x)))
	  (not (atom (car y)))
	  (eq (caar x) (caar y)))
         (cond
	  ((eq (caar x) 'mrat)
	   ;; Punt back to LIKE, which handles CREs.
	   (like x y))
	  (t (and
	      (eq (memqarr (cdar x)) (memqarr (cdar y)))
	      (alike (cdr x) (cdr y)))))))))

;; original code
(defun aex-alike1 (x y)
  ( if (equal (car (aex-head x)) (car (aex-head y)))
      (let ( (nx (length (aex-arr x)))
             (ny (length (aex-arr y)))
             (ay (aex-arr y))
             (ax (aex-arr x)))
        (declare (fixnum nx ny))
        (and
         (= nx ny)
         (progn
           (dotimes (i nx)
             (if (not (alike1 (elt ax i) (elt ay i)))
	  (return-from aex-alike1 nil)))
           t)))
    nil))

;; src/grind.lisp
;; dont nformat if aex, because that formats as
;; a normal maxima expression.
;;  We only want that with display2d:true.
;; msize is called with display2d:false and with sconcat
;;  There is probably a better way to do this.
(mext::no-warning
(defun msize (x l r lop rop)
  (when (not (aex-p x))
    (setq x (nformat x)))
  (cond ((atom x) (msize-atom x l r))
        ((and (atom (car x)) (setq x (cons '(mprogn) x)) nil))
	((or (<= (lbp (caar x)) (rbp lop)) (> (lbp rop) (rbp (caar x))))
	 (msize-paren x l r))
	((member 'array (cdar x) :test #'eq) (msize-array x l r))
	((safe-get (caar x) 'grind)
	 (the #-ecl (values t) #+ecl t (funcall (get (caar x) 'grind) x l r)))
	(t (msize-function x l r nil)))))

;;;  It appears that msize-atom must be changed, but not msize.
;;;  Now we do have to change msize, see above.

;; src/grind.lisp
;; last checked August 2013
(mext::no-warning
(defun msize-atom (x l r)
  (prog (y)
     (cond ((numberp x) (setq y (exploden x)))
           ((stringp x)
            (setq y (coerce x 'list))
            (do ((l y (cdr l))) ((null l))
              (cond ((member (car l) '(#\" #\\ ) :test #'equal)
                     (rplacd l (cons (car l) (cdr l)))
                     (rplaca l #\\ )
                     (setq l (cdr l)))))
            (setq y (cons #\" (nconc y (list #\")))))
           ((and (setq y (safe-get x 'reversealias))
                 (not (and (member x $aliases :test #'eq) (get x 'noun))))
            (setq y (exploden (stripdollar y))))
;;  Following two lines disappeared between 5.28 and 5.30, and aliaslist was removed
;;  Here we add the line again and check for existence
;;  of function aliaslist, for compatibility with 5.28. We can remove it later.
           ((and (boundp 'aliaslist) (setq y (rassoc x aliaslist :test #'eq)))
            (return (msize (car y) l r lop rop)))
           ((null (setq y (exploden x))))
           ((safe-get x 'noun) (return (msize-atom (get x 'noun) l r)))
           ((char= #\$ (car y)) (setq y (slash (cdr y))))
           ((member (marray-type x) '(array hash-table $functional))
            (return (msize-array-object x l r)))
           ((aex-p x) (return (msize-aex x l r)))   ; NEW FOR AEX
           (t (setq y (cons #\? (slash y)))))
     (return (msz y l r)))))

;; We want output to be usable as input.
;; hmm lop and rop ??, some kind of global variables ?
;; Note that they appear in msize-atom above, (copied
;; from the original) but their lexical scope is not
;; within msize-atom.
(defun msize-aex (x l r)
  (let* ((x1 (aex-lex x))
         (res1 (msize x1 (list (list 2 #\< #\<)) (list (list 2 #\> #\>)) lop rop))
         (res (cons 3 (append  l (list res1) r))))
    res))
