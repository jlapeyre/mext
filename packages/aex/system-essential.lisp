;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modified maxima core functions. These can
;; potentially break maxima, but test suite passes till now.

;;; The formatting code is modified/modelled on code in grind.lisp

(in-package :maxima)
(mext:mext-optimize)

(defmfun1:set-file-and-package "system-essential.lisp" "aex")

;; [1,2] not alike <<1,2>>, but it probably should be
(mext::no-warning
(defmfun alike1 (x y)
  (cond ((eq x y))
	((atom x)
     (cond
       ((arrayp x)
	(and (arrayp y) (lisp-array-alike1 x y)))
       ( (aex-p x)              ;  NEW FOR AEX
         (and (aex-p y) (aex-alike1 x y)))  ;  NEW FOR AEX

    ;; NOT SURE IF WE WANT TO ENABLE COMPARISON OF MAXIMA ARRAYS
    ;; ASIDE FROM THAT, ADD2LNC CALLS ALIKE1 (VIA MEMALIKE) AND THAT CAUSES TROUBLE
    ;; ((maxima-declared-arrayp x)
    ;;  (and (maxima-declared-arrayp y) (maxima-declared-array-alike1 x y)))
    ;; ((maxima-undeclared-arrayp x)
    ;;  (and (maxima-undeclared-arrayp y) (maxima-undeclared-array-alike1 x y)))

       (t (equal x y))))
	((atom y) nil)
	(t (and (not (atom (car x)))
		(not (atom (car y)))
		(eq (caar x) (caar y))
		(eq (memqarr (cdar x)) (memqarr (cdar y)))
		(alike (cdr x) (cdr y)))))))


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

;;;  It appears that msize-atom must be changed, but not msize.

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
;;  Here we check for it, for compatibility with 5.28. We can remove it later.
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

(defun msize-aex (x l r)
  (let ((x1 (aex-lex x)))
    (cond
      ((safe-get (caar x1) 'grind)
       (let ((msize-fun (get (caar x1) 'grind )))
         (when (eq 'msize-matchfix msize-fun) (setf msize-fun 'msize-matchfix-aex))
         (the #-ecl (values t) #+ecl t (funcall msize-fun x1 l r))))
      (t (msize-function-aex x1 l r nil)))))

;; Using << >> instead of < >, allows us to copy output
;; and use it as input.
(defun msize-matchfix-aex (x l r)
  (let* ((ssym (strsym (caar x)))

; single < >
;         (sleft (cons #\< (car ssym)))
;         (sright (append (cdr ssym) (list #\>))))

; double << >>
         (sleft (cons #\< (cons #\< (car ssym))))
         (sright (append (cdr ssym) (list #\> #\>))))


    (setq l (nreconc l sleft)
          l (cons (length l) l)
          r (append sright r)
          x (msize-list (cdr x) nil r))
    (cons (+ (car l) (car x)) (cons l (cdr x)))))

(defun msize-function-aex (x l r op)
  (let* ((head (caar x)))
    (cond ((not (symbolp head)))
          ((and (get head 'verb) (get head 'alias))
           (setq l (revappend '(#\' #\') l)))
          ((and (get head 'noun) (not (member head (cdr $aliases) :test #'eq))
                (not (get head 'reversealias)))
           (setq l (cons #\' l))))

; single < >
;    (setq l (msize (if op (getop head) head) l (list #\< ) 'mparen 'mparen)
;          r (msize-list (cdr x) nil (cons #\> r)))

;; double << >>
    (setq l (msize (if op (getop head) head) l (list #\< #\< ) 'mparen 'mparen)
          r (msize-list (cdr x) nil (cons #\> (cons #\> r))))

    (cons (+ (car l) (car r)) (cons l (cdr r)))))

#| don't mess with apply now
(defaesimp $apply (fun arg)
  (aesimp-in-to-ml arg)
  (unless ($listp arg)
    (merror1 (intl:gettext "apply: second argument must be a list; found: ~M") arg))
  (let ( (oexpr
          (let ((fun-opr (getopr fun)))
            (autoldchk fun-opr)
            (mapply1 fun-opr (cdr arg) fun `(($apply) ,fun ,arg)))))
    (aesimp-out oexpr)))
|#

;;; Code below can probably be removed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;

;; not used
;; whats this ?
(defmfun-ae-in $apply_a (fun arg)
  (aesimp-in-to-ml-1 arg)
  (unless ($listp arg)
    (merror1 (intl:gettext "apply: second argument must be a list; found: ~M") arg))
  (let ( (oexpr
          (let ((fun-opr (getopr fun)))
            (autoldchk fun-opr)
            (mapply1 fun-opr (cdr arg) fun `(($apply) ,fun ,arg)))))
    (aesimp-out-1 oexpr)))

#|
(defun msize (x l r lop rop) 
  (setq x (nformat x))
  (cond  ;((aex-p x) (msize-aex x l r))
         ((atom x) (msize-atom x l r))
         ((and (atom (car x)) (setq x (cons '(mprogn) x)) nil))
         ((or (<= (lbp (caar x)) (rbp lop)) (> (lbp rop) (rbp (caar x))))
          (msize-paren x l r))
         ((member 'array (cdar x) :test #'eq) (msize-array x l r))
         ((safe-get (caar x) 'grind)
          (the #-ecl (values t) #+ecl t (funcall (get (caar x) 'grind) x l r)))
         (t (msize-function x l r nil))))
|#

#|

;; Not used now
(defun msize-good-kinda (x l r lop rop)
  (let ( ( aef (if (aex-p x) t nil)))
    (setf x (aex-lex x))
    (setq x (nformat x))
    (let ( (ol
            (cond ((atom x) (msize-atom x l r))
                  ((and (atom (car x)) (setq x (cons '(mprogn) x)) nil))
                  ((or (<= (lbp (caar x)) (rbp lop)) (> (lbp rop) (rbp (caar x))))
                   (msize-paren x l r))
                  ((member 'array (cdar x) :test #'eq) (msize-array x l r))
                  ((safe-get (caar x) 'grind)
                   (the #-ecl (values t) #+ecl t (funcall (get (caar x) 'grind) x l r)))
                  (t (msize-function x l r nil)))))
      (if aef 
          (cons (+ (car ol) 1) (cons (list 1 *aex-msize-char*) (rest ol)))
          ol))))

(defun msize-orig (x l r lop rop)  ; original code, just renamed
  (setq x (nformat x))
  (cond ((atom x) (msize-atom x l r))
        ((and (atom (car x)) (setq x (cons '(mprogn) x)) nil))
	((or (<= (lbp (caar x)) (rbp lop)) (> (lbp rop) (rbp (caar x))))
	 (msize-paren x l r))
	((member 'array (cdar x) :test #'eq) (msize-array x l r))
	((safe-get (caar x) 'grind)
	 (the #-ecl (values t) #+ecl t (funcall (get (caar x) 'grind) x l r)))
	(t (msize-function x l r nil))))

(defvar *aex-msize-char* #\~)

;; 
;; If we use this function, then we just convert aex to lex and there is no visual difference
(defun msize-just-convert-aex (x l r lop rop)
  (setf x (aex-lex x))
  (setq x (nformat x))
  (cond ((atom x) (msize-atom x l r))
        ((and (atom (car x)) (setq x (cons '(mprogn) x)) nil))
	((or (<= (lbp (caar x)) (rbp lop)) (> (lbp rop) (rbp (caar x))))
	 (msize-paren x l r))
	((member 'array (cdar x) :test #'eq) (msize-array x l r))
	((safe-get (caar x) 'grind)
	 (the #-ecl (values t) #+ecl t (funcall (get (caar x) 'grind) x l r)))
	(t (msize-function x l r nil))))

|#

;;;  I think this does not matter because we changed msize-atom instead
#|  5.26.0 broke what I had, which wasn't great, but ok
  this is  completely wrong
(defun msize (x l r lop rop)
  (format t "x: '~a' l: '~a' r: '~a' lop: '~a' rop: '~a'~%" x l r lop rop)
  (let ( ( aef (if (aex-p x) t nil)))
    (setf x (aex-lex x))
    (setq x (nformat x))
    (let ( (ol
            (cond ((atom x) (msize-atom x l r))
                  ((and (atom (car x)) (setq x (cons '(mprogn) x)) nil))
                  ((or (<= (lbp (caar x)) (rbp lop)) (> (lbp rop) (rbp (caar x))))
                   (msize-paren x l r))
                  ((member 'array (cdar x) :test #'eq) (msize-array x l r))
                  ((safe-get (caar x) 'grind)
                   (the #-ecl (values t) #+ecl t (funcall (get (caar x) 'grind) x l r)))
                  (t (msize-function x l r nil)))))
      (format t " ** ol ~a~%" ol)
      (if aef
          (let ((e1 (car ol))
                (e2 (cadr ol))
                (e3 (cddr ol)))
            (format t "e1: ~a e2 ~a e3 ~a~%" e1 e2 e3)
            (cons e1 (cons e2 
               (cons (+ (car e3) 1) (cons (list 1 *aex-msize-char*) (rest e3)))))))
      ol)))

|#

#|
(defun msize-5-25-1 (x l r lop rop)
;;  (format t "x ~a l ~a r ~a lop ~a rop ~a~%" x l r lop rop)
  (let ( ( aef (if (aex-p x) t nil)))
    (setf x (aex-lex x))
    (setq x (nformat x))
    (let ( (ol
            (cond ((atom x) (if fortranp (msz (makestring x) l r) (msize-atom x l r)))
                  ((and (atom (car x)) (setq x (cons '(mprogn) x)) nil))
                ((or (<= (lbp (caar x)) (rbp lop)) (> (lbp rop) (rbp (caar x))))
                 (msize-paren x l r))
                ((member 'array (cdar x) :test #'eq) (msize-array x l r))
                ((safe-get (caar x) 'grind)
                 (the #-ecl (values t) #+ecl t (funcall (get (caar x) 'grind) x l r)))
                (t (msize-function x l r nil)))))
      (if aef 
          (cons (+ (car ol) 1) (cons (list 1 *aex-msize-char*) (rest ol)))
        ol))))
|#

;; following doesnt quite work right.
#|
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
           ((setq y (rassoc x aliaslist :test #'eq))
            (return (msize (car y) l r lop rop)))
           ((null (setq y (exploden x))))
           ((safe-get x 'noun) (return (msize-atom (get x 'noun) l r)))
           ((char= #\$ (car y)) (setq y (slash (cdr y))))
           ((member (marray-type x) '(array hash-table $functional))
            (return (msize-array-object x l r)))
           ((aex-p x) (return (msize (aex-lex x) l r)))
           (t (setq y (cons #\? (slash y)))))
     (return (msz y l r))))
|#
