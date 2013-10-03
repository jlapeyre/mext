;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These are modified versions of stock maxima functions. Many are just
;;; wrapped in defmfun1 to get argument checking, returning unevaluated
;;; forms, etc.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; src/comm.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; similar to memalike in src/mutils.lisp
;; this is not quite the same because it returns an element
;; from the array, while memalike returns the cdr of a cons cell.
(defun memalike-array (x a &aux el)
  (dotimes (i (length a))
    (when (alike1 x (setf el (aref a i))) (return-from memalike-array el)))
  nil)

;; alias madness screws up error reporting and return of of form 
(mext::no-warning
(defmfun1 $substitute (old new &optional (expr nil three-arg?))
  (cond (three-arg? (maxima-substitute old new expr))
	(t
	 (let ((l old) (z new))
	   (cond ((and ($listp l) ($listp (cadr l)) (null (cddr l)))
		  ($substitute (cadr l) z))
		 ((notloreq l) (improper-arg-err l '$substitute))
		 ((eq (caar l) 'mequal) (maxima-substitute (caddr l) (cadr l) z))
		 (t (do ((l (cdr l) (cdr l)))
			((null l) z)
		      (setq z ($substitute (car l) z))))))))))

;; alias madness screws up error reporting and return of of form 
(mext::no-warning
(defmfun1 $psubstitute (old new &optional (expr nil three-arg?))
  (cond (three-arg? (maxima-substitute old new expr))
        (t
         (let ((l old) (z new))
           (cond ((and ($listp l)
                       ($listp (cadr l))
                       (null (cddr l)))
                  ;; A nested list.
                  ($psubstitute (cadr l) z))
                 ((and ($listp l)
                       (eq (caar (cadr l)) 'mequal)
                       (null (cddr l)))
                  ;; A list with one equation.
                  ($psubstitute (cadr l) z))
                 ((notloreq l) (improper-arg-err l '$psubstitute))
                 ((eq (caar l) 'mequal)
                  ;; Do a substitution for one equation.
                  (maxima-substitute (caddr l) (cadr l) z))
                 (t
                  ;; We have a list of equations. We do parallel subsitution.
                  (let (gensymbol genlist eqn ($simp nil))
                    ;; At first substitute a gensym for the expressions of
                    ;; the left hand side of the equations.
                    (do ((l (cdr l) (cdr l)))
                        ((null l) z)
                      (setq eqn (car l))
                      (when (not (eq 'mequal (caar eqn)))
                        (improper-arg-err old '$substitute))
                      (setq gensymbol (gensym))
                      ;; Store the gensym and the new expression into a list.
                      (push (cons gensymbol (caddr eqn)) genlist)
                      ;; Substitute a gensym for the old expression.
                      (setq z (maxima-substitute gensymbol (cadr eqn) z)))
                      ;; Substitute the new expressions for the gensyms.
                      (do ((l genlist (cdr l)))
                          ((null l)
                           ;; Resimplify the result.
                           (let (($simp t)) (resimplify z)))
                        (setq z (maxima-substitute (cdar l) (caar l) z)))))))))))


(mext::no-warning
(defmfun1 $depends (&rest args)
  (when (oddp (length args))
    (defmfun1-error-return '$depends_odd_num_args
                        $depends "odd number of arguments given"))
;    (merror (intl:gettext "depends: number of arguments must be even.")))
  (do ((args args (cddr args))
       (l))
      ((null args) (i-$dependencies (nreverse l)))
    (if ($listp (first args))
	(mapc #'(lambda (e) (push (depends1 e (second args)) l))
	      (cdr (first args)))
	(push (depends1 (first args) (second args)) l)))))

#|
(defmfun $diff (&rest args)
  #-gcl
  (declare (dynamic-extent args))
  (let (derivlist)
    (deriv args)))
|#

;(mext::no-warning
; (defmfun1 $del (e)
;   (stotaldiff e)))

(mext::no-warning
(defmfun1 $trunc (e)
  (cond ((atom e) e)
	((eq (caar e) 'mplus) (cons (append (car e) '(trunc)) (cdr e)))
	((mbagp e) (cons (car e) (mapcar #'$trunc (cdr e))))
	((specrepp e) ($trunc (specdisrep e)))
	(t e))))


(mext::no-warning
(defmfun1 $dispterms (e)
  (cond ((or (atom e) (eq (caar e) 'bigfloat)) (displa e))
	((specrepp e) ($dispterms (specdisrep e)))
	(t (let (($dispflag t))
	     (mterpri)
	     (displa (getop (mop e)))
	     (do ((e (if (and (eq (caar e) 'mplus) (not $powerdisp))
			 (reverse (cdr e))
			 (margs e))
		     (cdr e))) ((null e)) (mterpri) (displa (car e)) (mterpri)))
	   (mterpri)))
  '$done))

(mext::no-warning
(defmfun1 $dispform (e &optional (flag nil (:member '(nil $all)) flag?))
  (when (and flag? (not (eq flag '$all)))
    (merror (intl:gettext "dispform: second argument, if present, must be 'all'; found ~M") flag))
  (if (or (atom e)
	  (atom (setq e (if flag? (nformat-all e) (nformat e))))
	  (member 'simp (cdar e) :test #'eq))
      e
      (cons (cons (caar e) (cons 'simp (cdar e)))
	    (if (and (eq (caar e) 'mplus) (not $powerdisp))
		(reverse (cdr e))
		(cdr e))))))

;; could put error checking here
;; and return %%integer or something for integers
;; or at least wrap part in handler
;; but this all depends on inflag
(mext::no-warning
(defmfun1 $op (expr)
  ($part expr 0)))

(mext::no-warning
(defmfun1 $operatorp (expr oplist)
  (if ($listp oplist)
      ($member ($op expr) oplist)
      (equal ($op expr) oplist))))

(mext::no-warning
(defmfun1 $listp (x)
  (and (not (atom x))
       (not (atom (car x)))
       (eq (caar x) 'mlist))))

(mext::no-warning
(defmfun1 $cons (x (e :atomchk))
  (atomchk (setq e (specrepcheck e)) '$cons t)
  (mcons-exp-args e (cons x (margs e)))))

(mext::no-warning
(defmfun1 $endcons (x (e :atomchk))
  (atomchk (setq e (specrepcheck e)) '$endcons t)
  (mcons-exp-args e (append (margs e) (ncons x)))))

(mext::no-warning
(defmfun1 $reverse ((e :atomchk))
  (atomchk (setq e (format1 e)) '$reverse nil)
  (mcons-exp-args e (reverse (margs e)))))

;;; append() would take some work

;; ($totaldisrep e) is slow on a very large list of numbers
;; well, maybe as fast as can be expected.
;; This will fail for some e that are aex because
;; we don't yet call totaldisrep.
;; but aex objects are meant for efficiency for long lists.
;; maybe we need a flag for this.
;; need to remove duplicate argument checks
(mext::no-warning
(defmfun1 $member (x (e :atomchk-ext)) ; same as atomchk, but allow aex
  (if (aex-p e)
      (if (memalike-array ($totaldisrep x) (aex-arr e)) t nil)
    (progn
      (atomchk (setq e ($totaldisrep e)) '$member t)
      (if (memalike ($totaldisrep x) (margs e)) t)))))

(mext::no-warning
 (defmfun1 $first ((e :atomchk))
   (atomchk (setq e (format1 e)) '$first nil)
   (if (null (cdr e)) ; (merror (intl:gettext "first: empty argument.")))
       (defmfun1-error-return '$index_too_big
                            $first "part specification fell off end"))
   (car (margs e))))

;; we also made this more efficient
(mext::no-warning
(macrolet ((make-nth (si i)
	     (let ((sim (intern (concatenate 'string "$" (symbol-name si)))))
	       `(defmfun1 ,sim ((e :atomchk))
		  (atomchk (setq e (format1 e)) ',sim nil)
                  (handler-case
                   (elt (margs e) ,i)
                   (error ()
                          (defmfun1-error-return '$index_too_big
                            ,sim "part specification fell off end")))))))
;                          (merror (intl:gettext "~:M: no such element in ~M") ',sim e)))))))
  (make-nth second  1)
  (make-nth third   2)
  (make-nth fourth  3)
  (make-nth fifth   4)
  (make-nth sixth   5)
  (make-nth seventh 6)
  (make-nth eighth  7)
  (make-nth ninth   8)
  (make-nth tenth   9)))

;; need to put error checking in the aex code
;; with defmfun1, some checks are redundant
(mext::no-warning
(defmfun1 $rest (e &optional (n :integer 1 n?))
  (prog (m fun fun1 revp)
     (when (and n? (equal n 0))
       (return e))
     (when (aex-p e)
       (let* ((are (aex-arr e))
              (newn (- (length are) (abs n)))
              (a (aex-copy-new-n e newn))
              (ar (aex-arr a)))
         (if (> n 0)
             (dotimes (i newn)
               (setf (aref ar i) (aref are (+ n i))))
             (dotimes (i newn)
               (setf (aref ar i) (aref are i))))
         (return a)))
     (atomchk (setq m (format1 e)) '$rest nil)
     (cond ; ((and n? (not (fixnump n))) GJL removed because defmfun1
	   ; (merror (intl:gettext "rest: second argument, if present, must be an integer; found ~M") n))
	   ((minusp n)
	    (setq n (- n) revp t)))
     (if (< (length (margs m)) n)
	 (if $partswitch
	     (return '$end)
	     (merror (intl:gettext "rest: fell off the end."))))
     (setq fun (car m))
     (when (eq (car fun) 'mqapply)
       (setq fun1 (cadr m)
	     m (cdr m)))
     (setq m (cdr m))
     (when revp (setq m (reverse m)))
     (setq m (nthcdr n m))
     (setq m (cons (if (eq (car fun) 'mlist) fun (delsimp fun))
		   (if revp (nreverse m) m)))
     (when (eq (car fun) 'mqapply)
       (return (cons (car m) (cons fun1 (cdr m)))))
     (return m))))

(mext::no-warning
(defmfun1 $last ((e :atomchk))
  (atomchk (setq e (format1 e)) '$last nil)
  (when (null (cdr e))
    (defmfun1-error-return '$index_too_big
      $last "part specification fell off end"))
;    (merror (intl:gettext "last: empty argument.")))
  (car (last e))))

;; src/comm.lisp
;; don't use :or-non-mapatom-subvar, it excludes 1/4
;; and triggers a bug in maxima testsuite
(mext::no-warning
(defmfun-ae ($args) ((e :atomchk-ext))
  (defmfun-final-to-ae
      (if-aex e (aex-mk-head-args '(mlist simp) (aex-cp-args e))
              (progn (atomchk (setq e (format1 e)) '$args nil)
                     (mk-mlist (margs e)))))))

;;; functions part,substpart, etc. determine
;;; whether to evaluate within mpart, so
;;; arg checking is difficult. it would take
;;; some rewriting.

(mext::no-warning
(defmfun1 $delete (x l &optional (n :non-neg-int -1 n?))
;  (when (and n? (or (not (fixnump n)) (minusp n))) ; if n is set, it must be a nonneg fixnum
;    (merror (intl:gettext "delete: third argument, if present, must be a nonnegative integer; found ~M") n))
  (atomchk (setq l (specrepcheck l)) '$delete t)
  (setq x (specrepcheck x)
	l (cons (delsimp (car l)) (copy-list (cdr l))))
  (do ((l1 (if (eq (caar l) 'mqapply) (cdr l) l)))
      ((or (null (cdr l1)) (zerop n)) l)
    (if (alike1 x (specrepcheck (cadr l1)))
	(progn
	  (decf n)
	  (rplacd l1 (cddr l1)))
	(setq l1 (cdr l1))))))

;; we don't ratdisrep inside aex objects.
;; need to consider this. maybe a flag.
;; consider making this defmfun1 with arg checks and
;; match_form capability
;; add patch so that inflag:false; length(-1/4) --> 1
;; this passes testsuite
;; should send this to devels
;; Added a patch
;; The current version passes tests.

(mext::no-warning
(defmfun1 ($length :doc) (e)
  :desc
  ("modified version of stock Maxima " :emrefdot "length"
   " Here, " :code "length(e)" " gives " :code "0" " for
   numbers and strings. It should probably give a non-zero
   result for rationals, because part can access the
   numerator and denominator. It would be better to treat
   all numbers consistently, but it is probably too late for that.")
  (setq e (cond (($listp e) e)
		((or $inflag (not ($ratp e))) (specrepcheck e))
		(t ($ratdisrep e))))
  (cond ((symbolp e) 0) ; (merror (intl:gettext "length: argument cannot be a symbol; found ~:M") e))
        ((aex-p e) (length (aex-arr e)))
        (($numberp e) (if (and (not $inflag) (mnegp e)) 1 0))
;	((or (numberp e) (and (consp e) (eq (caar e) 'bigfloat)))
;         (if (and (not $inflag) (mnegp e)) 1 0))
        ((atom e) 0)
	      ; (merror (intl:gettext "length: argument cannot be a number; found ~:M") e)))
;	((or $inflag (not (member (caar e) '(mtimes mexpt) :test #'eq)))
;         (length (margs e)))
	((or $inflag (and (not (member (caar e) '(mtimes mexpt) :test #'eq))
              (not (and (eq 'rat (caar e)) (< (cadr e) 0))))) (length (margs e))) ; GJL 2013
	((eq (caar e) 'mexpt)
	 (if (and (alike1 (caddr e) '((rat simp) 1 2)) $sqrtdispflag) 1 2))
	(t (length (cdr (nformat e)))))))

(mext::no-warning
(defmfun1 $atom (x)
  (setq x (specrepcheck x)) (or (atom x) (eq (caar x) 'bigfloat))))

(mext::no-warning
(defmfun1 $symbolp (x)
  (setq x (specrepcheck x)) (symbolp x)))

(mext::no-warning
(defmfun1 $num (e)
  (let (x)
    (cond ((atom e) e)
	  ((eq (caar e) 'mrat) ($ratnumer e))
	  ((eq (caar e) 'rat) (cadr e))
	  ((eq (caar (setq x (nformat e))) 'mquotient) (simplify (cadr x)))
	  ((and (eq (caar x) 'mminus) (not (atom (setq x (cadr x))))
		(eq (caar x) 'mquotient))
	   (simplify (list '(mtimes) -1 (cadr x))))
	  (t e)))))

(mext::no-warning
(defmfun1 $denom (e)
  (cond ((atom e) 1)
	((eq (caar e) 'mrat) ($ratdenom e))
	((eq (caar e) 'rat) (caddr e))
	((or (eq (caar (setq e (nformat e))) 'mquotient)
	     (and (eq (caar e) 'mminus) (not (atom (setq e (cadr e))))
		  (eq (caar e) 'mquotient)))
	 (simplify (caddr e)))
	(t 1))))

(mext::no-warning
(defmfun1 $entier (e) (take '($floor) e)))

(mext::no-warning
(defmfun1 $fix (e) (take '($floor) e)))

(mext::no-warning
(defmfun1 $coeff (e x &optional (n 1))
  (if (equal n 0)
      (coeff e x 0)
      (coeff e (power x n) 1))))

(mext::no-warning
(let (my-powers my-num my-flag)
  (declare (special my-powers my-num my-flag))

  (defmfun1 $hipow (e var)
    (findpowers e t var))
  
  ;; These work best on expanded "simple" expressions.
  
  (defmfun1 $lopow (e var)
    (findpowers e nil var))
  
  (defun findpowers (e hiflg var)
    (let (my-powers my-num my-flag)
      (declare (special my-powers my-num my-flag))
      (findpowers1 e hiflg var)
      (cond ((null my-powers) (if (null my-num) 0 my-num))
  	  (t (when my-num (setq my-powers (cons my-num my-powers)))
  	     (maximin my-powers (if hiflg '$max '$min))))))
  
  (defun findpowers1 (e hiflg var)
    (cond ((alike1 e var) (checkpow 1 hiflg))
  	((atom e))
  	((eq (caar e) 'mplus)
  	 (cond ((not (freel (cdr e) var))
  		(do ((e (cdr e) (cdr e))) ((null e))
  		  (setq my-flag nil) (findpowers1 (car e) hiflg var)
  		  (if (null my-flag) (checkpow 0 hiflg))))))
  	((and (eq (caar e) 'mexpt) (alike1 (cadr e) var)) (checkpow (caddr e) hiflg))
  	((specrepp e) (findpowers1 (specdisrep e) hiflg var))
  	(t (mapc #'(lambda (x) (findpowers1 x hiflg var)) (cdr e)))))
  
  (defun checkpow (pow hiflg)
    (setq my-flag t)
    (cond ((not (numberp pow)) (setq my-powers (cons pow my-powers)))
  	((null my-num) (setq my-num pow))
  	(hiflg (if (> pow my-num) (setq my-num pow)))
  	((< pow my-num) (setq my-num pow))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; src/rpart.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; src/rpart.lisp

;; Let's try to make the behavior somewhat uniform

(mext::no-warning
(defmfun1 $rectform ((xx :possible-complex-number))
  (let ((ris (trisplit xx)))
    (add (car ris) (mul (cdr ris) '$%i)))))

;; original gave error with string.
;; not sure what is correct
(mext::no-warning
(defmfun1 $polarform ((xx :possible-complex-number))
  (cond ((and (not (atom xx)) (member (caar xx) '(mequal mlist $matrix) :test #'eq))
	 (cons (car xx) (mapcar #'$polarform (cdr xx))))
	(t
	 (let ((aas (absarg xx)) ($%emode nil))
	   (mul (car aas) (powers '$%e (mul '$%i (cdr aas)))))))))

;; original: realpart of string is string. imagpart is 0
;; this seems not useful
;; try restricting to no string
(mext::no-warning
 (defmfun1 $realpart ((xx :possible-complex-number)) (car (trisplit xx))))

(mext::no-warning
(defmfun1 $imagpart ((xx :possible-complex-number)) (cdr (trisplit xx))))

;; orig: cabs("cat") --> abs("cat")
;;
(mext::no-warning
 (defmfun1 $cabs ((xx :possible-complex-number)) (cabs xx)))

;; orig: error with string input
(mext::no-warning
 (defmfun1 $carg ((xx :possible-complex-number))
  (cond ((and (not (atom xx)) 
              (member (caar xx) '(mequal mlist $matrix) :test #'eq))
	 (cons (car xx) (mapcar #'$carg (cdr xx))))
	(t (cdr (absarg xx))))))
