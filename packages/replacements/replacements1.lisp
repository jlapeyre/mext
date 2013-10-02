;; src/comm.lisp
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

;; src/comm.lisp
;; don't use :or-non-mapatom-subvar, it excludes 1/4
;; and triggers a bug in maxima testsuite
(mext::no-warning
(defmfun-ae ($args) ((e :atomchk-ext))
  (defmfun-final-to-ae
      (if-aex e (aex-mk-head-args '(mlist simp) (aex-cp-args e))
              (progn (atomchk (setq e (format1 e)) '$args nil)
                     (mk-mlist (margs e)))))))

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

;; similar to memalike in src/mutils.lisp
;; this is not quite the same because it returns an element
;; from the array, while memalike returns the cdr of a cons cell.
(defun memalike-array (x a &aux el)
  (dotimes (i (length a))
    (when (alike1 x (setf el (aref a i))) (return-from memalike-array el)))
  nil)

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
