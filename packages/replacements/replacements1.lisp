;; src/comm.lisp
;; we don't ratdisrep inside aex objects.
;; need to consider this. maybe a flag.
;; consider making this defmfun1 with arg checks and
;; match_form capability
(mext::no-warning
(defmfun $length (e)
  (setq e (cond (($listp e) e)
		((or $inflag (not ($ratp e))) (specrepcheck e))
		(t ($ratdisrep e))))
  (cond ((symbolp e) (merror (intl:gettext "length: argument cannot be a symbol; found ~:M") e))
        ((aex-p e) (length (aex-arr e)))
	((or (numberp e) (eq (caar e) 'bigfloat))
	 (if (and (not $inflag) (mnegp e))
	     1
	     (merror (intl:gettext "length: argument cannot be a number; found ~:M") e)))
	((or $inflag (not (member (caar e) '(mtimes mexpt) :test #'eq))) (length (margs e)))
	((eq (caar e) 'mexpt)
	 (if (and (alike1 (caddr e) '((rat simp) 1 2)) $sqrtdispflag) 1 2))
	(t (length (cdr (nformat e)))))))

;; don't use :or-non-mapatom-subvar, it excludes 1/4
;; triggers a bug in maxima testsuite
(mext::no-warning
(defmfun-ae ($args) (e)
  (defmfun-final-to-ae
      (if-aex e (aex-mk-head-args '(mlist simp) (aex-cp-args e))
              (progn (atomchk (setq e (format1 e)) '$args nil)
                     (mk-mlist (margs e)))))))

;; need to put error checking in the aex code
(mext::no-warning
(defmfun $rest (e &optional (n 1 n?))
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
     (cond ((and n? (not (fixnump n)))
	    (merror (intl:gettext "rest: second argument, if present, must be an integer; found ~M") n))
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

; this is not quite the same because it returns an element
; from the array, whicle memalike returns the cdr of a cons cell.
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
(mext::no-warning
(defmfun $member (x e)
  (if (aex-p e)
      (if (memalike-array ($totaldisrep x) (aex-arr e)) t nil)
    (progn
      (atomchk (setq e ($totaldisrep e)) '$member t)
      (if (memalike ($totaldisrep x) (margs e)) t)))))
