;; src/comm.lisp
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
;; triggers a bug
(mext::no-warning
(defmfun-ae ($args) (e)
  (defmfun-final-to-ae
      (if-aex e (aex-mk-head-args '(mlist simp) (aex-cp-args e))
              (progn (atomchk (setq e (format1 e)) '$args nil)
                     (mk-mlist (margs e)))))))


; This is trouble because it takes the cdr of maxima expression
; so what do we send to it ??
; I think we should make memalike-arrary that
; operates on a raw array.
;(defmfun memalike (x l)
;  (do ((l l (cdr l)))
;      ((null l))
;    (when (alike1 x (car l)) (return l)))))

;; probably not very fast!

;(defun memalike-array (x a)
;  (let ((res 
;         (dotimes (i (length a))
;           (when (alike1 x (aref a i)) (return (aref a i))))))
;    res))

;(mext::no-warning
;(defmfun $member (x e)
;  ((aex-p e) (length (aex-arr e)))
;  (atomchk (setq e ($totaldisrep e)) '$member t)
;  (if (memalike ($totaldisrep x) (margs e)) t)))