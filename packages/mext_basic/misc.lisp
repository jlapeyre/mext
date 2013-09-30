;;;  Copyright (C) (2012,2013) John Lapeyre. Licensed under GPL, v3 or greater. See the file
;;;  `LICENSE' in this directory.

(in-package :maxima)

(max-doc:set-cur-sec 'max-doc::runtime-fandv)
(defmfun1:set-file-and-package "misc.lisp" "mext_basic")

#|

(defmfun1 ($format1 :doc) (expr)
  :desc ("This calls the lisp function format1. It is mostly for testing code.")
  (format1 expr))

(defmfun1 ($nformat :doc) (expr)
  :desc ("This calls the lisp function nformat. It is mostly for testing code.")
  (nformat expr))

|#

(defmfun1 ($zerop :doc) (x)
  :desc 
  ("Returns " :code "true" " if " :arg "x" 
   " represents the number zero, otherwise, " :codedot "false"
   " This does not try to manipulate " :arg "x" " to determine
    if it can be reduced to the number zero.")
  (cond ((numberp x) (zerop x))
        (($bfloatp x) (zerop (cadr x)))
;        (($ratp x) (equal 0 (cadr x)))
        (($ratp x) (zerop (cadr x))) ; when would this fail ?
        (t nil)))

;; This is from RJF
;; zerop(x):=if bfloatp(x) then ?zerop(?cadr (x))
;;            else if ?numberp(x) then ?zerop(x)
;;            else if ratp(x) then ?equal(0, ?cadr (x))
;;            else false;

;; we should use recur-apply or something here.
;; this should apply at all levels of an expression.
(defmfun1 ($chop :doc) ((x :thread) &optional (eps 1e-10 :to-non-neg-float-bfloat))
  :desc
  ("Return " :code "0" " if " :arg "x" " is a float or bigfloat closer to zero than
    the optional argument " :argdot "eps")
  (cond ((and (numberp x)
              (let ((eps1 (if ($bfloatp eps) ($float eps) eps)))
                (<= (abs x) eps1))) 0)
        ((and ($bfloatp x) (fplessp (fpabs (cdr x)) (cdr ($bfloat eps)))) 0)
        (t x)))

;; iparse_string and ieval_string are the same as stock maxima,
;; but they get defmfun1 error checking and threading.

;; copied from eval_string.lisp, because we would need to autoload that
;; somehow. Note that this function will not give correct behavior on
;; all input.
(defun mext::ensure-terminator (s)
  (cond
    ((or (search "$" s :test #'char-equal) (search ";" s :test #'char-equal))
     s)
    (t
      (concatenate 'string s "$"))))

(defmfun1 ($iparse_string :doc) ((string :string :thread))
  :desc
  ("Slightly enhanced version of " :emrefdot "parse_string")
  (declare (special *mread-prompt*))
  (with-input-from-string
    (ss (mext::ensure-terminator string))
    (third (let ((*mread-prompt*)) (mread ss)))))

(defmfun1 ($ieval_string :doc) ((string :string :thread))
  :desc
  ("Slightly enhanced version of " :emrefdot "eval_string")
  (meval ($iparse_string string)))

(defmfun1 ($string_length :doc) ((string :string :thread))
  :desc
  ("Returns the number of characters in " :argdot "string")
  (length string))
