;;;  Copyright (C) (2012,2013) John Lapeyre. Licensed under GPL, v3 or greater. See the file
;;;  `LICENSE' in this directory.

(in-package :maxima)
(mext:mext-optimize)
(max-doc:set-cur-sec 'max-doc::number-theory-fandv)

(defmfun1:set-file-and-package "number-functions.lisp" "discrete_aex")

(defvar *constant-bigfloat-table* (make-hash-table))

(defvar *max-bigfloat-precision-table* (make-hash-table))

(defun set-bigfloat-hook (symbol func)
  (setf (gethash symbol *constant-bigfloat-table*) func))

(defun get-bigfloat-hook (symbol)
  (gethash symbol *constant-bigfloat-table*))

; should use functions as above
(defun set-max-bigfloat-precision (symbol val)
  (setf (gethash symbol *constant-bigfloat-table*) val))

; should use functions as above
(defun get-max-bigfloat-precision (symbol)
  (gethash symbol *constant-bigfloat-table*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; from_digits
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun from-digits2  ( digits &optional (base 10))
  "This may be faster than from-digits3 for base <= 10"
  (parse-integer (format nil "~{~d~}" digits) :radix base))

(defun from-digits3  ( digits &optional (base 10))
  (let ((fs (format nil "~~{~~~d,R~~}" base)))
    (parse-integer (format nil fs digits) :radix base)))

;; thread is not working here for some reason
(defmfun1 ($from_digits :doc)  ( (digits :or-ae-list-string :ensure-lex) &optional (base 10 :thread))
  :desc ( :var "base" " need not be number, but may be, for instance, a symbol. If "
  :var "base" " is a number it must be an integer between 2 and 36. " :var "digits"
  " may be a string rather than a list.")
  (when (stringp digits)
      (if (and (numberp base) (echeck-arg $from_digits :radix base))
          (return-from $from_digits (parse-integer digits :radix base))
          (setf digits (cons nil (loop :for c :across digits :collect (digit-char-p c)) ))))
    (pop digits)
    (if (and (numberp base) (echeck-arg $from_digits :radix base))
        (from-digits3 digits base)
        (let ((n (length digits)) (sum 0))
          (loop :for i fixnum :from (1- n) :downto 0 :do
               (setf sum (add sum  (mul `((mexpt simp) ,base ,i) (pop digits)))))
          (meval sum))))

(max-doc:see-also "from_digits" '("integer_digits" "integer_string"))
(add-call-desc '( "from_digits" ("digits")
                 ("returns the integer represented by the decimal digits in the list or string " :arg "digits" "."))
               '( "from_digits" ("digits" "base")
                 ("returns the integer represented by the base " :arg "base" " digits in the list or string" 
                  :arg "digits" ".")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; integer_digits
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmfun-ae ($integer_digits :doc) ((n :integer) &optional (base :radix 10) (len :non-neg-int nil))
  (setf n (abs n))
  (let* ((str (write-to-string n :base base))
         (digits (loop :for c :across str :collect (digit-char-p c base)))
         (n (length str)))
    (defmfun-final-to-ae
      (mk-mlist
       (if (and len (> len n)) (nconc (make-list (- len n) :initial-element 0) digits)
         digits)))))

(add-call-desc '( "integer_digits" ("n")
                  ("returns a list of the base 10 digits of " :arg "n" "."))
               '("integer_digits" ("n" "base")
                 ("returns a list of the base " :arg "base" " digits of " :arg "n" "."))
               '("integer_digits" ("n" "base" "len")
                 ("returns a list of the base " :arg "base" " digits of " :arg "n"
                  " padded with 0's so that the total length of the list is " :arg "len" ".")))
(max-doc:see-also "integer_digits" '("from_digits" "integer_string"))

(max-doc:implementation "integer_digits"
 '( 
   "gcl is much faster than the others. " :code "integer_digits(2^(10^6))" ": typical times for lisps:"
   :par ""
   "ecl-12.12.1 0.09s, sbcl-1.1.11 0.5s, clisp-2.49 9s, ccl-1.9 62s, "
   "cmucl-20d error, gcl-2.6.(7,8,9) 0.09s, allegro-8.2 = 23s, "
   "Mma-3.0 = 5s, Mma-8 = 0.04s."))



;; This makes too many mistakes
;; For binary, of course, we can use integer-length 
;; gmp library has no function to do this exactly. 
;; It returns the number
;; of digits or one more than the exact answer.
;; That much is easy to do.
;; We can use big floats to compute logs, but we need fpprec
;; to be as large as the number of decimal digits.
;; This does not work for eg mersenne primes.

#|

(defmfun1 ($integer_length :doc) ((n :non-neg-int :thread) &optional (base 10 :gt-1-int :thread))
  :desc
  ("Returns the number of digits in the integer " :argdot "n"
   " This function is not yet reliable. It miscounts by 1 for some numbers.")
;   " This function miscounts by 1 for some very large numbers (with millions of digits).
;   For instance, the 39th, 47th, and 48th Mersenne primes.")
  (if (= 0 n) 1
    (1+ (floor (log n base)))))

|#


#|

(add-call-desc
 '( "integer_length" ("n")
    ("returns the number of digits in the base 10 representation of the integer " :argdot "n"))
 '( "integer_length" ("n" "base")
    ("returns the number of digits in the base " :arg "base" " representation of the integer " :argdot "n")))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; integer_string
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmfun1 ($integer_string :doc)
  ((n :integer :thread) &optional (base :or-radix-string-symbol 10 :thread) 
;   (pad 0 :non-neg-int :thread)   
   &opt ($sep nil) ($group 3 :pos-int) 
   ($width 0 :non-neg-int) 
   ($padchar "0" :string) ($lc nil :bool))
  :desc
  ("The option " :opt "sep" " returns a string with commas."
   "The option " :opt "lc" " prints any alphbetic characters in lower case.")
  (when (symbolp base) (setf base ($sconcat base)))
  (when (member base '("roman" "roman_old") :test #'equal)
    (echeck-arg $integer_string :roman-integer n))
  (let* ((base (if (symbolp base) ($sconcat base) base))
         (fmt 
          (cond
           ((equal base "roman") "~@R")
           ((equal base "roman_old") "~:@R")
           ((equal base "ordinal") "~:R")
           ((equal base "cardinal") "~R")
           ((stringp base)
            (defmfun1-error-return '$chk_number_base $integer_string
              (format nil "integer_string: base ~s is not a radix or one of roman, cardinal, or ordinal" base)))
           ($sep
            (let ((sc
                   (cond ((eq $sep '$comma) "',")
                         ((eq $sep '$dot) "'.")
                         (t ""))))
              (format nil "~~~a,~a,'~a,~a,~a:R" base $width $padchar sc $group)))
           (t 
            (format nil "~~~a,~a,'~aR" base $width $padchar))))
         (str (format nil fmt n)))
    (when $lc (nstring-downcase str))
    str))

(add-call-desc  '( "integer_string" ("n")
                 ("returns a string containing the decimal digits of the integer " :arg "n" "."))
               '( "integer_string" ("n" "base")
                 ("returns a string containing the base " :arg "base" " digits of the integer " :arg "n" "."))
               '( "integer_string" ("n" "base" "pad")
                 ("pads the string on the left with 0's so that the length of the string is " :arg "pad"  "."))
               '( "integer_string" ("n" "roman")
                 ("returns a string containing the roman-numeral form of the integer " :arg "n" "."))
               '( "integer_string" ("n" ("lit" "\"roman_old\""))
                 ("returns a string containing the old-style roman-numeral form of the integer " :arg "n" "."))
               '( "integer_string" ("n" ("lit" "\"cardinal\""))
                 ("returns a string containing the english word form of the integer (cardinal number) " :arg "n" "."))
               '( "integer_string" ("n" ("lit" "\"ordinal\""))
                 ("returns a string containing the english word form of the ordinal (counting) number " :arg "n" ".")))

(max-doc:see-also "integer_string" '("integer_digits" "from_digits"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmfun1 ($prime_pi_soe1 :doc) ((n :non-neg-int))
  :desc ("The prime counting function. The algorithm is the sieve of Eratosthenes.
   Internally an array of " :var "n" " bits is used.")
  (if (= 0 n) 0
      (let ((a (make-array (+ n 1) :element-type 'bit :initial-element 1 :adjustable nil ))
            (c 1))
        (setf (sbit a 1) 0)
        (do ((p 2))
            ((> (* p p) n ) nil)
          (do ((j (* p p) (+ j p)))
              ((> j n) nil)
            (incf c (sbit a j))
            (setf (sbit a j) 0))
          (incf p)
          (loop  :until (= 1 (sbit a p)) :do (incf p)))
        (- n c))))

;; all of this fixnum business makes this run twice as
;; fast in sbcl. No difference in gcl.
;; dunno which are the important ones.
(defmfun1 ($prime_pi_soe2 :doc)  ((n :non-neg-int))
  :desc 
  ("The prime counting function. The algorithm is the sieve of Eratosthenes.
   Internally an array of " :arg "n" " bits is used. This function only
   works when " :arg "n" " is smaller than the largest fixnum.")
  (if (= 0 n) 0
      (let ((a (make-array (+ n 1) :element-type 'bit :initial-element 1 :adjustable nil ))
            (c 1)
            (n n))
        (declare (fixnum n c))
        (setf (sbit a 1) 0)
        (do ((p 2))
            ((> (the fixnum (f* p p)) (the fixnum n) ) nil)
          (declare (fixnum p))
          (do ((j (the fixnum (f* p p)) (the fixnum (f+ j p)) ))
              ((> j n) nil)
            (declare (fixnum j))
            (incf c (sbit a j))
            (setf (sbit a j) 0))
          (incf p)
          (loop :until (= 1 (sbit a p)) :do (incf p)))
        (f- n c))))

(max-doc:implementation "prime_pi_soe2" "This is not the most efficient way to compute primes.");
(max-doc:see-also "prime_pi_soe1" '("prime_pi" "next_prime" "prev_prime"))

;; copied from rosetta code, and modified a bit. This could also have been
;; done as above.
(defmfun-ae ($primes1 :doc) ((n1 :non-neg-int) &optional (n2 :non-neg-int) &aux (minimum 1) maximum)
  :desc
   ("The algorithm is the sieve of Eratosthenes. This is not an efficient algorithm.")
  (if n2
      (progn  (setf maximum n2) (setf minimum n1))
      (setf maximum n1))
  (let ((sieve (make-array (1+ maximum) :element-type 'bit
                           :initial-element 0)))
    (defmfun-final-to-ae (mk-mlist
          (loop :for candidate :from 2 :to maximum
             when (zerop (bit sieve candidate))
             if (>= candidate minimum) collect candidate end
             and do (loop :for composite :from (expt candidate 2) 
                       :to maximum :by candidate
                       :do (setf (bit sieve composite) 1)))))))


(defmfun-ae ($primes2 :doc) ((n1 :non-neg-int) &optional (n2 :non-neg-int) &aux (minimum 1) maximum)
  :desc
   ("The algorithm is the sieve of Eratosthenes. This is not an efficient algorithm."
   " This function only works when " :arg "n" " is smaller than the largest fixnum.")
  (if n2
      (progn  (setf maximum n2) (setf minimum n1))
      (setf maximum n1))
  (let ((sieve (make-array (1+ maximum) :element-type 'bit
                           :initial-element 0)))
    (let ((minimum minimum) (maximum maximum))
      (declare (fixnum minimum maximum))
      (defmfun-final-to-ae (mk-mlist
          (loop :for candidate fixnum :from 2 :to maximum
             when (zerop (bit sieve candidate))
             if (>= candidate minimum) collect candidate end
             and do (loop :for composite fixnum :from (expt candidate 2) 
                       :to maximum :by candidate
                       :do (setf (bit sieve composite) 1))))))))

(add-call-desc '( "primes1" ("max") ("returns a list of the primes less than or equal to " :arg "max" "."))
               '( "primes1" ("min" "max") ("returns a list of the primes between " :arg "min" " and "
                                           :arg "max" ".")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; catalan_number
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmfun1 ($catalan_number :doc) ((n :thread))
 :desc ("Returns the " :var "n" "th catalan number.")
  (if (numberp n) (/ ($binomial (* 2 n) n) (+ n 1))
      (meval (mul (list '(mexpt simp) (add 1 n) -1) ($binomial (mul 2 n) n)))))

(max-doc::oeis "catalan_number" "A000108")

(examples:clear-add-example "catalan_number"
                       '( :pretext "The catalan number for n from 1 through 12."
                          :code "map(catalan_number,lrange(12))")
                       '( :pretext "The n'th catalan number."
                          :vars "[n]"
                          :code "catalan_number(n)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; idivisors
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This was taken from factor.lisp with
;; unnecessary things removed.
(defun divisors-mext-new (l)
  (if (equal l '(1 1)) (setq l nil))
  (do ((ans (list 1 ))
       (l l (cddr l)))
      ((null l) (sort ans #'<))
    (do ((u ans)
	 (factor (car l))
	 (mult (cadr l) (1- mult)))
	((zerop mult))
      (setq u (mapcar #'(lambda (q) (* factor q)) u))
      (setq ans (nconc ans u)))))

;; The declarations don't help with sbcl
(defun divisors-mext (l)
  (if (equal l '(1 1)) (setq l nil))
  (do ((ans (list 1 ))
       (l l (cddr l)))
      ((null l) (sort ans #'<))
    (do ((u ans)
	 (factor (car l))
	 (mult (cadr l) (1- mult)))
	((zerop mult))
      (declare (integer mult factor))
      (setq u (mapcar #'(lambda (q) (declare (integer q factor)) 
                          (* (the integer factor) (the integer q))) u))
      (setq ans (nconc ans u)))))

(defmfun1 ($idivisors :doc) ((n :pos-int :thread))
  :desc
  ("Lists the divisors of the integer " :argdot "n"
   " This is similar to " :emrefcomma "divisors"
   " but it is faster and returns a list and is not
   a simplfying function.")
  (mk-mlist (divisors-mext-new (cfactorw n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; divisor_summatory
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmfun1 ($divisor_summatory :doc) ((x :to-non-neg-float :thread) )
  :desc 
  ("Returns the divisor summatory function "
   :math "D(x)" " for " :argdot "x"  " The " :mref "divisor_function" " " 
   :tmath ("\\sigma_0(n)" "sigma_0(n)") " counts
   the number of unique divisors of the natural number " :math "n" ". "
   :math "D(x)" " is the sum of " :tmath ("\\sigma_0(n)" "sigma_0(n)") 
   " over " :tmath ("n \\le x" "n <= x") ".")
  (let ((sum 0) (u (floor (sqrt x))) (nx (floor x)))
                    (loop :for i :from 1 :to u :do
                          (setf sum (+ sum (floor (/ nx i)))))
                    (- (* 2 sum) (* u u))))

(max-doc::oeis "divisor_summatory" "A006218")

(examples:clear-add-example "divisor_summatory"
                       '( :pretext "D(n) for n from 1 through 12"
                          :code "map(divisor_summatory,lrange(12))"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; oeis_A092143
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmfun1 (|$oeis_A092143| :doc) ((n :pos-int))
  :desc ("Returns the cumulative product of all divisors of integers from 1 to "
         :argdot "n")
  (let ((prod 1))
    (loop :for k :from 1 :to n :do
          (setf prod (* prod (expt k (floor (/ n k))))))
    prod))

(max-doc::oeis "oeis_A092143" "A092143")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; divisor_function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun divisor-function-1 (n)
 "divisor function with x=1."
  (let* ( (factors (cdr ($ifactors n))) (prod 1) (r (length factors)) )
    (dotimes (i r)
      (let ((multiplicity (third (car factors))) (prime (second (car factors))))
        (setf factors (cdr factors))
        (setf prod (* prod (/ (1- (expt prime (1+ multiplicity) )) (1- prime))))))
    prod))

(defun divisor-function-0 (n)
 "divisor function with x=0."
  (let* ( (factors (cdr ($ifactors n))) (prod 1) (r (length factors)) )
    (dotimes (i r)
      (let ((onefact (third (car factors))))
        (setf factors (cdr factors))
        (setf prod (* prod (1+ onefact)))))
    prod))

;         :tmath ( "\\sigma_{x}(n)" "sigma_x(n)")
(defmfun1 ($divisor_function :doc) ((n :non-neg-int :thread) &optional (x :number 0 :thread))
  :desc ("Returns the divisor function, or sum of positive divisors function " 
      :lif ((:dmath "\\sigma_{x}(n)=\\sum_{d|n} d^x," "where " :math "d|x"
          " means " :math "d" " divides " :math "n") "sigma_x(n)")
         ". If " :arg "x" " is omitted it takes the default value "
  :math "0" ". Currently, complex values for x are not supported. After writing this, I noticed that
  the function is implemented in the maxima core and is callled " :emref "divsum" ".")
  (cond ((= 0 x) (divisor-function-0 n) ) ; These are for efficiency, but I think
        ((= 1 x) (divisor-function-1 n) ) ; the advantage is negligible.
        (t
          (let* ((factors (cdr ($ifactors n))) (prod 1) (r (length factors)))
            (dotimes (i r)
              (let ((multiplicity (third (car factors))) (prime (second (car factors))))
                (setf factors (cdr factors))
                (setf prod (* prod (/ (1- (expt prime (* (1+ multiplicity) x))) (1- (expt prime x)))))))
            prod))))

(max-doc::oeis "divisor_function" '("A000005 for x=0" "A000203 for x=1"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; perfect_p
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmfun1 ($perfect_p :doc) ((n :pos-int :thread))
  :desc ("Returns true if " :arg "n" " is a perfect number. Otherwise, returns false.")
  (if (= n (- ($divisor_function n 1) n)) t nil))

(max-doc:implementation "perfect_p"
  '("This function computes divisors. It would be far more efficient to use a table of
   known perfect numbers, as very few of them are accessible by current computer hardware."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; abundant_p
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmfun1 ($abundant_p :doc) ((n :pos-int :thread))
  :desc ("Returns true if " :arg "n" " is an abundant number. Otherwise, returns false.")
  (if (< n (- ($divisor_function n 1) n)) t nil))

(examples:clear-add-example "abundant_p"
                       '( :pretext "The abundant numbers between 1 and 100"
                          :code "select(lrange(100),abundant_p)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; aliaquot_sum
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmfun1 ($aliquot_sum :doc) ((n :pos-int :thread))
  :desc ("Returns the aliquot sum of " :argdot "n" " The aliquot sum
 of " :arg "n" " is the sum of the proper divisors of " :argdot "n")
  (- ($divisor_function n 1) n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; amicable_p
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmfun1 ($amicable_p :doc) ((n :pos-int :thread) (m :pos-int :thread))
  :desc ("Returns true if " :arg "n" " and " :arg "m" " are amicable, and false otherwise.")
  (and (not (= m n)) (= ($aliquot_sum n) m) (= ($aliquot_sum m) n)))

(examples:clear-add-example "amicable_p"
 '( :pretext "The first few amicable pairs."
    :code "map(lambda([x],amicable_p(first(x),second(x))), [[220, 284], 
          [1184, 1210], [2620, 2924], [5020, 5564], [6232, 6368]])"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; aliquot_sequence
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: allow keeping only the later part of the sequence. Look for periods greater than 1
;;(defmfun1 ($aliquot_sequence :doc) ((k :pos-int) (n :non-neg-int) &optional (n2 0 n2-supplied-p :non-neg-int ) )
(defmfun1 ($aliquot_sequence :doc) ((k :pos-int :thread) (n :non-neg-int :thread) )
  :desc (" The aliquot sequence is a recursive sequence in which each term is the sum 
    of the proper divisors of the previous term. This function returns the first " :arg "n" 
   " elements (counting from zero) in the aliquot sequence whose first term is " :argdot "k" 
   " The sequence is truncated at an element if it is zero or repeats the previous element.")
  (let ( (seq (list k)) (cur k) (last 0) )
    (loop :for i :from 0 :to n :until (or (= 0 cur) (= cur last)) :do
          (setf last cur)
          (setf cur (- (divisor-function-1 cur) cur))
          (setf seq (cons cur seq)))
    (when (= cur last) (setf seq (cdr seq)))
    (mk-mlist (nreverse seq))))

(examples:clear-add-example "aliquot_sequence"
 '( :pretext "Perfect numbers give a repeating sequence of period 1."
    :code "imap(lambda([x],aliquot_sequence(x,100)),[6,28,496,8128])")
 '( :pretext "Aspiring numbers are those which are not perfect, but terminate with a repeating perfect number."
    :code "imap(lambda([x],aliquot_sequence(x,100)),[25, 95, 119, 143, 417, 445, 565, 608, 650, 652, 675, 685])"))

;; This is now done by default for all functions.
;(defmfun1::set-match-form '( $aliquot_sum $divisor_function $divisor_summatory ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; cbfloat
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; like bfloat(), see comments below for cfloat()
(defmfun cbfloat-do (x)
  (let (y bftest)
    (cond ((bigfloatp x)) ; perverse. this changes the precision
	  ((or (numberp x)
	       (member x '($%e $%pi $%gamma) :test #'eq))
	   (bcons (intofp x)))
          ((setf bftest (get-bigfloat-hook x))
           (funcall bftest x))
          ((aex-p x)
           (let* ((a (aex-copy-new-n x))
                  (ar (aex-arr a))
                  (arx (aex-arr x)))
             (dotimes (i (length ar))
               (setf (aref ar i) (cbfloat-do (aref arx i))))
             a))
	  ((or (atom x) (member 'array (cdar x) :test #'eq))
	   (if (eq x '$%phi)
	       (cbfloat-do '((mtimes simp)
			  ((rat simp) 1 2)
			  ((mplus simp) 1 ((mexpt simp) 5 ((rat simp) 1 2)))))
	       x))
          ((eq (caar x) 'mtimes)
           (let ((res (recur-apply #'cbfloat-do x)))
             ;; If we are multiplying complex numbers, expand result -- GJL 2013
             (if (and (consp res) (eq (caar res) 'mtimes)
                      (every #'(lambda (aa) (or (complex-number-p aa)
                                               (complex-number-p aa '$bfloatp)))
                                               (cdr res)))
                 ($expand res)
               res)))
	  ((eq (caar x) 'mexpt)
           (if 
               (and (complex-number-p (second x) '$numberp) ; GJL 2013
                    (complex-number-p (third x) '$numberp))
               (let* ((base (cbfloat-do (second x)))
                      (exp (cbfloat-do (third x)))
                      (form (list (car x) base exp)))
                 (cbfloat-do ($rectform form)))
             (if (equal (cadr x) '$%e)
                 (*fpexp (cbfloat-do (caddr x)))
	       (exptbigfloat (cbfloat-do (cadr x)) (caddr x)))))
	  ((eq (caar x) 'mncexpt)
	   (list '(mncexpt) (cbfloat-do (cadr x)) (caddr x)))
	  ((eq (caar x) 'rat)
	   (ratbigfloat (cdr x)))
	  ((setq y (safe-get (caar x) 'floatprog))
	   (funcall y (mapcar #'cbfloat-do (cdr x))))
	  ((or (trigp (caar x)) (arcp (caar x)) (eq (caar x) '$entier))
	   (setq y (cbfloat-do (cadr x)))
	   (if (cbfloat-do y)
	       (cond ((eq (caar x) '$entier) ($entier y))
		     ((arcp (caar x))
		      (setq y (cbfloat-do (logarc (caar x) y)))
		      (if (free y '$%i)
			  y (let ($ratprint) (fparcsimp ($rectform y)))))
		     ((member (caar x) '(%cot %sec %csc) :test #'eq)
		      (invertbigfloat
		       (cbfloat-do (list (ncons (safe-get (caar x) 'recip)) y))))
		     (t (cbfloat-do (exponentialize (caar x) y))))
	       (subst0 (list (ncons (caar x)) y) x)))
	  (t (recur-apply #'cbfloat-do x)))))

(defmfun1 ($cbfloat :doc) (expr)
  :desc
  ("Convert most numbers in " :arg "expr" " to bigfloat numbers."
   " This also converts some exponential expressions that " :emref "bfloat"
   " leaves unconverted. " :mref "cbfloat" " maps over aex expressions.")
  (cbfloat-do expr))

;; We have modified float to do this. So $cfloat is
;; not necessary.
;; Same as float() except
;; 1) do rectform on exponentials with base and exponent
;; both numbers.
;; 2) expand products of numbers and complex numbers
(defmfun $cfloat (e)
  (cond ((numberp e) (float e))
	((and (symbolp e) (mget e '$numer)))
	((or (atom e) (member 'array (cdar e) :test #'eq)) e)
	((eq (caar e) 'rat) (fpcofrat e))
	((eq (caar e) 'bigfloat) (fp2flo e))
        ((and (eq (caar e) 'mexpt)  ; GJL 2013
              (complex-number-p (second e) '$numberp)
              (complex-number-p (third e) '$numberp))
         (let* ((base ($cfloat (second e)))
                (exp ($cfloat (third e)))
                (form (list (car e) base exp)))
           ($cfloat ($rectform form))))
	((member (caar e) '(mexpt mncexpt) :test #'eq)
	 ;; avoid x^2 -> x^2.0, allow %e^%pi -> 23.14
	 (let ((res (recur-apply #'$cfloat e)))
	   (if (floatp res)
	       res
	       (list (ncons (caar e)) ($cfloat (cadr e)) (caddr e)))))
	((and (eq (caar e) '%log)
	      (complex-number-p (second e) '$ratnump))
	 ;; Basically we try to compute float(log(x)) as directly as
	 ;; possible, expecting Lisp to return some error if it can't.
	 ;; Then we do a more complicated approach to compute the
	 ;; result.  However, gcl and ecl don't signal errors in these
	 ;; cases, so we always use the complicated approach for these lisps.
	 (let ((n (second e)))
	   (cond ((integerp n)
		  ;; float(log(int)).  First try to compute (log
		  ;; (float n)).  If that works, we're done.
		  ;; Otherwise we need to do more.  
		  (to (or (try-float-computation #'(lambda ()
						     (log (float n))))
			  (let ((m (integer-length n)))
			    ;; Write n as (n/2^m)*2^m where m is the number of
			    ;; bits in n.  Then log(n) = log(2^m) + log(n/2^m).
			    ;; n/2^m is approximately 1, so converting that to a
			    ;; float is no problem.  log(2^m) = m * log(2).
			    (+ (* m (log 2e0))
			       (log (float (/ n (ash 1 m)))))))))
		 (($ratnump n)
		  ;; float(log(n/m)) where n and m are integers.  Try computing
		  ;; it first.  If it fails, compute as log(n) - log(m).
		  (let ((try (try-float-computation #'(lambda() 
							(log (fpcofrat n))))))
		    (if try
			(to try)
			(sub  ($cfloat `((%log) ,(second n)))
			      ($cfloat `((%log) ,(third n)))))))
		 ((complex-number-p n 'integerp)
		  ;; float(log(n+m*%i)).
		  (let ((re ($realpart n))
			(im ($imagpart n)))
		    (to (or (try-float-computation #'(lambda ()
						       (log (complex (float re)
								     (float im)))))
			    (let* ((size (max (integer-length re)
					      (integer-length im)))
				   (scale (ash 1 size)))
			      (+ (* size (log 2e0))
				 (log (complex (float (/ re scale))
					       (float (/ im scale))))))))))
		 (t
		  ;; log(n1/d1 + n2/d2*%i) =
		  ;;   log(s*(n+m*%i)) = log(s) + log(n+m*%i)
		  ;;
		  ;; where s = lcm(d1, d2), n and m are integers
		  ;;
		  (let* ((s (lcm ($denom ($realpart n))
				 ($denom ($imagpart n))))
			 (p ($expand (mul s n))))
		    (add ($cfloat `((%log) ,s))
			 ($cfloat `((%log) ,p))))))))
	((and (eq (caar e) '%erf)
	      (eq (second e) '$%i))
	 ;; Handle like erf(%i).  float(%i) (via recur-apply below)
	 ;; just returns %i, so we never numerically evaluate it.
	 (complexify (complex-erf (complex 0 1d0))))
        ;; If we are multiplying complex numbers, expand result -- GJL 2013
        ((eq (caar e) 'mtimes)
         (let ((res (recur-apply #'$cfloat e)))
           (if (and (consp res) (eq (caar res) 'mtimes)
                    (every #'complex-number-p (cdr res)))
                 ($expand res)
             res)))
	(t (recur-apply #'$cfloat e))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; tofloat
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If a number is too big convert to double float
;; try bfloat. We hope that was the problem.
(defun do-one-tofloat (expr n)
  (if (> n 15)
      (cbfloat-do expr)
    (handler-case
     #-(or gcl ecl) ($float expr)
     #+(or gcl ecl)
     (let ((res ($float expr)))
       (if (eql res (* 1.5e308 1.5e308))
           (cbfloat-do expr)
       res))
     (error ()
            (cbfloat-do expr)))))

(defmfun1 ($tofloat :doc) (expr &optional (n 15 :pos-int) &aux old-fpprec old-numer bfloat-flag)
  :desc
  ("This function does not change the printed precision, " :codedot "fpprintprec"
   " Either " :emref "float" " or " :emref "bfloat" " is called depending on the "
   " precision. More expressions, particularly some exponentials, are converted "
   " to numbers than with standard " :emref "float" " and " :emrefdot "bfloat"
   " This function temporarily sets " :var "fpprec" " to " :argdot "n"
   " But, Maxima does floating point operations with the current value of "
   :varcomma "fpprec" " regardless of the precision of the operands."
   :par ""
   :mref "tofloat" " maps over aex expressions.")
  (setf old-fpprec $fpprec)
  (setf old-numer  $numer)
  (mset '$fpprec n)
  (mset '$numer nil) ; true seems to hurt more than help
  (unwind-protect
      (do-one-tofloat expr n)
    (progn
      (mset '$fpprec old-fpprec)
      (mset '$numer old-numer))))

(add-call-desc 
 '("tofloat" ("expr") ("tries to convert numbers in ":arg "expr" " to floating point."))
 '("tofloat" ("expr" "n") ("tries to convert numbers in ":arg "expr" " to floating point "
   "with " :arg "n" "-digit precision.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Khintchine and Glaisher
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; need to put this somewhere to satisfy sbcl, etc.
;; $%Glaisher_bigfloat is defined in a .mac file and
;; it somehow is not defined for lisp.
;; (defvar |$%Glaisher_bigfloat|)
;; see src/init-cl.lisp
(let ((context '$global))
  (declare (special context))
  (dolist (x '( |$%Khintchine| |$%Glaisher|))
    (kind x '$constant)
    (setf (get x 'sysconst) t)))

; does not work. the hooks should be functions
;(eval-when (:compile-toplevel :load-toplevel :execute)
;  (set-max-bigfloat-precision '|$%Khintchine| ($precision |$%Khintchine_bigfloat|))
;  (set-max-bigfloat-precision '|$%Glaisher| ($precision |$%Glaisher_bigfloat|)))

;; see src/mlisp.lisp
(mdefprop |$%Khintchine| 2.685452001065306445309714835481795693820382293994462953051152345557218859537152002801141174931847698
          $numer)

;; careful, using \ for line continuation causes a strange result that I dont
;; understand, but float(%Glaisher) returns all digits and result is not a float.
(mdefprop |$%Glaisher| 1.2824271291006226368753425688697917277676889273250011920637400217404063088588264611297364919582023743942064612039900074893315779136277528040415907257386172752214334327143439787335067915257366856907877
  $numer)

(set-bigfloat-hook '|$%Khintchine|
   #'(lambda () 
       (when (> $fpprec ($precision |$%Khintchine_bigfloat|))
         (format t "Warning: fpprec is greater than internal precision of %Khintchine.~%"))
                 (bigfloatp |$%Khintchine_bigfloat|)))

(set-bigfloat-hook '|$%Glaisher|
   #'(lambda () 
       (when (> $fpprec ($precision |$%Glaisher_bigfloat|))
         (format t "Warning: fpprec is greater than internal precision of %Glaisher.~%"))
                 (bigfloatp |$%Glaisher_bigfloat|)))

(max-doc:add-doc-entry 
 '( :name "%Khintchine"
    :type "Constant"
    :contents ("The Khintchine constant. Float and bigfloat approximations
                can be obtained with " :mrefcomma "tofloat" " "
                :emrefcomma "float" " and " :emrefdot "bfloat")))

(max-doc:add-doc-entry 
 '( :name "%Glaisher"
    :type "Constant"
    :contents ("The Glaisher constant. Float and bigfloat approximations
                can be obtained with " :mrefcomma "tofloat" " "
                :emrefcomma "float" " and " :emrefdot "bfloat")))


;; ??? what is this all about ?
; from  compar.lisp
;; %initiallearnflag is only necessary so that %PI, %E, etc. can be LEARNed.
;(defun initialize-numeric-constant (c)
;  (setq %initiallearnflag t)
;  (let ((context '$global))
;    (learn `((mequal) ,c ,(mget c '$numer)) t))
;  (setq %initiallearnflag nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; harmonic_number
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; input either real or
;; e.g. (harmonic-number-float '((mplus) $%i 1))
;; for real nf, must have nf>1
(defun harmonic-number-float-complex (nf)
  (cadr 
   ($nintegrate 
    `((MTIMES SIMP) ((MEXPT SIMP) ((MPLUS SIMP) 1 ((MTIMES SIMP) -1 $X)) -1)
      ((MPLUS SIMP) 1 ((MTIMES SIMP) -1 ((MEXPT SIMP) $X ,nf))))
    '((mlist simp) $x 0 1) (rule-opt '$idomain '$complex))))

;; n >= 1
;; returns exact rational result
;; There must be a function already for the last line.., but where ?
(defun harmonic-number-integer (n)
  (let ((sum 0))
    (loop :for i :from 1 :to n :do
          (setf sum (+ sum (/ 1 i))))
    (if (rationalp sum) 
        (div (numerator sum) (denominator sum))
      sum)))

;; oops, only works for `integer' bfloat
;; not sure if this is better/worse than
;; doing rational numbers and taking ratio.
;; This is probably useless. Not used now.
(defun harmonic-number-bfloat (n)
  (let ((sum '(0 0))
        (hone (fpone))
;        (nbf (cdr n))
        (nlim (mfuncall '$floor n)))
    (loop :for i :from 1 :to nlim :do
          (setf sum (fpplus sum (fpquotient hone (intofp i)))))
    (bcons sum)))

;; We could return '$inf for negative or 0 realpart,
;; but harmonic_number is really undefined for this
;; case, so return form
(defmfun1 ($harmonic_number :doc) ((n :thread))
  :desc
  ("Returns the harmonic number " :math "H_n" ".") ;; :mathdot not defined!!
  ;; bfloat complex or mixed bfloat float complex --> float complex
  (when (complex-number-p n '$numberp) 
    (let ((rp ($realpart n))
          (ip ($imagpart n)))
      (when (and (or (floatp rp) ($bfloatp rp))
                 (or (floatp ip) ($bfloatp ip)))
        (setf n ($float n)))))
  (let (($ratprint nil))
    (cond 
     ((and (integerp n) (> n 0))
      (harmonic-number-integer n))
     ((and (floatp n) (= n 1.0)) 1.0) ; num integral fails
     ((and (complex-number-p n 'floatp) (> ($realpart n) 0))
          (harmonic-number-float-complex n))
;     (t nil))))
  (t  `(($harmonic_number) ,n)))))

;; Make this a simplifying function, so that
;; float(harmonic_number(7/2)) --> result
;; not really sure what I'm doing here!!, copied from simpbern
;; This is not quite right yet. Works for most things.
;; If too many args are given, they are thrown away on
;; simplifying
(defmfun simpharmonicnumber (x vestigial z)
  (declare (ignore vestigial))
  (let* ((u (simpcheck (cadr x) z))
         (res ($harmonic_number u)))
    (if res res 
      (eqtest (list '($harmonic_number) u) x))))

(setf (get '$harmonic_number 'operators) 'simpharmonicnumber)


(max-doc:see-also-group '( "tofloat" "cbfloat"))

(max-doc:see-also-group '( "divisor_function" "aliquot_sum" "aliquot_sequence" 
                           "divisor_summatory" "perfect_p" "abundant_p"))

#|
(defmfun1 ($sum_of_divisor_sigma :doc) ((x :non-neg-number) )
  "Returns the sum of the divisor function sigma_0(k) for k from 1 through n"
  (let ((sum 0) (u (floor (sqrt x))) (nx (floor x)))
                    (loop for k from 1 to u do
                          (setf sum (+ sum (* (+ k (/ nx k)) (floor (/ nx k))))))
                    (- sum (* u u))))
|#
