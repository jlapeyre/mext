;;; Copyright (C) 2012 John Lapeyre
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(in-package :maxima)
(declaim (optimize (speed 3) (space 0) (safety 0) (debug 0)))
(max-doc::set-cur-sec 'max-doc::number-theory-fandv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (declaim (optimize (speed 3) (space 0) (debug 0)))


(defun from-digits2  ( digits &optional (base 10))
  "This may be faster than from-digits3 for base <= 10"
  (parse-integer (format nil "~{~d~}" digits) :radix base))

(defun from-digits3  ( digits &optional (base 10))
  (let ((fs (format nil "~~{~~~d,R~~}" base)))
    (parse-integer (format nil fs digits) :radix base)))

(defmfun1 ($from_digits :doc)  ( (digits :or-ae-list-string :ensure-lex) &optional (base 10))
  "<base> need not be number, but may be, for instance, a symbol. If <base> is
   a number it must be an integer between 2 and 36. <digits> may be a string rather than a list."
  (if (stringp digits)
      (if (and (numberp base) (echeck-arg :radix base))
          (return-from $from_digits (parse-integer digits :radix base))
          (setf digits (cons nil (loop for c across digits collect (digit-char-p c)) ))))
  (progn
    (pop digits)
    (if (and (numberp base) (echeck-arg :radix base))
        (from-digits3 digits base)
        (let ((n (length digits)) (sum 0))
          (loop for i fixnum from (1- n) downto 0 do
               (setf sum (add sum  (mul `((mexpt simp) ,base ,i) (pop digits)))))
          (meval sum)))))

(max-doc::see-also "from_digits" '("integer_digits" "integer_string"))
(add-call-desc '( "from_digits" ("digits")
                 ("returns the integer represented by the decimal digits in the list " arg "digits" "."))
               '( "from_digits" ("digits" "base")
                 ("returns the integer represented by the base " arg "base" " digits in the list " arg "digits" ".")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmfun-ae ($integer_digits :doc) ((n :integer) &optional (base :radix 10) (len :non-neg-int nil))
  (setf n (abs n))
  (let* ((str (write-to-string n :base base))
         (digits (loop for c across str collect (digit-char-p c base)))
         (n (length str)))
    (defmfun-final-to-ae
        (cons '(mlist simp)
              (if (and len (> len n)) (append (make-list (- len n) :initial-element 0) digits)
                  digits)))))

(add-call-desc '( "integer_digits" ("n")
                 ("returns a list of the base 10 digits of " arg "n" "."))
               '("integer_digits" ("n" "base")
                 ("returns a list of the base " arg "base" " digits of " arg "n" "."))
               '("integer_digits" ("n" "base" "len")
                 ("returns a list of the base " arg "base" " digits of " arg "n"
                  " padded with 0's so that the total length of the list is " arg "len" ".")))
(max-doc::see-also "integer_digits" '("from_digits" "integer_string"))

(max-doc::implementation "integer_digits"
 "gcl is much faster than the others. integer_digits(2^(10^6)): typical times for lisps:
 ccl-1.7-r15184M = 65s, sbcl-1.0.52.0.debian = 1.5s,  allegro-8.2 = 23s, Mma-3.0 = 5s,
 gcl-2.6.7 = 0.11s, Mma-8 = 0.04s.
 The base is limited to 36 only because we call write-to-string.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmfun1 ($integer_string :doc) ((n :integer) &optional (base :or-radix-string 10) (pad :pos-int))
  (let ((fmt 
         (cond ((equal base "roman")
                (echeck-arg :roman-integer n)
                "~@R")
               ((equal base "ordinal") "~:R")
               ((equal base "cardinal") "~R")
               ((stringp base)
                (merror1 "integer_string: base ~s is not a radix or one of \"roman\", \"cardinal\", or \"ordinal\"" base))
               (pad
                (format nil "~~~a,~a,'0R" base pad))
               (t (format nil "~~~aR" base)))))
    (format nil fmt n)))

(add-call-desc  '( "integer_string" ("n")
                 ("returns a string containing the decimal digits of the integer " arg "n" "."))
               '( "integer_string" ("n" "base")
                 ("returns a string containing the base " arg "base" " digits of the integer " arg "n" "."))
               '( "integer_string" ("n" "base" "pad")
                 ("pads the string on the left with 0's so that the length of the string is " arg "pad"  "."))
               '( "integer_string" ("n" ("lit" "\"roman\""))
                 ("returns a string containing the roman-numeral form of the integer " arg "n" "."))
               '( "integer_string" ("n" ("lit" "\"cardinal\""))
                 ("returns a string containing the english word form of the integer (cardinal number) " arg "n" "."))
               '( "integer_string" ("n" ("lit" "\"ordinal\""))
                 ("returns a string containing the english word form of the ordinal (counting) number " arg "n" ".")))

(max-doc::see-also "integer_string" '("integer_digits" "from_digits"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmfun1 $prime_pi_soe1 ((n :non-neg-int))
  "The algorithm is the sieve of Eratosthenes. This algorithm uses
   internally an array of <n> bits."
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
          (loop  until (= 1 (sbit a p)) do (incf p)))
        (- n c))))


;; all of this fixnum business saves 10 or 15 percent in speed.
;; dunno which are the important ones.
(defmfun1 ($prime_pi_soe :doc)  ((n :non-neg-int))
  "The algorithm is the sieve of Eratosthenes. This algorithm uses
   internally an array of <n> bits."
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
          (loop  until (= 1 (sbit a p)) do (incf p)))
        (f- n c))))

(max-doc::implementation "prime_pi_soe" "This is not the most efficient way to compute primes.");

(max-doc::see-also "prime_pi_soe" '("prime_pi" "next_prime" "prev_prime"))

;; copied from rosetta code, and modified a bit. This could also have been
;; done as above.
(defmfun-ae ($primes1 :doc) ((n1 :non-neg-int) &optional (n2 :non-neg-int) &aux (minimum 1) maximum)
  "The algorithm is the sieve of Eratosthenes. This is not an
 efficient algorithm."
  (if n2
      (progn  (setf maximum n2) (setf minimum n1))
      (setf maximum n1))
  (let ((sieve (make-array (1+ maximum) :element-type 'bit
                           :initial-element 0)))
    (defmfun-final-to-ae (cons '(mlist simp)
          (loop for candidate from 2 to maximum
             when (zerop (bit sieve candidate))
             if (>= candidate minimum) collect candidate end
             and do (loop for composite from (expt candidate 2) 
                       to maximum by candidate
                       do (setf (bit sieve composite) 1)))))))

(add-call-desc '( "primes1" ("max") ("returns a list of the primes less than or equal to " arg "max" "."))
               '( "primes1" ("min" "max") ("returns a list of the primes between " arg "min" " and "
                                           arg "max" ".")))


(defmfun1 ($catalan_number :doc) (n)
  "Returns the nth catalan number."
  (if (numberp n) (/ ($binomial (* 2 n) n) (+ n 1))
      (meval (mul (list '(mexpt simp) (add 1 n) -1) ($binomial (mul 2 n) n)))))

(max-doc::oeis "catalan_number" "A000108")

(examples::clear-examples "catalan_number")
(examples::add-example "catalan_number"
                       '( :pretext "The catalan number for n from 1 through 12."
                          :code "map(catalan_number,lrange(12))")
                       '( :pretext "The n'th catalan number."
                          :vars "[n]"
                          :code "catalan_number(n)"))


(defmfun1 ($divisor_summatory :doc) ((x :non-neg-number) )
  "Returns the divisor summatory function D(<x>). The divisor function d(n) counts
 the number of unique divisors of the natural number n. D(x) is the sum of d(n) over n <= x."
  (let ((sum 0) (u (floor (sqrt x))) (nx (floor x)))
                    (loop for i from 1 to u do
                          (setf sum (+ sum (floor (/ nx i)))))
                    (- (* 2 sum) (* u u))))

(max-doc::oeis "divisor_summatory" "A006218")

(examples::clear-examples "divisor_summatory")
(examples::add-example "divisor_summatory"
                       '( :pretext "D(n) for n from 1 through 12"
                          :code "map(divisor_summatory,lrange(12))"))

(defmfun1 (|$oeis_A092143| :doc) ((n :pos-int))
  "Returns the cumulative product of all divisors of integers from 1 to <n>."
  (let ((prod 1))
    (loop for k from 1 to n do
          (setf prod (* prod (expt k (floor (/ n k))))))
    prod))



(defun divisor-function-1 (n)
 "divisor function with x=1."
  (let* ( (factors (cdr ($ifactors n))) (prod 1) (r (length factors)) )
    (dotimes (i  r)
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

(defmfun1 ($divisor_function :doc) ((n :non-neg-int) &optional (x :number 0 ))
  "The divisor function sigma_x(n). If x is omitted it takes the default value 0.
   Currently, complex values for x are not supported."
  (cond ( (= 0 x) (divisor-function-0 n) )
        ( (= 1 x) (divisor-function-1 n) )
        ( t
          (let* ( (factors (cdr ($ifactors n))) (prod 1) (r (length factors)) )
            (dotimes (i  r)
              (let ((multiplicity (third (car factors))) (prime (second (car factors))))
                (setf factors (cdr factors))
                (setf prod (* prod (/ (1- (expt prime (* (1+ multiplicity) x))) (1- (expt prime x)))))))
            prod))))


(max-doc::oeis "divisor_function" '("A000005 for x=0" "A000203 for x=1"))


(defmfun1 ($perfect_p :doc) ((n :pos-int))
  "Returns true if <n> is a perfect number. Otherwise, returns false."
  (if (= n (- ($divisor_function n 1) n)) t nil))

(defmfun1 ($abundant_p :doc) ((n :pos-int))
  "Returns true if <n> is an abundant number. Otherwise, returns false."
  (if (< n (- ($divisor_function n 1) n)) t nil))

(examples::clear-examples "abundant_p")
(examples::add-example "abundant_p"
                       '( :pretext "The abundant numbers between 1 and 100"
                          :code "select(lrange(100),abundant_p)"))

(defmfun1 ($aliquot_sum :doc) ((n :pos-int))
  "Returns the aliquot sum of <n>."
  (- ($divisor_function n 1) n))

(defmfun1 ($amicable_p :doc) ((n :pos-int) (m :pos-int))
  "Returns true if <n> and <m> are amicable, and false otherwise."
  (and (not (= m n)) (= ($aliquot_sum n) m) (= ($aliquot_sum m) n)))

(examples::clear-examples "amicable_p")
(examples::add-example "amicable_p"
                       '( :pretext "The first few amicable pairs."
                          :code "map(lambda([x],amicable_p(first(x),second(x))), [[220, 284], 
        [1184, 1210], [2620, 2924], [5020, 5564], [6232, 6368]])"))


;; TODO: allow keeping only the later part of the sequence. Look for periods greater than 1
;;(defmfun1 ($aliquot_sequence :doc) ((k :pos-int) (n :non-neg-int) &optional (n2 0 n2-supplied-p :non-neg-int ) )
(defmfun1 ($aliquot_sequence :doc) ((k :pos-int) (n :non-neg-int) )
  "Returns the first <n> elements (counting from zero) in the aliquot sequence of <k>.
   The sequence is truncated at an element if it is zero or repeats the previous element."
  (let ( (seq (list k)) (cur k) (last 0) )
    (loop for i from 0 to n until (or (= 0 cur) (= cur last)) do
          (setf last cur)
          (setf cur (- (divisor-function-1 cur) cur))
          (setf seq (cons cur seq)))
    (if (= cur last) (setf seq (cdr seq)))
    (cons '(mlist simp) (nreverse seq))))

(examples::clear-examples "aliquot_sequence")
(examples::add-example "aliquot_sequence"
                       '( :pretext "Perfect numbers give a repeating sequence of period 1."
                                   :code "imap(lambda([x],aliquot_sequence(x,100)),[6,28,496,8128])")
                       '( :pretext "Aspiring numbers are those which are not perfect, but terminate with a repeating perfect number."
                                   :code "imap(lambda([x],aliquot_sequence(x,100)),[25, 95, 119, 143, 417, 445, 565, 608, 650, 652, 675, 685])"))




($set_match_form '((mlist) $aliquot_sum $divisor_function $divisor_summatory ))

(max-doc::see-also-group '( "divisor_function" "aliquot_sum" "aliquot_sequence" "divisor_summatory" "perfect_p" "abundant_p"))


#|
(defmfun1 ($sum_of_divisor_sigma :doc) ((x :non-neg-number) )
  "Returns the sum of the divisor function sigma_0(k) for k from 1 through n"
  (let ((sum 0) (u (floor (sqrt x))) (nx (floor x)))
                    (loop for k from 1 to u do
                          (setf sum (+ sum (* (+ k (/ nx k)) (floor (/ nx k))))))
                    (- sum (* u u))))
|#


