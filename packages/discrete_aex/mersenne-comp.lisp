;;;  Copyright (C) (2012,2013) John Lapeyre. Licensed under GPL, v3 or greater. See the file
;;;  `LICENSE' in this directory.

;;; This data could be organized better.
;;; Eg, in a struct or something.

;;; We don't compute and store these.
;;; Tried writing a big number (5*10^5 digits) to file and wrapping with
;;; (defvar *bignumber* xxxxxxx ....)
;;; then compiling.
;;; Then loading the fasl is much slower than recomputing the number.
;;; Maybe 50 times slower.

(defvar *max-mersenne-rank* (length *mersenne-exponents*))

(defun generate-mersenne-number (x)
  (- (expt 2 x) 1))

(defun generate-perfect-number (p)
  (let ((m (generate-mersenne-number p)))
    (* (/ (+ m 1) 2) m)))

;; The keys here are the exponents p (or n for composite)
(defvar *perfect-numbers* (make-hash-table :test 'eq))
(defvar *mersenne-numbers* (make-hash-table :test 'eq))

(defun get-mersenne-exponent-by-rank (n)
  (elt *mersenne-exponents* (1- n)))

(defun get-mersenne-num-dig-by-rank (n)
  (elt *mersenne-prime-num-digits* (1- n)))

(defun get-perfect-num-dig-by-rank (n)
  (elt *perfect-numbers-num-digits* (1- n)))

(defun get-mersenne-date-by-rank (n)
  (elt *mersenne-numbers-full-date* (1- n)))

n(defun get-mersenne-discoverer-by-rank (n)
  (elt *mersenne-numbers-full-discoverer* (1- n)))

(defun get-mersenne-method-by-rank (n)
  (elt *mersenne-numbers-method* (1- n)))

;; Hash is not neccessary! It takes no time to compute these
(defun get-mersenne-prime-by-rank (n)
  (let* ((p (get-mersenne-exponent-by-rank n))
         (m (gethash p *mersenne-numbers*)))
    (if m m
      (let ((m1 (generate-mersenne-number p)))
        (setf (gethash p *mersenne-numbers*) m1)))))

;; Hash is neccessary. Higher ones take a long time
(defun get-perfect-number-by-rank (n)
  (let* ((p (get-mersenne-exponent-by-rank n))
         (perf (gethash p *perfect-numbers*)))
    (if perf perf
      (let ((perf1 (generate-perfect-number p)))
        (setf (gethash p *perfect-numbers*) perf1)))))

(defmfun1 ($mersenne_by_rank :doc) ((n :thread (:int-range 1 48)))
  :desc
  ("Returns the " :arg "n" "th mersenne prime.")
  (get-mersenne-prime-by-rank n))

(defmfun1 ($perfect_number_by_rank :doc) ((n :thread (:int-range 1 48)))
  :desc
  ("Returns the " :arg "n" "th perfect number.")
  (get-perfect-number-by-rank n))

(defmfun1 ($mersenne_exponent :doc) ((n :thread (:int-range 1 48)))
  :desc
  ("Returns the exponent " :code "p" " for the " :arg "n" "th mersenne prime.")
  (get-mersenne-exponent-by-rank n))

(defmfun1 ($mersenne_numdig :doc) ((n :thread (:int-range 1 48)))
  :desc
  ("Returns the number of digits in the " :arg "n" "th mersenne prime.")
  (get-mersenne-num-dig-by-rank n))

(defmfun1 ($perfect_numdig :doc) ((n :thread (:int-range 1 48)))
  :desc
  ("Returns the number of digits in the " :arg "n" "th perfect number.")
  (get-perfect-num-dig-by-rank n))

(defmfun1 ($mersenne_date :doc) ((n :thread (:int-range 1 48)))
  :desc
  ("Returns date of discovery of the " :arg "n" "th mersenne prime.")
  (get-mersenne-date-by-rank n))

(defmfun1 ($mersenne_discoverer :doc) ((n :thread (:int-range 1 48)))
  :desc
  ("Returns the discoverers of the " :arg "n" "th mersenne prime.")
  (get-mersenne-discoverer-by-rank n))

(defmfun1 ($mersenne_method :doc) ((n :thread (:int-range 1 48)))
  :desc
  ("Returns the method of discovery of the " :arg "n" "th mersenne prime.")
  (get-mersenne-method-by-rank n))
