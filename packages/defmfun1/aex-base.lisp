(in-package :maxima)

(mext:mext-optimize)
(use-package :gjl.lisp-util)
(use-package :max-doc)

(defmfun1:set-file-and-package "aex-core.lisp" "aex")

(defvar *aex-core-dummy-vector* (make-array 0)
  "This is only here so that we can automatically check the type.")

(defstruct (aex
            (:print-function
             $print_aex))
  (head '(mlist simp) :type list)
  (arr *aex-core-dummy-vector* :type vector)
  (adjustable nil :type boolean))

(defun aex-make-n-head (n &key (adjustable t)  (head '(mlist simp)))
  (make-aex :head head :arr (make-array n :adjustable adjustable)
              :adjustable adjustable))

(defun aex-op (e)
  (getop (car (aex-head e))))

(defun aex-length (e)
  (length (aex-arr e)))
