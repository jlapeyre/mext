(in-package :maxima)

;;; There is a small amount of support for aex
;;; in defmfun1-max.lisp. So we define a few things
;;; here rather than in packge aex.

(mext:mext-optimize)
(use-package :gjl.lisp-util)
(use-package :max-doc)

(defmfun1:set-file-and-package "aex-base.lisp" "defmfun1")

(defvar *aex-core-dummy-vector* (make-array 0)
  "This is only here so that we can automatically check the type.")

(defstruct (aex
            (:print-function
             $print_aex))
  (head '(mlist simp) :type list)
  (arr *aex-core-dummy-vector* :type vector)
  (adjustable nil :type boolean)
  (element-type t))

(defun max-to-lisp-element-type (e)
  (cond ((eq e '$fixnum) 'fixnum)
        ((eq e '$flonum) 'flonum)
        ((eq e '$float) 'flonum)
        (t e)))

(defun aex-make-n-head (n &key (adjustable t)  (head '(mlist simp))
                          (element-type t) (initial-element nil) (supply-init-element nil))
  (setf element-type (max-to-lisp-element-type element-type))
  (make-aex :head head :arr 
            (if supply-init-element
                (make-array n :adjustable adjustable :element-type element-type :initial-element initial-element)
              (make-array n :adjustable adjustable :element-type element-type))
              :adjustable adjustable :element-type element-type))

(defun aex-op (e)
  (getop (car (aex-head e))))

(defun aex-length (e)
  (length (aex-arr e)))
