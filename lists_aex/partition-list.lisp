(if (find-package :partition-list ) t (defpackage :partition-list (:use :common-lisp )))
(in-package :partition-list)
(mext:mext-optimize)
(use-package :gjl.lisp-util)

(max-doc:set-cur-sec 'max-doc::lists-fandv)
(defmfun1:set-file-and-package "partition-list.lisp" "lists_aex")

;; (use-package :maxima)

;; Only lex are supported on input and output. aex is created temporarily in some cases.
;; aex support needs to be written

;; adding support for padding would be nice.

;; groups of n with offset n

(defun partition-list-a-1 (ein n)
  (declare (fixnum n))
  (let ((h (car ein)))
    (do ((e (cdr ein) (cdr e))
         (count 0)
         (res)
         (res1))
        ((null e) (cons h (nreverse res)))
      (declare (fixnum count))
      (push (car e) res1)
      (incf count)
      (when (= count n)
        (progn
          (push (cons h (nreverse res1)) res)
          (setf res1 nil)
          (setf count 0))))))

;; groups of n with offset d

(defun partition-list-b-1 (ein n d)
  (declare (fixnum n d))
  (let* ((e (maxima::$aex ein))
         (h (maxima::aex-head  e))
         (a (maxima::aex-arr e))
         (len (length a))
         (nlis (truncate (- (+ len d) n) d)))
    (declare (fixnum len nlis))
    (if (<= nlis 0) (list h)
        (cons h (loop for i fixnum from 0 below nlis
                      for start fixnum from 0 by d  collect
                     (cons h (loop for j fixnum from start below (+ n start) collect
                                  (aref a j))))))))


(maxima::mk-level-func-list partition-list-a partition-list-a-1 partition-list-a 1)
(maxima::mk-level-func-list partition-list-b partition-list-b-1 partition-list-b 2)

(max-doc::delete-doc-entry "partition_list")
(maxima::defmfun1 (maxima::$partition_list :doc) ((e :non-atom) (nlist :integer-or-listof)
                                                  &optional (dlist :integer-or-listof))
  :desc ("Omitting " :arg "d" " is equivalent to giving " :arg "d" " equal to " :argdot "n" " "
   :arg "e" " can be any expression, not only a list. If " :arg "n" " is a list, then "
   :mref "partition_list" " partitions at sucessively deeper levels with elements of "
   :argdot "n" " If " :arg "n" " and " :arg "d" " are lists, the first elements"
   "of " :arg "n" " and " :arg "d" " apply at the highest level and so on. If "
   :arg "n" " is a list and " :arg "d" " is a number, then the offset "
   :arg "d" " is used with each of the " :argdot "n")
  (maxima::s-or-mlist-to-list nlist)
  (cond ( dlist
         (if (numberp dlist)
             (setf dlist (make-list (length nlist) :initial-element dlist))
             (pop dlist))
         (partition-list-b e nlist dlist))
        (t
         (partition-list-a e nlist))))

(max-doc::add-call-desc
 '("partition_list" ("e" "n") ("partitions " :arg "e" " into sublists of length " :arg "n"))
 '("partition_list" ("e" "n" "d") ("partitions " :arg "e" " into sublists of length " :arg "n"
                                   " with offsets " :arg "d" ".")))

(examples::add-example "partition_list" '( :pretext "Partition the numbers from 1 through 10 into pairs."
                                :code ( "partition_list([1,2,3,4,5,6,7,8,9,10],2)")))
