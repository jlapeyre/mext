(in-package :mext-maxima)

;;; Setup tables and functions for protecting symbols from `kill'.
;;; An interface function for maxima is in mext_defmfun1.

(defvar *dont-kill-share-table* 
  (make-hash-table :test 'equal)
  "Don't kill lists for stock maxima packages.")

(defun set-dont-kill-share (package list)
  "Store a list of dont-kill symbols for a share package in the table.
   This does not add them yet to the don't kill list. package is a string."
  (let ((name (maxima::$sconcat package)))
    (setf (gethash name *dont-kill-share-table*) list)))

(defun do-dont-kill-share (package)
  "Add the symbols for share package 'package' to the don't kill list."
  (let ((symbols (gethash package *dont-kill-share-table*)))
    (if symbols
        (apply #'add-to-dont-kill (gethash package *dont-kill-share-table*))
      (format t "No dont-kill symbols found for package '~a'~%" package))))

(in-package :maxima)

(mext::set-dont-kill-share "basic"
   '($herald_package $load_package $setup_autoload_macro
     $prog1  $push $pop $tr_ev $symbolcheck))

(mext::set-dont-kill-share "lrats"
   '($messlrats2 $fullratsubstflag $lratsubst $lratsubst1 $fullratsubst1 $fullratsubst
       $lrats))

;(mext-maxima::do-dont-kill-share "basic")
