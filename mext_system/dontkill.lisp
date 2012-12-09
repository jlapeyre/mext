(in-package :mext-maxima)

(defvar *dont-kill-share-table* 
  (make-hash-table :test 'equal)
  "Don't kill lists for stock maxima packages.")

(defun set-dont-kill-share (package list)
  (let ((name (maxima::$sconcat package)))
    (setf (gethash name *dont-kill-share-table*) list)))

(defun do-dont-kill-share (package)
  (apply #'add-to-dont-kill (gethash package *dont-kill-share-table*)))

(set-dont-kill-share "basic"
   '( $symbolcheck $herald_package $load_package $setup_autoload_macro
     $prog1  $push $pop $tr_ev))

(do-dont-kill-share "basic")
