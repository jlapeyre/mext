(in-package :maxima)

(defmfun $show_infolists ()
  (loop for list in 
        `($values $functions $macros $arrays $myoptions $props $aliases 
                   $rules $gradefs $dependencies $let_rule_packages $structures) do
                   (format t "  ~a:~% ~a~%" list ($sconcat (eval list)))))
