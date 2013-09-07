(in-package :maxima)
(mext:mext-optimize)
(max-doc:set-cur-sec 'max-doc::runtime-fandv)
(defmfun1:set-file-and-package "timing.lisp" "runtime")

;; TODO, option to report wall clock time, as well
(defmfun1:set-hold-all '$timing)
(defmfun1 ($timing :doc) (&rest exprs &opt ($print nil :bool) ($result t :bool))
  :desc
  ( :mref "timing" " evaluates each of the " :argcomma "exprs" " and returns a list of "
    " the total time in seconds used, together the result of the last expression. "
    " See also " :emrefdot "showtime")
  (let (( start (get-internal-run-time)))
    (let ((last-result
           (eval
            `(let ((body ',exprs) each-result)
               (dolist (v body)
                 (setq each-result (meval* v)))
               each-result)))
          (elapsed-seconds
           (/ (- (get-internal-run-time) start)
               (float internal-time-units-per-second))))
      (if $print
          (progn
              ($disp elapsed-seconds)
            (if $result last-result '$done))
        (if $result
            (make-mlist-simp elapsed-seconds last-result)
          elapsed-seconds)))))

(add-call-desc '("timing" ("exprs") 
                 ("evaluates each of the expressions " :arg "exprs" 
                  " and returns a list of the time in seconds used, together with the result of evaluating "
                  "the final expression."))
               '("timing" ("exprs" "print->true")
                 ("returns the result of evaluating the final expression and prints the "
                  "time in seconds used."))
               '("timing" ("exprs" "result->false")
                 ("returns the time in seconds used, and discards all results.")))
