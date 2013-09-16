(in-package :maxima)
(mext:mext-optimize)
(max-doc:set-cur-sec 'max-doc::runtime-fandv)
(defmfun1:set-file-and-package "timing.lisp" "runtime")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; timing
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is somehow not gc'ing intermediate results.
;; Because we switched to compile before load ??
;; apparently not.
;; Also not gc'ing well the final result if it is thrown
;; away. Not sure why
;  gc'ing. tested with
;  kill(labels);timing(lrange(10^7),0) repeatedly
;  but it is still possible to crash by doing
;  repeatedly with sbcl
; This seems ok: for i:1 thru 20 do timing(lrange(3*10^6),0,print->true);
(defmfun1:set-hold-all '$timing)
(defmfun1 ($timing :doc) (&rest exprs &opt ($print nil :bool) ($result t :bool)
                                ($time $all (:member '($all $cpu $real))))
  :desc
  ( :mref "timing" " evaluates each of the " :argcomma "exprs" " and returns a list of "
    " the total cpu time in seconds and real time in seconds used, together with the 
      result of the last expression."
    " See also " :emrefdot "showtime")
  (let* ((start-run  (get-internal-run-time))
         (start-real (get-internal-real-time))
         (to-sec (float internal-time-units-per-second))
         (last-result
          (eval
           `(let ((body ',exprs) each-result)
              (dolist (v body)
                (setq each-result (meval* v)))
               each-result)))
         (elapsed-run-seconds
          (/ (- (get-internal-run-time) start-run) to-sec))
         (elapsed-real-seconds
          (/ (- (get-internal-real-time) start-real) to-sec)))
    (if $print
        (progn
          (ecase $time
            ($all
             ($disp (format nil "user ~as    real ~as" elapsed-run-seconds elapsed-real-seconds)))
            ($cpu
             ($disp (format nil "user ~as" elapsed-run-seconds)))
            ($real
             ($disp (format nil "real ~as" elapsed-real-seconds))))
          (if $result last-result '$done))
      (let ((out-res '()))
        (when $result (push last-result out-res))
        (ecase $time 
          ($all
           (setf out-res (append (list elapsed-run-seconds elapsed-real-seconds) out-res)))
          ($cpu
           (push elapsed-run-seconds out-res))
          ($real
           (push elapsed-real-seconds out-res)))
        (mk-mlist out-res)))))

(add-call-desc '("timing" ("exprs") 
                 ("evaluates each of the expressions " :arg "exprs" 
                  " and returns a list of the total cpu time and real time in 
                   seconds used, together with the result of evaluating "
                  "the final expression."))
               '("timing" ("exprs" ("lit" "print->true"))
                 ("returns the result of evaluating the final expression and prints the "
                  "cpu and real time used."))
               '("timing" ("exprs" ("lit" "result->false"))
                 ("returns the time in seconds used, and discards all results."))
               '("timing" ("exprs" ("lit" "time->cpu"))
                 ("Return only the cpu (run) time used and the last result."))
               '("timing" ("exprs" ("lit" "time->real"))
                 ("Return only the real time used and the last result."))
               '("timing" ("exprs" ("lit" "time->all"))
                 ("Return both the cpu and real time used and the last result.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; with_output_to_string
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(max-doc::set-cur-sec 'max-doc::strings-fandv)

(defmfun1:set-hold-all '$with_output_to_string)
(defmfun1 ($with_output_to_string :doc) (&rest exprs)
  :desc
  ("Evaluates " :argcomma "expr_1" " " :argcomma "expr_2" " " :argcomma "expr_3" :dots ""
   " and writes any output generated to a string, which is returned.")
  (eval
    `(with-output-to-string (*standard-output*)
       (let ((body ',exprs) result)
         (dolist (v body)
           (setq result (meval* v)))
         result))))

(examples::clear-examples "with_output_to_string")
(examples::add-example "with_output_to_string" 
     '( :code "sreverse(with_output_to_string(for i:5 thru 10 do print(\"i! for i=\",i,i!)))"))
