;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of $table function
;;;
;;; From Maxima function can be called in the form:
;;; table(expression, iterator1, iterator2,...);
;;; At least one iterator is needed, further iterators are optional.
;;; Iterators can have different forms:
;;;
;;; [number]
;;; Evaluates expression 'number' times. If number is not an integer
;;; or a floating point number, then $float is called. If we have a
;;; floating point number, it is truncated into an integer. Iterators
;;; like [0], [-4], [0.2], [-sin(4)] in effect return an empty list.
;;; This type of iterator is the fastest, since no variable is bound.
;;;
;;; [variable, initial, end, step]
;;; Returns a list of evaluated expressions where 'variable' (a symbol) is set
;;; to a value. First element of the returned list is expression evaluated
;;; with variable set to initial. i-th element of the returned list is expression
;;; evaluated with variable set to 'initial' + (i-1)*step. The iteration stops
;;; once the value is greater (if step is positive) or smaller (if step negative)
;;; than `end'. Requirement: Difference of end - intial must return a $numberp
;;; number. Step must be nonzero $numberp number. This allows for iterators of
;;; rather general forms like [i, %i - 2, %i, 0.1b0] ...
;;;
;;; [variable, initial, end]
;;; This iterator uses a step of 1 and is equal to [variable, initial, end, 1].
;;;
;;; [variable, end]
;;; This iterator uses a step of 1 and initial value is also 1, therefore it's
;;; equal to iterator [variable, 1, end, 1].
;;; 
;;; [variable, Maxima list or set]
;;; This iterator evaluates the expression for each element of the Maxima list
;;; or set. Variable is sequentally set to values in the list or set during
;;; iteration. The resulting list has the same number of elements as the list/set
;;; in the iterator.
;;;
;;; NOTE on evaluation of iterators:
;;; All iterator elements, except the variable, get evaluated right before the iteration
;;; starts. Values bound in previous iterators, are used for evaluation, so constructs like
;;; table( 1 , [i,5],[j,i]);
;;; work as expected (iterator [j,i] gets evaluated for each iteration of previous
;;; iterator [i,j]). Thus it is possible to use iterators for manipulating iterators:
;;; Example of 'sampling' sine with different steps
;;; table( sin(t) , [step,[0.1,0.5,1.0]], [t,0,10,step]);
;;;
;;; IMPLEMENTATION DETAILS
;;;
;;; The goal of $table is a fast and flexible Maxima function for creating
;;; (nested) lists. Besides being fast, it should also be 'correct'.
;;; For binding values, `mbinding' is used - which throws an error if we
;;; are binding to protected sybols, and restores the original values
;;; of variables on exit.
;;; Because `mbinding' is slow (= takes a considerable amount of time
;;; compared to a simple maxima expression evaluation like (meval '$i))
;;; mbinding is called only once(!) per every iterator and not once
;;; per evaluation of expression. Because the number of iterators is
;;; usually 1 or 2, `mbinding' is not performance critical for $table.
;;; Per evaluation, values are set via (setf (symbol-value var) value)
;;; which is very quick - ofcourse this happens within `mbinding'.
;;; Thus we gain very speedy value binding, while perserving the correctnes
;;; of `mbinding'.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This library is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the 
;;; Free Software Foundation; either version 2 of the License, or (at
;;; your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along 
;;; with this library; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; Copyright (C) 2009 Ziga Lenarcic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
(mext:mext-optimize)
(max-doc:set-cur-sec 'max-doc::lists-fandv)
(defmfun1:set-mext-package "lists_aex")

;; ITERATING FUNCTIONS for $table
;;
;; table-general-iterator:
;; Takes the first of iterators and decides what kind of iteration to call.
;; For more than one iterators, iterating functions (which all return a proper
;; Maxima list):
;; table-step-iterator
;; table-list-iterator
;; table-number-iterator
;; then call this function again, to determine the next iteration.

(defun table-general-iterator (expr iterators)
  ;; pop the first iterator and call proper iterating function
  (let* ((next-iterator (pop iterators))
         (n (length next-iterator)))

   (cond ;; decide which iterator to call, depends on length of iterator list []
     ;; variable iterator with step [i, 1, 100, 5]
     ((= n 4) (table-step-iterator expr iterators
                                   (first next-iterator) ;; don't evaluate symbol - var
                                   (meval (second next-iterator))
                                   (meval (third next-iterator))
                                   (meval (fourth next-iterator))))
     ;; variable iterator [i,1,100] = step is 1
     ((= n 3) (table-step-iterator expr iterators
                                   (first next-iterator)
                                   (meval (second next-iterator))
                                   (meval (third next-iterator))
                                   1))
     ;; variable iterator with two elements - we have two choices
     ((= n 2) (let ((second-iterator-element
                      (meval (second next-iterator)))) ; compute second el.
                (if (or ($listp second-iterator-element) ($setp second-iterator-element))
     ;; variable iterator with list of values [i, [1,2,3,4]] or set [i,{1,2,3}]
       (table-list-iterator expr iterators
                            (first next-iterator) ; var
                            (rest second-iterator-element)) ; list of values
     ;; variable iterator [i,100]
       (table-step-iterator expr iterators
                            (first next-iterator)   ; var
                            1                       ; var-start
                            second-iterator-element ; var-end
                            1))))                   ; var-step
     ;; number iterator [100]
     ((= n 1) (table-number-iterator expr iterators (meval (first next-iterator))))

     ;; empty iterator [] or iterator too long
     (t (merror1 "table: Iterator must be a Maxima list of length between 1 and 4.")))))

;; Simplest, fastest iteration (no value bound):
;; handles iterations like table(a, [100]);
(defun table-number-iterator (expr iterators num)
  ;; error check, we want a number
  (unless (numberp num)
    (setf num ($float num)))
  (unless (integerp num)
    (if (realp num)
      (setf num (truncate num))
      (merror1 "table: ~M is not a number." num)))

  ;; calculation, ans holds the resulting maxima list
    (let ((ans '()))
      (if (null iterators)
        ;; no more iterators, just evaluate expr num-times
        (dotimes (i num) (push (meval expr) ans))
        ;; there are more iterators, call table-general-iterator
        (dotimes (i num) (push (table-general-iterator expr iterators) ans)))
      ;; reurn Maxima list
      `((mlist simp) ,@(nreverse ans))))

;; return mlist, iterates over one variable with step
(defun table-step-iterator (expr iterators var var-start var-end var-step)
  (unless ($symbolp var) (merror1 "table: ~M is not a symbol." var))
  (cond
    ((and (typep var-start 'fixnum)  (typep var-step 'fixnum))
     (if (> var-step 0)
         (table-step-iterator-lisp-numbers-fixnum-pos expr iterators var var-start var-end var-step)
         (table-step-iterator-lisp-numbers-fixnum-neg expr iterators var var-start var-end var-step)))
    ((or (typep var-start 'flonum) (typep var-step 'flonum))
     (setf var-step (float var-step) var-start (float var-start))
     (if (> var-step 0)
         (table-step-iterator-lisp-numbers-float-pos expr iterators var var-start var-end var-step)
         (table-step-iterator-lisp-numbers-float-neg expr iterators var var-start var-end var-step)))
    ((and (numberp var-start) (numberp var-end) (numberp var-step))
     (table-step-iterator-lisp-numbers expr iterators var var-start var-end var-step))
    (t 
     (let ()
       (unless ($numberp ($float (sub* var-end var-start)))
         (merror1 "table: Difference of ~M and ~M is not a number." var-end var-start))
       (unless ($numberp ($float var-step)) (merror1 "table: ~M is not a number." var-step))
       ;; OK we have maxima numbers!
       (table-step-iterator-maxima-numbers expr iterators var var-start var-end var-step)))))

(defmacro mk-table-step-iterator-lisp-numbers (function-name step-sign type)
  (let ((test-code (if (< step-sign 0) `(< i var-end) `(> i var-end))))
    `(defun ,function-name (expr iterators var var-start var-end var-step)
;;       (declare (,type var-start var-end var-step ))
       (when (zerop var-step) (merror1 "table: Step is zero."))
       (mbinding ((list var) (list var-start))
                 (if (null iterators)
                     (do ((i var-start (+ i var-step))
                          (ans))
                         (,test-code (cons '(mlist simp) (nreverse ans)))
                       (declare (,type i))
                       (set var i)
                       (push (meval expr) ans))
                     (do ((i var-start (+ i var-step)) ; there are more iterators, call table-general-iterator
                          (ans))
                         (,test-code (cons '(mlist simp) (nreverse ans)))
                       (declare (,type i))
                       (set  var i)
                       (push (table-general-iterator expr iterators) ans)))))))
  
(mk-table-step-iterator-lisp-numbers table-step-iterator-lisp-numbers-fixnum-pos 1 fixnum)
(mk-table-step-iterator-lisp-numbers table-step-iterator-lisp-numbers-fixnum-neg -1 fixnum)
(mk-table-step-iterator-lisp-numbers table-step-iterator-lisp-numbers-float-pos 1 flonum)
(mk-table-step-iterator-lisp-numbers table-step-iterator-lisp-numbers-float-neg -1 flonum)

;; this function only works when var-start, var-end and var-step are integers of floats (lisp)
;; It's faster than maxima number function, which uses more complicated addition and
;; < > checks.
(defun table-step-iterator-lisp-numbers (expr iterators var var-start var-end var-step)
;  (declare (fixnum var-start var-end var-step)) split out integer code
  (when (zerop var-step) (merror1 "table: Step is zero."))

  (when (or (floatp var-start) (floatp var-step)) ;; convert to floating point if step or start floatp
    (setf var-start (float var-start))
    (setf var-end (float var-end))
    (setf var-step (float var-step)))

  (mbinding ((list var) (list var-start))
    (let ((end-test (if (< (signum var-step) 0.0)
                      ;; negative step
                      #'<
                      ;; positive step
                      #'>)))
        ;; calculation, ans holds the resulting maxima list
        (if (null iterators)
          ;; no more iterators, just evaluate expr num-times
          (do ((i var-start (+ i var-step))
               (ans))
            ((funcall end-test i var-end) (cons '(mlist simp) (nreverse ans)))
            (setf (symbol-value var) i)
            (push (meval expr) ans))
          ;; there are more iterators, call table-general-iterator
          (do ((i var-start (+ i var-step))
               (ans))
            ((funcall end-test i var-end) (cons '(mlist simp) (nreverse ans)))
            (setf (symbol-value var) i)
            (push (table-general-iterator expr iterators) ans))))))

;; step iteration for 'maxima numbers', slower than lisp numbers
(defun table-step-iterator-maxima-numbers (expr iterators var var-start var-end var-step)
  ;; check for errors in iterator
  (when (= ($signum var-step) 0.0) (merror1 "table: Step is zero."))
  ;; convert var-start to that of type var-step
  (when ($floatnump var-step) (setf var-start ($float var-start)))
  (when ($bfloatp var-step) (setf var-start ($bfloat var-start)))

  (mbinding ((list var) (list var-start))
    ;; proceed with calculation
    (let ((end-test (if (< ($signum var-step) 0)
                      ;; negative step
                      #'mlsp
                      ;; positive step
                      #'mgrp)))
      ;; calculation, ans holds the resulting maxima list
      (if (null iterators)
        ;; no more iterators, just evaluate expr num-times
        (do ((i var-start (add* i var-step))
             (ans))
          ((funcall end-test i var-end) (cons '(mlist simp) (nreverse ans))) ; when i > num, return maxima list
          (setf (symbol-value var) i)
          (push (meval expr) ans))
        ;; there are more iterators, call table-general-iterator
        (do ((i var-start (add* i var-step))
             (ans))
          ((funcall end-test i var-end) (cons '(mlist simp) (nreverse ans))) ; when i > num, return maxima list
          (setf (symbol-value var) i)
          (push (table-general-iterator expr iterators) ans))))))

;; for iterators of the form [i, [1,2,3,4]] or [i, {1,2,3,4}]
;; var-value-list is a 'lisp lisp': '(1 2 3 4) for the above cases
(defun table-list-iterator (expr iterators var var-value-list)
  ;; error checks
  (unless ($symbolp var) (merror1 "table: ~M is not a symbol." var))

  ;; mbinding should throw an error if the `var' is a protected
  ;; symbol like $%pi or $%e
  (mbinding ((list var) (list (car var-value-list)))
    ;; calculation, ans holds the resulting maxima list
    (let ((ans '()))
      (if (null iterators)
        ;; no more iterators, just evaluate expr num-times
        (dolist (i var-value-list)
          (setf (symbol-value var) i) 
          (push (meval expr) ans))
        ;; there are more iterators, call table-general-iterator
        (dolist (i var-value-list)
          (setf (symbol-value var) i) 
          (push (table-general-iterator expr iterators) ans)))
      ;;return
      `((mlist simp) ,@(nreverse ans)))))

;; Maxima function $table
;; Defined by `defmspec' to prevent expression evaluation

(defmfun1:set-hold-all '$table)
(max-doc:add-doc-entry '( :name "table" :type "Function"))
(defmfun-ae ($table) ( expr (iterator1 :list) &rest (iterators :list) )
  (push iterator1 iterators)
  (let ((res
         (table-general-iterator expr (mapcar #'cdr iterators))))
    (if (eq o-type '$ml) res ; convert to depth of table
        ($raex res (length iterators)))))

(max-doc:see-also "table" '("makelist" "lrange" "constant_list"))
(max-doc:author "table" "Ziga Lenarcic")
(max-doc:copyright "table" '( 2009 "Ziga Lenarcic"))

(add-call-desc '( "table" ("expr" ("list" "n"))
                  ("Evaluates expression " :arg "number" " times. If " :arg "number" " is not an integer
  or a floating point number, then " :emref "float" " is called. If we have a
  floating point number, it is truncated into an integer.
  This type of iterator is the fastest, since no variable is bound.")))


(add-call-desc '( "table" ("expr" ("list" "variable" "initial" "end" "step") )
    ("Returns a list of evaluated expressions where " :arg "variable" " (a symbol) is set
      to a value. The first element of the returned list is " :arg "expression" " evaluated
      with " :arg "variable" " set to " :argdot "initial" 
      " The " :math "i" "-th element of the returned list is " :arg "expression" " 
      evaluated with " :arg "variable" " set to " 
      :arg "initial" :math " + (i-1) " :arg "step" ". The iteration stops
      once the value is greater (if " :arg "step" " is positive) or smaller (if " :arg "step" " is negative)
      than " :argdot "end" " Requirement: The difference between " :arg "end" " and " :arg "intial" 
      " must return a " :emref "numberp" " number. " :arg "step" 
      " must be a nonzero " :emref "numberp" " number. This allows for iterators of
      rather general forms like " :code "[i, %i - 2, %i, 0.1b0] " :dots "." )))

(add-call-desc '( "table" ("expr" ("list" "variable" "initial" "end"))
                  ("This iterator uses a step of 1 and is equal to  [" :arg "variable" ","
                   :arg "initial" "," :arg "end" ", 1].")))

(examples:clear-add-example "table"
                       '( :pretext "Make a list of function values"
                         :code ( "table(sin(x),[x,0,2*%pi,%pi/4])" ))
                       '( :pretext "Make a nested list."
                         :code ( "table( x^y, [x,1,2], [y,1,2])" )))
