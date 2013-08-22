;;; Copyright (C) 2012 John Lapeyre
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;;; Below `EC' denotes 'exact copy'

(in-package :descr1)
(mext:mext-optimize)

(defvar *prompt-prefix* "")
(defvar *prompt-suffix* "")

(maxdoc:set-cur-sec 'max-doc::doc-fandv)

(maxdoc:mdefmvar maxima::$read_docs_with_pager nil
 ("If true, then documenation printed by " :emref "describe" " or ? or ?? is sent through the pager
  specified by " :mrefdot "pager_command" " This will most
 likely only work with a command line interface under linux/unix
 with certain lisp implementations."))

(maxdoc:mdefmvar maxima::$doc_system_list nil
  ("A list of the documenatation systems that will be searched by ? and ??.
 This can be set to all avaliable systems with the function " :mrefdot "set_all_doc_systems"
 " If this variable is false, then all documentation is enabled."))

(maxima::defmfun1 (maxima::$set_all_doc_systems :doc)  ()
  "Enable all documentation databases for describe, ? and ??.
   This sets doc_system_list to a list of all doc systems."
  (setf maxima::$doc_system_list (maxima::$doc_system_list)))

(defmacro maybe-read-with-pager (&body body)
  `(if maxima::$read_docs_with_pager
       (maxima::read-output-with-pager ,@body)
       (progn ,@body)))

;; EC cl-info.lisp, 5.28
(defun print-prompt (prompt-count)
  (format t "~&~a~a~a"
	  *prompt-prefix*
	  (if (zerop prompt-count)
	      (intl:gettext "Enter space-separated numbers, `all' or `none': ")
	      (intl:gettext "Still waiting: "))
	  *prompt-suffix*))

;; EC cl-info.lisp, 5.28
(defvar +select-by-keyword-alist+
  '((noop "") (all "a" "al" "all") (none "n" "no" "non" "none")))

;; EC cl-info.lisp, 5.28
(defun parse-user-choice (nitems)
  (loop
     with line = (read-line) and nth and pos = 0
     while (multiple-value-setq (nth pos)
             (parse-integer line :start pos :junk-allowed t))
     if (or (minusp nth) (>= nth nitems))
     do (format *debug-io* (intl:gettext "~&Discarding invalid number ~d.") nth)
     else collect nth into list
     finally
     (let ((keyword
            (car (rassoc
                  (string-right-trim
                   '(#\space #\tab #\newline #\;) (subseq line pos))
                  +select-by-keyword-alist+
                  :test #'(lambda (item list)
                            (member item list :test #'string-equal))))))
       (unless keyword
         (setq keyword 'noop)
         (format *debug-io* (intl:gettext "~&Ignoring trailing garbage in input.")))
       (return (cons keyword list)))))

;; EC cl-info.lisp, 5.28
(defun select-info-items (selection items)
  (case (pop selection)
    (noop (loop
	   for i in selection
	   collect (nth i items)))
    (all items)
    (none 'none)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Moved from doc-system

;; This function only called by the next two functions
(defun collect-regex-matches (regex-string names)
  "Collect the query matches from multiple doc systems
   and sort the results."
  (cl-info::autoload-maxima-index) ; this should really be elsewhere
  (let ((res (apply 'append (mapcar #'(lambda (name)
                             (doc-system::ds-find-regex-matches regex-string
                              (doc-system::ds-table-get name)))
                         names))))
    res)
  (stable-sort (apply 'append (mapcar #'(lambda (name)
                                          (doc-system::ds-find-regex-matches regex-string
                                                                 (doc-system::ds-table-get name)))
                                      names))
               #'string-lessp :key #'caadr)) ;; note we sort on second element in list.

;; exact copy cl-info.lisp, 5.28
;; Not sure why we don't use the maxima version
;; This is only called by following functions
(defun regex-sanitize (s)
  "Precede any regex special characters with a backslash."
  (let
    ((L (coerce maxima-nregex::*regex-special-chars* 'list)))

    ; WORK AROUND NREGEX STRANGENESS: CARET (^) IS NOT ON LIST *REGEX-SPECIAL-CHARS*
    ; INSTEAD OF CHANGING NREGEX (WITH POTENTIAL FOR INTRODUCING SUBTLE BUGS)
    ; JUST APPEND CARET TO LIST HERE
    (setq L (cons #\^ L))

    (coerce (apply #'append
                   (mapcar #'(lambda (c) (if (member c L :test #'eq)
					     `(#\\ ,c) `(,c))) (coerce s 'list)))
            'string)))

;; Modified from cl-info.lisp, 5.28
;; Note that Maxima 5.30 has extensively rewritten the original function.
;; Why is this function here ?
(defun inexact-topic-match (topic names)
  (collect-regex-matches (regex-sanitize topic) names))

;; Modified from cl-info.lisp, 5.28
;; Note that Maxima 5.30 has extensively rewritten the original function,
;; largely to be more efficient, as I have done here.
(defun exact-topic-match (topic names)
  (collect-regex-matches (concatenate 'string "^" (regex-sanitize topic) "$") names))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; modified, cl-info.lisp, 5.28
;; Check each database in $doc_system_list, or all all if it is nil
;; Note that the stock code changed between 5.28 and 5.30
;; We have two copies of autoload-maxima-index in two packages and call each.
;; I think there is no reason for this.
(defun info-exact (x)
  (info-database::autoload-maxima-index)
  (let* ((names (if (eq nil maxima::$doc_system_list) (doc-system::ds-list)
                    (cdr maxima::$doc_system_list)))
         (exact-matches (exact-topic-match x names)))
    (if (null exact-matches)
      (progn
        (format t (intl:gettext "  No exact match found for topic `~a'.~% ~
                       Try `?? ~a' (inexact match) instead.~%~%") x x)
        nil)
      (progn
        (format t "~%")
        (maybe-read-with-pager (doc-system::print-match-items exact-matches))
        (if (some-inexact x (inexact-topic-match x names))
            (progn
            (format t "  There are also some inexact matches for `~a'.~%  Try `?? ~a' to see them.~%~%" x x)))
        t))))

;; modified, cl-info.lisp, 5.28
;; caadr instead of car
;; 
;; Stock function changed from 5.28 to 5.30
(defun some-inexact (x inexact-matches)
  (some #'(lambda (y)
;;            ( format t "comparing ~a and ~a~%" x y)
                  (not (equal y x))) (mapcar #'caadr inexact-matches)))

;; modified from display-items in cl-info.lisp, 5.28
;; Check all databases in $doc_system_list
;; Note that display-items is modified in 5.30.0.
(defun info (x)
  "Display a list of inexact matches and prompt the user to choose some."
  (info-database::autoload-maxima-index)
  (let ((names (if (eq nil maxima::$doc_system_list) (doc-system::ds-list)
                    (cdr maxima::$doc_system_list)))
         wanted tem)
    (setf tem (inexact-topic-match x names))
    (when tem
      (let ((nitems (length tem)))
        (when (> nitems 1)
          (loop for i from 0 for item in tem do
               (format t "~a " i) (doc-system::print-match-item-name item)
               (format t "~%")))
;;               (display-item-in-list i item)))
        (setq wanted
              (if (> nitems 1)
                  (loop
                     for prompt-count from 0
                     thereis (progn
                               (finish-output *debug-io*)
                               (print-prompt prompt-count)
                               (force-output)
                               (clear-input)
                               (select-info-items
                                (parse-user-choice nitems) tem)))
                  tem))
        (clear-input)
        (finish-output *debug-io*)
        (when (consp wanted)
          (format t "~%")
          (maybe-read-with-pager (doc-system::print-match-items wanted)))))
    (not (null tem))))

;; Replace the stock Maxima command with our own version.
;; Modified from from src/macdes.lisp
;; We call our own versions of info and info-exact, rather than cl-info version
;; No change in original code from 5.28 to 5.30
(maxima::defmspec maxima::$describe (x)
  (setf x (cdr x))
  (let ((topic (maxima::$sconcat (car x)))
        (exact-p (or (null (cadr x)) (eq (cadr x) 'maxima::$exact)))
        (*prompt-prefix* maxima::*prompt-prefix*)
        (*prompt-suffix* maxima::*prompt-suffix*))
    (if exact-p
        (info-exact topic)
        (info topic))))

;; Following is original code from Maxima 5.28
;; (defmspec $describe (x)
;;   (let ((topic ($sconcat (cadr x)))
;; 	(exact-p (or (null (caddr x)) (eq (caddr x) '$exact)))
;; 	(cl-info::*prompt-prefix* *prompt-prefix*)
;; 	(cl-info::*prompt-suffix* *prompt-suffix*))
;;     (if exact-p
;; 	(cl-info::info-exact topic)
;; 	(cl-info::info-inexact topic))))
