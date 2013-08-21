;;;  doc-system manages distinct documentation databases and integrates them with
;;;  maxima online help.
;;;  functions.
;;;  Copyright (C) (2012) John Lapeyre. Licensed under GPL, v3 or greater. See the file
;;;  `LICENSE' in this directory.

(in-package :doc-system)
(mext:mext-optimize)
;(declaim (optimize (speed 3) (space 0) (safety 0) (debug 0)))

(defvar *doc-systems* nil)

(defvar *source-package* "doc-system")
(defvar *source-file-name* "doc-system.lisp")

(defun set-source-file-name (s)
  (if (stringp s) (setf *source-file-name* s)
    (maxima::merror1 (intl:gettext "maxima-dev-doc::set-source-file-name: Argument ~M is not a string.") s)))

(defun set-source-package (s)
  (if (stringp s) (setf *source-package* s)
    (maxima::merror1 (intl:gettext "maxima-dev-doc::set-source-package: Argument ~M is not a string.") s)))

(defun get-source-file-name  ()
  *source-file-name*)

(defun get-source-package  ()
  *source-package*)

(defun ds-table-arg-check (table)
  (if (hash-table-p table) t
      (maxima::merror1 (intl:gettext "doc-system::get-ds doc system table ~M is not a hash-table") table)))

(defun name-check (name caller)
  (if (stringp name) t
      (maxima::merror1 (intl:gettext (format nil "~A: doc system name ~A not a string."
                                    caller name)))))

(defun get-ds-argcheck (name ds-table)
  (and  (name-check name "doc-system::get-ds")
        (ds-table-arg-check ds-table)))

(defun get-ds-1 (name ds-table)
  (when (get-ds-argcheck name ds-table)
    (gethash name ds-table)))

(defsetf get-ds-1 (name ds-table) (ds)
  `(progn (if (and (get-ds-argcheck ,name ,ds-table)
                   (doc-system-p ,ds))
              (setf (gethash  ,name ,ds-table) ,ds)
              (maxima::merror1 (intl:gettext "doc-system: item ~M to enter in doc systems table is not a doc system.")
                                    ,ds))))
    
(defstruct (doc-system)
  (name "" :type string)
  (data)
  (search-key-func (lambda () ) :type function)
;;  (search-key-func)
  (str-item-name-func)
  (str-item-func)
  (full-match-arg)) ; last slot is kinda a hack for the infor database


(defmacro mk-ds-check (name slot type-c t1 t2)
  `(defun ,name (ds)
     (if (,type-c (,slot ds)) t
         (maxima::merror1 (intl:gettext (concatenate 'string "ds-make-check: "  ,t1 " ~a not a " ,t2 "."))
                         (,slot ds)))))

(mk-ds-check check-data doc-system-data hash-table-p "data slot" "hash-table")
(mk-ds-check check-str-item-name-func doc-system-str-item-name-func functionp "str-item-name-func" "function")
(mk-ds-check check-str-item-func doc-system-str-item-func functionp "str-item-func" "function")
(mk-ds-check check-search-key doc-system-search-key-func functionp "search-key-func" "function")

(defun check-doc-system (ds)
  (and
   (check-data ds)
   (check-str-item-func ds)
   (check-str-item-name-func ds)
   (check-search-key ds)
   (name-check (doc-system-name ds) "check-doc-system")))

(defun ds-table-check-init ()
  (cond ((hash-table-p *doc-systems*) t)
        ((null *doc-systems*)
         (setf *doc-systems* (make-hash-table :test 'equal)))
        (t (maxima::merror1 (intl:gettext "check-doc-systems-table: illegal value for check-doc-system-table")))))

(defun ds-table-get (name)
  (name-check name "doc-systems-table-get")
  (let ((ds (get-ds-1 name *doc-systems*)))
    (if ds ds
        (maxima::merror1 (intl::gettext "doc-system-table-get: doc system ~M does not exist") name))))

(defun ds-table-set (ds)
  (let ((name (doc-system-name ds)))
    (name-check name "ds-table-set")
    (setf (get-ds-1 name *doc-systems*)  ds)))

(defun ds-list ()
  "Get a like of the tags of all the registered documentation systems."
  (get-hash-keys *doc-systems*))

(maxima::defmfun maxima::$doc_system_list ()
  "Get a list of the tags of all the registered documentation systems."
  (cons (list 'maxima::mlist 'maxima::simp) (get-hash-keys *doc-systems*)))

(defun ds-find-regex-matches (regex-string ds)
  "Find matches in the hash of a single doc system.  This is
   copied from older search code. Here we take a doc-system
   struct rather than a hash-table. We return a list with
   the name of the ds as the first element and the (cons key item)
   of the matching item as the second. We allow searching in  a
   string returned by a function, rather than searching on the key."
  (let*
      ((regex (maxima-nregex::regex-compile regex-string :case-sensitive nil))
       (regex-fcn (coerce regex 'function))
       (search-key-func (doc-system-search-key-func ds))
       (name (doc-system-name ds))
       (hashtable (doc-system-data ds))
       (regex-matches nil))
    (maphash
     #'(lambda (key value)
         (if (funcall regex-fcn (funcall search-key-func key value))
             (progn
               (setq regex-matches (cons (list name `(,key . ,value)) regex-matches)))
             nil))
     hashtable)
    regex-matches))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Why are the following four functions here and not in maxdoc ?
;; It seems they are only called from there ???

;; This function only called by the next two functions
(defun collect-regex-matches (regex-string names)
  "Collect the query matches from multiple doc systems
   and sort the results."
  (cl-info::autoload-maxima-index) ; this should really be elsewhere
  (let ((res (apply 'append (mapcar #'(lambda (name)
                             (ds-find-regex-matches regex-string
                              (ds-table-get name)))
                         names))))
    res)
  (stable-sort (apply 'append (mapcar #'(lambda (name)
                                          (ds-find-regex-matches regex-string
                                                                 (ds-table-get name)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-match-item (match-item)
  (let* ((ds-name (car match-item))
         (ds (ds-table-get ds-name))
         (ds-item (cdr (second match-item))))
        (format t "~a" (funcall (doc-system-str-item-func ds) ds-item))))

(defun print-match-items (match-items)
  (dolist (match-item match-items)
    (print-match-item match-item)))

(defun print-match-item-name (match-item)
  (let* ((ds-name (car match-item))
         (ds (ds-table-get ds-name))
         (ds-item (cdr (second match-item))))
    (if (doc-system-full-match-arg ds)
        (format t "~a" (funcall (doc-system-str-item-name-func ds) match-item))
    (format t "~a" (funcall (doc-system-str-item-name-func ds) ds-item)))))

(defun print-match-items-name (match-items)
  (dolist (match-item match-items)
    (print-match-item-name match-item)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API (quasi) functions

;; why can't a struct do this?
(defun ds-make-check (&rest args)
  "Make a doc-system struct to register a database with document system"
 (let ((ds (apply #'make-doc-system args)))
   (if (check-doc-system ds) ds
       (maxima::merror1 (intl:gettext "ds-make-check: illegal doc-system initialization")))))

(defun ds-registered-p (ds-or-name)
  (let ((name
         (if (doc-system-p ds-or-name) (doc-system-name ds-or-name)
             ds-or-name)))
    (name-check name "ds-registered-p")
    (ds-table-check-init)
    (if (get-ds-1 name *doc-systems*) t nil)))

(defun ds-register (ds)
  (unless (doc-system-p ds)
    (maxima::merror1 (intl:gettext "doc-system::ds-register ~M is not a doc system") ds))
  (ds-table-check-init)
  (if (ds-registered-p (doc-system-name ds))
      (maxima::merror1 (intl:gettext "register-ds: doc system ~M already registered")
                      (doc-system-name ds)))
  (ds-table-set ds))

(defun ds-make-and-register (&rest args)
  (let ((ds (apply #'ds-make-check  args)))
    (ds-table-check-init)
    (ds-register ds)))

(defun ds-de-register (name)
  (name-check name "de-register-ds")
  (if (not (ds-registered-p name))
      (maxima::merror1 (intl:gettext "de-register-doc-system: doc system ~M not registered") name)
      (remhash name *doc-systems*)))
