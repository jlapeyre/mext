;;; Copyright (C) 2012 John Lapeyre
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(defmfun1:set-file-and-package "simple-doc.lisp" "maxdoc")

(in-package :simple-doc)
(mext:mext-optimize)
;(declaim (optimize (speed 3) (space 0) (safety 0) (debug 0)))
(defvar *simple-doc-hashtable* nil)

(defstruct (entry)
  (name)
  (contents))

(defun init-table ()
  (setf *simple-doc-hashtable* (make-hash-table :test 'equal)))

(defmacro defun-simp-doc (name args &body body)
  `(defun ,name (,@args &optional (table  *simple-doc-hashtable*))
     ,@body))

(defun check-database (h)
  (if (hash-table-p h) t
      (maxima::merror1 (intl:gettext "simple-doc: simple-doc data base %M is not a hash-table.") h)))

(defun check-name (name)
  (if (stringp name) t
      (maxima::merror1 (intl:gettext "simple-doc: key %M is not a string.") name)))

(defun check-get-item-args (name h)
  (and (check-database h) (check-name name)))

(defun get-item-1 (name h)
  (when (check-get-item-args name h)
    (gethash name h)))

(defsetf get-item-1 (name h) (val)
    `(progn (check-get-item-args ,name ,h)
            (setf (gethash ,name ,h) ,val)))

(defun check-init-table ()
  (if (hash-table-p *simple-doc-hashtable*) t
    (init-table)))

(defun-simp-doc exists-item (item-name)
  (maxima::maxima-symbol-to-string item-name)
  (not (null (get-item-1 item-name table))))

(defun-simp-doc add-item (item-name content)
  (maxima::maxima-symbol-to-string item-name)
  (if (exists-item item-name) nil
      (let ((item (make-entry :name item-name :contents content)))
        (setf (get-item-1 item-name table)
          item))))

(defun-simp-doc remove-item (item-name)
  (maxima::maxima-symbol-to-string item-name)
  (remhash item-name table))

(defun-simp-doc get-item (item-name)
  (maxima::maxima-symbol-to-string item-name)
  (get-item-1 item-name table))

(defun get-item-contents (item-name)
  (entry-contents (get-item item-name)))

(defun search-key (key item)
  (declare (ignore key))
  "We only search on the 'name' of the entry."
  (entry-name item))

(defun str-item  (item)
  (format nil "~a~%~%" (entry-contents item)))

(defun str-item-name (item)
  (format nil "~a  *(User document)" (entry-name item)))

(check-init-table)

;;;;;;;;;; test interface

(when (doc-system::ds-registered-p "simple-doc")
  (doc-system::ds-de-register "simple-doc"))

(doc-system::ds-make-and-register
 :name "simple-doc"
 :data *simple-doc-hashtable*
 :search-key-func  #'search-key
 :str-item-func  #'str-item
 :str-item-name-func  #'str-item-name)

(simple-doc::add-item "function_1" "-- Function 1: This is a great function")
(simple-doc::add-item "function_2" "-- Function 2: This is also a great function")
(simple-doc::add-item "function_3" "-- Function 3: This is also a great function")

