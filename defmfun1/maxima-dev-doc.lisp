;;; Copyright (C) 2012 John Lapeyre
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(in-package :maxima-dev-doc)
(mext:mext-optimize)
;(declaim (optimize (speed 3) (space 0) (safety 0) (debug 0)))

(defstruct (doc-item)
  (name nil)
  (source-file nil)
  (package nil)
  (protocol nil)
  (type 'function)
  (contents nil))

(defvar *developer-doc-hash* (make-hash-table :test 'equal))
(defparameter *source-file-name* "")
(defparameter *source-package* "")

(defun minv (e)
  (maxima::maybe-invert-string-case (format nil "~a" e)))

#|  moved to doc-system
(defun set-source-file-name (s)
  (if (stringp s) (setf *source-file-name* s)
    (maxima::merror1 (intl:gettext "maxima-dev-doc::set-source-file-name: Argument ~M is not a string.") s)))

(defun set-source-package (s)
  (if (stringp s) (setf *source-package* s)
    (maxima::merror1 (intl:gettext "maxima-dev-doc::set-source-package: Argument ~M is not a string.") s)))
|#

(defun new-make-doc-item (name protocol contents )
  (make-doc-item :name name :contents contents
                 :protocol protocol
                 :package (doc-system::get-source-package)
                 :source-file  (doc-system::get-source-file-name)))


(defun new-make-doc-var-item (name contents var-type)
  (make-doc-item :name name :contents contents
                 :protocol nil
                 :type var-type
                 :package (doc-system::get-source-package)
                 :source-file (doc-system::get-source-file-name)))


(defun add-item (name-str protocol docstring)
  (setf (gethash name-str *developer-doc-hash*)
        (new-make-doc-item name-str protocol docstring)))

(defun mk-ddefun-form (defname name args protocol docstring body)
  "Helper function for mk-ddefun."
  (let ((name-str (minv name)))
    `(progn (setf (gethash ,name-str *developer-doc-hash*)
                  (new-make-doc-item ,name-str ,protocol ,docstring ))
            (,defname ,name ,args
              ,docstring
              ,@body))))

(defmacro mk-ddefun (deftype defname)
  `(defmacro ,defname (name args &body body)
     (let* ((protocol (format nil "~a ~a ~a"  ',(minv deftype) (minv name) (mapcar #'minv args)))
            (docstring (if (stringp (car body))
                           (progn (let ((s (car body)))
                                    (setf body (cdr body)) s))
                         "")))
       (mk-ddefun-form ',deftype name args protocol docstring body))))



;; multiple evaluation !
(defmacro print-unless (fmt-str e)
  `(unless (= 0 (length ,e))
     (format t ,fmt-str ,e)))

(defmacro str-unless (fmt-str e)
  `(if (= 0 (length ,e)) ""
     (format nil ,fmt-str ,e)))

;; this is not , should not be used.  print the string formatting function below instead
#|
(defun print-item (item)
  (setf item (third item))
  (format t " -- Internal Function: ~a : ~a~%" (doc-item-name item)
          (doc-item-protocol item))
  (print-unless  "  ~a~%~%" (doc-item-contents  item))
  (print-unless  "   Defined in source file: ~a~%~%" (doc-item-source-file  item)))

(defun print-item-label (item)
  (setf item (third item))
  (format t "~a  *(Developer documentation)" (doc-item-name  item)))
|#

(defun search-key (key item)
  (declare (ignore key))
  (doc-item-name item))

(defun str-item  (item)
  (let ((name (if (doc-item-package item)
                  (concatenate 'string (doc-item-package item) "::" (doc-item-name item))
                  (doc-item-name item))))
    (concatenate 'string
                 (cond ((eq (doc-item-type item) 'function)
                        (format nil " -- Internal Function: ~a : ~a~%" name
                                (doc-item-protocol item)))
                       ((eq (doc-item-type item) 'defvar)
                        (format nil " -- Internal Special Variable (defvar): ~a~%" name))
                       ((eq (doc-item-type item) 'defparameter)
                        (format nil " -- Internal Special Variable (defparameter): ~a~%" name))
                       (t ""))
                 (str-unless  "  ~a~%~%" (doc-item-contents  item))
                 (str-unless  "   Defined in source file: ~a~%" (doc-item-source-file  item))
                 (str-unless  "   In package: ~a~%~%" (doc-item-package  item)))))

(defun str-item-name (item)
  (format nil "~a  *(Internal item)" (doc-item-name item)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (doc-system::ds-registered-p "devel-doc")
    (doc-system::ds-de-register "devel-doc"))

(doc-system::ds-make-and-register
 :name "devel-doc"
 :data *developer-doc-hash*
 :search-key-func #'search-key
 :str-item-func #'str-item
 :str-item-name-func #'str-item-name)

(mk-ddefun defun maxima::ddefun)
(mk-ddefun defmacro maxima::ddefmacro)
(mk-ddefun maxima::defmfun maxima::ddefmfun)

(defmacro maxima::ddefvar (name &body body)
  (let* ((maybe-docstring (car (last body)))
         (docstring (if (stringp maybe-docstring) maybe-docstring 
;;                        (progn (setf body (butlast body)) maybe-docstring) ; hmm maybe don't strip it out
                        "You wrote ddefvar, but no docstring... should be an error."))
         (name-str (minv name)))
    `(progn (setf (gethash ,name-str *developer-doc-hash*)
                  (new-make-doc-var-item ,name-str ,docstring 'defvar))
            (defvar ,name ,@body))))

(defmacro maxima::ddefparameter (name &body body)
  (let* ((maybe-docstring (car (last body)))
         (docstring (if (stringp maybe-docstring) maybe-docstring 
;;                        (progn (setf body (butlast body)) maybe-docstring) ; hmm maybe don't strip it out
                        "You wrote ddefparameter, but no docstring... should be an error."))
         (name-str (minv name)))
    `(progn (setf (gethash ,name-str *developer-doc-hash*)
                  (new-make-doc-var-item ,name-str ,docstring 'defparameter))
            (defparameter ,name ,@body))))
  
