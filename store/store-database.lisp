(use-package :gjl.lisp-util)
(use-package :max-doc)
;(set-cur-sec 'max-doc::io-fandv)
(defmfun1:set-mext-package "store")

(defvar $options_database "options_database.cls")

(defun aex-write-database (data file)
  (let ((cl-store::*check-for-circs* nil))
    (cl-store::store data file)))

(defun aex-read-database (file)
  (let ((cl-store::*check-for-circs* nil))
    (cl-store::restore file)))

(defun aex-write-options-database ()
  (aex-write-database defmfun1::*option-table* $options_database))

(defun aex-read-options-database ()
  (setf defmfun1::*option-table* (aex-read-database $options_database)))

(defmfun1 $read_options_database ()
  (aex-read-options-database))

(defmfun1 $write_options_database ()
  (aex-write-options-database))






  
