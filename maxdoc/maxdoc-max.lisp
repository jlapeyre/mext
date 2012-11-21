;;; Copyright (C) 2012 John Lapeyre
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

;;; Some of these functions may be generally useful. But
;;; right now, they are for debugging.

(in-package :maxima)
(declaim (optimize (speed 3) (space 0) (safety 0) (debug 0)))
(use-package :gjl.lisp-util :max-doc)

(max-doc::set-cur-sec 'max-doc::doc-fandv)

(defmfun1 $add_doc_section ((s :string))
  (if (not (null (max-doc::get-doc-sec s)))
      (maxima::merror1 "add_doc: Section \"~a\" already exists~%" s))
  (max-doc::add-doc-sec s))

(defmfun1 $get_cur_doc_section ()
 (max-doc::get-cur-sec))


(defmfun1 ($print_max_doc_sections :doc) ()
  "Print all sections of max_doc documentation. This does not include other documentation
 databases, such as the main maxima documentation."
  (format t " ---  max_doc sections~%~%")
  (let* ( (sections (sort (get-hash-keys max-doc::*max-doc-section-hashtable*) #'string-lessp)))
    (loop for section--name in sections do
                (format t " *  ~a~%" section--name))
    (format t "~%")
    (loop for section--name in sections do
          (max-doc::print-doc-section (gethash section--name max-doc::*max-doc-section-hashtable*)))))


;; (max-doc::set-cur-sec 'max-doc::number-theory-fandv)

;; (defmfun1 ($oeis :doc) ( (n :string) )
;;   "Search for a maxima function corresponding to the online encyclopedia
;;  of integer sequences OEIS number <n>."
;;   (gethash n max-doc::*max-doc-oeis-hashtable*))

;; (examples::clear-examples "oeis")
;; (examples::add-example "oeis"
;;                        '( :code ( "oeis(\"A000108\")")))

;;(defmfun1 $set_doc_section ((s :string))
;;                            nil)

;; print the entry
;; doesnt work correctly
#|
(defmfun1 $doc_entry ( (es :string) (ss :string) )
  (let ((e (max-doc::get-doc-entry :es es :secs ss)))
    (format t "~a ~a: ~a~%" (max-doc::entry-type e) (max-doc::entry-name e)
            (max-doc::entry-protocol e))
    (format t "~a~%" (max-doc::entry-contents e))))
|#

#|
(defun print-doc-entry (e)
  "Called by system documentation routines in cl-info.lisp."
  (let ((name (max-doc::entry-name e)))
    (format t "~% -- ~a ~a: ~a~%~%" (max-doc::entry-type e) name
            (max-doc::entry-protocol e))
    (format t "~a~%~%" (max-doc::entry-contents e))
    (let ((opts ($foptions name)))
      (if (> (length opts) 1)
          (format t " ~a takes options with default values:~% ~a~%~%" name ($sconcat opts))))))
|#

