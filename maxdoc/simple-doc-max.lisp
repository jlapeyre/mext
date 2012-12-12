;;; Copyright (C) 2012 John Lapeyre
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(in-package :maxima)
(mext:mext-optimize)
;(declaim (optimize (speed 3) (space 0) (safety 0) (debug 0)))
;;  The functions manage a hash table of documenation strings keyed by strings.
;;  The data is not persistant.
;;  The data is accesible via maxima describe(), and ? and ??

(max-doc::set-cur-sec 'max-doc::doc-fandv)

(defmfun1 ($simple_doc_init :doc) ()
  "Initialize the simple_doc documentation database."
  (simple-doc::init-table)
  '$done)

(defmfun1 ($simple_doc_add :doc) ((name :string) (content :string))
 :desc ("Adds documentation string " :arg "content" " for item " :argdot "name"
 " These documentation  strings are accessible via '?' and '??'.")
  (simple-doc::check-init-table)
  (if (simple-doc::add-item name content) name
    nil))

(defmfun1 ($simple_doc_delete :doc)  ((name :string))
  :desc ("Deletes the simple_doc documentation string for item " :argdot "name")
  (let ((res (simple-doc::remove-item name)))
    (if res name
      (format t "simple_doc_delete: No documentation found for '~a'~%" name))))

(defmfun1 ($simple_doc_get :doc) ((name :string))
  :desc ("Returns the simple_doc documentation string for item " :argdot "name")
  (simple-doc::get-item-contents name))

(defmfun1 ($simple_doc_print :doc) ((name :string))
 :desc ("Prints the simple_doc documentation string for item " :argdot "name")
  (format t "~%~a~%~%" (simple-doc::get-item-contents name)))

(max-doc::see-also-group '( "simple_doc_init" "simple_doc_add" 
 "simple_doc_delete" "simple_doc_get" "simple_doc_print"))
