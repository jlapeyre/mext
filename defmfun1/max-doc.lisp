;;; Copyright (C) 2012 John Lapeyre
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;;(if (find-package :max-doc ) t (defpackage :max-doc (:use :common-lisp :lisp-util )
;;   (:export :add-doc-sec :get-doc-sec :add-doc-entry :get-doc-entry :init-doc-toplevel)))

;;(defpackage :max-doc (:use :common-lisp :lisp-util )
;;   (:export :add-doc-sec :get-doc-sec :add-doc-entry :get-doc-entry :init-doc-toplevel))
(in-package :max-doc)
;(declaim (optimize (speed 3) (space 0) (safety 0) (debug 0)))

(use-package :gjl.lisp-util)

(defstruct (sections)
  (list (make-array 0 :adjustable t :fill-pointer 0))
  (hash (make-hash-table :test 'equal))
  (name-hash (make-hash-table :test 'equal)))

(defstruct (section)
  (name nil)
  (tag nil)
  (contents nil)
  (list (make-array 0 :adjustable t :fill-pointer 0))
  (hash (make-hash-table :test 'equal)))

(defstruct (entry)
  (name "" :type string)
  (type "Function" :type string)
  (see-also '() :type list)
  (default-value nil)
  (def-type nil)
  (protocol nil)
  (section nil)
  (distribution nil)
  (implementation nil)
  (call-desc-list '() :type list)
  (author nil)
  (copyright nil)
  (examples '() :type list)
  (credits nil)
  (oeis nil) ; online encyclopedia of integer sequences
  (contents nil))

(defstruct (call-desc)
  "A description of one way to call a maxima function. ie, Each one of these
   call descriptions has a different number of arguments."
  (name "" :type string)
  (args '() :type list)
  (text  '() :type list))

(defun add-call-desc1 (name args text)
  (let ((cd (make-call-desc :name name :args args :text text))
        (entry (get-doc-entry :es name)))
    (setf (entry-call-desc-list entry)
          (push cd (entry-call-desc-list entry)))))


(defun clear-call-desc (&rest names)
  "Remove all call descriptions for string 'names' or list of names.
   This is used to avoid repeating entries when a file is loaded multiple times."
  (loop for name in names do
        (let ((entry (get-doc-entry :es name)))
          (setf (entry-call-desc-list entry) nil))))

(defun add-call-desc (&rest args)
  (unless (every #'listp args)
    (maxima::merror1 (intl:gettext "max-doc::add-call-desc: Arguments are not all lists. In file (no not really in this file) ~a")
                                   maxima::$load_pathname))
  "Enter a list of descriptions of how to call the function. Each description
ia a list of three elements: hame, protoco, contents."
  (mapc #'add-call-desc-one args))

(defun add-call-desc-one (arg)
  (unless (= 3 (length arg))
    (maxima::merror1 "max-doc::add-call-desc: expected a list of three items in a call description, found ~s~%" (length arg)))
  (destructuring-bind (name args text) arg
    (let ((cd (make-call-desc :name name :args args :text text))
          (entry (get-doc-entry :es name)))
      (unless entry
        (maxima::merror1 "max-doc::add-call-desc: no max-doc entry for ~s~%" name))
      (setf (entry-call-desc-list entry)
            (push cd (entry-call-desc-list entry))))))

(defun format-call-desc (cd)
  (let ((args (mapcar (lambda (x) (if (listp x)
                                      (cond ((equal (car x) "list")
                                             (format nil "[~{<~a>~^, ~}]" (cdr x)))
                                            ((equal (car x) "lit")
                                             (format nil "~{~a ~}" (cdr x)))
                                            (t
                                             (maxima::merror1 "max-doc: unknown call descritpton argument ~s" x)))
                                      (format nil "<~a>" x)))
                                      (call-desc-args cd))))
  (format nil "    ~a(~{~a~^, ~})~%  ~a~%~%" (call-desc-name cd) args
          (wrap-text :text (format-call-desc-text cd) :width 80 :indent 6) )))

(defun format-call-desc-list (cd-list)
  (format nil "~{~a~}" (nreverse (mapcar #'format-call-desc cd-list))))

(defun format-call-desc-text (cd)
  "This could be used more generally for formatting. It also
  does not handle nested format codes. Using texi and a lisp parser
  would be better."
  (let ((txt (call-desc-text cd))
        (scode (symbol-name 'code))
        (sarg (symbol-name 'arg)))
    (do* ((txt1 txt (cdr txt1))
          (item (car txt) (car txt1))
          (res))
         ((null txt1) (format nil "~{~a~}" (nreverse res)))
      (cond ((and (symbolp item) (equal (symbol-name item) sarg))
             (pop txt1)
             (push (format nil "<~a>" (car txt1)) res))
            ((and (symbolp item) (equal (symbol-name item) scode))
             (pop txt1)
             (push (format nil "`~a'" (car txt1)) res))
            (t
             (push item res))))))

;; created a function for setting a slot
(defmacro def-setter (setter-name slot)
  `(defun ,setter-name (name val)
     (if (listp val) t (setf val (list val)))
     (let ((entry (get-doc-entry :es name)))
       (if entry
           (setf (,slot entry) val)
           (maxima::merror1 "max-doc::~a: No entry for doc item ~a" ',setter-name name)))))


(def-setter author entry-author)
(def-setter see-also entry-see-also)
(def-setter copyright entry-copyright)
(def-setter oeis-doc entry-oeis)  ; see below

;; put this in more than one database, so we can do reverse lookup of oeis number
(defun oeis (name val)
  (oeis-doc name val)
  (if (listp val) t (setf val (list val)))
  (loop for oeis in val do
        (setf (gethash (subseq oeis 0 7) max-doc::*max-doc-oeis-hashtable*) name)))

(defun see-also-group (see-list)
  "Make a see-also entry for each member of see-list that includes all the other members."
  (loop for e in see-list do
       (let ((others (remove e see-list)))
         (see-also e others))))

(defmacro defmvar (var &body val-and-doc)
  (if (= (length val-and-doc) 2)
      (let ((doc (second val-and-doc)))
        `(progn
;;           (add-doc-entry1 '(:name ,(concatenate 'string "\"" (maxima::$sconcat var) "\"")
           (add-doc-entry '(:name ,(maxima::$sconcat var)
                             :type "Variable"
                             :contents ,doc))
           (maxima::defmvar ,var ,@val-and-doc)))))

#|
(defun author (name author)
  (ensure-list author)
  (let ((entry (get-doc-entry :es name)))
    (setf (entry-see-also entry) see-list)))
|#

(defun implementation (name implemention-string)
  (let ((entry (get-doc-entry :es name)))
    (setf (entry-implementation entry) implemention-string)))

(defvar *max-doc-top* (make-sections))

(defvar max-doc::*max-doc-deffn-defvr-hashtable*
  (make-hash-table :test 'equal))

(defvar max-doc::*max-doc-section-hashtable*
  (make-hash-table :test 'equal))

(defvar max-doc::*max-doc-oeis-hashtable*
  (make-hash-table :test 'equal))

(defun init-doc-top-level ()
  (setf *max-doc-top* (make-sections))
  (set-cur-sec nil))

;; current (default) manual section
(defvar *current-section* nil)

;; current (default) mext distribution
(defvar *current-distribution* nil)

(defvar *ignore-silently* t)

(defun get-cur-sec ()
  *current-section*)

;;(defun set-cur-sec (secs)
;;  (cond ((stringp secs)
;;          (setf *current-section* secs))
;;        ((section-p secs)
;;         (setf *current-section* (section-name secs)))
;;        (t (maxima::merror1 (intl:gettext "max-doc: can't set current section to ~m. Neiter a string nor section.") secs))))

(defun set-cur-sec (sec-tag)
  (cond ((section-p sec-tag)
         (setf *current-section* sec-tag))
        ((symbolp sec-tag)
         (setf *current-section* (get-doc-sec sec-tag)))
        (t
         (maxima::merror1 (intl:gettext "max-doc::set-cur-sec Can't set current section to ~m. Not a section struct or tag.") sec-tag))))

(defun set-cur-dist (dist-name)
  (cond ((stringp dist-name)
         (if (gethash dist-name mext-maxima::*installed-dist-table*)
             (setf *current-distribution* dist-name)
           (maxima::merror1 (intl:gettext "max-doc::set-cur-dist 
 Can't set current distribution to ~m. The distribution does not exist.") dist-name)))
        (t
         (maxima::merror1 (intl:gettext "max-doc::set-cur-dist 
 Can't set current distribution to ~m. Not a string.") dist-name))))

(defun exists-section (sec-tag)
  (gethash sec-tag (sections-hash *max-doc-top*)))

(defun add-doc-sec (sec-spec)
  "Add a new documentation section to max-doc, or set the current section if
 it already exists."
  (if (not (listp sec-spec))
      (maxima::merror1 (intl:gettext "max-doc::add-doc-sec: argument ~M is not a section specification") sec-spec)
      (let* ((new-sec
              (apply #'make-section sec-spec))
             (tag (section-tag new-sec))
             (name (section-name new-sec)))
        (if (null tag)
            (maxima::merror1 (intl:gettext "max-doc::add-doc-sec: no tag specified in section specification: ~M") sec-spec)
            (if (exists-section tag)
                (if *ignore-silently* (progn (set-cur-sec new-sec) (return-from add-doc-sec t))
                    (maxima::merror1 "section ~a already exists." ))
                (progn 
                  (setf (gethash tag (sections-hash *max-doc-top*)) new-sec)
                  (vector-push-extend tag (sections-list *max-doc-top*))
                  (setf (gethash name max-doc::*max-doc-section-hashtable*) new-sec)
                  (set-cur-sec new-sec)))))))

(defun get-doc-sec (sec-tag)
  "Return documentation section structure specified by tag sec-tag."
  (if (symbolp sec-tag)
      (let ((h (gethash sec-tag (sections-hash *max-doc-top*))))
        (if h h
            (maxima::merror1 "max-doc::get-doc-sec: section with tag ~a does not exist." sec-tag)))
      (maxima::merror1 "max-doc::get-doc-sec: argument ~a not a section tag." sec-tag)))

; add-doc-entry is more convenient
(defun add-doc-entry1 (&key e  (section *current-section*) (distribution *current-distribution*))
  "add an entry, eg for a function. if *ignore-silently* is true
   then do nothing but return true if the entry already exists. this
   to accomodate loading code several times."
  (let* ((entry (apply #'make-entry e))
         (name (entry-name entry)))
    (if (get-doc-entry :es name)
        (if *ignore-silently* (return-from add-doc-entry1 t)
          (format t "add-entry: ** warning replacing entry '~a'~%" name)))
    (if (not (section-p section))
        (let ((nsec (get-doc-sec section)))
          (if nsec
              (setf section nsec)
            (maxima::merror1 "max-doc::add-doc-entry1: can't find section for tag ~a." section))))
    (setf (entry-section entry) (section-name section))
    (if *current-distribution* (setf (entry-distribtion entry) *current-distribution*))
    (setf (gethash name (section-hash section)) entry)
    (setf (gethash name max-doc::*max-doc-deffn-defvr-hashtable*) entry)))

(defun delete-doc-entry (e)
  (unless (stringp e)
    (maxima::merror1 "max-doc::delete-doc-entry: Argument must be a string."))
  (remhash e max-doc::*max-doc-deffn-defvr-hashtable*))

(defun add-doc-entry (e)
"Add a max doc entry. <e> is either a string giving the name of then entry,
 or a list. If <e> is a list, the first element may be string giving the name
of the entry, or the keyword :name followed by the string. The remaining elements
must be keyword,value pairs for the doc entry struct."
  (cond ((stringp e)
         (setf e (list :name e)))
        ((and (listp e) (stringp (car e)))
         (setf e (cons :name e)))
        ((listp e) t)
        (t (maxima::merror1 "max-doc::add-doc-entry: Entry argument not a string or list of entry key,value pairs")))
  (add-doc-entry1  :e e ))

(defun get-doc-entry (&key es (section *current-section*))
  (gethash es (section-hash section)))

;; not used
(defun print-doc-item (e)
  (cond ((entry-p e)
         (print-doc-entry e))
        ((section-p e)
         (print-doc-section e))))

(defun print-doc-section (s)
;  (let ((hline "------------------------------------------------------------~%"))
  (let ((hline "==============================================================~%"))
    (format t hline)
    (format t "==  section: ~a~%" (section-name s))
    (format t hline)
    (format t "  note: this 'section' is not in the maxima manual.~%")
    (unless (null (section-contents s)) (format t "~% ~a~%~%" (section-contents s)))
    (let* ((h (section-hash s))
           (items (sort (get-hash-keys h) #'string-lessp)))
      (loop for item in items do
            (format t " *  ~a~%" (entry-name (gethash item h))))
      (loop for item in items do
            (print-doc-entry (gethash item h))))))

(defun format-doc-section (s)
  (with-output-to-string (maxima::*standard-outptut*)
    (print-doc-section s)))
;;  (let ((stream maxima::*standard-outptut*))

(defun print-doc-entry (e)
  (format t "~a" (format-doc-entry e)))

(defun format-arg-specs (name)
  "make string like: foo requires between 3 and 6 arguments"
  (let* ((keyname (concatenate 'string "$" name))
         (lambda-info (defmfun1::get-lambda-list-etc keyname)))
    (if lambda-info
        (let* ((arg-list (second lambda-info))
               (req (getf  arg-list :req))
               (optional (getf arg-list :optional ))
               (rest-args (getf arg-list :rest ))
               (nmin (length req))
               (nmax (+ nmin (length optional))))
          (concatenate 'string (format nil "   ~a requires " name)
                       (defmfun1::format-nargs-expected nmin nmax (not (null rest-args))  nil)
                       (if (and (= nmin 1) (= nmax 1))  "" (format nil ".~%"))
                       (format-arg-specs1 rest-args name (append req optional) )
                       (if rest-args (format-arg-specs-rest name rest-args)
                           ""))))))

(defun format-one-spec-in-list (arg-count arg type)
  (format nil "    The ~:R argument ~a must be ~a.~%"
          arg-count
          (maxima::maybe-invert-string-case (format nil "<~a>" arg))
          (defmfun1::get-arg-spec-to-english type)))

(defun format-single-spec (rest-args arg type)
  (format nil (if rest-args  ". The first argument ~a must be ~a.~%"
                  " ~a, which must be ~a.~%")
          (maxima::maybe-invert-string-case (format nil "<~a>" arg))
          (defmfun1::get-arg-spec-to-english type)))

(defun format-arg-specs-rest (name arg)
  (declare (ignore name))
  (let ((type (second (first arg))))
    (if type
        (format nil "   Each of the remaining arguments must be ~a.~%"
                (defmfun1::get-arg-spec-to-english type))
        "")))

(defun format-arg-specs1 (rest-args name arg-list)
  (declare (ignore name))
  (let ( (arg-count 0) (res))
    (if (= 1 (length arg-list))
        (let ((arg (car arg-list)))
          (if (> (length arg) 1)
            (setf res (list (format-single-spec rest-args (first arg) (second arg))))
            (setf res (list (format nil ".~%")))))
        (dolist (r arg-list)
          (incf arg-count)
          (when (cadr r)
            (let ((type (cadr r)))
              (push (format-one-spec-in-list arg-count (car r) type) res)))))
    (format nil "~{~a~}" (nreverse res))))
;;    (apply #'(lambda (x) (concatenate 'string x))  (nreverse res))))


;; should be macrolet
(defmacro form-ent (slot &body body)
  `(let ((x (,slot e)))
     (if x
       (format nil ,@body)  "")))

(defun format-doc-entry (e)
  "called by system documentation routines."
  (let ((name (entry-name e)))
    (concatenate 'string
                 (format nil "------------------------------------------------~% -- ~a: ~a" (entry-type e) name)
                 (if (null (entry-protocol e))
                     (format nil "~%")
                     (format nil ": ~a~%" (entry-protocol e)))
                 (if (null (entry-section e)) ""
                   (format nil "    Section: ~a~%~%" (entry-section e)))
                 (if (null (entry-default-value e))  ""
                     (format nil "  default value `~a'~%" (entry-default-value e)))
                 (if (> (length (entry-call-desc-list e)) 0)
                     (format nil "Calling:~%~a"
                             (format-call-desc-list (entry-call-desc-list e))))
                 (if (> (length (entry-contents e)) 0)
                     (format nil "Description:~%~a~%" (wrap-text :text (entry-contents e) :width 80 :indent 3))
;; following does not work. nothing to support markup in entry-contents
;;                     (format nil "Description:~%~a~%"   (wrap-text :text (format-call-desc-text (entry-contents e)) :width 80 :indent 3))
                     (format nil ""))
                 (let ((sspec (format-arg-specs name)))
                   (when (> (length sspec) 0) (format nil "Arguments:~%~a" sspec)))
                 (let ((opts (maxima::$foptions name)))
                   (if (> (length opts) 1)
                       (format nil "Options:  ~a takes options with default values: ~a~%" name (maxima::$sconcat opts))
                       ""))
                 (let ((atts (maxima::$attributes name)))
                   (if (> (length atts) 1)
                       (format nil "Attributes: ~a has attributes: ~a~%" name (maxima::$sconcat atts))
                       ""))
                 (examples::format-examples name)
                 (form-ent entry-oeis "~%OEIS number: ~a.~%" (comma-separated-english x))
                 (form-ent entry-see-also "~%See also: ~a.~%" (comma-separated-english x))
                 (form-ent entry-implementation "~%Implementation:~%   ~a~%" (wrap-text :text x :width 80 :indent 4))
                 (form-ent entry-author
                     "~%  Author~p: ~a.~%" (length x)  (comma-separated-english x))
;;                 (form-ent entry-copyright ; there should be some control of how much is printed
;;                     "~%  Copyright (C) ~{~a ~}.~%" x)
                 (format nil "~%"))))



(defun search-key (key item)
  "we only search on the 'name' of the entry."
  (declare (ignore key))
    (entry-name item))

(defun search-key-sec (key item)
  (declare (ignore key))
    (section-name item))

(defun str-item-name (item)
  (format nil "~a  ~a *(Contributed document)" (entry-name item) (entry-section item)))

(defun str-item-name-sec (item)
  (format nil "~a  *(Contributed document)" (section-name item)))

(if *max-doc-top* t (init-doc-top-level))

(if (doc-system::ds-registered-p "max-doc")
    (doc-system::ds-de-register "max-doc"))

(if (doc-system::ds-registered-p "max-doc-sec")
    (doc-system::ds-de-register "max-doc-sec"))

;; Register two documentation systems: max-doc for
;; function or variable documentation; and max-doc-sec
;; for sections of the the former.

(doc-system::ds-make-and-register
 :name "max-doc"
 :data *max-doc-deffn-defvr-hashtable*
 :search-key-func #'search-key
 :str-item-func #'format-doc-entry
 :str-item-name-func #'str-item-name)

(doc-system::ds-make-and-register
 :name "max-doc-sec"
 :data *max-doc-section-hashtable*
 :search-key-func #'search-key-sec
 :str-item-func #'format-doc-section
 :str-item-name-func #'str-item-name-sec)

;; move to new file
;;(add-doc-sec '( :tag maxima::$misc :name  "Miscellaneous Functions"))

