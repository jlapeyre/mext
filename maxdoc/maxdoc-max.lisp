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
(mext:mext-optimize)

(use-package :gjl.lisp-util :max-doc)

(max-doc:set-cur-sec 'max-doc::doc-fandv)
(doc-system:set-source-file-name "maxdoc-max.lisp")
(doc-system:set-source-package "maxdoc")

(defmfun1 $add_doc_section ((s :string))
  (if (not (null (max-doc::get-doc-sec s)))
      (maxima::merror1 "add_doc: Section \"~a\" already exists~%" s))
  (max-doc:add-doc-sec s))

(defmfun1 $get_cur_doc_section ()
 (max-doc:get-cur-sec))

(defmfun1 ($print_maxdoc_sections :doc) ()
  "Print all sections of maxdoc documentation. This does not include other documentation
 databases, such as the main maxima documentation."
  (format t " ---  maxdoc sections~%~%")
  (let* ( (sections (sort (get-hash-keys max-doc::*max-doc-section-hashtable*) #'string-lessp)))
    (loop for section--name in sections do
                (format t " *  ~a~%" section--name))
    (format t "~%")
    (loop for section--name in sections do
          (max-doc:print-doc-section (gethash section--name max-doc::*max-doc-section-hashtable*))))
  '$done)

(defmfun1 ($print_sections_latex :doc) ( &optional (filename :string "./maxsecs.tex") )
 :desc ("Print all sections of maxdoc documentation currently loaded in latex format
   to the file " :argdot "filename" " This does not include other documentation
   databases, such as the main maxima documentation.")
  (with-open-file (stream filename :direction :output :if-exists :supersede)
(format stream "
\\documentclass[]{article}
\\usepackage[utf8]{inputenc}
\\usepackage{fancyvrb}
\\usepackage{fullpage}
\\usepackage{hyperref}
\\begin{document}
\\title{Third-party maxima software}
\\author{John Lapeyre}
\\maketitle
\\tableofcontents~%")
  (let* ( (sections (sort (get-hash-keys max-doc::*max-doc-section-hashtable*) #'string-lessp)))
    (loop for section--name in sections do
          (max-doc:print-doc-section-latex (gethash section--name max-doc::*max-doc-section-hashtable*)
                                           stream )))
(format stream "
\\end{document}~%"))
  '$done)

;    (format stream "\\begin{itemize}~%")  ; do this with latex toc
;    (loop for section--name in sections do
;                (format stream "\\item ~a~%" section--name))
;    (format stream "\\end{itemize}~%")


(defmfun1 ($print_maxdoc_entry :doc) ((item :string))
  (let ((entry (max-doc:get-doc-entry :es item :section :all)))
    (if entry (max-doc:print-doc-entry entry)
      (format t "Can't find maxdoc entry '~a'.~%" item))))

(defmfun1 ($print_entry_latex :doc) ((item :string))
  (let ((entry (max-doc:get-doc-entry :es item :section :all)))
    (if entry (max-doc:print-doc-entry-latex entry)
      (format t "Can't find maxdoc entry '~a'.~%" item))))

(defmfun1:set-hold-all '$maxdoc)
(defmfun1 ($maxdoc :doc) ((name :string) docs)
  :desc ("Add maxdoc documentation entry for item " :arg "name"
    " specified by " :argdot "docs")
    (max-doc:add-doc-entry (list :name name :contents (do-maxdoc docs)))
    '$done)

(defun do-maxdoc (x)
  (cond ((stringp x) x)
        (($listp x)
         (loop for e in (cdr x) append
               (if (stringp e) (list e)
                 (do-maxdoc e))))
        ((listp x)
         (list (keywordify (caar x)) (cadr x)))
        (t (merror (format nil "bad maxdoc argument ~a " x)))))

(defmfun1 ($maxdoc_split_text :doc) ((text :string))
  :desc ("Split the string " :arg "text" " into a list of strings, using a sequence
 of one or more spaces as the delimeter. Single newlines are removed.")
  (cons '(mlist simp) (gjl:split-by-space-and-newline text)))

;; (defmfun1 ($oeis :doc) ( (n :string) )
;;   "Search for a maxima function corresponding to the online encyclopedia
;;  of integer sequences OEIS number <n>."
;;   (gethash n max-doc::*max-doc-oeis-hashtable*))

;; (examples::clear-examples "oeis")
;; (examples::add-example "oeis"
;;                        '( :code ( "oeis(\"A000108\")")))

;;(defmfun1 $set_doc_section ((s :string))
;;                            nil)
