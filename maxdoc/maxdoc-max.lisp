;;;  Functions to make maxdoc documentation entries from maxima.
;;;
;;;  Copyright (C) (2012) John Lapeyre. Licensed under GPL, v3 or greater. See the file
;;;  `LICENSE' in this directory.

(in-package :maxima)
(mext:mext-optimize)

(use-package :gjl.lisp-util)
(use-package :max-doc)
(use-package :examples)

(max-doc:set-cur-sec 'max-doc::doc-fandv)
(defmfun1:set-file-and-package "maxdoc-max.lisp" "maxdoc")

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

;; Translate documentation specification from maxima to lisp.
;; This is only tested thus far with alt_eigen.
;; Some example code and description code works fine. I inadvertently
;; changed the syntax with the new examples slot code-text. 
;; `do-maxdoc' will not work with code-text. The new syntax is
;; probably worth keeping anyway. A short, similar routine will
(defun do-maxdoc (x)
 "Convert a maxima expression to a lisp expression specifying
  documentation. We need a similar routine for examples, etc."
  (cond ((stringp x) x)
        ((numberp x) x)
        (($listp x)
         (loop for e in (cdr x) append
               (if (stringp e) (list e)
                 (do-maxdoc e))))
        ((listp x)
         (list (keywordify (caar x)) (do-maxdoc (cadr x))))
        (t (merror (format nil "do-maxdoc: bad argument ~a " x)))))

(defmfun1:set-hold-all '$maxdoc)
(defmfun1 ($maxdoc :doc) ((name :string) docs &opt ($source_filename nil :string)
                          ($package nil :string))
  :desc ("Add maxdoc documentation entry for item " :arg "name"
    " specified by " :argdot "docs")
    (max-doc:add-doc-entry (list :name name :contents (do-maxdoc docs)
                                 :source-filename $source_filename
                                 :mext-package $package))
    '$done)

;; Note that this works for some, but not all examples. The
;; code-text slot in an example uses a different syntax that
;; is not parsed correctly by `do-maxdoc'.
(defmfun1:set-hold-all '$maxdoc_examples)
(defmfun1 ($maxdoc_examples :doc) ((name :string) &rest examples)
 :desc ("Add maxdoc examples entry for item " :arg "name"
        " specified by " :argdot "examples")
 (format t "!!! ~s~%~%" examples)
 (let ((exs (loop for ex in examples collect
                (do-maxdoc ex))))
   (format t "Example text: ~s~%~%" exs)
   (apply #'examples:add-example (cons name exs)))
 '$done)

(defmfun1 ($maxdoc_split_text :doc) ((text :string))
  :desc ("Split the string " :arg "text" " into a list of strings, using a sequence
 of one or more spaces as the delimeter. Single newlines are removed.")
  (cons '(mlist simp) (gjl:split-by-space-and-newline text)))

(defmfun1 ($maxdoc_author :doc) ((name :string) (author :string-or-listof))
  :desc ("Set the author(s) for the documentation item " :argdot "name")
  (when (listp author) (pop author))
  (max-doc:author name author)
  '$done)

(defmfun1 ($maxdoc_copyright :doc) ((name :string) copyright)
  :desc ("Set the copyright information for the documentation item " :argdot "name"
  " " :arg "copyright" " should typically be a list whose first element is an integer
 (the year), with the remaining strings naming the copyright holder. This copyright
 information will not be printed with documentation, unless " :var "print_copyrights"
 " is true.")
  (when (listp copyright) (pop copyright))
  (max-doc:copyright name copyright)
  '$done)

(defmfun1 ($maxdoc_set_cur_sec :doc) ((shortname :string))
 :desc 
 ("Set the current section for maxdoc to " :argdot "shortname"
 " This section will be used by functions such as " :mrefcomma "maxdoc"
 " and " :mrefdot "maxdoc_author")
 (max-doc:set-cur-sec-shortname shortname)
 '$done)

(defmfun1 ($maxdoc_set_mext_package :doc) ((packagename :string))
 :desc 
 ("Set the current mext package name for maxdoc to " :argdot "packagename"
 " This name will be used by functions specifying documentation for functions
 until the name is set to another value. When documenting functions written in
 maxima code, calling " :mref "mext_record_package" " is probably more useful.")
 (defmfun1:set-mext-package packagename)
 '$done)

(defmfun1 ($mext_package_record :doc) ((docitems :or-string-symbol-or-listof) (packagename :string)
                                       &optional (source-filename nil :string))
 :desc 
 ("Set the mext packagename for the function or variable (or list of them) "
  :arg "docitems" " to " :argdot "packagename"
  "This name will be used when displaying documentation."
  "The function " :mref "maxdoc_set_mext_package" " is useful for setting
  the package name of a group of functions, but there is currently no maxima
  hook for doing this.")
 (defmfun1:record-mext-package docitems packagename source-filename)
 '$done)

;; (defmfun1 ($oeis :doc) ( (n :string) )
;;   "Search for a maxima function corresponding to the online encyclopedia
;;  of integer sequences OEIS number <n>."
;;   (gethash n max-doc::*max-doc-oeis-hashtable*))

;; (examples::clear-examples "oeis")
;; (examples::add-example "oeis"
;;                        '( :code ( "oeis(\"A000108\")")))

;;(defmfun1 $set_doc_section ((s :string))
;;                            nil)
