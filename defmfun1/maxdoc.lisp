;;; Copyright (C) 2012 John Lapeyre
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(in-package :max-doc)
(mext:mext-optimize)

(use-package :gjl.lisp-util)

(defstruct (sections)
  (list (make-array 0 :adjustable t :fill-pointer 0))
  (hash (make-hash-table :test 'equal))
  (shortname-hash (make-hash-table :test 'equal)))

(defstruct (section)
  (name nil)
  (shortname nil)
  (tag nil)
  (contents nil)
  (list (make-array 0 :adjustable t :fill-pointer 0))
  (hash (make-hash-table :test 'equal)))

;; It seems that the `distribution' slot is not used
;; We are storing this information in a hash table: *mext-package*
(defstruct (entry)
  (name "" :type string)
  (type "Function" :type string)
  (see-also '() :type list)
  (default-value nil)
  (def-type nil)
  (protocol nil)
  (protocol-list nil)
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

;; current (default) manual section
(defvar *current-section* nil)

;; current (default) mext distribution
(defvar *current-distribution* nil)

(defvar *ignore-silently* t)

(defvar *text-width* 80)
;; kludge. we wrap latex text just to improve human readability. but this may
;; split argument \verb, which causes latex error. so make lines large.
(defvar *latex-text-width* 300)
(defvar *indent1* 3)
(defvar *indent2* 4)
(defvar *indent3* 6)

(defvar *format-codes-text*
  (make-hash-table :test 'equal))

(defvar *format-codes-texi*
  (make-hash-table :test 'equal))

(defvar *format-codes-latex*
  (make-hash-table :test 'equal))

(defvar *format-codes-default* *format-codes-text*)

(defvar *max-doc-top* (make-sections))

(defvar max-doc::*max-doc-deffn-defvr-hashtable*
  (make-hash-table :test 'equal))

(defvar max-doc::*max-doc-section-hashtable*
  (make-hash-table :test 'equal))

(defvar max-doc::*max-doc-oeis-hashtable*
  (make-hash-table :test 'equal))

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
   is a list of three elements: name, protocol, contents."
  (mapc #'add-call-desc-one args))

(defun add-call-desc-one (arg)
  (unless (length-eq arg 3)
    (maxima::merror1 "max-doc:add-call-desc: expected a list of three items in a call description, found ~s~%" (length arg)))
  (destructuring-bind (name args text) arg
    (let ((cd (make-call-desc :name name :args args :text text))
          (entry (get-doc-entry :es name)))
      (unless entry
        (maxima::merror1 "max-doc::add-call-desc: no max-doc entry for ~s~%" name))
      (setf (entry-call-desc-list entry)
            (push cd (entry-call-desc-list entry))))))

(defun set-format-codes-table (code-name)
  (cond ((string= "text" code-name)
         (setf *format-codes-default* *format-codes-text*))
        ((string= "texi" code-name)
         (setf *format-codes-default* *format-codes-texi*))
        ((string= "latex" code-name)
         (setf *format-codes-default* *format-codes-latex*))
        (t (maxima::merror1 (intl:gettext 
           "max-doc:set-format-codes-table Unknown code name ~s.") code-name))))

(defun latex-esc (str)
  (let ((res str))
    (loop for pair in '( ("_" "\\_") ("^" "\\^") ("$" "\\$"))
          do (setf res (replace-all res (first pair) (second pair))))
    res))

;; like fill-hash-from-list, but make string from keys
(defun fill-format-codes (hash-table element-list)
  (mapcar (lambda (pair) 
            (setf (gethash (first pair) hash-table) (second pair))) element-list)
  hash-table)

;;; :code -- computer code as in other markup systems
;;; :mref -- references to other entries in the maxdoc system
;;; :emref -- references to stock Maxima command. (info docs)
;;; :arg -- argument to function
;;; :var -- other variable


(fill-format-codes *format-codes-text*
   '( (:code "`~a'")  (:codedot "`~a'.")  (:codecomma "`~a',") 
      (:mref "`~a'")  (:mrefdot "`~a'.")  (:mrefcomma "`~a',") 
      (:emref "`~a'")  (:emrefdot "`~a'.")  (:emrefcomma "`~a',") 
      (:arg "<~a>")   (:argdot "<~a>.")   (:argcomma "<~a>,")
      (:var "<~a>")   (:vardot "<~a>.")   (:varcomma "<~a>,")
      (:opt "<~a>")   (:optdot "<~a>.")   (:optcomma "<~a>,")
      (:par "~%~%~a")
      (:dquote "\"~a\"") (:dquotedot "\"~a\".") (:dquotecomma "\"~a\",")
      (:dcode "~%~%`~a'~%~%")
      (:math "~a") (:tmath "~a") (:lif "~a")
      (:dmath "~a") (:dots " ... ")))

;; mref, mrefdot, mrefcomma below are not used. caught in earlier branch
;; emref etc. should eventually do external links
(fill-format-codes *format-codes-latex*
   '( (:code "{\\tt ~a}")  (:codedot "{\\tt ~a}.")  (:codecomma "{\\tt ~a},") 
      (:mref "{\\tt ~a}")  (:mrefdot "{\\tt ~a}.")  (:mrefcomma "{\\tt ~a},")
      (:emref "{\\tt ~a}")  (:emrefdot "{\\tt ~a}.")  (:emrefcomma "{\\tt ~a},")
      (:arg "{\\it ~a}")   (:argdot "{\\it ~a}.")   (:argcomma "{\\it ~a},")
      (:var "{\\it ~a}")   (:vardot "{\\it ~a}.")   (:varcomma "{\\it ~a},")
      (:opt "{\\it ~a}")   (:optdot "{\\it ~a}.")   (:optcomma "{\\it ~a},")
      (:dquote "``~a''") (:dquotedot "``~a''.") (:dquotecomma "``~a'',")
      (:par "~%~%~%~a")
      (:math "$~a$") (:tmath "$~a$") (:lif "~a")
      (:dcode "~%\\begin{verbatim}~%~a~%\\end{verbatim}~%~%")
      (:dmath "~%$$~a$$~%") (:dots "\\ldots")))

(defun make-texi-codes (table codes)
 "Make a table of symbols to codes for texi. E.g. var --> @var{~a}"
  (loop for code in codes do
        (let ((name code))
          (setf (gethash name table) (format nil "@~a{~~a}" (string-downcase name))))))

(make-texi-codes *format-codes-texi*
   '( :code :codecomma :codedot :var :varcomma :vardot
      :mref :mrefcomma :mrefdot :arg :argcomma :argdot
      :var :varcomma :vardot :opt :optcomma :optdot
      :math :dmath :dots))

(defun format-doc-text (text-descr &optional (code-table *format-codes-default*))
 "Return a string of formatted text from a text description list and table that
  takes format codes to strings."
  (let ((txt (if (listp text-descr) text-descr (list text-descr))))
    (do* ((txt1 txt (cdr txt1))
          (item (car txt) (car txt1))
          (res))
        ((null txt1) (format nil "~{~a~}" (nreverse res)))
      (if (symbolp item)
          (let ((fmt (gethash item code-table)))
            (unless (keyword-p item) 
              (format t "max-doc: Markup tag `~s' is not a keyword, in ~s." item txt)
              (maxima::merror1 "An error"))
            (if fmt
                (let ((txt2 (progn (pop txt1) (car txt1))))
                  (when (member item '(:lif :tmath)) (setf txt2 (second txt2)))
                  (push (format nil fmt
                                (if (listp txt2) (format-doc-text txt2 code-table) txt2))
                        res))
              (maxima::merror1 "max-doc: Unrecognized format code: ~a." item)))
        (push item res)))))

;; we should try to refactor the text and latex code at some point.
;; lots of things to clean up.
(defun format-doc-text-latex (text-descr &optional (code-table *format-codes-latex*))
 "Return a string of formatted text from a text description list and table that
  takes format codes to strings."
  (let ((txt (if (listp text-descr) text-descr (list text-descr))))
    (do* ((txt1 txt (cdr txt1))
          (item (car txt) (car txt1))
          (res))
        ((null txt1) (format nil "~{~a~}" (nreverse res)))
      (if (symbolp item)
          (let* ((fmt (gethash item code-table))
                 (s (cadr txt1)))
            (unless (keyword-p item) 
              (format t "max-doc: Format symbol not a keyword ~s, in ~s." item txt)
              (maxima::merror1 "An error"))
            (pop txt1)
            (cond ((member item '(:code :codedot :codecomma)  :test #'equal)
                   (let* ((delim (loop for char in '(#\# #\$ #\~ #\@ #\& #\- #\% \#^ \#?) do
                                       (when (not (find char s)) (return char))))
                          (str (format nil "\\verb~a~a~a" delim s delim)))
                     (push 
                      (cond ((eq item :code) str)
                            ((eq item :codedot) (format nil "~a." str))
                            ((eq item :codecomma) (format nil "~a," str)))
                      res)))
                  ((member item '(:mref :mrefdot :mrefcomma)  :test #'equal)
                   (let ((str (format nil "\\hyperlink{~a}{{\\tt ~a}}" s (latex-esc s))))
                     (push 
                      (cond ((eq item :mref) str)
                            ((eq item :mrefdot) (format nil "~a." str))
                            ((eq item :mrefcomma) (format nil "~a," str)))
                      res)))
                  (t
                   (when (member item '(:lif :tmath))
                     (setf s (first s)))
                   (if fmt (push
                                (format nil fmt
                                 (if (listp s) (format-doc-text-latex s code-table)
                                   (if (member item '(:math :tmath :dmath :dcode)) s (latex-esc s))))
                            res)
                     (maxima::merror1 "max-doc: Unrecognized format code: ~a." item)))))
        (push (latex-esc item) res)))))

(defun format-call-desc (cd)
  (let ((args (mapcar (lambda (x) 
        (if (listp x)
            (cond ((equal (car x) "list")
                   (let* ((fmt (gethash :arg *format-codes-default*))
                          (fmt1 (format nil "[~~{~a~~^, ~~}]" fmt)))
                     (format nil fmt1 (cdr x))))
                  ((equal (car x) "lit")
                   (format nil "~{~a ~}" (cdr x)))
                  (t
                   (maxima::merror1 "max-doc: unknown call description argument ~s" x)))
          (format-doc-text (list :var x) *format-codes-default*)))
                      (call-desc-args cd))))
  (format nil "    ~a(~{~a~^, ~})~%  ~a~%~%" (call-desc-name cd) args
          (wrap-text :text (format-doc-text (call-desc-text cd) *format-codes-default*) :width 80 :indent *indent3* ) )))

(defun format-call-desc-list (cd-list)
  (format nil "~{~a~}" (nreverse (mapcar #'format-call-desc cd-list))))

(defun format-arg-list (args)
  (let ((code (gethash :arg *format-codes-default*)))
    (format nil "~{~a~^, ~}"
            (loop for arg in args collect
                  (progn 
                    (when (listp arg) (setf arg (car arg)))
                    (setf arg (latex-esc (maxima::maybe-invert-string-case (format nil "~a" arg))))
                    (format nil code arg))))))

(defun format-protocol-latex (protocol)
  "This formats the protocol (lambda list) of a defmfun1 form as a string for printing
   documentation."
  (destructuring-bind (sname req optional rest) protocol
    (let ((sarg (format-arg-list req)))
      (when (not (null optional))
        (setf sarg (concatenate 'string sarg " :optional "
                              (format-arg-list (rest optional)))))
      (when (not (null rest))
        (setf sarg (concatenate 'string sarg " :rest "
                                (format-arg-list (rest rest)))))
      (format nil "{\\bf ~a}(~a)" (latex-esc sname) sarg))))

(defun format-call-desc-latex (cd)
  (let ((args (mapcar (lambda (x) 
        (if (listp x)
            (cond ((equal (car x) "list")
                   (let* ((fmt (gethash :arg *format-codes-latex*))
                          (fmt1 (format nil "[~~{~a~~^, ~~}]" fmt)))
                     (format nil fmt1 (cdr x))))
                  ((equal (car x) "lit")
                   (format nil "~{~a ~}" (cdr x)))
                  (t
                   (maxima::merror1 "max-doc: unknown call description argument ~s" x)))
          (format-doc-text-latex (list :var x))))
                      (call-desc-args cd))))  ;; map latex-esc over args in next line
  (format nil "\\item[] {\\bf ~a}(~{~a~^, ~})~%  ~a~%~%" (latex-esc (call-desc-name cd)) args
          (wrap-text :text (format-doc-text-latex (call-desc-text cd)) :width *latex-text-width* :indent 0 ) )))

(defun format-call-desc-list-latex (cd-list)
  (format nil "~{~a~}" (nreverse (mapcar #'format-call-desc-latex cd-list))))

;; created a function for setting a slot
(defmacro def-setter (setter-name slot)
  `(defun ,setter-name (name val)
     (unless (listp val) (setf val (list val)))
     (let ((entry (get-doc-entry :es name)))
       (if entry
           (setf (,slot entry) val)
           (maxima::merror1 "max-doc:~a: No entry for doc item ~a" ',setter-name name)))))

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

;; use mdefmvar instead of defmvar to avoid issue of shadowing, importing, etc.
;; defmvar is defined in src/commac.lisp
;; TODO: checking for, or handling number of args should be improved.
;; TODO: rewrite so that the eval is not neccessary.
;;       It is needed for $homedir in mext_defmfun1/mext_defmfun1_code.lisp.
;; Where does the unwanted newline come from  when printing the error message (inside `~m') ?
(defmacro mdefmvar (var &body val-and-doc)
  (let ( (length-val-and-doc (length val-and-doc))
         (max-varname (maxima::$sconcat var)))
    (if (= length-val-and-doc 2)
        (let* ((doc (second val-and-doc))
               (val (eval (first val-and-doc)))
               (val1 (cond ((eq t val) "true")
                           ((null val) "false")
                           (t val)))
               (pass-arg (if (stringp val)
                             val-and-doc (list val))))
          `(progn
             (add-doc-entry '( :name ,max-varname
                               :type "Variable"
                               :default-value ,val1
                               :contents ,doc))
             (maxima::defmvar ,var ,@pass-arg)))
      (maxima::merror1 (intl:gettext 
                        "max-doc:mdefmvar : ~m arguments given in definition of `~m'; three are expected.~%")
                       (+ 1 length-val-and-doc) max-varname))))

(defun implementation (name implemention-string)
  (let ((entry (get-doc-entry :es name)))
    (setf (entry-implementation entry) implemention-string)))

(defun init-doc-top-level ()
  (setf *max-doc-top* (make-sections))
  (set-cur-sec nil))

(defun get-cur-sec ()
  *current-section*)

(defun set-cur-sec (sec-tag)
  (cond ((section-p sec-tag)
         (setf *current-section* sec-tag))
        ((symbolp sec-tag)
         (setf *current-section* (get-doc-sec sec-tag)))
        (t
         (maxima::merror1 (intl:gettext "max-doc::set-cur-sec Can't set current section to ~m. Not a section struct or tag.") sec-tag))))

(defun set-cur-sec-shortname (shortname)
 "Set the current doc section from its shortname, which is a string. Error checking
  should be done by the calll to get-doc-sec-shortname."
  (setf *current-section* (get-doc-sec-shortname shortname)))

(defun set-cur-dist (distname)
  (cond ((stringp distname)
         (if (gethash distname mext-maxima::*loaded-dist-table*)
             (setf *current-distribution* distname)
           (maxima::merror1 (intl:gettext "max-doc::set-cur-dist 
 Can't set current distribution to ~m. The distribution does not exist.") distname)))
        (t
         (maxima::merror1 (intl:gettext "max-doc::set-cur-dist 
 Can't set current distribution to ~m. Not a string.") distname))))

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
             (name (section-name new-sec))
             (shortname (section-shortname new-sec)))
        (unless shortname
          (maxima::merror1 (intl:gettext "max-doc:add-doc-sec: no shortname specified in section specification: ~M") sec-spec))
        (if (null tag)
            (maxima::merror1 (intl:gettext "max-doc::add-doc-sec: no tag specified in section specification: ~M") sec-spec)
            (if (exists-section tag)
                (if *ignore-silently* (progn (set-cur-sec new-sec) (return-from add-doc-sec t))
                    (maxima::merror1 "section ~a already exists." ))
                (progn 
                  (setf (gethash tag (sections-hash *max-doc-top*)) new-sec)
                  (setf (gethash shortname (sections-shortname-hash *max-doc-top*)) new-sec) ; easier for maxima access
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

(defun get-doc-sec-shortname (shortname)
  "Return documentation section structure specified by string shortname. We don't need two
  separate hashes with the same information. But, I want to avoid symbol-hell compounded by
  access from maxima."
  (if (stringp shortname)
   (let ((h (gethash shortname (sections-shortname-hash *max-doc-top*))))
     (if h h
       (maxima::merror1 "max-doc:get-doc-sec-shortname section with shortname ~a does not exist." shortname)))
   (maxima::merror1 "max-doc:get-doc-sec-shortname argument ~a not a string." shortname)))

; add-doc-entry is more convenient
(defun add-doc-entry1 (&key e  (section *current-section*) (distribution *current-distribution*))
  "add an entry, eg for a function. if *ignore-silently* is true
   then do nothing but return true if the entry already exists. this
   to accomodate loading code several times."
  (let* ((entry (apply #'make-entry e))
         (name (entry-name entry)))
    (when (get-doc-entry :es name)
        (if *ignore-silently* (return-from add-doc-entry1 t)
          (format t "add-doc-entry: ** warning replacing entry '~a'~%" name)))
    (when (not (section-p section))
        (let ((nsec (get-doc-sec section)))
          (if nsec
              (setf section nsec)
            (maxima::merror1 "max-doc::add-doc-entry1: can't find section for tag ~a." section))))
    (setf (entry-section entry) (section-name section))
    (when *current-distribution* (setf (entry-distribution entry) *current-distribution*))
    (when defmfun1::*mext-package*
      (defmfun1::record-mext-package name defmfun1::*mext-package*))
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
  "return the doc entry for string 'es'."
  (if (eq section :all)
      (let ((sections (get-hash-keys max-doc::*max-doc-section-hashtable*)))
        (loop for section1 in sections do
              (let ((item (gethash es (section-hash 
                                       (gethash section1 *max-doc-section-hashtable*)))))
                (when item (return item)))))
  (gethash es (section-hash section))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-doc-sec '( :tag doc-fandv :name "Functions and Variables for Documentation"
                :shortname "fvdocumentation"))

(mdefmvar maxima::$print_authors t
 "If true, then print the names of the authors with maxdoc documentation.")

(mdefmvar maxima::$print_copyrights nil
 "If true, then print copyright information with maxdoc documentation.")

(mdefmvar maxima::$print_implementation t
 "If true, then print implementation information with maxdoc documentation.")

;; Should not be in documentation section
;; should not $error_code  and $pager_command be in package maxima ?
(maxdoc:mdefmvar $error_code nil
 ( "This is an error code set by " :codedot "merror1"))

(maxdoc:mdefmvar $pager_command "/usr/bin/less"
 "The pathname to the system command used for paged output, for
 instance, for reading documentation.")

;; only for bug hunting
;(maxdoc:mdefmvar maxima::$compile_lambda_verbose nil
;  "If this is true, then print translated code when automatically compiling lambda functions
;  passed as arguments. This is done in the macro option-compile-lambda."  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun print-doc-section-latex (s &optional (stream t) )
  (format stream "\\section{~a}~%" (section-name s))
  (unless (null (section-contents s)) (format stream "~% ~a~%" (section-contents s)))
  (let* ((h (section-hash s))
           (items (sort (get-hash-keys h) #'string-lessp)))
    (when items
      (format stream "\\begin{itemize}~%")
      (loop for item in items do
            (let ((name (entry-name (gethash item h))))
              (format stream "\\item \\hyperlink{~a}{{\\tt ~a}}~%" name (latex-esc name))))
      (format stream "\\end{itemize}~%")
      (loop for item in items do
            (print-doc-entry-latex (gethash item h) stream)))))

(defun format-doc-section (s)
  (with-output-to-string (maxima::*standard-outptut*)
    (print-doc-section s)))

(defun print-doc-entry (e)
  (format t "~a" (format-doc-entry e)))

(defun print-doc-entry-latex (e &optional (stream t))
  (format stream "~a" (format-doc-entry-latex e)))

(defun format-arg-specs (name)
  "make string like: foo requires between 3 and 6 arguments"
  (let* ((keyname (concatenate 'string "$" name))
         (lambda-info (defmfun1::get-lambda-list-etc keyname)))
    (when lambda-info
        (let* ((arg-list (second lambda-info))
               (req (getf  arg-list :req))
               (optional (getf arg-list :optional ))
               (rest-args (getf arg-list :rest ))
               (nmin (length req))
               (nmax (+ nmin (length optional))))
          (concatenate 'string (format nil "   ~a requires "
                                       (format-doc-text (list :code name)))
                       (defmfun1::format-nargs-expected nmin nmax (not (null rest-args))  nil)
                       (if (and (< nmin 2) (= nmax 1))  "" (format nil ".~%"))
                       (format-arg-specs1 name req optional rest-args)
                       (if rest-args (format-arg-specs-rest name rest-args)
                           ""))))))

(defun format-one-spec-in-list (arg-count arg type)
  (format nil "    The ~:R argument ~a must be ~a.~%"
          arg-count
;          (maxima::maybe-invert-string-case 
           (format-doc-text (list :arg 
                             (maxima::maybe-invert-string-case (symbol-name arg))) *format-codes-default*)
          (defmfun1::get-arg-spec-to-english type)))

(defun format-single-spec (req optional rest-args arg type)
 "rest-args is form after &rest in defmfun1 lambada list. arg is name of a single arg,
  type is a type spec from defmfun1."
  (format nil 
          (cond ((and rest-args req)
                 ". The first argument ~a must be ~a.~%") ; one or more args
                (optional
                 ". If present, the argument ~a must be a ~a.~%") ; zero or one arg
                (t
                 " ~a, which must be ~a.~%")) ; exactly one arg
           (format-doc-text (list :arg
                                  (maxima::maybe-invert-string-case (symbol-name arg))) *format-codes-default*)
          (defmfun1::get-arg-spec-to-english type)))

(defun format-arg-specs-rest (name arg)
  (declare (ignore name))
  (let ((type (second (first arg))))
    (if type
        (format nil "   Each of the remaining arguments must be ~a.~%"
                (defmfun1::get-arg-spec-to-english type))
        "")))

(defun format-arg-specs1 (name req optional rest-args)
 "name is maxima function name (no $). The remaining three
  args are forms for required, optional, and rest from the
  lambda list."
  (declare (ignore name))
  (let ((arg-count 0) (res)
        (arg-list (append req optional)))
    (if (= 1 (length arg-list))
        (let ((arg (car arg-list)))
          (if (> (length arg) 1)
            (setf res (list (format-single-spec req optional rest-args (first arg) (second arg))))
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
"Format the information in documentation slot as a string if it exists,
 else return empty string."
  `(let ((x (,slot e)))
     (if x
       (format nil ,@body)  "")))

(defmacro form-ent-cond (slot test &body body)
 "same as form-ent, but a test must also be satisfied"
  `(let ((x (,slot e)))
     (if (and x ,test)
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
                   (format nil "    Section: ~a~%" (entry-section e)))
                 (let ((pack (defmfun1:get-mext-package-for-function name)))
                   (if pack (format nil "    mext package: ~a~%" pack) ""))
                 (format nil "~%")
                 (if (null (entry-default-value e))  ""
                   (format nil "~a.~%"
                           (format-doc-text 
                            (list "  default value " :code (entry-default-value e)) *format-codes-default* )))
                 (if (> (length (entry-call-desc-list e)) 0)  ; doesn't this put a nil in the list ?
                     (format nil "Calling:~%~a"
                             (format-call-desc-list (entry-call-desc-list e))))
                 (if (> (length (entry-contents e)) 0)
                     (format nil "Description:~%~a~%" (wrap-text 
                               :text (format-doc-text (entry-contents e) *format-codes-default* ) 
                               :width *text-width* :indent *indent1*)) "")
                 (let ((sspec (format-arg-specs name)))
                   (when (> (length sspec) 0) (format nil "Arguments:~%~a" sspec)))
                 (let ((opts (maxima::$foptions name)))
                   (if (> (length opts) 1)
                       (format nil "Options:  ~a takes options with default values: ~a.~%" name 
                               (format nil "~{~a~^, ~}" (cdr opts)))
                       ""))
                 (let ((atts (maxima::$attributes name)))
                   (if (> (length atts) 1)
                       (format nil "Attributes: ~a has attributes: ~a~%" name (maxima::$sconcat atts))
                       ""))
                 (examples:format-examples name)
                 (form-ent entry-oeis "~%OEIS number: ~a.~%" (comma-separated-english x))
                 (form-ent entry-see-also "~%See also: ~a.~%" (comma-separated-english x))
                 (form-ent-cond entry-implementation maxima::$print_implementation "~%Implementation:~%   ~a~%" 
                           (wrap-text :text (format-doc-text x) :width *text-width* :indent *indent2*))
                 (form-ent-cond entry-author maxima::$print_authors
                     "~%  Author~p: ~a.~%" (length x) (comma-separated-english x))
                 (form-ent-cond entry-copyright maxima::$print_copyrights
                     "~%  Copyright (C) ~{~a ~}.~%" x)
                 (format nil "~%"))))

(defun format-doc-entry-latex (e)
  "called by system documentation routines."
  (let* ((name (entry-name e))
         (fname (latex-esc name))
         (*format-codes-default* *format-codes-latex*))
    (flet ((skip () (format nil "~%\\vspace{5 pt}~%"))
           (sect (sec) (format nil "\\noindent{\\bf ~a}\\hspace{5pt}" sec)))
      (concatenate 'string
                   (format nil "\\subsection{~a: ~a\\label{sec:~a}}~%\\hypertarget{~a}{}~%" 
                           (entry-type e) fname name name)
                   (if (null (entry-protocol-list e))
                       (format nil "~%")
                     (format nil "~a~%~%" (format-protocol-latex (entry-protocol-list e))))
;                 (if (null (entry-section e)) "" ; should be included depending on how this is called
;                   (format nil "Section: ~a~%~%" (entry-section e)))
                   (let ((pack (defmfun1:get-mext-package-for-function name)))
                     (if pack (format nil "~%\\noindent mext package: ~a~%~%" (latex-esc pack)) ""))
                   (format nil "~%")
                   (skip)
                   (if (null (entry-default-value e))  ""
                     (format nil "~a.~%~%"
                             (format-doc-text-latex 
                              (list "  default value " :code (entry-default-value e)) *format-codes-latex* )))
                   (if (> (length (entry-call-desc-list e)) 0)
;;  conveters dont like this  (format nil "~a~%\\begin{itemize}\\itemsep5pt \\parskip0pt \\parsep0pt ~%~a\\end{itemize}~%"
                       (format nil "~a~%\\begin{itemize}~%~a\\end{itemize}~%"
                               (sect "Calling")
                               (format-call-desc-list-latex (entry-call-desc-list e))))
                   (if (> (length (entry-contents e)) 0)
                       (format nil "~a~%~a~%~a~%" (sect "Description") (wrap-text 
                               :text (format-doc-text-latex (entry-contents e) *format-codes-latex* )
                               :width *latex-text-width* :indent 0) (skip)) "")
                   (let ((sspec (format-arg-specs name)))
                     (if (> (length sspec) 0) 
                       (format nil "~a~%~a~%~a~%" (sect "Arguments") (latex-esc (format nil "~a" sspec)) (skip))
                       ""))
                   (let ((opts (maxima::$foptions name)))
                     (if (> (length opts) 1)
                         (format nil "~a~%{\\tt ~a} takes options with default values: ~a.~a~%" 
                                 (sect "Options") fname 
                                 (latex-esc (format nil "~{{\\tt ~a}~^, ~}" (cdr opts)))
                                 (skip))
                       ""))
                   (let ((atts (maxima::$attributes name)))
                     (if (> (length atts) 1)
                         (format nil "~a~%~a has attributes: ~a~%~a~%" 
                                 (sect "Attributes") fname (latex-esc (maxima::$sconcat atts))
                                 (skip))
                       ""))
                   (examples:format-examples-latex name)
                   (form-ent entry-oeis "~%OEIS number: ~a.~%~%" (comma-separated-english x))
                   (form-ent entry-see-also "~%~a~% ~a.~%~a~%"
                             (sect "See also")
                             (comma-separated-english 
                                         (loop for e in x collect
                                               (format nil "\\hyperlink{~a}{{\\tt ~a}}" e (latex-esc e)))) (skip))
                   (form-ent-cond entry-implementation maxima::$print_implementation "~%~a~%~a~%~a~%"
                           (sect "Implementation")
                           (wrap-text :text (format-doc-text-latex x) :width *latex-text-width* :indent 0)
                           (skip))
                   (form-ent-cond entry-author maxima::$print_authors
                             "~%~a~%~a.~%~a~%" 
                             (sect (format nil "Author~p" (length x)))  (comma-separated-english x)
                             (skip))
                   (form-ent-cond entry-copyright maxima::$print_copyrights
                             "~%~a~%~{~a~^ ~}.~%~a~%"
                             (sect "Copyright")
                             (loop for e in x collect
                                   (cond ((stringp e) (latex-esc e))
                                         ((numberp e) (format nil "(~a)" e))
                                         (t e)))
                             (skip))
                 (format nil "~%")))))
  
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

(when (doc-system::ds-registered-p "max-doc")
    (doc-system::ds-de-register "max-doc"))

(when (doc-system::ds-registered-p "max-doc-sec")
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

