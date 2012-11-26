;; These are copied from maxima source.
;; The maxima functions do not allow the intermediate
;; filenames to be specified. So, if the source is in
;; a dir for which user has no write permission, translation
;; and compilation will fail

(in-package :maxima)

(defvar *mext-translated-ext* "LISP")
(defvar *mext-warn-file-ext* "UNLISP")

(defun mext-translate-file (in-file-name out-file-name
                                         &optional (ttymsgsp $tr_file_tty_messagesp)
		       &aux warn-file translated-file *translation-msgs-files*
		       *untranslated-functions-called* *declared-translated-functions*)
  (format t "mext-translate-file in ~s out ~s~%" in-file-name out-file-name)
  (bind-transl-state
   (setq *in-translate-file* t)
   (setq translated-file (alter-pathname (or out-file-name in-file-name) :type *mext-translated-ext*))
   (setq warn-file (alter-pathname translated-file :type *mext-warn-file-ext*)) ;gjl change
   (format t " translated-file ~s warn-file ~s~%" translated-file warn-file)
   (with-open-file (in-stream in-file-name)
     (with-open-file (out-stream translated-file :direction :output :if-exists :supersede)
       (with-open-file (warn-stream warn-file :direction :output :if-exists :supersede)
	 (setq *translation-msgs-files* (list warn-stream))
	 (if ttymsgsp
	     (setq *translation-msgs-files* (cons *standard-output* *translation-msgs-files*)))
	 (format out-stream ";;; -*- Mode: Lisp; package:maxima; syntax:common-lisp ;Base: 10 -*- ;;;~%")
	 (flet ((timezone-iso8601-name (dst tz)
		  ;; This function was borrowed from CMUCL.
		  (let ((tz (- tz)))
		    (if (and (not dst) (= tz 0))
			"Z"
			(multiple-value-bind (hours minutes)
			    (truncate (if dst (1+ tz) tz))
			  (format nil "~C~2,'0D:~2,'0D"
				  (if (minusp tz) #\- #\+)
				  (abs hours)
				  (abs (truncate (* minutes 60)))))))))
	   (multiple-value-bind (secs mins hours day month year dow dst tz)
	       (decode-universal-time (get-universal-time))
	     (declare (ignore dow))
	     (format out-stream (intl:gettext ";;; Translated on: ~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D~A~%")
		     year month day hours mins secs (timezone-iso8601-name dst tz))))
	 (format out-stream (intl:gettext ";;; Maxima version: ~A~%") *autoconf-version*)
	 (format out-stream (intl:gettext ";;; Lisp implementation: ~A~%") (lisp-implementation-type))
	 (format out-stream (intl:gettext ";;; Lisp version: ~A~%") (lisp-implementation-version))
	 (format out-stream "(in-package :maxima)~%")
	 (format warn-stream (intl:gettext "This is the unlisp file for ~A~%")
		 (namestring (pathname in-stream)))
	 (mformat out-stream
		  (intl:gettext ";;** Translator flags were: **~%~%"))
	 (loop for v in (cdr $tr_state_vars)
		do (mformat out-stream   ";; ~:M: ~:M;~%" v (symbol-value v)))
	 (mformat *terminal-io* (intl:gettext "translator: begin translating ~A.~%")
		  (pathname in-stream))
	 (call-batch1 in-stream out-stream)
	 (insert-necessary-function-declares warn-stream)
	 ;; BATCH1 calls TRANSLATE-MACEXPR-toplevel on each expression read.
	 (cons '(mlist)
	       (mapcar 'namestring
		       (mapcar 'pathname (list in-stream out-stream warn-stream)))))))))

;; The original 5.28.0 function allows translation-output-file to be specified,
;; but this is not in the documentation.

(defun $mext_compile_file (input-file &optional bin-file translation-output-file  &aux result)
  (format t "mext_compile_file in ~s out ~s~%" input-file bin-file)
  (setq input-file (maxima-string input-file))
  (and bin-file (setq  bin-file (maxima-string bin-file)))
  (if translation-output-file
       (setq translation-output-file (maxima-string translation-output-file))
    (if bin-file
        (setq translation-output-file (alter-pathname bin-file :type *mext-translated-ext*))))
  (cond ((string-equal (pathname-type input-file) *mext-translated-ext*)
	 (setq result (list '(mlist) input-file)))
	(t (setq result (mext-translate-file input-file translation-output-file)) ; gjl change
	   (setq input-file (third result))))
  #+(or cmu scl sbcl clisp allegro openmcl lispworks ecl)
  (multiple-value-bind (output-truename warnings-p failure-p)
      (if bin-file
	  (compile-file input-file :output-file bin-file)
	  (compile-file input-file))
    (declare (ignore warnings-p))
    ;; If the compiler encountered errors, don't set bin-file to
    ;; indicate that we found errors. Is this what we want?
    (unless failure-p
      (setq bin-file output-truename)))
  #-(or cmu scl sbcl clisp allegro openmcl lispworks ecl)
  (setq bin-file (compile-file input-file :output-file bin-file))
  (append result (list bin-file)))

