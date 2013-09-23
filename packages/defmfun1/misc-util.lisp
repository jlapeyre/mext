;;;  Copyright (C) (2012,2013) John Lapeyre. Licensed under GPL, v3 or greater. See the file
;;;  `LICENSE' in this directory.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; Utility functions. These are for calling from maxima generally or
;; for writing maxima code These utilities require defmfun1. Code in
;; misc-util1 does not require it.

(in-package :maxima)
(mext:mext-optimize)
(defmfun1:set-file-and-package "misc-util.lisp" "defmfun1")

(use-package :gjl.lisp-util)
;;(use-package :maxima-dev-doc)

(max-doc:set-cur-sec 'max-doc::misc-util)

(maxdoc:mdefmvar maxima::$compile_lambda_verbose nil
  "If this is true, then print translated code when automatically compiling lambda functions
  passed as arguments. This is done in the macro option-compile-lambda.")

;; this one does not rely on the list  *compiled-lisp-file-extension* above

(max-doc::set-cur-sec 'max-doc::func-def-fandv)


(defmfun1 ($compile_file1 :doc) ( (input-file :string) &optional bin-file translation-output-file &aux result)
  :desc 
  ("This is copied from maxima " :emref "compile_file" 
   ", with changes. Sometimes a loadable binary file is apparently compiled, but"
   " an error flag is set and " :emref "compile_file" 
   " returns false for the output binary filename. Here we return the binary
   filename in any case.")
  (setq input-file (maxima-string input-file))
  (and bin-file(setq  bin-file (maxima-string bin-file)))
  (and translation-output-file
       (setq translation-output-file (maxima-string translation-output-file)))
  (cond ((string-equal (pathname-type input-file) "LISP")
	 (setq result (make-mlist-simp input-file)))
	(t (setq result (translate-file input-file translation-output-file))
	   (setq input-file (third result))))
  #+(or cmu scl sbcl clisp allegro openmcl lispworks ecl)
  (multiple-value-bind (output-truename warnings-p failure-p)
      (if bin-file
	  (compile-file input-file :output-file bin-file)
	  (compile-file input-file))
    (declare (ignore warnings-p failure-p))
    ;; If the compiler encountered errors, don't set bin-file to
    ;; indicate that we found errors. Is this what we want?
    (setq bin-file output-truename))
;;    (unless failure-p   GJL removed these lines from original compile_file
;;      (setq bin-file output-truename)))
  #-(or cmu scl sbcl clisp allegro openmcl lispworks ecl)
  (setq bin-file (compile-file input-file :output-file bin-file))
  (append result (list bin-file)))

(defmfun1 ($comp_load :doc) ((fname :string) &optional (pathlist "" :string-or-listof :ensure-list ))
  :desc 
 ("Compile and load a lisp file. Maxima does not load it by default with "
  :emrefdot "compile_file" " If the input filename does not end with " :dquotecomma 
   ".lisp" " it will be appended. If "
  :arg "pathlist" " is  specified, then " :arg "fname" " is only searched for in directories in "
  :argdot "pathlist")
  (format t "pathlist is ~a~%" ($sconcat pathlist))
  (let (($file_search_lisp (if (and (stringp pathlist) (equal pathlist "")) $file_search_lisp
                            (mk-mlist pathlist))))
    (format t "new pathlist is ~a~%" ($sconcat $file_search_lisp))
        (let* ((lext ".lisp")
               (ss (remove-terminal-substring fname lext))
               (compile-file-name ($file_search (if ss fname (concatenate 'string fname lext)))))
          (if (null compile-file-name)
              (format t "Unable to find the file '~a'~%." fname)
            (let ((res ($compile_file1 compile-file-name)))
              (when (and res (third res))
                  (load (third res))))))))

(ddefmacro option-compile-lambda (func)
  "If compile->true option is given (or default), check if func is a maxima
   lambda function, and if so, translate it, which with some lisps, sbcl, for
   instance, also compiles it. Some iterative functions: nest_while, take_while, etc.
   will use lisp funcall on the result."
  `(when (and $compile (listp ,func) (eq (caar ,func) 'maxima::lambda))
     (setf *in-translate* t)  ; don't really know what's going on here.
     (setf *in-compile* t)
     (setf *in-compfile* nil)
     (when $compile_lambda_verbose
       (format t "compiled: ~s~%" (tr-lambda ,func)))
     (setf ,func (eval (cdr (tr-lambda ,func))))))


(max-doc::set-cur-sec 'max-doc::io-fandv)
(defmfun1 ($pager_string :doc) ( (s :string) )
 :desc 
 ("Read the string " :arg "s" " in the pager given by the maxima variable " :codedot "pager_command"
  "This works at least with gcl under linux.")
  (with-open-file (*standard-output* "tmppager.dat" :direction :output
                                     :if-exists :supersede)
                  (format t "~a" s))
  (maxima::$system (concatenate 'string $pager_command " tmppager.dat")))

(max-doc::set-cur-sec 'max-doc::program-flow-fandv)
(defmfun1 ($error_str :doc)  ()
  :desc ("Returns the last error message as a string. This differs from "
         :emrefcomma "errormsg" " which prints the error message.")
  (let ((err (cdr $error)))
    (push nil err)
    (apply #'format err)))

(max-doc::see-also "error_str" '("error" "errormsg"))
