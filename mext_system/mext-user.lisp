;; This huge mess finds the mext files and loads them.

;;  Define path functions for gcl that are more like ansi common lisp
#+gcl (in-package :cl)

#+gcl (defun gcl-make-pathname (&rest args)
        (let ((dir-spec (getf args :directory)))
          (if dir-spec
              (cond ( (stringp dir-spec)
                      (setf (getf args :directory) (list :root dir-spec)))
                    ( (listp dir-spec)
                      (let ((word1 (car dir-spec)))
                        (cond ( (eq :absolute word1)
                                (setf (getf args :directory)
                                      (cons :root (cdr dir-spec))))
                              ( (eq :relative word1)
                                (setf (getf args :directory)
                                      (cdr dir-spec)))
                              (t
                       (error "List of directory components must start with :ABSOLUTE or :RELATIVE."))))))))
        (apply 'make-pathname args))

#+gcl (defun gcl-pathname-directory (&rest args)
        (let ((dir-spec (apply 'pathname-directory args)))
          (if (and (listp dir-spec) (not (null dir-spec)))
            (let ((word1 (car dir-spec)))
              (if (and (symbolp word1) (eq :ROOT word1))
                  (setf dir-spec (cons :ABSOLUTE (cdr dir-spec)))
                (setf dir-spec (cons :RELATIVE dir-spec)))))
          dir-spec))

#+gcl (export 'gcl-make-pathname)
#+gcl (export 'gcl-pathname-directory)

(load (make-pathname :name "mext-maxima-packages" :type "lisp"
           :defaults  #-gcl *load-pathname* #+gcl sys:*load-pathname*))

; this appears in too many places. Is it available from within maxima ?
(defparameter *binary-ext*
              #+gcl "o"
	      #+(or cmu scl) (c::backend-fasl-file-type c::*target-backend*)
	      #+sbcl "fasl"
	      #+clisp "fas"
	      #+allegro "fasl"
	      #+openmcl (pathname-type ccl::*.fasl-pathname*)
	      #+lispworks (pathname-type (compile-file-pathname "foo.lisp"))
	      #+ecl "fas"
	      #-(or gcl cmu scl sbcl clisp allegro openmcl lispworks ecl)
	      ""
"File extension for binary files produced from compiling lisp code.
This was copied from maxima source init-cl.lisp.")

;; stolen from someone who stole from swank/slime
(defun lisp-version-string ()
  #+cmu       (substitute #\- #\/ (lisp-implementation-version))
  #+sbcl      (lisp-implementation-version)
  #+ecl       (lisp-implementation-version)
  #+gcl       (let ((s (lisp-implementation-version))) (subseq s 4))
  #+openmcl   (format nil "~d.~d"
                      ccl::*openmcl-major-version* 
                      ccl::*openmcl-minor-version*)
  #+lispworks (lisp-implementation-version)
  #+allegro   (concatenate 'string (if (eq 'h 'H) "A" "M") 
                           excl::*common-lisp-version-number*)
  #+clisp     (let ((s (lisp-implementation-version)))
                (subseq s 0 (position #\space s)))
  #+armedbear (lisp-implementation-version)
  #+cormanlisp (lisp-implementation-version)
  #+digitool   (subseq (lisp-implementation-version) 8))

; for use in directory names. We hope the result is a valid and painless directory name on all platforms.
(defparameter *maxima-and-lisp-version*
  (substitute #\- #\Space (remove #\) (remove #\( (concatenate 'string "v" maxima::*autoconf-version* "-" 
                               maxima::*maxima-lispname* "-v" (lisp-version-string))))))

; this is a list starting with ABSOLUTE. not a path object
(defparameter *mext-installation-dir*
  (append (#-gcl pathname-directory #+gcl gcl-pathname-directory #-gcl *load-pathname* #+gcl sys:*load-pathname*)
                                    (list "mext" *maxima-and-lisp-version* "mext-system")))

(load (#-gcl make-pathname #+gcl gcl-make-pathname :name "mext-maxima-packages" :type "lisp"
    :directory *mext-installation-dir*))

(loop for file in (list #+openmcl "defsystem"
         "operate-on-system2" "gjl-lisp-util" "pathname-library" "mext-maxima-system") do
      (load (#-gcl make-pathname #+gcl gcl-make-pathname :name file :type *binary-ext*
          :directory *mext-installation-dir*)))

(loop for file in (list "mext-component-operations")  do
      (load (#-gcl make-pathname #+gcl gcl-make-pathname :name file  :directory *mext-installation-dir*)))
