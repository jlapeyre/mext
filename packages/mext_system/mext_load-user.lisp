;;; This file is copied to the user's maxima directory and is renamed
;;; mext_load.lisp. It is not loaded directly.  There is a file
;;; `mext.lisp' that is also copied to the user's maxima directory. It
;;; is loaded with `load(mext)'.  It then loads this file, unless it has
;;; already been loaded.
;;; The user may instead do `load(mext_core)', which loads this file and
;;; then some mext packages.

;;; This file just tries to find the mext installation directory and
;;; the file `load_mext_maxima1' within it. It then loads
;;; `load_mext_maxima1', which loads the rest of the mext system.
;;; mext packages themselves are then loaded with `require(packname)'.

(if (find-package :mext-maxima-load ) t  
  (defpackage :mext-maxima-load (:use common-lisp )))

(in-package :mext-maxima-load)

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
  (append (pathname-directory #-gcl *load-pathname* #+gcl sys:*load-pathname*)
          (list "mext" *maxima-and-lisp-version* "mext_system")))

(let ((file 
      (make-pathname :name "load_mext_maxima1" :type "lisp"
        :directory *mext-installation-dir*)))
  (if (probe-file file) (maxima::$load file)
    (format t "Warning: cant find mext installation directory ~s~%" file)))
