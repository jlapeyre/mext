;;; This file is loaded when building and installing mext-maxima It
;;; should be called via 00-mext-ibuild.lisp.  This file is similar to
;;; load_mext_maxima1.lisp, but that file is installed in the mext
;;; subdirectory of the user's maxima directory. It loads the rest of
;;; the mext system.
;;;
;;; load_mext_maxima.lisp -- only used during building and installation
;;; load_mext_maxima1.lisp -- only used when user loads the mext system.

;;;
;;; I think the instructions below are obsolete.
;;; Start maxima cd into this directory and type
;;; load("./load_mext_maxima.lisp");
;;;  Or, if you can't cd into the distribution directory, type
;;; load("/path/tofile/load_mext_maxima.lisp");
;;; or whatever you type on your platform to load a file
;;; in particular directory.
(in-package :maxima)

;;; Load the three lisp files and maxima file.
(defvar *load-mext-maxima-load-pathname* (pathname-directory (probe-file $load_pathname)))

;#+gcl (load (make-pathname :directory *load-mext-maxima-load-pathname*
;                     :name "gcl-pathnames" :type "lisp"))

(let ((dir *load-mext-maxima-load-pathname*))
  (loop for file in (list  
      #+openmcl "defsystem" "operate-on-system2" "mext-maxima-packages" "gjl-lisp-util" "gjl-maxima-util"
       "pathname-library" "mext-maxima-system" "dontkill" "compile" "mext-component-operations")
        do
        (let ((file-path (make-pathname :directory dir :name file :type "lisp")))
                         (format t " load_mext Loading ~a~%" file-path)
                         (load file-path))))

(defvar mext-maxima::*dist-name* "mext_system")

(let ((dir *load-mext-maxima-load-pathname*))
  (loop for file in (list  "mext")
        do
        (let ((file-path (make-pathname :directory dir :name file :type "system")))
                         (format t " load_mext Loading ~a~%" file-path)
                         (load file-path))))
