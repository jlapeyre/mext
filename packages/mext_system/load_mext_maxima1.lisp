(in-package :maxima)
(use-package :mext-maxima-load)

;;; This file is installed in the mext subdirectory of the user's
;;; maxima directory. It loads the rest of the mext system.  It is
;;; similar to load_mext_maxima.lisp, but that file is only loaded
;;; during building and installing of mext.
;;;
;;; load_mext_maxima.lisp -- only used during building and installation
;;; load_mext_maxima1.lisp -- only used when user loads the mext system.


(defvar *load-mext-maxima-load-pathname* (pathname-directory (probe-file $load_pathname)))

(flet ((load-one (file type)
      (let ((file-path (make-pathname :directory *load-mext-maxima-load-pathname* :name file :type type)))
        (load (probe-file file-path)))))
  (load-one "mext-maxima-packages" "lisp")
  (loop for file in (list  #+openmcl 
      "defsystem" "operate-on-system2"
      "gjl-lisp-util" "gjl-maxima-util" "pathname-library" 
      "mext-maxima-system" "dontkill" "fix-tex"
      "mext-component-operations" "compile") do
    (load-one file mext-maxima-load::*binary-ext*))
;  #-gcl (load-one "quicklisp" mext-maxima-load::*binary-ext*)
;  #-gcl (load-one "maxima-asdf" "lisp")
  (load-one "mext_system" "mxt"))
