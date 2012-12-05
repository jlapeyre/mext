(in-package :maxima)
(use-package :mext-maxima-load)

(defvar *load-mext-maxima-load-pathname* (pathname-directory (probe-file $load_pathname)))

(flet ((load-one (file type)
      (let ((file-path (make-pathname :directory *load-mext-maxima-load-pathname* :name file :type type)))
        (load (probe-file file-path)))))
  (load-one "mext-maxima-packages" "lisp")
  (loop for file in (list  #+openmcl "defsystem" "operate-on-system2"
      "gjl-lisp-util" "pathname-library" "mext-maxima-system"
      "mext-component-operations" "compile") do
    (load-one file mext-maxima-load::*binary-ext*))
;  #-gcl (load-one "quicklisp" mext-maxima-load::*binary-ext*)
;  #-gcl (load-one "maxima-asdf" "lisp")
  (load-one "mext_system" "mxt"))
