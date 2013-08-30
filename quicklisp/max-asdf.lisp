(in-package :max-ql)
(defmfun1:set-file-and-package "max-asdf.lisp" "quicklisp")

(defvar *quicklisp-pathname*
   (mext:subdir-pathname (list "quicklisp")  mext::*homedir-pathname*))

(defvar *asdf-pathname*
  #-(or :win32 :windows :mswindows)  
  (pathname "/usr/share/common-lisp/source/cl-asdf/asdf.lisp")
  #+(or :win32 :windows :mswindows)
    (mext:fmake-pathname :name "asdf" :type "lisp"
      :directory (mext:fpathname-directory *quicklisp-pathname*)))

(defun load-asdf ()
  #-(or :gcl :sbcl) (load *asdf-pathname*)
  #+sbcl (require :asdf)
  #+gcl (format t "gcl cannot use asdf. Not loading~%"))
