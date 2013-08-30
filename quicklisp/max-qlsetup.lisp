(in-package :max-ql)

(defmfun1:set-file-and-package "max-qlsetup.lisp" "quicklisp")

(defun quicklisp-start ()
  (let* ((setup (make-pathname :name "setup" :type "lisp"
                               :directory (pathname-directory *quicklisp-pathname*)))
         (probed (probe-file setup)))
    (when probed 
        (progn 
          #+openmcl
          (progn (format t "ccl: loading asdf~%")
                 (load-asdf))
          (load probed)
          (let ((max-ql (make-pathname :name "max-ql" :type "lisp")))
            (loop for dir in 
                  (list *load-pathname* 
                        (mext:subdir-pathname '( "quicklisp" ) mext::*mext-user-dir-as-string*)) do
                  (when dir
                      (let* ((pn (merge-pathnames max-ql dir))
                             (ppn (probe-file pn)))
                        (when ppn (return (load ppn)))))))))))

(quicklisp-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(max-doc::set-cur-sec 'max-doc::quicklisp-fandv)

(defmfun1 ($quicklisp_start :doc) ()
  "Load (setup) quicklisp. It must already be installed."
  (max-ql::quicklisp-start))

;; quicklisp-quickstart:install will report failure with ccl under windows, but will be more or less
;; installed. Then we load asdf explicitly, then setup
(defmfun1 ($quicklisp_install :doc) ()
  "Download and install quicklisp from the internet. This is usually done automatically as the
 final step of building and installing the maxima interface to quicklisp."
  #-gcl
  (quicklisp-quickstart:install)
  #-gcl (max-ql::quicklisp-start)
  #+gcl (format t "gcl cannot use quicklisp. Not loading~%"))
