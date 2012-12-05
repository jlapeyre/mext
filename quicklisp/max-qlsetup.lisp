(in-package :max-ql)

(defun quicklisp-start ()
  (let* ((setup (make-pathname :name "setup" :type "lisp"
                               :directory (pathname-directory *quicklisp-pathname*)))
         (probed (probe-file setup)))
    (if probed 
        (progn 
          #+openmcl
          (progn (format t "ccl: loading asdf~%")
                 (load-asdf))
          (load probed)
          (let ((max-ql (make-pathname :name "max-ql" :type "lisp")))
            (loop for dir in 
                  (list *load-pathname* 
                        (mext:subdir-pathname '( "quicklisp" ) mext::*mext-user-dir-as-string*)) do
                  (if dir
                      (let* ((pn (merge-pathnames max-ql dir))
                             (ppn (probe-file pn)))
                        (when ppn (return (load ppn)))))))))))

(quicklisp-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(defmfun1 $quicklisp_start ()
  (max-ql::quicklisp-start))

;; quicklisp-quickstart:install will report failure with ccl under windows, but will be more or less
;; installed. Then we load asdf explicitly, then setup
(defmfun1 $quicklisp_install ()
  #-gcl
  (quicklisp-quickstart:install)
  #-gcl (max-ql::quicklisp-start)
  #+gcl (format t "gcl cannot use quicklisp. Not loading~%"))
