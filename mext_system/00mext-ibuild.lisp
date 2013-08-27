;; load this to build the mext_system distribution

(let ((file (make-pathname :name "load_mext_maxima" :type "lisp"
         :defaults  #-gcl *load-pathname* #+gcl sys:*load-pathname*)))
  (load file))
($mext_dist_clean)
($mext_dist_build)
($mext_dist_user_install)
($mext_dist_clean)
