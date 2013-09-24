(in-package :maxima)
(max-doc:set-cur-sec 'max-doc::quicklisp-fandv)
(defmfun1:set-file-and-package "max-ql.lisp" "quicklisp")

(defmfun1 ($quickload :doc) ((package_name :string :thread)
                             &opt ($verbose nil :bool))
  :desc ("Load the asdf lisp package " :arg "package_name"
         ", or, if not installed, install from the internet and then load.")
  (quicklisp-client:quickload package_name :verbose $verbose)
  '$done)

(defmfun1 ($quicklisp_search :doc) ( (term :string) )
  :desc 
  ("Search quicklisp for lisp `systems' (packages) matching " :argdot "term"
  " The empty string returns all avaliable systems.")
  (format t "~s~%" (quicklisp-client:system-apropos term))
  '$done)

(defmfun1 ($quicklisp_update_client :doc) ()
  :desc ("Update the quicklisp client.")
  (quicklisp-client:update-client)
  '$done)

(defmfun1 ($quicklisp_update_all_dists :doc) ()
  :desc ("Update all distributions installed with quicklisp."
   " This checks the versions of all installed software distributions (packages)
   installed via quicklisp and downloads and installs newer versions, if they are
   available.")
  (quicklisp-client:update-all-dists)
  '$done)
