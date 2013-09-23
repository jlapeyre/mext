(in-package :maxima)
(max-doc:set-cur-sec 'max-doc::quicklisp-fandv)
(defmfun1:set-file-and-package "max-ql.lisp" "quicklisp")

(defmfun1 ($quicklisp_load :doc) ((package_name :string))
  :desc ("Load the asdf lisp package " :arg "package_name"
         ", or, if not installed, install from the internet and then load.")
  (quicklisp-client:quickload package_name)
  '$done)

(defmfun1 ($quicklisp_apropos :doc) ( (term :string) )
  :desc 
  ("Search quicklisp for lisp `systems' (packages) matching " :argdot "term"
  " The empty string returns all avaliable systems.")
  (format t "~s~%" (quicklisp-client:system-apropos term))
  '$done)
