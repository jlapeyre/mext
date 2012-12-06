(in-package :maxima)
(max-doc:set-cur-sec 'max-doc::quicklisp-fandv)

(defmfun1 ($quicklisp_load :doc) ((package_name :string))
  :desc ("Load the asdf lisp package " arg "package_name"
                   " or install from the internet and load.")
  (quicklisp-client:quickload package_name)
  '$done)

(defmfun1 ($quicklisp_apropos :doc) ( (term :string) )
  :desc ("Search quicklisp for lisp 'systems' (packages) matching " arg "term" ".")
  (format t "~s~%" (quicklisp-client:system-apropos term))
  '$done)
