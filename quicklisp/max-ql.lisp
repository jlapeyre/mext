(in-package :maxima)
(max-doc::set-cur-sec 'max-doc::quicklisp-fandv)

(defmfun1 ($quicklisp_load :doc) ((package-name :string))
  "Load the asdf lisp package <package-name> or install from the internet and load."
  (quicklisp-client:quickload package-name)
  '$done)

(defmfun1 ($quicklisp_apropos :doc) ( (term :string) )
  "Search quicklisp for lisp 'systems' (packages) matching <term>." 
  (format t "~s~%" (quicklisp-client:system-apropos term))
  '$done)
