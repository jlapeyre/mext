(in-package :maxima)

(defmfun1 $quicklisp_load ((package-name :string))
  (quicklisp-client:quickload package-name)
  '$done)

(defmfun1 $quicklisp_apropos ( (term :string) )
  (format t "~s~%" (quicklisp-client:system-apropos term))
  '$done)
