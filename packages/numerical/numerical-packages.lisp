;; gcl seems to need this. I am not sure which other lisps need it.
;; This remains uncompiled and is loaded before the compiled object code.
(in-package "COMMON-LISP-USER")

(if (find-package :nintegrate ) t  
  (defpackage :nintegrate (:use common-lisp :gjl.lisp-util)
    (:nicknames :nint)))
