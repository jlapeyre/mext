;; Put all the package definitions in this file

(in-package "COMMON-LISP-USER")
;;(in-package :common-lisp-user)

(if (find-package :simple-doc ) t (defpackage :simple-doc (:use :common-lisp :gjl.lisp-util)))

(if (find-package :doc-system ) t (defpackage :doc-system (:use :common-lisp :gjl.lisp-util)))

(if (find-package :descr1 ) t (defpackage :descr1 (:use :common-lisp :gjl.lisp-util)))

(if (find-package :info-database ) t (defpackage :info-database (:use :common-lisp :gjl.lisp-util)))
