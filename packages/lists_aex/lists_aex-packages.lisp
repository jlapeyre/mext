;; Put all the package definitions in this file
(in-package "COMMON-LISP-USER")
;;(in-package :common-lisp-user)

(if (find-package :maxima-take ) t
  (defpackage :maxima-take
    (:use :common-lisp :gjl.lisp-util :max-doc)))

(if (find-package :partition-list ) t (defpackage :partition-list (:use :common-lisp )))

(defpackage :max-list (:use :common-lisp :gjl.lisp-util)
                (:import-from :maxima :$sconcat :merror :mlist :$listp ))
