;; Put all the package definitions in this file

(in-package "COMMON-LISP-USER")
;;(in-package :common-lisp-user)

(if (find-package :gjl.lisp-util ) t  (defpackage :gjl.lisp-util (:use common-lisp)
  (:export
   :gaif
   :cmp-length
   :comma-separated-english
   :copy-array
   :copy-array-type
   :dbind
   :dump-hash
   :ensure-list
   :fill-hash-from-list
   :get-hash-keys
   :get-or-make-subhash
   :anit
   :keyword-p
   :length-eq
   :length1p
   :not-comma-separated-english
   :or-comma-separated-english
   :print-hash-entry
   :remove-terminal-substring
   :split-by-one-space
   :string-ends-with-pos
   :wrap-text
)))

(if (find-package :defmfun1 ) t
  (defpackage :defmfun1
    (:use :common-lisp :gjl.lisp-util)
    (:import-from :maxima :$sconcat :merror)
    (:export :rule :set-mext-package :set-file-and-package :get-mext-package-for-function
             :get-filename-for-function
             :record-mext-package
             :set-hold-all)))

(if (find-package :max-doc ) t
  (defpackage :max-doc (:use :common-lisp :gjl.lisp-util)
    (:nicknames :maxdoc) ; I want to switch from max-doc to maxdoc
    (:export :add-doc-sec :add-doc-entry :add-doc-entry1
     :add-call-desc :add-call-desc1
     :clear-call-desc :mdefmvar :implementation :latex-esc :print-doc-section :print-doc-entry
     :format-doc-text :format-doc-text-latex
     :print-doc-entry-latex :print-doc-section-latex :author :copyright
     :set-cur-sec :set-cur-sec-shortname :get-doc-entry
     :get-cur-sec :see-also :see-also-group)))

(if (find-package :maxima-dev-doc ) t (defpackage :maxima-dev-doc (:use :common-lisp :gjl.lisp-util)))
;;       (:export :ddefmfun :ddefun :ddefvar :ddefparameter )))

(if (find-package :simple-doc ) t (defpackage :simple-doc (:use :common-lisp :gjl.lisp-util)))

(if (find-package :doc-system ) t (defpackage :doc-system (:use :common-lisp :gjl.lisp-util)
         (:export :ds-registered-p :ds-de-register :ds-make-and-register
                  :set-source-package :set-source-file-name
                  :get-source-file-name :get-source-package)))

(if (find-package :descr1 ) t (defpackage :descr1 (:use :common-lisp :gjl.lisp-util)))

(if (find-package :info-database ) t (defpackage :info-database (:use :common-lisp :gjl.lisp-util)))

(if (find-package :examples ) t (defpackage :examples (:use :common-lisp :gjl.lisp-util)
            (:import-from :maxima :$sconcat :merror
             :mfuncall   :$eval_string)
            (:export :add-example :clear-example :clear-add-example
                     :format-examples :format-examples-latex :wrap-text)))
