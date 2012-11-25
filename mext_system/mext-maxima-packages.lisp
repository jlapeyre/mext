(in-package "COMMON-LISP-USER")

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
   :wrap-text)))

;; I think this must be synced with mext-user.lisp
(if (find-package :mext-maxima ) t  
  (defpackage :mext-maxima (:use common-lisp :gjl.lisp-util)
    (:nicknames :mext)
    (:export :list-directory
             :directory-exists-p
             :compact-pathname
             :pathname-as-directory
             :fmake-pathname
             :fpathname-directory
             :fwild-pathname-p
             :fensure-directories-exist
             :fenough-namestring
             :change-root-pathname
             :directory-pathname-p
             :mext-optimize
)))
;    (:import-from :gjl.lisp-util :ensure-list)))
