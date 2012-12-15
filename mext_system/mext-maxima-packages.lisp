(in-package "COMMON-LISP-USER")

;   :copy-array  conflicts with symbol in graphs
(if (find-package :gjl.lisp-util ) t  (defpackage :gjl.lisp-util (:use common-lisp)
  (:nicknames :gjl)
  (:export 
   :gaif
   :cmp-length
   :length-eq
   :comma-separated-english
   :copy-array-type
   :dbind 
   :dump-hash
   :ensure-list
   :fill-hash-from-list
   :get-hash-keys 
   :get-or-make-subhash
   :replace-all
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

(if (find-package :mext-maxima ) t  
  (defpackage :mext-maxima (:use common-lisp :gjl.lisp-util)
    (:nicknames :mext)
    (:export :add-to-dont-kill
             :compact-pathname
             :list-directory
             :directory-exists-p
             :do-dont-kill-share
             :pathname-as-directory
             :fmake-pathname
             :fload-pathname
             :fpathname-directory
             :fwild-pathname-p
             :fensure-directories-exist
             :fenough-namestring
             :change-root-pathname
             :directory-pathname-p
             :mext-optimize
             :mext-require
             :mext-list
             :subdir-pathname
             :subdir-of-shared
             :subdir-of-contrib
             :load-asdf
)))
