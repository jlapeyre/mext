(in-package "COMMON-LISP-USER")

;   :copy-array  conflicts with symbol in graphs
(if (find-package :gjl.lisp-util ) t  (defpackage :gjl.lisp-util (:use common-lisp)
  (:nicknames :gjl)
  (:export 

   :anit
   :cmp-length
   :comma-separated-english
   :copy-array-type
   :dbind 
   :dump-hash
   :ensure-list
   :fill-hash-from-list
   :gaif
   :get-hash-keys 
   :get-or-make-subhash
   :keyword-p
   :length-eq
   :length-eq 
   :length1p 
   :not-comma-separated-english
   :nthcdr-check
   :or-comma-separated-english
   :print-hash-entry 
   :remove-terminal-substring
   :replace-all
   :sconcat
   :split-by-one-newline
   :split-by-one-space
   :split-by-space-and-newline
   :string-ends-with-pos 
   :to-string-lc
   :wrap-text )))

; Easiest to just intern these in :maxima
;(if (find-package :gjl.maxima-util ) t  (defpackage :gjl.maxima-util (:use common-lisp :gjl.lisp-util)
;  (:nicknames :gjl-max)
;  (:export
;  )))

(if (find-package :mext-maxima ) t  
  (defpackage :mext-maxima (:use common-lisp :gjl.lisp-util)
    (:nicknames :mext)
    (:export 

     :add-to-dont-kill
     :change-root-pathname
     :compact-pathname
     :directory-exists-p
     :directory-pathname-p
     :do-allow-kill-share
     :do-dont-kill-share
     :fenough-namestring
     :fensure-directories-exist
     :fload-pathname
     :fmake-pathname
     :fpathname-directory
     :fwild-pathname-p
     :list-directory
     :load-asdf
     :mext-list
     :mext-optimize
     :mext-require
     :pathname-as-directory
     :remove-from-dont-kill
     :subdir-of-contrib
     :subdir-of-shared
     :subdir-pathname )))
