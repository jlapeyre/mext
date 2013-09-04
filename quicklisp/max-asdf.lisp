(in-package :max-ql)
(defmfun1:set-file-and-package "max-asdf.lisp" "quicklisp")

;;; Define the variables and functions that will try to load asdf.lisp
;;;
;;; We either load asdf from the quicklisp dir, or use a builtin (I think with sbcl),
;;; or we say that we can't load asdf at all (gcl).
;;; It would be better to try first to load the quicklisp version, and then
;;; search elsewhere if that fails, eg /usr/share/....
;;; But for now, we require quicklisp.

(defvar *quicklisp-pathname*
   (mext:subdir-pathname (list "quicklisp")  mext::*homedir-pathname*))

;; We removed the debian asdf because sbcl wanted it. Now ccl
;; complains, because it needs the pathname.  So now, all platforms
;; use the quicklisp version, except those that have their own
;; (eg. sbcl)

(defvar *asdf-pathname*
  (mext:fmake-pathname :name "asdf" :type "lisp"
                       :directory (mext:fpathname-directory *quicklisp-pathname*)))

(defun load-asdf ()
  #-(or :gcl :sbcl :cmu :ecl ) (load *asdf-pathname*)
  #+(or :sbcl :cmu :ecl ) (require :asdf)
  #+gcl (format t "gcl cannot use asdf. Not loading~%"))

;;; Only comments and tests below.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#| Old way
(defvar *asdf-pathname*
  #-(or :win32 :windows :mswindows)  
  (pathname "/usr/share/common-lisp/source/cl-asdf/asdf.lisp")
  #+(or :win32 :windows :mswindows)
    (mext:fmake-pathname :name "asdf" :type "lisp"
      :directory (mext:fpathname-directory *quicklisp-pathname*)))
|#

#|

  Here are tests with the latest version for who can
  load asdf with require, who can load it from the quicklisp
  dir, and who can do neither.
  
 maxima-5.31.0-ccl-1.9 	maxima-5.31.0-clisp-2.49
 maxima-5.31.0-cmucl-20d 	maxima-5.31.0-ecl-12.12.1
 maxima-5.31.0-gcl-2.6.8 	maxima-5.31.0-sbcl-1.1.11

 Try :lisp (require :asdf), within maximas built with 
 various lisps. We get
 sbcl -- nil
   I think it has it built in or something.
 clisp -- LOAD: A file with name ASDF does not exist
   fails
 ecl -- (ASDF)
   looks ok
 ccl -- MK:DEFSYSTEM: missing system :ASDF.
   fails
 gcl -- error The slot MAKE::COMPONENT is unbound in the object #<MAKE:MISSING-SYSTEM.0>.
   fails
 cmucl -- (ASDF)
   looks ok

  Now try: load("~/quicklisp/asdf.lisp");
 
  clisp -- OK
  ccl  -- OK
  gcl -- loadfile: failed to load ~/quicklisp/asdf.lisp
       -- an error. To debug this try: debugmode(true);
       gcl can't find it.

  :lisp (load "/home/jlapeyre/quicklisp/asdf.lisp")
  gcl  -- finds it but errors trying to load

|#

#| This seemed to be working, but try require for all that will accept

(defun load-asdf ()
  #-(or :gcl :sbcl) (load *asdf-pathname*)
  #+sbcl (require :asdf)
  #+gcl (format t "gcl cannot use asdf. Not loading~%"))

|#
