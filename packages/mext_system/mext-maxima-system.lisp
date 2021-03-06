;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;  Copyright (C) (2012,2013) John Lapeyre. Licensed under GPL, v3 or greater. See the file
;;;  `LICENSE' in this directory.

;;; This file contains the bulk of the code for the mext package,
;;; which manages third party Maxima packages.

(in-package :mext-maxima)

;(defmacro mext-optimize ()
;  `(declaim (optimize (compilation-speed 0) (speed 3) (space 0) (safety 0) #-gcl (debug 0))))

;; for testing.
(defmacro mext-optimize ()
  `(declaim (optimize (compilation-speed 0) (speed 3) (space 0) (safety 0) #-gcl (debug 3))))

;; turn off optimization
;(defmacro mext-optimize ())

(mext-optimize)

;; borrowed from wxmaxima by Andrej Vodopivec
(defmacro no-warning (form)
  #+sbcl `(handler-bind
	     ((style-warning #'muffle-warning)
              (sb-ext:compiler-note #'muffle-warning))
	   ,form)
  #+clisp `(let ((custom:*suppress-check-redefinition* t)) ,form)
  #-(or sbcl clisp) `(progn ,form))

;;; Code for building, loading, installing a maxima
;;; distribution.  Here, 'distribution' means some third
;;; party code; maybe one file, or several files. Here,
;;; 'distribution' corresponds roughly to a linux 'package'.

(defparameter *binary-ext*
              #+gcl "o"
	      #+(or cmu scl) (c::backend-fasl-file-type c::*target-backend*)
	      #+sbcl "fasl"
	      #+clisp "fas"
	      #+allegro "fasl"
	      #+openmcl (pathname-type ccl::*.fasl-pathname*)
	      #+lispworks (pathname-type (compile-file-pathname "foo.lisp"))
	      #+ecl "fas"
	      #-(or gcl cmu scl sbcl clisp allegro openmcl lispworks ecl)
	      ""
"File extension for binary files produced from compiling lisp code.
This was copied from maxima source init-cl.lisp.")

;; stolen from someone who stole from swank/slime
(defun lisp-version-string ()
  #+cmu       (substitute #\- #\/ (lisp-implementation-version))
  #+sbcl      (lisp-implementation-version)
  #+ecl       (lisp-implementation-version)
  #+gcl       (let ((s (lisp-implementation-version))) (subseq s 4))
  #+openmcl   (format nil "~d.~d"
                      ccl::*openmcl-major-version* 
                      ccl::*openmcl-minor-version*)
  #+lispworks (lisp-implementation-version)
  #+allegro   (concatenate 'string (if (eq 'h 'H) "A" "M") 
                           excl::*common-lisp-version-number*)
  #+clisp     (let ((s (lisp-implementation-version)))
                (subseq s 0 (position #\space s)))
  #+armedbear (lisp-implementation-version)
  #+cormanlisp (lisp-implementation-version)
  #+digitool   (subseq (lisp-implementation-version) 8))

(defun lisp-type-symbol ()
  #+cmu       'maxima::$cmu
  #+sbcl      'maxima::$sbcl
  #+gcl       'maxima::$gcl
  #+ecl       'maxima::$ecl
  #+openmcl   'maxima::$openmcl
  #+clisp     'maxima::$clisp
  #+allegro   'maxima::$allegro
)

(defun os-type ()
  (cond ((string= maxima::*autoconf-win32* "true")
         'maxima::$win32)
        ((search "linux" maxima::*autoconf-host*)
         'maxima::$linux)
        (t
         'maxima::$unknown)))         

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  save and restore maxima flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; only have a couple of these so far!
(defvar *maxima-flag-symbols*
  '(maxima::$inflag maxima::$domain))

;; save current values in a hash table
(defun save-maxima-flags ()
  (let ((h (make-hash-table :test 'equal)))
    (dolist (e *maxima-flag-symbols*)
      (setf (gethash e h) (eval e)))
    h))

;; save flags at the time mext is loaded
(defvar *intial-maxima-flags*
  (save-maxima-flags))

;; need to copy in case of lists, etc.
(defun mext-mset (var val)
  (let ((flag) (oldval (eval var)))
    (when (not (equal val oldval))
      (setf flag t))
    (maxima::mset var val)
    (if flag (list (maxima::mk-mlist (list var oldval val))) nil)))

;; set all maxima flags to the values stored in h.
;; use mset in case something is needed there.
(defun set-maxima-flags (h &aux changed)
  (dolist (e *maxima-flag-symbols*)
    (setf changed (append (mext-mset e (gethash e h)) changed)))
  (maxima::mk-mlist changed))

;(defun list-maxima-flags (h)
;  "Return a maxima list of flags and their values."
;  (maxima::hash-to-maxima-list h))

(defvar *maxima-flags-stack* '())

(defun push-maxima-flags ()
  (push (save-maxima-flags) *maxima-flags-stack*))

;; pop and set flags
(defun pop-maxima-flags ()
  (if (consp *maxima-flags-stack*)
      (set-maxima-flags (pop *maxima-flags-stack*))))

(defun clear-maxima-flags-stack ()
  (setf *maxima-flags-stack* '()))

(defun set-modern-maxima-flags ( &aux retval )
  (setf retval (append (mext-mset 'maxima::$inflag t) retval))
  (setf retval (append (mext-mset 'maxima::$domain 'maxima::$complex) retval))
  (maxima::mk-mlist retval))

(defun list-maxima-flags-stack ()
  (cons '(maxima::mlist maxima::simp)
        (loop :for x :in *maxima-flags-stack* :collect
              (maxima::hash-to-maxima-list x))))

;; only restores to values when mext was loaded.
;; we can't know what happened before.
;; or we could also keep track of what stock maxima
;; does and hard code it somewhere here.
(defun restore-initial-maxima-flags ()
  (set-maxima-flags *intial-maxima-flags*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  END save and restore maxima flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun maxima::$lisp_type_symbol ()
  (lisp-type-symbol))

(defun maxima::$os_type_symbol ()
  (os-type))

;; extensions of filenames created during building that we want to remove
;; (clean). defsystem does some of this, but not all.
(defparameter *extensions-to-clean* (list
                                     *binary-ext*
                                     #+sbcl "data"
                                     #+cmu  "err"
                                     #+clisp "lib"))

;; name of the distribution. this is the top level name. similar to
;;  a linux package name
(defvar *distname* nil)
; current defsystem system name. There can be multiple systems in a distribution
(defvar *system-name* nil)
;; Following are set to either a string or a list of strings specifying
;; distribution names.
(defvar *systems-to-compile* nil)
(defvar *systems-to-load* nil)
(defvar *systems-to-install* nil )
(defvar *systems-to-install-other* nil ) ; neither source nor binary
(defvar *systems-to-install-to-mext-root* nil )
(defvar *systems-to-clean* nil)
(defvar *systems-required-source* nil) ; probably remove
(defvar *user-install-explicit* nil)
(defvar *post-user-install-hooks*  nil)
;; distribution directory while building
(defvar *dist-dir* (fpathname-directory (fload-pathname)))

(defvar *userdir-pathname* maxima::*maxima-userdir*)

(defvar *userdir-pathname-as-list* (mext::fpathname-directory 
				    (mext::pathname-as-directory  maxima::*maxima-userdir*)))

(defvar *homedir-pathname* (pathname-as-directory (pathname (maxima::maxima-getenv "HOME"))))

;; These are nil when the first maxima prompt is printed.
#+(or clisp cmu) (setf *default-pathname-defaults* (ext:default-directory))
#+openmcl (setf *default-pathname-defaults* (ccl::current-directory))

;; gcl initializes *default-pathname-defaults* to an empty pathname and does not
;; provide any extension to find the current or working directory. Other lisps
;; either initialize *default-pathname-defaults* to a useful pathname or provide
;; another facility to get the current directory.
;;
;; This can cause big headaches. I spent hours tracking down a bug resulting from
;; mext reinitializing *dpnd* only in gcl.
;; I found the form (truename ".") in the gcl source, where it was used to get
;; the current directory. It seems to work here with linux.
;; But, I go a bit further and take it from environment variable PWD, if it
;; exists. This probably makes no difference, but maybe it is more portable.
;; But *dpnd* should be initialized by *dpnd* only once, we have mext.lisp
;; check if the package :mext-maxima exists already.
;; I don't know why the eval-when is necessary, but it is only for the
;; form (si:getenv "PWD"), not (truename ".")
;; Need to add condition for win32 gcl.
;; (si:getenv "PWD") returns nil.  (si:getenv "HOME") returns
;; a string representing the user home folder in windows syntax, ie with
;; backslashes: "C:\\Users\\Tom\somedir"  pathnames and strings in gcl in win32 use unix-like forward
;; slashes.
;; but gcl/win32 pwd and chdir use "C:/Users/Tom/somedir"
#+gcl (eval-when (:compile-toplevel :load-toplevel :execute)
        (setf *default-pathname-defaults* (mext::pathname-as-directory 
                                           (or (si:getenv "PWD") (truename ".")))))

;; for chdir() to return to initial directory
(defvar *initial-default-pathname* *default-pathname-defaults*)

;; for use with chdir
(defvar *pwd-directory-stack* '())

;; for registering which mext distributions have been loaded.
(defvar *loaded-dist-table* (make-hash-table :test 'equal))

;; descriptions of distributions
(defvar *dist-descr-table* (make-hash-table :test 'equal))

; for use in directory names. We hope the result is a valid and painless directory name on all platforms.
(defparameter *maxima-and-lisp-version*
  (substitute #\- #\Space (remove #\) (remove #\( (concatenate 'string "v" maxima::*autoconf-version* "-" 
                               maxima::*maxima-lispname* "-v" (lisp-version-string))))))

;; This is a pathname representing a directory and represented as as a list.
;; for use with keyword :directory
(defparameter *mext-user-dir-as-list* (append (fpathname-directory
                 (pathname-as-directory (pathname maxima::*maxima-userdir*)))
          (list "mext" mext-maxima::*maxima-and-lisp-version*)))

;; note that trailing slash is present
;; For example: /home/joeuser/.maxima/mext/v5.28.0-gcl-vGCL2.6.7/
(defparameter *mext-user-dir-as-string* 
  (namestring (pathname-as-directory (fmake-pathname :directory mext-maxima::*mext-user-dir-as-list*))))

(defparameter maxima::$mext_userdir *mext-user-dir-as-string*)

;; this is taken from  init-cl.lisp
(defparameter *maxima-patterns* "$$$.{mac,mc}")
(defparameter *lisp-patterns* (concatenate 'string "$$$.{" *binary-ext* ",lisp,lsp}"))

(defparameter *maxima-exts* (list "mac" "mc"))
(defparameter *lisp-exts* (list *binary-ext* "lisp" "lsp"))
(defparameter *lisp-and-max-exts* (append *lisp-exts* *maxima-exts*))

(defparameter *search-path-maxima-mext-user*
  (concatenate 'string *mext-user-dir-as-string* *maxima-patterns*))

(defparameter *search-path-lisp-mext-user*
  (concatenate 'string *mext-user-dir-as-string* *lisp-patterns*))

(unless (boundp 'maxima::$mext_verbose)
(defparameter maxima::$mext_verbose 0))

(defun mext-message (level mssg)
  (when (or (eq t maxima::$mext_verbose)  
            (and (numberp maxima::$mext_verbose)
                 (>= maxima::$mext_verbose level)))
    (format t "~a~%" mssg)))

;(defun mext-message (mssg)
;  (when (> maxima::$mext_verbose 0)
;    (format t "~a~%" mssg)))

(defun find-mext-description (distname)
  "Find the .mxt file in the distribution directory. (not installed)"
  (fmake-pathname :name distname :type "mxt" :directory *dist-dir*))

(defun create-distribution (name &rest body-form)
  "Define a distribution. This is called at the top of a defsystem .system file.
   But, it is not a defsystem command, by rather in the :mext package."
  (setf *distname* name)
  (setf *dist-dir* (fpathname-directory (fload-pathname)))
  (let ((mxtfile (find-mext-description name)))
    (unless (probe-file mxtfile)
      (maxima::merror (intl:gettext "create-distribution: The distribution ~s is missing the description file
      ~s.~%") name mxtfile)))
  (setf *systems-to-compile*
        (if (getf body-form :dont-compile-distname) nil
          name))
  (setf *systems-to-load* name)
  (setf *systems-to-clean* name)
  (setf *systems-to-install* name)
  (setf *systems-to-install-to-mext-root*
        (getf body-form :loader))
  (setf *systems-to-install-other*
        (getf body-form :install-only)))

;; not finished
(defmacro defdistribution (name &rest definition-body)
  (unless (find :source-pathname definition-body)
    (setf definition-body
	  (list* :source-pathname
		 '(when (fload-pathname)
		        (make-pathname :name nil
			               :type nil
			               :defaults (fload-pathname)))
		 definition-body)))
 `(create-distribution ',name ,@definition-body))

;; return a list of all symbols in package named by
;; string 'package'
(defun list-symbols-in-package (package)
  (let (symbols)
    (do-external-symbols (s (find-package package))
                         (push s symbols))
    symbols))

(defun print-symbols-in-package (package)
  (format t "~{~a~% ~}" (list-symbols-in-package package)))

;; add string to list of strings if it is not already present
(defmacro add-dir-to-file-search (to-add dir-list)
  `(unless (maxima::$member ,to-add ,dir-list)
    (setf ,dir-list
          (maxima::mk-mlist (cons ,to-add (cdr ,dir-list))))))

;(add-dir-to-file-search *search-path-maxima-mext-user* maxima::$file_search_maxima)
;(add-dir-to-file-search *search-path-lisp-mext-user* maxima::$file_search_lisp)


(defvar *maxima-contribdir* (merge-pathnames "contrib" 
                  (pathname-as-directory maxima::*maxima-sharedir*)))

(defun subdir-of-shared (subdir)
 "Return pathname of a directory under the maxima share directory."
  (merge-pathnames subdir (mext:pathname-as-directory maxima::*maxima-sharedir*)))

(defun subdir-of-contrib (subdir)
 "Return pathname of a directory under the maxima contrib directory."
  (merge-pathnames subdir (mext:pathname-as-directory *maxima-contribdir*)))

;; use defaults here as well ?
;; clisp part is hacked in, it will raise error if a directory is given.
;; bug file-exists-p is broken for clisp
;; Search for name with various exts in paths.
;; name -- string
;; exts -- list of possible extensions
;; paths -- a  list of directories each in list form
;;
(defun file-search (name exts paths &aux file )
  (loop for ext in exts while (not file) do
        (loop for path in paths while (not file) do
              (let ((trial (fmake-pathname :name name :type ext :directory path)))
                (setf file (#-(or clisp gcl) file-exists-p #+(or clisp gcl) probe-file trial)))))
  file)

(defun mext-file-search (name)
 "Find lisp or mac file in mext installation directories (only user now)."
  (file-search name *lisp-and-max-exts* (list *mext-user-dir-as-list*)))

(defun mext-mxt-file-search (name)
 "Find the .mxt file in the installation directory of a mext distribution."
  (file-search name (list "mxt") (list (append *mext-user-dir-as-list* (list name)))))

;;; Following functions are called from defsystem operate-on-component, etc.

(defun find-trial-source-full-pathname (cfpn)
  (let* ((sdir  *dist-dir*)
         (trial-source-full-pathname 
          (merge-pathnames cfpn
    (pathname-as-directory (fmake-pathname :directory sdir)))))
;      (format t "!!!!! sdir ~s~%" sdir)
;      (format t "!!!!! tsfp ~s~%" trial-source-full-pathname)
    trial-source-full-pathname))

(defun find-source-full-pathname (tsfpn)
  (let  ((source-full-pathname (probe-file tsfpn)))
;      (format t "!!!!! sfpn ~s~%" source-full-pathname)
    (unless source-full-pathname
      (maxima::merror (intl:gettext "mext-install: file in the source distribution ~s does not exist.~%")
                              tsfpn))
    source-full-pathname))

(defun find-install-dir (target-info)
  (format t "finding install dir: sytem name is ~s~%" *system-name*)
  (let ((install-dir 
         (if (getf target-info :mext-root) *mext-user-dir-as-list*
           (append *mext-user-dir-as-list* 
                   (or (let ((res (getf target-info :inst-dir)))
                         (if (null res) nil 
                           (if (listp res) res (list res))))
                       (list *system-name*))))))
    install-dir))

(defun find-target-full-pathname (cfpn install-dir source-full-pathname)
  (let ((target-full-pathname 
     (if (eq :relative (car (fpathname-directory cfpn)))
         (merge-pathnames cfpn (fmake-pathname :directory install-dir :defaults *default-pathname-defaults*))
       (change-root-pathname source-full-pathname 
                                  (directory-namestring source-full-pathname)
                                  (fmake-pathname :directory install-dir :defaults *default-pathname-defaults*)))))
    target-full-pathname))

(defun install-source-to-target (sfpn tfpn)
  (when (equal sfpn tfpn)
    (maxima::merror (intl:gettext
    "mext-install: Bug. Source and pathname are the same: ~s") sfpn))
  (fensure-directories-exist tfpn)
  (format t "mext-install: copying ~s to ~s~%" sfpn tfpn)
  (copy-file sfpn tfpn :overwrite t))

(defun install-file (pathname target-info)
  (let* ((install-dir (find-install-dir target-info))
         (trial-source-full-pathname (find-trial-source-full-pathname pathname))
         (source-full-pathname (find-source-full-pathname trial-source-full-pathname))
         (target-full-pathname (find-target-full-pathname pathname install-dir source-full-pathname)))
;    (format t "!!!!! pathname ~s~%" pathname)
;    (format t "!!!!! target-info ~s~%" target-info)
;    (format t "!!!!! install-dir ~s~%" install-dir)
;    (format t "!!!!! trial-source-full-pathname ~s~%" trial-source-full-pathname)
;    (format t "!!!!! source-full-pathname)~s~%" source-full-pathname)
;    (format t "!!!!! target-full-pathname ~s~%" target-full-pathname)
    (install-source-to-target source-full-pathname target-full-pathname)))

(defun list-distribution-dirs ()
  "Return list of pathnames to directories of installed distributions."
  (loop for name in (list-directory *mext-user-dir-as-string*) 
        when (directory-pathname-p name) collect it))

(defun list-installed-distributions ()
 "Return list of names of installed distributions. (names are the same as the installation directory
   name."
  (loop for dir in (list-distribution-dirs)
        collect (car (last (fpathname-directory dir)))))

(defun list-loaded-distributions ()
  (get-hash-keys mext-maxima::*loaded-dist-table*))

(defun scan-installed-distributions ()
"Read the .mxt files from all installed distributions. This
 rebuilds the data-base of installed .mxt packages. (packages
 or distributions ?"
  (loop for name in (list-installed-distributions) do
        (let ((file (mext-mxt-file-search name)))
          (when file (load file))))
  t)

(defun install-mext-description (distname)
 "Copy the .mxt file from the distribution to the installation directory."
  (let ((mxt-file (find-mext-description distname))
        (*system-name* distname))
    (install-file mxt-file nil)))

(defun distribution-description (&rest descr)
 "Record the distribtuion information in the table of distribution
 descriptions. This is called in a .mxt file."
  (let ((name (getf descr :name)))
      (setf (gethash (maxima::$sconcat name) *dist-descr-table*) descr)))

(defun print-dist-info-record (info txt key)
  (let ((record (getf info key)))
    (if record
        (format t " ~a: ~a~%" txt record)
      nil)))

(defun print-dist-info (info)
  (print-dist-info-record info "Name" :name)
  (print-dist-info-record info "Short Description" :description)
  (print-dist-info-record info "Description" :long-description)
  (print-dist-info-record info "Version" :version)
  (print-dist-info-record info "Author" :author)
  (print-dist-info-record info "License" :license)
  (print-dist-info-record info "Maintainer" :maintainer)
  (print-dist-info-record info "URL" :url))

(defun pwd ()
  (namestring *default-pathname-defaults*))

;; we should call this in chdir
(defun dir-rel-or-abs (dir)
  (let* ((dir (fpathname-directory (pathname-as-directory dir))))
    (pathname-as-directory 
     (fmake-pathname :directory (if (eq (car dir) :absolute) dir
                                  (append
                                   (fpathname-directory *default-pathname-defaults*)
                                   (cdr dir)))))))

(defun mext-dir-exists-p (dir)
  (directory-exists-p (dir-rel-or-abs dir)))

(defun chdir ( &key dir (push t) )
  (let ((curdir *default-pathname-defaults*))
    (cond ((null dir)
           (setf *default-pathname-defaults* *initial-default-pathname*)
           #+clisp (ext:cd *default-pathname-defaults*)  ; necessary
           #+cmu (setf (ext:default-directory) *default-pathname-defaults*)
           #+openmcl (setf (ccl::current-directory) *default-pathname-defaults*)
           (when push (push curdir *pwd-directory-stack*))
	   (namestring *default-pathname-defaults*))
          (t 
           ;; (let* ((dir (fpathname-directory (pathname-as-directory dir)))
           ;;        (fdir 
           ;;         (pathname-as-directory 
           ;;          (fmake-pathname :directory (if (eq (car dir) :absolute) dir
           ;;                (append
           ;;                 (fpathname-directory *default-pathname-defaults*)
           ;;                 (cdr dir)))))))
           (let ((fdir (dir-rel-or-abs dir)))
             (if (directory-exists-p fdir)
                 (progn (setf *default-pathname-defaults*
			      (truename fdir)) ; we want exception ?
;                              (mext:compact-pathname fdir))
                        #+clisp (ext:cd *default-pathname-defaults*)
                        #+cmu (setf (ext:default-directory) *default-pathname-defaults*)
                        #+openmcl (setf (ccl::current-directory) *default-pathname-defaults*)
                        (when push (push curdir *pwd-directory-stack*))
                        (namestring *default-pathname-defaults*))
                 nil))))))

(defun popdir (&optional (n 1))
  (chdir :dir 
         (loop :for i :from 1 :to n :do (pop *pwd-directory-stack*)) :push nil))

;; TODO need function to list rtests for a mext package

(defun mext-test  ( list-only &rest dists )
 "Run regression tests in the sub-directories of the installed distributions.
 dists is a list of names of distributions. If no argument is given,
 then look in the current directory for rtests. A single sym or string
 `all' or `loaded' have special meanings.
 rtest files must have form rtest*.mac"
 (when (length1p dists)
   (let ((d (car dists)))
     (cond ((string= "all" (maxima::$sconcat d))
            (setf dists (mext-list)))
           ((string= "loaded" (maxima::$sconcat d))
            (setf dists (list-loaded-distributions)))
           (t nil))))
  (let ((testdirs-dists ; locate the requested rtest subdirs of the mext installation dir.
         (cond (dists
                (loop :for dist :in dists :collect
                      (let* ((have-list (maxima::$listp dist))
                             (d (if have-list (cadr dist) dist))
                             (testdir 
                              (fmake-pathname :directory
                                              (append *mext-user-dir-as-list*
                                                      (list (maxima::$sconcat d) "rtests"))))
                             (test-spec (if have-list (cddr dist) nil)))
                        (unless list-only (maxima::$require d))
                        (list d testdir test-spec))))
;                          (list dist (fmake-pathname :directory
;                                            (append *mext-user-dir-as-list*
;                                                    (list (maxima::$sconcat dist) "rtests"))))))
               (t  (list "rtests")))))
;    (format t "Testdirs-dists ~s~%" testdirs-dists)
    (let ((testdir-list))
      (loop :for testdir-dist :in testdirs-dists :do
            (let* ( ; (dist (first testdir-dist))
                   (testdir (second testdir-dist))
                   (test-spec (third testdir-dist))
                   (inlist (list-directory testdir)))
;              (format t "dist: ~s~% testdir: ~s~% test-spec: ~s~% inist: ~s~%" dist testdir test-spec inlist)
              (loop :for file :in inlist :do
                    (let* ((pnfile (pathname-name file)) ; eg "rest_pack"
                           (posn (search "rtest" pnfile)))
;                      (format t "**pnfile: ~s~%" pnfile)
;                      (format t "**test-spec: ~s~%" test-spec)
                     ; inefficient list construction
                      (when (and (equal "mac" (pathname-type file)) (numberp posn) (= 0 posn)
                                 (or (not test-spec) (member pnfile test-spec :test #'string-equal)))
                          (setf testdir-list (cons 
                                              (if (eq list-only t)
                                                  pnfile (namestring file)) testdir-list)))))))
      (if list-only (maxima::$sort (maxima::mk-mlist testdir-list))
        (progn
          (setf maxima::$testsuite_files (maxima::mk-mlist testdir-list))
          (maxima::$run_testsuite))))))

#|
 Under construction: in rtests/ directory we want  rtests.cfg with code like:
 mext_rtest_config( "packname", [ "rtest_file1", 34, 76 ], ... );
 This will be searched for by mext-test above.

(defun set-rtests ( specs )
  (dolist (spec (maxima::ensure-lisp-list specs))
    (let ((dist (car spec))
          (t-specs (cdr spec)))
      (dolist (t-spec (maxima::ensure-lisp-list t-specs))

|#      

(defun updir (&optional (n 1))
  (if (and (numberp n) (> n 0))
      (chdir :dir (namestring (fmake-pathname :directory 
            (cons :relative (make-list n :initial-element :up ))
                                         :name nil :type nil :defaults *default-pathname-defaults*))
             :push t)
    (maxima::merror (intl:gettext "Invalid number to updir."))))

(defun maxima-list-directory ( &optional dirname)
 "A directory list function that is meant to be called from maxima."
  (maxima::mk-mlist
        (list-directory (if dirname dirname *default-pathname-defaults*))))

(defun mext-info (distname)
  "Print the description of a distribution."
  (let* ((sname (maxima::$sconcat distname))
         (info (gethash sname *dist-descr-table*)))
      (if info (progn (print-dist-info info) 'maxima::$done)
        (progn
          (scan-installed-distributions)
          (let ((info1 (gethash sname *dist-descr-table*)))
            (if info1 (progn (print-dist-info info1) 'maxima::$done)
              nil))))))

(defun mext-list ()
  "List installed distributions."
  (scan-installed-distributions)
  (list-installed-distributions))

(defun maxima-list-loaded-distributions ()
  (maxima::$sort (maxima::mk-mlist (list-loaded-distributions))))

(defun mext-clear ()
 "Clear list of loaded mext distributions."
 (clrhash *loaded-dist-table*))

(defun mext-require (pack-name-list &optional force)
  (let ((maxima::$file_search_lisp (copy-list maxima::$file_search_lisp))
        (maxima::$file_search_maxima (copy-list maxima::$file_search_maxima)))
    (add-dir-to-file-search *search-path-maxima-mext-user* maxima::$file_search_maxima)
    (add-dir-to-file-search *search-path-lisp-mext-user* maxima::$file_search_lisp)
    (dolist (pack-name (maxima::ensure-lisp-list pack-name-list))
      (setf pack-name (maxima::$sconcat pack-name))
      (if (string= "all" pack-name)
          (progn (loop :for dist :in (mext-list) :do
                       (mext-require dist))
                 'maxima::$done)
        (if (string= "mext_system" pack-name) t
          (let ((is-loaded (gethash pack-name *loaded-dist-table*)))
            (if (or (not is-loaded) force)
                (let ((file (mext-file-search pack-name)))
                  (if file (progn (mext-message 1 (format nil "Require loading ~a" file))
                                  (maxima::$load file) 'maxima::$done)
                    (progn
                      (setf maxima::$error_code 'maxima::$mext_package_loader_not_found) ; not defined yet
                      (maxima::merror
                       (intl:gettext "mext require: Unable to find file to load package '~a'.")  pack-name))))
              t)))))
    'maxima::$done))

(defun add-to-dont-kill (&rest items )                         
  (loop :for item :in items :do
        (when (not (member item maxima::allbutl)) (push item maxima::allbutl))))

(defun remove-from-dont-kill (&rest items )
  (setf maxima::allbutl 
        (remove-if #'(lambda (e) (member e items)) maxima::allbutl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :maxima)

(defparameter $lispname *maxima-lispname*)

(defmacro mk-mext-operation (name default-system body)
  `(defmfun ,name ( &optional distnames )
     (unless distnames (setf distnames ,default-system))
     (unless (listp distnames) (setf distnames (list distnames)))
     ,body
     '$done))

(defmacro mk-mext-all-operation (name default-system &rest body)
  `(mk-mext-operation ,name ,default-system
     (loop :for distname :in distnames :do
           (let ((mext-maxima::*system-name* distname))
             ,@body))))

(mk-mext-all-operation $mext_dist_compile mext-maxima::*systems-to-compile*
       (setf mk::*bother-user-if-no-binary* nil)
       (mk:oos2 distname :compile))

(mk-mext-all-operation $mext_dist_load mext-maxima::*systems-to-load*
       (setf mk::*bother-user-if-no-binary* nil)
       (setf make::*load-source-if-no-binary* t)
       (mk:oos2 distname :load)
       (setf make::*load-source-if-no-binary* nil))

; this operation does not exist
;(mk-mext-all-operation $mext_dist_compile_and_load mext-maxima::*systems-to-load*
;       (setf mk::*bother-user-if-no-binary* nil)
;       (setf make::*load-source-if-no-binary* t)
;       (mk:oos2 distname :compile-and-load)
;       (setf make::*load-source-if-no-binary* nil))

(mk-mext-all-operation $mext_dist_clean mext-maxima::*systems-to-clean*
     (mk:oos2 distname :mext-clean-lisp-compilation)
     (mk:oos2 distname :mext-clean-intermediate))

(mk-mext-all-operation $mext_dist_user_install_pref_binary mext-maxima::*systems-to-install*
      (mk:oos2 distname :mext-user-install-pref-binary))

(mk-mext-all-operation $mext_dist_user_install_binary mext-maxima::*systems-to-install*
    (mk:oos2 distname :mext-user-install-binary))

(mk-mext-all-operation $mext_dist_user_install_source mext-maxima::*systems-to-install*
     (mk:oos2 distname :mext-user-install-source))

;; these are arbitrary non-code fileds; not really source files
;; In the system definition, give eg, :source-extension "txt",
;; and include this system in *systems-to-install-other*
(mk-mext-all-operation $mext_dist_user_install_other mext::*systems-to-install-other*
     (mk:oos2 distname :mext-user-install-source :data (list :inst-dir mext::*distname*)))

(mk-mext-all-operation $mext_dist_user_install_mext_root mext::*systems-to-install-to-mext-root*
     (mk:oos2 distname :mext-user-install-source :data '( :mext-root t )))

(defmfun $mext_dist_user_install_additional ()
  (format t "installing additional files.~%")
  (loop :for copy-spec :in mext-maxima::*user-install-explicit* :do
        (apply 'mext-maxima::copy-file-all-components copy-spec))
  (format t "executing post install hooks.~%")
  (loop :for func :in mext-maxima::*post-user-install-hooks* :do
        (funcall func))
  t)

;; return name of current distribution.
(defmfun $mext_distname ()
  mext::*distname*)

(defmfun $mext_dist_user_install_remaining (&optional distnames)
  (mext::install-mext-description ($mext_distname))
  ($mext_dist_user_install_other distnames)
  ($mext_dist_user_install_mext_root distnames)
  ($mext_dist_user_install_additional))

;; This installs binaries of each component, or source if load-only was true.
;; It also installs things through special hooks.
;; To install source as well, call as well $mext_dist_user_install_source.
(defmfun $mext_dist_user_install (&optional distnames)
  ($mext_dist_user_install_pref_binary distnames)
  ($mext_dist_user_install_remaining distnames))

;; This installs everything, but with source, rather than binary
;; note the similar name of the first function called.
(defmfun $mext_dist_user_source_install (&optional distnames)
  ($mext_dist_user_install_remaining distnames)
  ($mext_dist_user_install_source distnames))

;; Added enough eval-when's etc to the packages,
;; to now compile before load. I hope this
;; means that lisp files are not loaded
;; unnecessarily.
#-gcl (defmfun $mext_dist_build (&optional distnames)
  ($mext_dist_clean distnames)
  ($mext_dist_compile distnames)
  ($mext_dist_load distnames))

;; eval-when s don't do the trick with gcl. we must load first.
#+gcl (defmfun $mext_dist_build (&optional distnames)
  ($mext_dist_clean distnames)
  ($mext_dist_load distnames)
  ($mext_dist_compile distnames))

(defmfun $mext_dist_user_all (&optional distnames)
  ($mext_dist_build distnames)
  ($mext_dist_user_install distnames))

;; load file in a subdir dir, or use *default-pathname-defaults*
(defmfun $load_in_dsubdir (file &optional dir)
  (when (and dir ($listp dir)) (setf dir (cdr dir)))
  (let* ((abs-dir (mext:fpathname-directory *default-pathname-defaults*))
         (new-dir (if dir (append abs-dir dir) abs-dir)))
    (when ($listp file) (setf file (cdr file)))
    (mext-maxima::ensure-list file)
    (when (and file (find #\. (car file)))
        (format t "load_in_subdir: warning: dot in filename that should have no extension.~%"))
    (let ((nfile (length file)))
      (cond ((= 1 nfile)
             ($load (namestring (mext:fmake-pathname :name (first file) :directory new-dir))))
            ((= 2 nfile)
             ($load (namestring (mext:fmake-pathname :name (first file) :type (second file) :directory new-dir))))
            (t
             (merror 
              (intl:gettext "File specification ~a is neither a string nor a list of one or two strings.~%")))))))

;; Maxima 5.30 has a bug that prevents $load_pathname from being set with 'maxima -b',
;; so tests fail in this case. No! it's the reverse. It is set in 5.30, but not in 5.28
;; Still need to sort this out.
;; Use load_pathname if defined, else *default-pathname-defaults*
(defmfun $load_in_subdir (file &optional dir)
  (when (and dir ($listp dir)) (setf dir (cdr dir)))
;  (format t "load_in_subdir : load_pathname : `~a'~%" $load_pathname)
  (let* ((abs-dir (mext:fpathname-directory
                   (if $load_pathname $load_pathname *default-pathname-defaults*)))
         (new-dir (if dir (append abs-dir dir) abs-dir)))
;    (format t "abs-dir `~s', new-dir `~s'~%" abs-dir new-dir)
;    (format t "def-pn-def `~s'~%" *default-pathname-defaults*)
    (when ($listp file) (setf file (cdr file)))
    (mext-maxima::ensure-list file)
    (when (and file (find #\. (car file)))
      (format t "load_in_subdir: warning: dot in filename that should have no extension.~%"))
    (let ((nfile (length file)))
      (cond ((= 1 nfile)
             ($load (namestring (mext:fmake-pathname :name (first file) :directory new-dir))))
            ((= 2 nfile)
             ($load (namestring (mext:fmake-pathname :name (first file) 
                                                     :type (second file) :directory new-dir))))
            (t
             (merror (intl:gettext 
                      "File specification ~a is neither a string nor a list of one or two strings.~%")))))))

(defmfun $load_files_in_subdir (files &optional dir)
  (unless ($listp files)
    (merror (format nil "List of files ~a is not a list." files)))
  (when (and dir ($listp dir)) (setf dir (cdr dir)))
  (let* ((abs-dir (mext:fpathname-directory
                         (if $load_pathname $load_pathname *default-pathname-defaults*)))
         (new-dir (if dir (append abs-dir dir) abs-dir)))
    (setf files (cdr files))
    (loop :for file :in files :do
          (when ($listp file) (setf file (cdr file)))
          (mext-maxima::ensure-list file)
          (when (and file (find #\. (car file)))
              (format t "load_files_in_subdir: warning: dot in filename that should have no extension.~%"))
          (let* ((nfile (length file))
                 (lfile
                  (cond ((= 1 nfile)
                         (namestring (mext:fmake-pathname :name (first file) :directory new-dir)))
                        ((= 2 nfile)
                         (namestring (mext:fmake-pathname
                                       :name (first file) :type (second file) :directory new-dir)))
                        (t
                         (merror "File specification ~a is neither a string nor a list of one or two strings.~%")))))
            (mext::mext-message 2 (format nil " loading ~a" lfile))
            ($load lfile)))))

;; satisfy a mext_require call
;; We could use verbose or debugging option
(defmfun $mext_provide (name files &optional dir)
  (let ((dir (if dir dir (mk-mlist (list ($sconcat name))))))
    ($load_files_in_subdir (list '(mlist simp) (list '(mlist simp) name "mxt")) dir)
    ($load_files_in_subdir files dir)
    ($mext_provided name)))

;; load a mext package 'name', only if it has not already been loaded in the current
;; maxima session. force forces a reload, but not of packages that name depends
;; on.  'name' is a symbol or a string. A file is searched for and loaded in the mext
;; installation dirs with this name and one of a list of extensions:
;; The binary lisp extension, lisp,lsp,mac, or mc. A file or with no extension
;; does not match.
;; 
;; I tried using the maxima file_search routine, but it matched directories
;; even with a path list supplied, while we only want to match files.

#|
(defmfun $mext_require (name &optional force)
  (setf name ($sconcat name))
  (let ((is-loaded (gethash name mext-maxima::*loaded-dist-table*)))
    (if (or (not is-loaded) force)
        (let ((file (mext-maxima::mext-file-search name)))
          (if file (progn (format t "loading ~a~%" file) ($load file))
            (merror "mext require: Unable to find '~a'." name)))
      t)))

|#

(defmfun $require (name &optional force)
  (mext:mext-require name force))

;; declare that a mext package has been loaded. mext_require will see
;; that it has been loaded
;; This should only be called from mext_provide
(defmfun $mext_provided (name)
  (setf (gethash ($sconcat name) mext-maxima::*loaded-dist-table*) t))

(defmfun $pwd ()
  (mext::pwd))

;; change the default directory for lisp *default-pathname-defaults*
(defmfun $chdir (&optional dir)
  (mext::chdir :dir dir :push t ))

(defmfun $popdir ( &optional (n 1) )
  (mext::popdir n))

(defmfun $dirstack ()
  (mk-mlist mext::*pwd-directory-stack*))

;; chdir up n parent dirs.
;; This is probably not too portable. But it is better to use this than to
;; have '..' in user code.
(defmfun $updir (&optional (n 1))
  (mext::updir n))

;; run rtests. If dists is nil then look only in dir "rtests"
;; Otherwise dists is a symbol or string naming a  mext dist,
;; or a list of these. We search for a folder 'rtests' in the
;; installation directory of each dist, then for files rtest*.mac
;; in this folder. We set testsuite_files and run run_testsuite.
(defmfun $mext_test  ( &rest dists )
  (apply #'mext::mext-test (cons nil dists)))

(defmfun $truename (filespec)
  (namestring (truename filespec)))

(defmfun $probe_file (filespec)
  (let ((res (probe-file filespec)))
    (if res (namestring res) nil)))

(defmfun $mkdir (filespec &optional (mode "0770"))
  (mext::mkdir filespec mode))

(defmfun $mext_info ( distname )
  (or (mext::mext-info distname)
        (merror (intl:gettext "mext_info: Unknown distribtuion '~a'.~%") 
                ($sconcat distname))))

;; list installed distributions
(defmfun $mext_list ()
  (mk-mlist (mext::mext-list)))

(defmfun $mext_clear ()
  (mext::mext-clear))

($mext_provided "mext_system")
