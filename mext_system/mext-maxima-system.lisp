(in-package :mext-maxima)
;(declaim (optimize (speed 3) (space 0) (safety 0) #-gcl (debug 0)))

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


;; extensions of filenames created during building that we want to remove
;; (clean). defsystem does some of this, but not all.
(defparameter *extensions-to-clean* (list
                                     *binary-ext*
                                     #+sbcl "data"
                                     #+cmu  "err"
                                     #+clisp "lib"))

;; name of the distribution. this is the top level name. similar to
;;  a linux package name
(defvar *dist-name* nil)
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
(defvar *dist-dir* (fpathname-directory
          #-gcl *load-pathname* #+gcl sys:*load-pathname* ))
(defvar *userdir-pathname* maxima::*maxima-userdir*)

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
#+gcl (eval-when (:compile-toplevel :load-toplevel :execute)
        (setf *default-pathname-defaults* (mext::pathname-as-directory 
                                           (or (si:getenv "PWD") (truename ".")))))

;; for chdir() to return to initial directory
(defvar *initial-default-pathname* *default-pathname-defaults*)

;; for registering which mext distributions have been loaded.
(defvar *installed-dist-table* (make-hash-table :test 'equal))

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
  (namestring (fmake-pathname :directory mext-maxima::*mext-user-dir-as-list*)))

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

(defun create-distribution (name &rest body-form)
  (setf *dist-name* name)
  (setf *dist-dir* (fpathname-directory
          #-gcl *load-pathname* #+gcl sys:*load-pathname* ))
  (setf *systems-to-compile*
        (if (getf body-form :no-compile-dist-name) nil
          name))
  (setf *systems-to-load* name)
;        (if (getf body-form :no-load-name) nil
;            name))
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
		 '(when #-gcl *load-pathname* #+gcl si::*load-pathname*
		        (make-pathname :name nil
			               :type nil
			               :defaults #-gcl *load-pathname* 
			                         #+gcl si::*load-pathname*))
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
          (cons '(maxima::mlist) (cons ,to-add (cdr ,dir-list))))))

(add-dir-to-file-search *search-path-maxima-mext-user* maxima::$file_search_maxima)
(add-dir-to-file-search *search-path-lisp-mext-user* maxima::$file_search_lisp)

;; use defaults here as well ?
;; clisp part is hacked in, it will raise error if a directory is given.
;; bug file-exists-p is broken for clisp
(defun file-search (name exts paths &aux file )
;  (format t "got: name:~a exts:~a paths ~s~%" name exts paths)  
  (loop for ext in exts while (not file) do
        (loop for path in paths while (not file) do
;              (format t "trying: name:~a ext:~a path ~s~%" name ext path)
              (let ((trial (fmake-pathname :name name :type ext :directory path)))
;                (format t " trial ~a~%" trial)
                (setf file (#-(or clisp gcl) file-exists-p #+(or clisp gcl) probe-file trial)))))
  file)

;; find lisp or mac file in mext installation directories (only user now)
(defun mext-file-search (name)
  (file-search name *lisp-and-max-exts* (list *mext-user-dir-as-list*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :maxima)

(defparameter $lispname *maxima-lispname*)

(defmacro mk-mext-operation (name default-system body)
  `(defmfun ,name ( &optional dist-names )
     (unless dist-names (setf dist-names ,default-system))
     (unless (listp dist-names) (setf dist-names (list dist-names)))
     ,body
     '$done))

(defmacro mk-mext-all-operation (name default-system &rest body)
  `(mk-mext-operation ,name ,default-system
     (loop for dist-name in dist-names do
           (let ((mext-maxima::*system-name* dist-name))
             ,@body))))

(mk-mext-all-operation $mext_dist_compile mext-maxima::*systems-to-compile*
       (mk:oos2 dist-name :compile))

(mk-mext-all-operation $mext_dist_load mext-maxima::*systems-to-load*
       (setf make::*load-source-if-no-binary* t)
       (mk:oos2 dist-name :load)
       (setf make::*load-source-if-no-binary* nil))

(mk-mext-all-operation $mext_dist_clean mext-maxima::*systems-to-clean*
     (mk:oos2 dist-name :mext-clean-lisp-compilation)
     (mk:oos2 dist-name :mext-clean-intermediate))

(mk-mext-all-operation $mext_dist_user_install_pref_binary mext-maxima::*systems-to-install*
      (mk:oos2 dist-name :mext-user-install-pref-binary))

(mk-mext-all-operation $mext_dist_user_install_binary mext-maxima::*systems-to-install*
    (mk:oos2 dist-name :mext-user-install-binary))

(mk-mext-all-operation $mext_dist_user_install_source mext-maxima::*systems-to-install*
     (mk:oos2 dist-name :mext-user-install-source))

;; these are arbitrary non-code fileds; not really source files
;; In the system definition, give eg, :source-extension "txt",
;; and include this system in *systems-to-install-other*
(mk-mext-all-operation $mext_dist_user_install_other mext::*systems-to-install-other*
     (mk:oos2 dist-name :mext-user-install-source :data (list :inst-dir mext::*dist-name*)))

(mk-mext-all-operation $mext_dist_user_install_mext_root mext::*systems-to-install-to-mext-root*
     (mk:oos2 dist-name :mext-user-install-source :data '( :mext-root t )))

(defmfun $mext_dist_user_install_additional ()
  (format t "installing additional files.~%")
  (loop for copy-spec in mext-maxima::*user-install-explicit* do
        (apply 'mext-maxima::copy-file-all-components copy-spec))
  (format t "executing post install hooks.~%")
  (loop for func in mext-maxima::*post-user-install-hooks* do
        (funcall func))
  t)

;; This installs binaries of each component, or source if load-only was true.
;; It also installs things through special hooks.
;; To install source as well, call as well $mext_dist_user_install_source.
(defmfun $mext_dist_user_install (&optional dist-names)
  ($mext_dist_user_install_pref_binary dist-names)
  ($mext_dist_user_install_other dist-names)
  ($mext_dist_user_install_mext_root dist-names)
  ($mext_dist_user_install_additional))

(defmfun $mext_dist_build (&optional dist-names)
  ($mext_dist_clean dist-names)
  ($mext_dist_load dist-names)
  ($mext_dist_compile dist-names))

(defmfun $mext_dist_user_all (&optional dist-names)
  ($mext_dist_build dist-names)
  ($mext_dist_user_install dist-names))

;; load in a subdir dir, or use *default-pathname-defaults*
(defmfun $load_in_dsubdir (file &optional dir)
  (if (and dir ($listp dir)) (setf dir (cdr dir)))
  (let* ((abs-dir (mext:fpathname-directory *default-pathname-defaults*))
         (new-dir (if dir (append abs-dir dir) abs-dir)))
    (if ($listp file) (setf file (cdr file)))
    (mext-maxima::ensure-list file)
    (if (and file (find #\. (car file)))
        (format t "load_in_subdir: warning: dot in filename that should have no extension.~%"))
    (let ((nfile (length file)))
      (cond ((= 1 nfile)
             ($load (namestring (mext:fmake-pathname :name (first file) :directory new-dir))))
            ((= 2 nfile)
             ($load (namestring (mext:fmake-pathname :name (first file) :type (second file) :directory new-dir))))
            (t
             (merror "File specification ~a is neither a string nor a list of one or two strings.~%"))))))

;; use load_pathname if defined, else *default-pathname-defaults*
(defmfun $load_in_subdir (file &optional dir)
  (if (and dir ($listp dir)) (setf dir (cdr dir)))
  (let* ((abs-dir (mext:fpathname-directory
                   (if $load_pathname $load_pathname *default-pathname-defaults*)))
         (new-dir (if dir (append abs-dir dir) abs-dir)))
    (if ($listp file) (setf file (cdr file)))
    (mext-maxima::ensure-list file)
    (if (and file (find #\. (car file)))
        (format t "load_in_subdir: warning: dot in filename that should have no extension.~%"))
    (let ((nfile (length file)))
      (cond ((= 1 nfile)
             ($load (namestring (mext:fmake-pathname :name (first file) :directory new-dir))))
            ((= 2 nfile)
             ($load (namestring (mext:fmake-pathname :name (first file) :type (second file) :directory new-dir))))
            (t
             (merror "File specification ~a is neither a string nor a list of one or two strings.~%"))))))
  

(defmfun $load_files_in_subdir (files &optional dir)
  (unless ($listp files)
    (merror (format nil "List of files ~a is not a list." files)))
  (if (and dir ($listp dir)) (setf dir (cdr dir)))
  (let* ((abs-dir (mext:fpathname-directory
                         (if $load_pathname $load_pathname *default-pathname-defaults*)))
         (new-dir (if dir (append abs-dir dir) abs-dir)))
    (setf files (cdr files))
    (loop for file in files do
          (if ($listp file) (setf file (cdr file)))
          (mext-maxima::ensure-list file)
          (if (and file (find #\. (car file)))
              (format t "load_files_in_subdir: warning: dot in filename that should have no extension.~%"))
          (let ((nfile (length file)))
            (cond ((= 1 nfile)
             ($load (namestring (mext:fmake-pathname :name (first file) :directory new-dir))))
                  ((= 2 nfile)
            ($load (namestring (mext:fmake-pathname
                                      :name (first file) :type (second file) :directory new-dir))))
                  (t
                   (merror "File specification ~a is neither a string nor a list of one or two strings.~%")))))))

;; satisfy a mext_require call
(defmfun $mext_provide (name files &optional dir)
  (let ((dir (if dir dir (cons '(mlist simp) (list ($sconcat name))))))
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
(defmfun $mext_require (name &optional force)
  (setf name ($sconcat name))
  (let ((registered (gethash name mext-maxima::*installed-dist-table*)))
    (if (or (not registered) force)
        (let ((file (mext-maxima::mext-file-search name)))
          (if file (progn (format t "loading ~a~%" file) ($load file))
            (merror "Unable to find '~a'." name)))
      t)))

;; identical. I want to switch to this name
(defmfun $require (name &optional force)
  (setf name ($sconcat name))
  (let ((registered (gethash name mext-maxima::*installed-dist-table*)))
    (if (or (not registered) force)
        (let ((file (mext-maxima::mext-file-search name)))
          (if file (progn (format t "loading ~a~%" file) ($load file))
            (merror "Unable to find '~a'." name)))
      t)))

;; declare that a mext package has been loaded. mext_require will see
;; that it has been loaded
(defmfun $mext_provided (name)
  (setf (gethash ($sconcat name) mext-maxima::*installed-dist-table*) t))

;; broken in gcl
;; returns absolute pathname
;; I want to use the word directory, because it is a
;; directory
(defmfun $pwd ()
  (namestring *default-pathname-defaults*))

;; change the default directory for lisp *default-pathname-defaults*
(defmfun $chdir (&rest dirs)
  (let ((len (length dirs)))
    (cond ((= len 0)
           (setf *default-pathname-defaults* mext-maxima::*initial-default-pathname*)
           #+clisp (ext:cd *default-pathname-defaults*)  ; necessary
           #+cmu (setf (ext:default-directory) *default-pathname-defaults*)
           #+openmcl (setf (ccl::current-directory) *default-pathname-defaults*))
          ((= len 1) 
           (let* ((dir (mext:fpathname-directory (mext:pathname-as-directory (car dirs))))
                  (fdir 
                   (mext:pathname-as-directory 
                    (mext:fmake-pathname :directory (if (eq (car dir) :absolute) dir
                          (append
                           (mext:fpathname-directory *default-pathname-defaults*)
                           (cdr dir)))))))
             (if (mext:directory-exists-p fdir)
                 (progn (setf *default-pathname-defaults*
                              (mext:compact-pathname fdir))
                        #+clisp (ext:cd *default-pathname-defaults*)
                        #+cmu (setf (ext:default-directory) *default-pathname-defaults*)
                        #+openmcl (setf (ccl::current-directory) *default-pathname-defaults*)
                        (namestring *default-pathname-defaults*))
               (merror "chdir: ~a: not a directory." fdir)))))))

;; chdir up n parent dirs.
;; This is probably not too portable. But it is better to use this than to
;; have '..' in user code.
(defmfun $updir (&optional (n 1))
  (if (and (numberp n) (> n 0))
      ($chdir (namestring (mext:fmake-pathname :directory 
            (cons :relative (make-list n :initial-element :up ))
                                         :name nil :type nil :defaults *default-pathname-defaults*)))
    (merror "Invalid number to updir.")))

(defmfun $list_directory ( &optional dirname)
  (cons '(mlist simp) (mext:list-directory (if dirname dirname *default-pathname-defaults*))))

;; run rtests. If dists is nil then look only in dir "rtests"
;; Otherwise dists is a symbol or string naming a  mext dist,
;; or a list of these. We search for a folder 'rtests' in the
;; installation directory of each dist, then for files rtest*.mac
;; in this folder. We set testsuite_files and run run_testsuite.
(defmfun $mext_test  ( &optional dists )
  (let ((testdirs
         (cond (dists
                (setf dists 
                      (if ($listp dists) (cdr dists)
                        (list ($sconcat dists))))
                (loop for dist in dists collect
                            (mext:fmake-pathname :directory
                                                (append mext::*mext-user-dir-as-list*
                                                        (list ($sconcat dist) "rtests")))))
               (t  (list "rtests")))))
    (let ((testdir-list))
      (loop for testdir in testdirs do 
            (let ((inlist (mext:list-directory testdir)))
              (loop for file in inlist do
                    (let ((posn (search "rtest" (pathname-name file))))
                      (if (and (equal "mac" (pathname-type file)) (numberp posn) (= 0 posn))
                          (setf testdir-list (cons (namestring file) testdir-list)))))))
      (setf $testsuite_files (cons '(mlist simp) testdir-list)))
    ($run_testsuite)))

(defmfun $truename (filespec)
  (namestring (truename filespec)))

(defmfun $probe_file (filespec)
  (let ((res (probe-file filespec)))
    (if res (namestring res) nil)))

(defmfun $mkdir (filespec &optional (mode "0770"))
  (mext::mkdir filespec mode))
