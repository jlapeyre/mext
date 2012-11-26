;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Portable pathname library, modified for our purpose.

;;; Copyright (c) 2012  John Lapeyre. All rights reserved.
;;;

;;; This software includes code from the cl-fad package

;;  copyright information from cl-fad
;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; end copyright information from cl-fad
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cl-fad does not support gcl, so we must change some things.
;; First, try to fix some missing or non-ansi-compliant gcl functions.
;; Sometimes, the ansi standard function 'funcname exists in gcl, but is not compliant.
;; In this case, we write a function 'gcl-funcname that is closer to ansi-compliant behavior.
;; In other cases, the ansi standard function 'funcname is not present in gcl,
;; In this case, we  write a function and call it 'funcname.
;;
;; Note that two or more non-compliant functions can interact to give compliant behavior.
;; For instance, suppose gcl has two standard functions 'funcname1 and 'funcname2, but
;; they are both non-compliant. You should use either 'funcname1 together with 'funcname2,
;; or 'gcl-funcname1 together with 'gcl-funcname2. But, do not mix them.

(in-package :mext-maxima)
;(declaim (optimize (speed 3) (space 0) (safety 0) #-gcl (debug 0)))

#-gcl  (defmacro fmake-pathname (&rest body)
        `(make-pathname ,@body))

#+gcl (defmacro fmake-pathname (&rest body)
        `(gcl-make-pathname ,@body))

#-gcl (defmacro fpathname-directory (&rest body)
        `(pathname-directory ,@body))

#+gcl (defmacro fpathname-directory (&rest body)
        `(gcl-pathname-directory ,@body))

#-gcl (defmacro fwild-pathname-p (&rest body)
        `(cl:wild-pathname-p ,@body))

#+gcl (defmacro fwild-pathname-p (&rest body)
        `(gcl-wild-pathname-p ,@body))

#-gcl (defmacro fensure-directories-exist (&rest body)
        `(cl:ensure-directories-exist ,@body))

#+gcl (defmacro fensure-directories-exist (&rest body)
        `(gcl-ensure-directories-exist ,@body))

#-gcl (defmacro fphysicalize-pathname (&rest body)
        `(physicalize-pathname ,@body))

#+gcl (defmacro fphysicalize-pathname (&rest body)
        `(gcl-physicalize-pathname ,@body))


#-gcl (defmacro fenough-namestring (&rest body)
        `(cl:enough-namestring ,@body))

#+gcl (defmacro fenough-namestring (&rest body)
        `(gcl-enough-namestring ,@body))

;; This should probably be done with truename ??
;; For pathnames like "/a/b/.." or "a/b/..",
;; The component :name is non-nil. Eg for sbcl, it is ".".
;; Here, we convert to a
;; pathname with :name equal nil and :up instead of '..'.
;;
;; fix-trailing-updir will fail if ".." is a valid filename.
;; But, in the year 2012, on mac OS X, *nix, and win32, this would
;;  be at best a pathological case.
;; Non-portable features:  1) we assume :up means parent dir in lisp
;;  2) we assume ".." means parent dir in platform.
;; Tested on:
;;   sbcl,linux
(defun fix-trailing-updir (pname)
  (if (stringp pname) (setf pname (parse-namestring pname)))  
  (let* ((posdir (fpathname-directory (pathname-as-directory pname)))
         (chk (car (last posdir))))
    (if (equal ".." chk)
        (let* ((new-dir (reverse (cons :up (cdr (reverse posdir))))))
          (fmake-pathname :name nil :type nil :directory new-dir :defaults pname))
      pname)))

;; probably can be done with truename ?? truename only works for files that exist.
;; compact-pathname: remove :up's and associated parent directories from
;; pathname.
;; pname must be a pathspec.
;; Returns a pathname object
;; Does not remove ".".
;; Does not rely any other implementation specific details:
;;   no '/', '..', etc.
;; Preserves all components of pathname
;; Probably not portable as written. :up is not standard
(defun compact-pathname (pname)
  (if (stringp pname) (setf pname (parse-namestring pname)))
  (setf pname (fix-trailing-updir pname))
  (let* ((dir (reverse (fpathname-directory pname)))
         (odir nil))
    (do ((dir dir (cdr dir)))
        ((null dir) odir)
      (let ((count 0))
        (loop while (eq :up (car dir)) do
              (incf count) (setf dir (cdr dir)))
        (if (> count 0) (setf dir (nthcdr count dir))))
      (setf odir (cons (car dir) odir)))
    (fmake-pathname :directory odir :defaults pname)))


;; This is from Peter Seibel's Practical Common Lisp 
;; chapter on Files and File I/O.
(defun change-root-pathname (full-source-pathname source-dir target-dir)
  (merge-pathnames
   (fenough-namestring full-source-pathname source-dir) target-dir))

;; copied from  sbcl. We have no good use for this now.
#-gcl(defun physicalize-pathname (possibly-logical-pathname)
  (if (typep possibly-logical-pathname 'logical-pathname)
      (translate-logical-pathname possibly-logical-pathname)
      possibly-logical-pathname))

#+gcl 
(defun gcl-physicalize-pathname (possibly-logical-pathname)
      possibly-logical-pathname)

;; built-in is not ansi compliant
#+gcl (defun gcl-make-pathname (&rest args)
        (let ((dir-spec (getf args :directory)))
          (if dir-spec
              (cond ( (stringp dir-spec)
                      (setf (getf args :directory) (list :root dir-spec))) ; fix this
                    ( (listp dir-spec)
                      (let ((word1 (car dir-spec)))
                        (cond ( (eq :absolute word1)
                                (setf dir-spec
                                      (cons :root (cdr dir-spec))))
                              ( (eq :relative word1)
                                (setf dir-spec
                                      (cdr dir-spec)))
                              (t
                       (error "List of directory components must start with :ABSOLUTE or :RELATIVE."))))
                      (let (odir)
                        (dolist (word dir-spec)
                          (push (if (eq word :up) :parent word) odir))
                        (setf dir-spec (nreverse odir)))
                      (setf (getf args :directory) dir-spec))))
        (apply 'cl::make-pathname args)))

;; built-in is not ansi compliant
;; To specify relative directory: gcl has nothing, ansi has :relative
;;            absolute directory: gcl has :root, ansi has :absolute
;;  parent directory: apparantly not specified by standard.b
;;  parent directory: gcl has :parent other implementations have :up
;;  We choose :up
#+gcl (defun gcl-pathname-directory (&rest args)
        (let ((dir-spec (apply 'cl::pathname-directory args)))
          (if (and (listp dir-spec) (not (null dir-spec)))
            (let ((word1 (car dir-spec)))
              (if (and (symbolp word1) (eq :ROOT word1))
                  (setf dir-spec (cons :ABSOLUTE (cdr dir-spec)))
                (setf dir-spec (cons :RELATIVE dir-spec)))))
          (let (odir)
            (dolist (word dir-spec)
              (push (if (eq word :parent) :up word) odir))
            (nreverse odir))))

;; This is not present at all in gcl.
;; This will be system dependent because we search for * in strings
;; This is supposed to be in ansi enabled gcl somehow, but I can't find it.
#+gcl (defun gcl-wild-pathname-p (pathname-in &optional field-key)
        (let ((pathname (pathname pathname-in)))
          (if (null field-key)
              (loop for key in '(:DIRECTORY :NAME :TYPE :VERSION :HOST :DEVICE) do
                    (let ((res (mext-maxima::pathname-component pathname key)))
                      (if (or (eq :wild res) (and (stringp res) (find #\* res))) (return t))))
            (let ((res (mext-maxima::pathname-component pathname field-key)))
              (or (eq :wild res) (and (stringp res) (find #\* res)))))))

;; The cl function 'enough-namestring' is broken in gcl. We write one that is un-broken
;; enough for our purposes. This is copied almost exactly from the clozure lisp source.
;; But, host and device are probably not treated correctly.
#+gcl (defun neq (a b)  ; could use flet here.
  (not (eq a b)))

#+gcl (defun gcl-enough-namestring (path
                          &optional
                          (defaults0 *default-pathname-defaults*))
        (if (null defaults0)
            (namestring path)
          (let* ((dir (fpathname-directory path))
                 (nam (pathname-name path))
                 (typ (pathname-type path))
                 (ver (pathname-version path))
                 (host (pathname-host path))
                 (device (pathname-device path))
                 (logical-p (neq host :unspecific))
                 (defaults (pathname-as-directory defaults0))
                 (default-dir (fpathname-directory defaults)))
            (cond ((equalp dir default-dir)
                   (setq dir '(:relative)))
                  ((and dir default-dir
                        (eq (car dir) :absolute) (eq (car default-dir) :absolute))
                   ;; maybe make it relative to defaults             
                   (do ((p1 (cdr dir) (cdr p1))
                        (p2 (cdr default-dir) (cdr p2)))
                       ((or (null p2) (null p1) (not (equalp (car p1) (car p2))))
                        (when (and (null p2) (or t (neq p1 (cdr dir))))
                          (setq dir (cons :relative p1)))))))
            (when (or (equalp ver (pathname-version defaults))
                      (not logical-p))
              (setq ver nil))
            (when (and (null ver) (equalp typ (pathname-type defaults)))
              (setq typ nil))
            (when (and (null typ) (equalp nam (pathname-name defaults)))
              (setq nam nil))
            (namestring (fmake-pathname :name nam :type typ :version ver 
                            :host host :device device :directory dir)))))

(defun mkdir (dir &optional (mode "0770"))
  (let ((adir (merge-pathnames dir *default-pathname-defaults*)))
    (setf adir (namestring adir))
    (let ((cmd (if (member :win32 *features*)
                   (format nil "mkdir \"~a\""
                           (coerce (subst #\\ #\/ (coerce adir 'list)) 'string))
                 (format nil "/bin/mkdir -m ~S ~S" mode adir))))
      (maxima::$system cmd)
      cmd)))

;; This is based on code from defsystem.lisp. Code by andrejv and rtoy.
#+gcl
(defun gcl-ensure-directories-exist (pathspec &key verbose)
 (declare (ignore verbose))
 ;; A very gross implementation of ensure-directories-exist.  Just
 ;; call /bin/mkdir with our desired path.
 (setf pathspec (merge-pathnames pathspec *default-pathname-defaults*))
 (let* ((dir (make-pathname :host (pathname-host pathspec)
                            :device (pathname-device pathspec)
                            :directory (pathname-directory pathspec)))
        (cmd (if (member :win32 *features*)
                 (format nil "mkdir \"~a\""
                         (coerce (subst #\\ #\/ (coerce (namestring dir) 'list)) 'string))
                 (format nil "/bin/mkdir -p ~S" (namestring dir)))))
   (unless (directory dir)
     (lisp:system cmd))
   ;; The second return value is supposed to be T if directories were
   ;; created.  I don't know how to tell that, so we just return T.
   ;; (Would NIL be better?)
   (values pathspec t)))

(defun native-namestring (pathname)
  (namestring pathname))

;; This does not work. I am not sure how much work it would take to fix it.
;; copied from sbcl source
#+gcl (defun test-ensure-directories-exist (pathspec &key verbose (mode "0777"))
  (let ((pathname (gcl-physicalize-pathname (merge-pathnames (pathname pathspec))))
        (created-p nil))
;    (format t "here ~a" pathspec)
    (when (fwild-pathname-p pathname)
      (error ;  'simple-file-error
             :format-control "bad place for a wild pathname"
             :pathname pathspec))
    (let* ((dir (fpathname-directory pathname))
           ;; *d-p-d* can have name and type components which would prevent
           ;; PROBE-FILE below from working
           (*default-pathname-defaults*
             (fmake-pathname :directory dir :device (pathname-device pathname))))
      (loop for i from 1 upto (length dir)
            do
            (let* ((newpath (fmake-pathname
                             :host (pathname-host pathname)
                             :device (pathname-device pathname)
                             :directory (subseq dir 0 i)))
                   (probed (probe-file newpath)))
              (unless (directory-pathname-p probed)
                (let ((namestring (coerce (native-namestring newpath)
                                          'string)))
                  (when verbose
                    (format *standard-output*
                            "~&creating directory: ~A~%"
                            namestring))
;                  (sb!unix:unix-mkdir namestring mode)
                  (mkdir namestring mode)
                  (unless (directory-pathname-p (probe-file newpath))
                    (restart-case
                        (error
                         ; 'simple-file-error
;                         :pathname pathspec
;                         :format-control
                         (if (and probed
                                  (not (directory-pathname-p probed)))
                             "Can't create directory ~A,~
                                 ~%a file with the same name already exists."
                             "Can't create directory ~A")
                         (list namestring))
                      (retry ()
                        :report "Retry directory creation."
                        (test-ensure-directories-exist
                         pathspec
                         :verbose verbose :mode mode))
                      (continue ()
                        :report
                        "Continue as if directory creation was successful."
                        nil)))
                  (setf created-p t)))))
      (values pathspec created-p))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Much of this was taken/modified from the  cl-fad library.
;;; But we added several things

;; helper function for gcl-wild-pathname-p
(defun pathname-component (pathname-in field-key)
        (let ((pathname (pathname pathname-in)))
          (cond ((eq field-key :name) (pathname-name pathname))
                ((eq field-key :directory) (#-gcl pathname-directory
                                            #+gcl gcl-pathname-directory pathname))
                ((eq field-key :type) (pathname-type pathname))
                ((eq field-key :host) (pathname-host pathname))
                ((eq field-key :device) (pathname-device pathname))
                ((eq field-key :version) (pathname-version pathname))
                (t (error (format nil "Pathname component ~a~% is not of type ~a" field-key
                                  '(:HOST :DEVICE :DIRECTORY :NAME :TYPE :VERSION)))))))

;; causes error in gcl, both windows and linux
;; gcl win32 device returns ("C:")
(defun print-pathname-components (pathname-in)
        (let ((pathname (pathname pathname-in)))
          (loop for component in 
                (list (list "name" 'pathname-name) (list "directory" 'fpathname-directory)
                      '("type" pathname-type) '("host" pathname-host) '("device" pathname-device)
                      '("version" pathname-version)) do
                (format t "~a : ~s~%" (car component) (funcall (cadr component) pathname))))
        t)

;; need to put in key overwrite
(defun copy-file-from-dir-to-dir (file ext source-dir target-dir)
;  (format t "copying file:~a, ext:~a, source:~a target:~a~%" file ext source-dir target-dir)
  (let ((source-file (fmake-pathname :name file :type ext :directory source-dir))
        (target-file (fmake-pathname :name file :type ext :directory target-dir)))
    (format t "Copying '~a' to '~a'~%" source-file target-file)
    (copy-file source-file target-file :overwrite t)))

(defun copy-file-all-components (source-file source-ext source-dir 
                                             target-file target-ext target-dir)
; next line needed by clisp maxima. and must not be present for sbcl,ecl,gcl.  we should look into a general approach.
#+clisp  (if (stringp target-dir) (setf target-dir (pathname-as-directory target-dir)))
;  (format t "sf ~s, se ~s, sd ~s, tf ~s, te ~s, td ~s~%" source-file source-ext source-dir 
;                                                         target-file target-ext target-dir)
  (let ((source-path (fmake-pathname :name source-file :type source-ext :directory source-dir))
        (target-path (fmake-pathname :name target-file :type target-ext :directory target-dir)))
    (format t "Copying '~a' to '~a'~%" source-path target-path)
    (copy-file source-path target-path :overwrite t)))

(defvar *stream-buffer-size* 8192)

(defun copy-stream (from to &optional (checkp t))
  "Copies into TO \(a stream) from FROM \(also a stream) until the end
of FROM is reached, in blocks of *stream-buffer-size*.  The streams
should have the same element type.  If CHECKP is true, the streams are
checked for compatibility of their types."
  (when checkp
    (unless (subtypep (stream-element-type to) (stream-element-type from))
      (error "Incompatible streams ~A and ~A." from to)))
  (let ((buf (make-array *stream-buffer-size*
                         :element-type (stream-element-type from))))
    (loop
       (let ((pos #-(or :clisp :cmu) (read-sequence buf from)
                  #+:clisp (ext:read-byte-sequence buf from :no-hang nil)
                  #+:cmu (sys:read-n-bytes from buf 0 *stream-buffer-size* nil)))
         (when (zerop pos) (return))
         (write-sequence buf to :end pos))))
  (values))

;; example (copy-file (pathname "aex.mac") (pathname "../aex1.mac") :overwrite t)

(defun copy-file (from to &key overwrite)
  "Copies the file designated by the non-wild pathname designator FROM
to the file designated by the non-wild pathname designator TO.  If
OVERWRITE is true overwrites the file designtated by TO if it exists."
  #+:allegro (excl.osi:copy-file from to :overwrite overwrite)
  #-:allegro
  (let ((element-type #-:cormanlisp '(unsigned-byte 8)
                      #+:cormanlisp 'unsigned-byte))
    (with-open-file (in from :element-type element-type)
      (with-open-file (out to :element-type element-type
                              :direction :output
                              :if-exists (if overwrite
                                           :supersede
                                           #-:cormanlisp :error
                                           #+:cormanlisp nil))
        #+:cormanlisp
        (unless out
          (error (make-condition 'file-error
                                 :pathname to
                                 :format-control "File already exists.")))
        (copy-stream in out))))
  (values))

(defun component-present-p (value)
  "Helper function for DIRECTORY-PATHNAME-P which checks whether VALUE
is neither NIL nor the keyword :UNSPECIFIC."
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (pathspec)
  "Returns NIL if PATHSPEC \(a pathname designator) does not designate
a directory, PATHSPEC otherwise.  It is irrelevant whether file or
directory designated by PATHSPEC does actually exist."
  (and 
    (not (component-present-p (pathname-name pathspec)))
    (not (component-present-p (pathname-type pathspec)))
    pathspec))

(defun pathname-as-directory (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to directory
form."
  (let ((pathname (pathname pathspec)))
    (when (fwild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (cond ((not (directory-pathname-p pathspec))
           (fmake-pathname :directory (append (or (fpathname-directory pathname)
                                             (list :relative))
                                             (list (file-namestring pathname)))
                          :name nil
                          :type nil
                          :defaults pathname))
          (t pathname))))

;; gjl changed gcl to work with linux, hope it does not break under win32
(defun directory-wildcard (dirname)
  "Returns a wild pathname designator that designates all files within
the directory named by the non-wild pathname designator DIRNAME."
  (when (fwild-pathname-p dirname)
    (error "Can only make wildcard directories from non-wildcard directories."))
  (fmake-pathname :name #-:cormanlisp :wild #+:cormanlisp "*"
                 :type #-(or :clisp :cormanlisp :gcl) :wild
                       #+:clisp nil
                       #+gcl nil
                       #+:cormanlisp "*"
                 :defaults (pathname-as-directory dirname)))

#+:clisp
(defun clisp-subdirectories-wildcard (wildcard)
  "Creates a wild pathname specifically for CLISP such that
sub-directories are returned by DIRECTORY."
  (make-pathname :directory (append (pathname-directory wildcard)
                                    (list :wild))
                 :name nil
                 :type nil
                 :defaults wildcard))

;;; gjl added support for gcl to this function
(defun list-directory (dirname)
  "Returns a fresh list of pathnames corresponding to the truenames of
all files within the directory named by the non-wild pathname
designator DIRNAME.  The pathnames of sub-directories are returned in
directory form - see PATHNAME-AS-DIRECTORY."
  (when (fwild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  #+:ecl 
  (let ((dir (pathname-as-directory dirname)))
    (concatenate 'list
                 (directory (merge-pathnames (pathname "*/") dir))
                 (directory (merge-pathnames (pathname "*.*") dir))))
  #-:ecl 
  (let ((wildcard (directory-wildcard dirname)))
    #+:abcl (system::list-directory dirname)
    #+(or :sbcl :cmu :scl :lispworks) (directory wildcard) 
    #+:gcl
    (let ((entries (directory wildcard)))
;      (format t "Wildcard is ~s~%" wildcard)
;      (loop for entry in entries do
;            (format t "stat is ~s~%" (si:stat entry)))
      (loop for entry in entries collect
            (if (eq (car (si:stat entry)) :directory)
                (pathname-as-directory entry) entry)))
    #+(or :openmcl :digitool) (directory wildcard :directories t)
    #+:allegro (directory wildcard :directories-are-files nil)
    #+:clisp (nconc (directory wildcard :if-does-not-exist :keep)
                    (directory (clisp-subdirectories-wildcard wildcard)))
    #+:cormanlisp (nconc (directory wildcard)
                         (cl::directory-subdirs dirname)))
  #-(or :sbcl :cmu :scl :gcl :lispworks :openmcl :allegro :clisp :cormanlisp :ecl :abcl :digitool)
  (error "LIST-DIRECTORY not implemented"))

(defun pathname-as-file (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to file form."
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (cond ((directory-pathname-p pathspec)
           (let* ((directory (pathname-directory pathname))
                  (name-and-type (pathname (first (last directory)))))
             (make-pathname :directory (butlast directory)
                            :name (pathname-name name-and-type)
                            :type (pathname-type name-and-type)
                            :defaults pathname)))
          (t pathname))))

;; This is broken with clisp
;;  This (mext-maxima::file-exists-p (pathname "/home/jlapeyre/.maxima/mext.lisp"))
;;   returns  nil and a function when the file in fact does exist
;; gjl added gcl here. Don't know if it is correct.
;;  should probably use gcl si:stat to fix it.
(defun file-exists-p (pathspec)
  "Checks whether the file named by the pathname designator PATHSPEC
exists and returns its truename if this is the case, NIL otherwise.
The truename is returned in `canonical' form, i.e. the truename of a
directory is returned as if by PATHNAME-AS-DIRECTORY."
  #+(or :sbcl :lispworks :openmcl :ecl :digitool :gcl) (probe-file pathspec)
  #+:allegro (or (excl:probe-directory (pathname-as-directory pathspec))
                 (probe-file pathspec))
  #+(or :cmu :scl :abcl) (or (probe-file (pathname-as-directory pathspec))
                             (probe-file pathspec))
  #+:cormanlisp (or (and (ccl:directory-p pathspec)
                         (pathname-as-directory pathspec))
                    (probe-file pathspec))
  #+:clisp (or (ignore-errors
                 (let ((directory-form (pathname-as-directory pathspec)))
                   (when (ext:probe-directory directory-form)
                     directory-form)))
               (ignore-errors
                 (probe-file (pathname-as-file pathspec))))
  #-(or :sbcl :cmu :scl :lispworks :openmcl :allegro :clisp :cormanlisp :ecl :abcl :digitool :gcl)
  (error "FILE-EXISTS-P not implemented"))

;; copied from cl-fad, clisp thing is still broken, so I disabled
;; it for the moment
(defun directory-exists-p (pathspec)
  "Checks whether the file named by the pathname designator PATHSPEC
exists and if it is a directory.  Returns its truename if this is the
case, NIL otherwise.  The truename is returned in directory form as if
by PATHNAME-AS-DIRECTORY."
;  #+:clisp (and (ext:probe-directory (pathname-as-directory pathspec)) ; gjl 2012
;                (pathname-as-directory (truename pathspec)))
  #+:allegro
  (and (excl:probe-directory pathspec)
       (pathname-as-directory (truename pathspec)))
  #+:lispworks
  (and (lw:file-directory-p pathspec)
       (pathname-as-directory (truename pathspec)))
  #+gcl ; does not work in win32. returns nil will valid path
  (eq :directory (first (si:stat pathspec)))
  #-(or :allegro :lispworks :gcl )
  (let ((result (file-exists-p pathspec)))
    (and result
         (directory-pathname-p result)
         result)))
