(in-package :mk )

;;; component operation definitions for defsystem to support
;;; third party maxima packages.
;;; Note that some of these routines to operate on
;;; components rely on global variables being correctly
;;; set. This is because defsystem does not allow any
;;; information to be passed to these, afaik.
;;;
;;; This code needs refactoring, etc.

;; otherwise in clisp, we get annoying interactive.
(setf mk::*bother-user-if-no-binary* nil)

;; refactor this
(defun mext-clean-files (component force file-types)
  (when (or (eq force :all)
	    (eq force t)
	    (and (find force '(:new-source :new-source-and-dependents
					   :new-source-all)
		       :test #'eq)
		 (needs-compilation component nil)))
;    (format t "binary component is ~s~%" (component-full-pathname component :binary))
;    (format t " probed binary component is ~s~%" (probe-file (component-full-pathname component :binary)))
    (loop for ext in file-types do
        (let* ((source-full-pathname (probe-file 
                                  #-gcl (component-full-pathname component :source)
                                  #+gcl (merge-pathnames (component-full-pathname component :source)
                                     (mext:pathname-as-directory *default-pathname-defaults*))))
               (pname-to-clean (if source-full-pathname (mext:fmake-pathname  :type ext :defaults source-full-pathname)))
               (binary-full-pathname (probe-file 
                                  #-gcl (component-full-pathname component :binary)
                                  #+gcl (merge-pathnames (component-full-pathname component :binary)
                                     (mext:pathname-as-directory *default-pathname-defaults*))))
               (bpname-to-clean (if binary-full-pathname (mext:fmake-pathname  :type ext :defaults binary-full-pathname))))
;          (format t "bpname-to-clean 1 ~s~%" bpname-to-clean)
;          (format t " pname-to-clean 1 ~s~%" pname-to-clean)
          (when pname-to-clean
              (let ((probed-to-clean (probe-file pname-to-clean)))
                (when (and probed-to-clean (not (equal probed-to-clean source-full-pathname)))
                  (or *oos-test*
                      (progn 
                        (format t "cleaning probed: ~s, source: ~s~%" probed-to-clean source-full-pathname)
                        (format t " Cleaning '~s'~%" pname-to-clean)
                        (delete-file pname-to-clean))))))
          (when (not (null bpname-to-clean))
              (let ((bprobed-to-clean (probe-file bpname-to-clean)))
                (when (and bprobed-to-clean (not (equal bprobed-to-clean source-full-pathname)))
                  (or *oos-test*
                      (progn 
                        (format t "cleaning probed: ~s, source: ~s~%" bprobed-to-clean source-full-pathname)
                        (format t " * Cleaning '~s'~%" bpname-to-clean)
                        (delete-file bpname-to-clean))))))))))

;; Clean LISP and UNLISP from compilation of .mac
(component-operation2 :mext-clean-intermediate  'mext-clean-intermediate)

(defun mext-clean-intermediate (name component force data)
  (declare (ignore name data))
     #+(or :win32 :mswindows :windows) (format t "In win32, I can't distinguish .LISP from .lisp.~%
  Not cleaning ~a.LISP~%" (component-pathname component :source))
	  #-(or :win32 :mswindows :windows) (mext-clean-files component force '("LISP" "UNLISP")))

(component-operation2 :mext-clean-lisp-compilation  'mext-clean-lisp-compilation)

(defun mext-clean-lisp-compilation (name component force data)
  (declare (ignore name data))
  (mext-clean-files component force mext-maxima::*extensions-to-clean* ))

;; Copy a file specified in a system definition to an installation location
;; component -- the structure specifying the file.
;; file-type -- must be :binary or :source. We interpret these as we like.
;; data   -- we use this for additional info on where to install.

(defun mext-user-install-one (component force file-type data)
  (when (or (eq force :all) (eq force t)  
            (and (find force '(:new-source :new-source-and-dependents :new-source-all) :test #'eq)
		 (needs-compilation component nil)))
    (let ((cfpn (component-full-pathname component file-type)))
      (mext::install-file cfpn data))))

;;; install source files to a subdir of user dir
(component-operation2 :mext-user-install-source  'mext-user-install-source)
(defun mext-user-install-source (name component force data)
  (declare (ignore name))
    (mext-user-install-one component force :source data))

(component-operation2 :mext-user-install-binary 'mext-user-install-binary)
(defun mext-user-install-binary (name component force data)
  (declare (ignore name))
  (mext-user-install-one component force :binary data))

;; install binary, unless load-only is true, in which case, install source
(component-operation2 :mext-user-install-pref-binary 'mext-user-install-pref-binary)
(defun mext-user-install-pref-binary (name component force data)
  (declare (ignore name))
  (let ((file-type (if (component-load-only component) :source :binary)))
    (mext-user-install-one component force file-type data)))

; sometimes 3 args are passed, sometimes 7
(defun mext-maxima-compile (&rest args)
  (format t "defsystem compiling maxima code: args are ~s~%" args)
  (let* ((source-file (car args))
         (args (cdr args))
         (output-file (getf args :OUTPUT-FILE)))
    (format t "Got output file ~s~%" output-file)
    (if output-file (mext::mext-compile-file source-file output-file)
      (mext::mext-compile-file source-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :cl-user)

;; language definition for translating and compiling maxima source files
(mk:define-language :mext-maxima
    :compiler #'mk::mext-maxima-compile
    :loader #'(lambda (x) 
                (format t "mext-system build: loading maxima file ~s~%" x) (maxima::$load x))
;;    :loader #'identity
    :source-extension "mac"
    :binary-extension mext::*binary-ext*)

