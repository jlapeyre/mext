(in-package #-gcl #:mext-maxima #+gcl :mext-maxima)
(in-package #-gcl #:mk  #+gcl :mk )

;(declaim (optimize (speed 0) (space 0) (safety 0) (debug 0)))

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

(defun mext-clean-files (component force file-types)
  (when (or (eq force :all)
	    (eq force t)
	    (and (find force '(:new-source :new-source-and-dependents
					   :new-source-all)
		       :test #'eq)
		 (needs-compilation component nil)))
    (loop for ext in file-types do
          (let* ((pname (component-pathname component :source))
           (pname-to-clean (mext:fmake-pathname :name pname :type ext :defaults *default-pathname-defaults*)))
;            (format t " pwd '~s'~%" (maxima::$pwd))
;            (format t " Cleaning '~s'~%" pname-to-clean)
;            (format t "Attempting clean file ~s, directory ~s.~%" pname *default-pathname-defaults*)
            (when (probe-file pname-to-clean)
              (or *oos-test*
                  (delete-file pname-to-clean)))))))

;; Clean LISP and UNLISP from compilation of .mac
(component-operation2 :mext-clean-intermediate  'mext-clean-intermediate)

(defun mext-clean-intermediate (name component force data)
  (mext-clean-files component force '("LISP" "UNLISP")))

(component-operation2 :mext-clean-lisp-compilation  'mext-clean-lisp-compilation)

(defun mext-clean-lisp-compilation (name component force data)
  (mext-clean-files component force mext-maxima::*extensions-to-clean* ))

;; what was this comment ? -->
;; extra thing on end causes error : note: probably could solve with pathname-as-directory

;; Copy a file specified in a system definition to an installation location
;; component -- the structure specifying the file.
;; file-type -- must be :binary or :source. We interpret these as we like.
;; data   -- we use this for additional info on where to install.
(defun mext-user-install-one (component force file-type data)
  (when (or (eq force :all) (eq force t)  
            (and (find force '(:new-source :new-source-and-dependents :new-source-all) :test #'eq)
		 (needs-compilation component nil)))
    (let* ((sname (component-pathname component file-type))
      (sdir  (mext:fpathname-directory (component-root-dir component file-type)))
      (sext  (component-extension component file-type))
      (install-dir 
       (if (getf data :mext-root) mext::*mext-user-dir-as-list*
         (append mext::*mext-user-dir-as-list* 
                 (or (let ((res (getf data :inst-dir)))
                       (if (null res) nil 
                         (if (listp res) res (list res))))
                     (list mext::*system-name*))))))
      (if (probe-file (component-full-pathname component file-type))
          (progn
            (mext:fensure-directories-exist (mext:pathname-as-directory (mext:fmake-pathname 
                                              :name nil :directory install-dir)))
            (mext::copy-file-from-dir-to-dir sname sext sdir install-dir))))))

;;; install source files to a subdir of user dir
(component-operation2 :mext-user-install-source  'mext-user-install-source)
(defun mext-user-install-source (name component force data)
    (mext-user-install-one component force :source data))

(component-operation2 :mext-user-install-binary 'mext-user-install-binary)
(defun mext-user-install-binary (name component force data)
  (mext-user-install-one component force :binary data))

;; install binary, unless load-only is true, in which case, install source
(component-operation2 :mext-user-install-pref-binary 'mext-user-install-pref-binary)
(defun mext-user-install-pref-binary (name component force data)
  (let ((file-type (if (component-load-only component) :source :binary)))
    (mext-user-install-one component force file-type data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :cl-user)

;; language definition for translating and compiling maxima source files
(mk:define-language :mext-maxima
    :compiler #'(lambda (x y z) (declare (ignore y z)) (maxima::$compile_file x))
    :loader #'identity
    :source-extension "mac"
    :binary-extension mext::*binary-ext*)


