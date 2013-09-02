(in-package :maxima)

;;; Functions that are marked `redefined' were defined previously without defmfun1
;;; in mext-maxima-system.lisp.

(max-doc:set-cur-sec 'max-doc::runtime-fandv)
(defmfun1:set-file-and-package "mext_defmfun1_code.lisp" "mext_defmfun1")

(maxdoc:mdefmvar $homedir (namestring mext::*homedir-pathname*)
 "The user's home directory.")

;; For testing error message
;;(maxdoc:mdefmvar $anotherhomedir (namestring mext::*homedir-pathname*) )

;; redefined
(mext::no-warning
(defmfun1 ($chdir :doc) ( &optional (dir :string))
  "Set the working directory for maxima/lisp. With some lisps, such as cmu lisp the system
 directory is changed as well. This should be made uniform across lisp implementations."
  (let ((result (mext::chdir :dir dir :push t)))
    (or result (merror1 (intl:gettext (format nil "chdir: ~a is not a directory" dir)))))))

(max-doc:add-call-desc '( "chdir" ()
                           ("Set the working directory to the value it had when mext was loaded."))
                        '( "chdir" ("dir")
                           ("Set the working directory to " :arg "dir" ".")))

;; redefined
(mext::no-warning
(defmfun1 ($popdir :doc) ( &optional (n 1 :non-neg-int))
  :desc ( "Pop a value from the current directory stack and chdir to this value.
 If " :arg "n" " is given, pop " :arg "n" " values and chdir to the last value popped.")
  (mext::popdir n)))

;; redefined
(mext::no-warning
(defmfun1 ($dirstack :doc) ()
  :desc ("Return a list of the directories on the directory stack. This list is
 manipulated with " :mref "chdir" ", " :mrefcomma "updir" " and " :mrefdot "popdir")
  (cons '(mlist simp) mext::*pwd-directory-stack*)))

;; redefined
(mext::no-warning
(defmfun1 ($pwd :doc) ()
  "Return the current working directory."
  (mext::pwd)))

;; redefined
(mext::no-warning
(defmfun1 ($mext_test :doc) ( &optional (dists :or-string-symbol-or-listof))
  :desc ("Run the test suites for a mext distribution or list of distributions. If "
 " the argument " :code "all" " is given, then all tests are run for all installed mext distributions. "
 "If the argument " :code "loaded" " is given, then all tests are run for all loaded mext distributions. "
 "If no argument is given, a subfolder named " :code "rtests" " is searched for in the current directory.")
  (mext::mext-test dists)))

;; redefined
(mext::no-warning
(defmfun1 ($mext_list :doc) ()
  "Returns a list of all installed mext distributions. These are installed, but not neccessarily loaded."
    (mext::mext-list)))

;; redefined
(mext::no-warning
(defmfun1 ($mext_clear :doc) ()
  "Clears the list of mext packages that have been loaded with require.
   Subsequent calls to require will reload the packages."
    (mext::mext-clear)))

;; redefined
(mext::no-warning
(defmfun1 ($mext_info :doc) ((distname :or-string-symbol))
  :desc ("Print information about installed mext distribution " :arg "distname"
    ". The list of installed  distributions is built by calling " :mref "mext_list" ".")
    (or (mext::mext-info distname)
        (merror1 (intl:gettext "mext_info: Unknown distribtuion '~a'.~%") ($sconcat distname)))))

;; redefined
(mext::no-warning
(defmfun1 ($require :doc) ((distname :or-string-symbol-or-listof) &optional force)
  :desc ( "Load the mext pacakge " :arg "distname" " and register that it has been loaded."
          " " :code "require('all)" " will load all installed mext packages. If " :arg "force" " is true, "
          "then " :arg "distname" " is loaded even if it has been loaded previously. "
          :arg "distname" " may also be a list of package names to be loaded.")
  (mext:mext-require distname force)))

;; redefined
(mext::no-warning
(defmfun1 ($updir :doc) (&optional (n 1 :non-neg-int))
 :desc ("Change the working directory to be " :arg "n" " (or 1 if " :arg "n" " is not given) subdirectories "
   "higher than the current working directory.")
  (mext::updir n)))


(defmfun1:set-hold-all '$dont_kill)
(defmfun1 ($dont_kill :doc) (&rest item)
  :desc ("Add the " :arg "item" "s to the list of symbols that are not killed
          by " :codedot "kill(all)" " This facility is part of the maxima core,
          but is apparantly unused. Maybe putting a property in the symbol's
          property list would be better.")
  (apply #'mext::add-to-dont-kill item)
  '$done)


(defmfun1:set-hold-all '$allow_kill)
(defmfun1 ($allow_kill :doc) (&rest items)
  :desc ("Remove " :arg "item" "s from the list of symbols that are not killed
          by " :codedot "kill(all)" " This facility is part of the maxima core,
          but is apparantly unused. Maybe putting a property in the symbol's
          property list would be better.")
  (apply #'mext::remove-from-dont-kill items)
  '$done)

(defmfun1 ($get_dont_kill :doc) ()
  :desc ("Returns the list of symbols that are not killed
          by " :codedot "kill(all)" " Items are added to this list
          with " :mrefdot "dont_kill")
  (cons '(mlist simp) allbutl))

(defmfun1 ($dont_kill_share :doc) ((package :or-string-symbol))
  :desc (
 "Prevent symbols in maxima share package " :arg "package"
 " from being killed by " :emrefdot "kill"
 " Currently (if this document is up-to-date) only `basic' and"
 " `lrats' are in the database.")
  (let ((name (maxima::$sconcat package)))
    (mext:do-dont-kill-share name))
  '$done)

(defmfun1 ($allow_kill_share :doc) ((package :or-string-symbol))
  :desc (
 "Allow symbols in maxima share package " :arg "package"
 " from being killed by " :emrefdot "kill"
 " This undoes the effect of " :mrefdot "dont_kill_share"
 " Currently (if this document is up-to-date) only `basic' and"
 " `lrats' are in the database.")
  (let ((name (maxima::$sconcat package)))
    (mext:do-allow-kill-share name))
  '$done)

(defmfun1 ($dir_exists :doc) ((dir :string))
  :desc ("Returns the pathname as a string  if " :arg "dir" " exists, and " :code "false" " otherwise.")
  (let ((res  (mext::mext-dir-exists-p dir)))
    (if res (namestring res) nil)))

(defmfun1 ($list_directory :doc) ( &optional (dir :string))
  :desc ("Returns a directory listing for " :arg "dir" " or the current directory if no argument is given.")
  (mext::maxima-list-directory dir))


(defmfun1 ($mext_list_loaded :doc) ()
  :desc ("Returns a list of mext packages currently loaded.")
  (mext::maxima-list-loaded-distributions))
;  ($sort (cons '(mlist simp) (get-hash-keys mext-maxima::*loaded-dist-table*))))

(maxdoc:mdefmvar $lisp_bin_ext mext-maxima::*binary-ext*
"The extension of compiled lisp binaries for the lisp
implementation used by Maxima. This should be read-only.
Setting it has no effect.")

;; build_info() does a lot of this. Probably don't need these thing. */

(maxdoc:mdefmvar $maxima_version maxima::*autoconf-version*
"The Maxima version number.")

(maxdoc:mdefmvar $lisp_version (mext-maxima::lisp-version-string)
"The lisp version number of the lisp implementation on which Maxima is running.")

(maxdoc:mdefmvar $lisp_type (cl::lisp-implementation-type)
"The name of the lisp implementation on which Maxima is running.")

#|

This is from wxmaxima

(defun $wxbuild_info ()
  (let ((wxmaxima-version (cdr ($get '$wxmaxima '$version)))
        (year (sixth cl-user:*maxima-build-time*))
        (month (fifth cl-user:*maxima-build-time*))
        (day (fourth cl-user:*maxima-build-time*))
        (hour (third cl-user:*maxima-build-time*))
        (minute (second cl-user:*maxima-build-time*))
        (seconds (first cl-user:*maxima-build-time*)))
    (format t "wxMaxima version: ~{~d~^.~}~%" wxmaxima-version)
    (format t "Maxima version: ~a~%" *autoconf-version*)
    (format t "Maxima build date: ~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d~%"
            year month day hour minute seconds)
    (format t "Host type: ~a~%" *autoconf-host*)
    (format t "Lisp implementation type: ~a~%" (lisp-implementation-type))
    (format t "Lisp implementation version: ~a~%" (lisp-implementation-version)))
  "")

|#

;; Note: this feature is new. Some functions and variables
;; are not properly registered.
;; 
(defmfun1 ($mext_list_package :doc) ((package :or-string-symbol))
  :desc ("List functions and variables defined in the mext pacakge "
 :argdot "package" " A mis-feature is that an empty list is returned
 if the package is not loaded. This function incorrectly returns an empty list
 for some packages, and may miss some functions.")
  (let ((name (maxima::$sconcat package)))
    (cons '(mlist simp) ; hmm need copy below
          (sort (copy-list (gethash name defmfun1::*mext-functions-table*)) #'string-lessp))))


(defmfun1 ($mext_find_package :doc) ((item :or-string-symbol))
  :desc ("Find mext packages in which the function or variable "
  :arg "item" " is defined. This only works if the package has been"
  " loaded, and its symbols registered. A string or list of strings is returned.")
  (let ((iname (maxima::$sconcat item))
        (h defmfun1::*mext-functions-table*)
        (res '()))
    (dolist (pack (get-hash-keys h))
      (when (member iname (gethash pack h) :test #'string-equal)
        (push pack res)))
    (if (cdr res)
        (cons '(mlist simp) res)
      (car res))))

(max-doc:see-also-group '("mext_list_loaded" "mext_list" "mext_info" "mext_clear" 
                          "mext_list_package" "mext_find_package"))

(max-doc:see-also-group '("chdir" "pwd" "popdir" "updir" "dirstack" "list_directory"))

(max-doc:see-also-group '("dont_kill" "dont_kill_share" "get_dont_kill" "allow_kill"))

