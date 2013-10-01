;;;  Copyright (C) (2012,2013) John Lapeyre. Licensed under GPL, v3 or greater. See the file
;;;  `LICENSE' in this directory.

(in-package :maxima)

;;; Functions that are marked `redefined' were defined previously without defmfun1
;;; in mext-maxima-system.lisp.

(max-doc:set-cur-sec 'max-doc::runtime-fandv)
(defmfun1:set-file-and-package "mext_basic_code.lisp" "mext_basic")

(maxdoc:mdefmvar $homedir (namestring mext::*homedir-pathname*)
 "The user's home directory.")

(max-doc:add-doc-entry 
 '( :name "mext_verbose"
    :type "Variable"
    :default-value 0
    :contents ("Set this to " :code "0" " to suppress messages from mext while
                loading packages, or " :code "1"
                " to enable messages.")))

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
  (mk-mlist mext::*pwd-directory-stack*)))

;; redefined
(mext::no-warning
(defmfun1 ($pwd :doc) ()
  "Return the current working directory."
  (mext::pwd)))

;; redefined
(mext::no-warning
 (defmfun1 ($mext_test :doc) ( &rest (dists :or-string-symbol-or-listof)
                                     &opt ($list (:member '(nil t $long))))
  :desc 
  ("Run the test suites for a mext distribution or list of distributions. If "
   " the argument " :code "all" " is given, then all tests are run for all installed mext distributions. "
   "If the argument " :code "loaded" " is given, then all tests are run for all loaded mext distributions. "
   "If no argument is given, a subfolder named " :code "rtests" " is searched for in the current directory."
   " An item may be a list, in which case, the first element is the package name and the remaining "
   " elements are strings specifying the name of the rtests to run. The strings must not include "
   " directory or file extension parts. If the option " :opt "list" " is " :emrefcomma "true"
   " then the tests are not performed, but a list of the rtest files is returned. If "
   :opt "list" " is " :varcomma "long" " then the full pathnames of the rtest files are listed. "
   " Note: if the package " :mref "mext_basic" " is not loaded, then only a rudimentary "
   " version of " :mrefcomma "mext_test" " which does not accept options, is available.")
  (apply #'mext::mext-test (cons $list dists))))

(max-doc:add-call-desc 
 '( "mext_test" (("lit" "'loaded"))
    ("Run rtests for all loaded mext packages."))
 '( "mext_test" ("package")
    ("Run rtests for the mext pacakge " :argdot "package")))

(examples:clear-add-example 
 "mext_test"
 '(:code-text
 (
  :text ("Run regression tests for the packages " :mrefcomma "aex" " and " :mrefdot "lists_aex")
  :ex ("mext_test(aex,lists_aex)" "done")
  :text ("Run only some of the regression tests.")
  :ex ("mext_test([aex, \"rtest_aex\"],[lists_aex, \"rtest_table\"])" "done")
  :text "Only list the test files; do not run them."
  :ex ("mext_test(tpsolve, list->true)" "[rtest_to_poly_solve, rtest_to_poly]"))))

;; redefined
(mext::no-warning
(defmfun1 ($mext_list :doc) ()
  "Returns a list of all installed mext distributions. These are installed, but not neccessarily loaded."
    (mk-mlist (mext::mext-list))))

;; redefined
(mext::no-warning
(defmfun1 ($mext_clear :doc) ()
  "Clears the list of mext packages that have been loaded with require.
   Subsequent calls to require will reload the packages."
    (mext::mext-clear)))

;; redefined
(mext::no-warning
(defmfun1 ($mext_info :doc) ((distname :or-string-symbol :thread))
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

;; redefined
(mext::no-warning
(defmfun1 ($truename :doc) ((filespec :string))
  (namestring (truename filespec))))

(mext::no-warning
(defmfun1 ($probe_file :doc) ((filespec :string))
  (let ((res (probe-file filespec)))
    (if res (namestring res) nil))))

(mext::no-warning
(defmfun1 ($mkdir :doc) ((filespec :string) &optional (mode "0770"))
  (mext::mkdir filespec mode)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmfun1:set-hold-all '$dont_kill))
(defmfun1 ($dont_kill :doc) (&rest item)
  :desc ("Add the " :arg "item" "s to the list of symbols that are not killed
          by " :codedot "kill(all)" " This facility is part of the maxima core,
          but is apparantly unused. Maybe putting a property in the symbol's
          property list would be better.")
  (apply #'mext::add-to-dont-kill item)
  '$done)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmfun1:set-hold-all '$allow_kill))
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
  (mk-mlist allbutl))

(defmfun1 ($dont_kill_share :doc) ((package :or-string-symbol :thread))
  :desc (
 "Prevent symbols in maxima share package " :arg "package"
 " from being killed by " :emrefdot "kill"
 " Currently (if this document is up-to-date) only `basic' and"
 " `lrats' are in the database.")
  (let ((name (maxima::$sconcat package)))
    (mext:do-dont-kill-share name))
  '$done)

(defmfun1 ($allow_kill_share :doc) ((package :or-string-symbol :thread))
  :desc (
 "Allow symbols in maxima share package " :arg "package"
 " from being killed by " :emrefdot "kill"
 " This undoes the effect of " :mrefdot "dont_kill_share"
 " Currently (if this document is up-to-date) only `basic' and"
 " `lrats' are in the database.")
  (let ((name (maxima::$sconcat package)))
    (mext:do-allow-kill-share name))
  '$done)

(defmfun1 ($dir_exists :doc) ((dir :string :thread))
  :desc ("Returns the pathname as a string  if " :arg "dir" " exists, and " :code "false" " otherwise.")
  (let ((res  (mext::mext-dir-exists-p dir)))
    (if res (namestring res) nil)))

(defmfun1 ($list_directory :doc) ( &optional (dir :string :thread))
  :desc ("Returns a directory listing for " :arg "dir" " or the current directory if no argument is given.")
  (mext::maxima-list-directory dir))

(defmfun1 ($mext_list_loaded :doc) ()
  :desc ("Returns a list of mext packages currently loaded.")
  (mext::maxima-list-loaded-distributions))

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
"The name of the lisp implementation, as a string, on which Maxima is running.")

;; These are defined in mext-maxima-system.lisp,
;; but doc system says they are defined here. Could fix this.
(max-doc:add-doc-entry 
 '( :name "lisp_type_symbol"
    :type "Function"
    :contents ("Returns a maxima symbol representing the lisp vendor. "
               "For example, gcl, sbcl, clisp, ...")))

(max-doc:add-doc-entry 
 '( :name "os_type_symbol"
    :type "Function"
    :contents ("Returns a maxima symbol representing the type of operating systm. "
               "This is one of " :varcomma "win32" " " :varcomma "linux"
               " or " :vardot "unknown")))

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
;; !!! Still not right. Not that it does not exist. it's that
;; it is not loaded!
(defmfun1 ($mext_list_package :doc) ((package :or-string-symbol :thread))
  :desc ("List functions and variables defined in the mext pacakge "
 :argdot "package" " A mis-feature is that an empty list is returned
 if the package is not loaded. This function incorrectly returns an empty list
 for some packages, and may miss some functions.")
  (let ((name (maxima::$sconcat package)))
    (unless (member name (mext::mext-list) :test #'string-equal)
      (defmfun1-error-return '$no_such_package $mext_list_package
        (format nil "Package `~a' does not exist" name)))
    (multiple-value-bind (symbol-list present) (gethash name defmfun1::*mext-functions-table*)
      (if present
          (mk-mlist ; hmm need copy below
                (sort (copy-list symbol-list) #'string-lessp))
        (defmfun1-error-final '$package_not_loaded
          (format nil "Package `~a' is not loaded" name))))))
                                                        

;; convoluted. should rewrite to use :thread
(defmfun1 ($mext_find_package :doc) ( &rest (items :or-string-symbol) &opt ($file nil :bool))
  :desc 
  ("Find mext packages in which the function or variable "
   :arg "items" " are defined. This only works if the package has been "
   "loaded, and its symbols registered. If more than one package is found, then all are listed. "
   "If the option " :opt "file" " is true, then the filename in which the item is "
   "defined is also returned.")
  (let ((allres
         (loop :for item :in items :collect
               (let ((iname (maxima::$sconcat item))
                     (h defmfun1::*mext-functions-table*)
                     (res '()))
                 (dolist (pack (get-hash-keys h))
                   (when (member iname (gethash pack h) :test #'string-equal)
                     (let ((fn (defmfun1:get-filename-for-function item)))
                       (push (if (and $file fn)
                                 (make-mlist-simp pack fn) pack)
                             res))))
                 (mk-mlist res)))))
    (if (cdr allres)
        (mk-mlist allres)
      (car allres))))
;    (mk-mlist allres)))
;; Maybe this is too complicated for the user
;                 (if (cdr res)
;                     (mk-mlist res)
;                   (car res))))))
;    (if (cdr allres)
;        (mk-mlist allres)
;      (car allres))))


(defmfun1 ($mtranslate_file :doc) ((input-file :string :thread)
        &optional (ttymsgsp $tr_file_tty_messagesp) 
                                 &opt ($output_file :string))
  :desc( "Like " :emrefcomma "translate_file" " except that the "
         " output filename may be specified as an option.")
  (mext::mext-translate-file input-file $output_file ttymsgsp))

(defmfun1 ($mcompile_file :doc) ((input-file :string :thread) &optional
       (bin_file :string) &opt ($tr_file :string))
  :desc( "Like " :emrefcomma "compile_file" " except that the "
         " intermediate, translated filename may be specified as an option. "
         "If the intermediate filename, " :optcomma " is not given, then "
         "it will be written in the same directory as the ouput (binary) file.")
  (mext::mext-compile-file input-file bin_file $tr_file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flags, set and manipulate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmfun1 ($mext_flags :doc) ()
  :desc
  ("Set inflag to false and domain to complex, after saving the current values to the flag stack."
  " A list of which flags changed and their old and new values is returned.")
  (mext::push-maxima-flags)
  (mext::set-modern-maxima-flags))

(defmfun1 ($initial_flags :doc) ()
  :desc
  ("Set inflag and domain and maybe some other flags to their
    values at the time the mext system was loaded, after pushing current values
    to the flag stack. A list of which flags changed and their old and new values is returned.")
  (mext::push-maxima-flags)
  (mext::restore-initial-maxima-flags))


(defmfun1 ($push_flags :doc) ()
  :desc
  ("Push the current values of some flags to the flag stack.")
  (mext::push-maxima-flags)
  '$done)

(defmfun1 ($pop_flags :doc) ()
  :desc
  ("Pop the values of some flags from the flag stack and set them."
   " A list of which flags changed and their old and new values is returned.")
  (mext::pop-maxima-flags))


(defmfun1 ($list_flag_stack :doc) ()
  :desc
  ("Return a list of lists of the flags and values in the flag stack.")
  (mext::list-maxima-flags-stack))

(defmfun1 ($clear_flag_stack :doc) ()
  :desc
  ("Set the flag stack to an empty list.")
  (mext::clear-maxima-flags-stack)
  '$done)

(max-doc:see-also-group 
 '("pop_flags" "push_flags" "initial_flags" 
   "mext_flags" "list_flag_stack" "clear_flag_stack"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END Flags, set and manipulate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(max-doc:see-also-group '("mext_list_loaded" "mext_list" "mext_info" "mext_clear" 
                          "mext_list_package" "mext_find_package"))

(max-doc:see-also-group '("chdir" "pwd" "popdir" "updir" "dirstack" "list_directory"))

(max-doc:see-also-group '("dont_kill" "dont_kill_share" "get_dont_kill" "allow_kill"))
