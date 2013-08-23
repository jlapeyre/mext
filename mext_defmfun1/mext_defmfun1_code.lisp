(in-package :maxima)

;;; Functions that are marked `redefined' were defined previously without defmfun1
;;; in mext-maxima-system.lisp.

(doc-system:set-source-file-name "mext_defmfun1_code.lisp")
(doc-system:set-source-package "mext_defmfun1")
(max-doc:set-cur-sec 'max-doc::runtime-fandv)
(defmfun1:set-mext-package "mext_defmfun1")

(maxdoc:mdefmvar $homedir (namestring mext::*homedir-pathname*)
 "The user's home directory.")

;; For testing error message
;;(maxdoc:mdefmvar $anotherhomedir (namestring mext::*homedir-pathname*) )

;; redefined
(defmfun1 ($chdir :doc) ( &optional (dir :string))
  "Set the working directory for maxima/lisp. With some lisps, such as cmu lisp the system
 directory is changed as well. This should be made uniform across lisp implementations."
  (let ((result (mext::chdir :dir dir :push t)))
    (or result (merror1 (intl:gettext (format nil "chdir: ~a is not a directory" dir))))))

(max-doc:add-call-desc '( "chdir" ()
                           ("Set the working directory to the value it had when mext was loaded."))
                        '( "chdir" ("dir")
                           ("Set the working directory to " :arg "dir" ".")))

;; redefined
(defmfun1 ($popdir :doc) ( &optional (n 1 :non-neg-int))
  :desc ( "Pop a value from the current directory stack and chdir to this value.
 If " :arg "n" " is given, pop " :arg "n" " values and chdir to the last value popped.")
  (mext::popdir n))

;; redefined
(defmfun1 ($dirstack :doc) ()
  :desc ("Return a list of the directories on the directory stack. This list is
 manipulated with " :mref "chdir" ", " :mrefcomma "updir" " and " :mrefdot "popdir")
  (cons '(mlist simp) mext::*pwd-directory-stack*))

(defmfun1 ($dir_exists :doc) ((dir :string))
  :desc ("Returns the pathname as a string  if " :arg "dir" " exists, and " :code "false" " otherwise.")
  (let ((res  (mext::mext-dir-exists-p dir)))
    (if res (namestring res) nil)))

;; redefined
(defmfun1 ($pwd :doc) ()
  "Return the current working directory."
  (mext::pwd))

;; redefined
(defmfun1 ($mext_test :doc) ( &optional (dists :or-string-symbol-or-listof))
  :desc ("Run the test suites for a mext distribution or list of distributions. With
 no argument, a subfolder named " :code "rtests" " is searched for in the current directory.")
  (mext::mext-test dists))

;; redefined
(defmfun1 ($mext_list :doc) ()
  "Returns a list of all installed mext distributions."
    (mext::mext-list))

;; redefined
(defmfun1 ($mext_clear :doc) ()
  "Clears the list of mext packages that have been loaded with require.
   Subsequent calls to require will reload the packages."
    (mext::mext-clear))

(defmfun1:set-hold-all '$dont_kill)
(defmfun1 ($dont_kill :doc) (&rest item)
  :desc ("Add the " :arg "item" "s to the list of symbols that are not killed
          by " :codedot "kill(all)" " This facility is part of the maxima core,
          but is apparantly unused. Maybe putting a property in the symbol's
          property list would be better.")
  (apply #'mext::add-to-dont-kill item)
  '$done)

(defmfun1 ($get_dont_kill :doc) ()
  :desc ("Returns the list of symbols that are not killed
          by " :codedot "kill(all)" " Items are added to this list
          with " :mrefdot "dont_kill")
  (cons '(mlist simp) allbutl))

(defmfun1 ($dont_kill_share :doc) ((package :or-string-symbol))
  :desc ("Prevent symbols in maxima share package " :arg "package"
         " from being killed by " :emrefdot "kill")
  (let ((name (maxima::$sconcat package)))
    (mext:do-dont-kill-share name))
  '$done)

;; redefined
(defmfun1 ($mext_info :doc) ((distname :or-string-symbol))
  :desc ("Print information about installed mext distribution " :arg "distname"
    ". The list of installed  distributions is built by calling " :mref "mext_list" ".")
    (or (mext::mext-info distname)
        (merror1 (intl:gettext "mext_info: Unknown distribtuion '~a'.~%") ($sconcat distname))))

;; redefined
(defmfun1 ($require :doc) ((distname :or-string-symbol) &optional force)
  :desc ( "Load the mext pacakge " :arg "distname" " and register that it has been loaded."
 " " :code "require('all)" " will load all installed mext packages. If " :arg "force" " is true,
 then " :arg "distname" " is loaded even if it has been loaded previously.")
  (mext:mext-require distname force))
