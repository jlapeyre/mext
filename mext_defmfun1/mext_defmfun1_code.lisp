(in-package :maxima)

(doc-system::set-source-file-name "mext_defmfun1_code.lisp")
(doc-system::set-source-package "mext_defmfun1")

(max-doc::set-cur-sec 'max-doc::runtime-fandv)

(defmfun1 ($chdir :doc) ( &optional (dir :string))
  "Set the working directory for maxima/lisp. With some lisps, such as cmu lisp the system
 directory is changed as well. This should be made uniform across lisp implementations."
  (let ((result (mext::chdir :dir dir :push t)))
    (or result (merror1 (intl:gettext (format nil "chdir: ~a is not a directory" dir))))))

(max-doc:add-call-desc '( "chdir" ()
                           ("Set the working directory to the value it had when mext was loaded."))
                        '( "chdir" ("dir")
                           ("Set the working directory to " arg "dir" ".")))

(defmfun1 ($popdir :doc) ( &optional (n 1 :non-neg-int))
  :desc ( "Pop a value from the current directory stack and chdir to this value.
 If " arg "n" " is given, pop " arg "n" " values and chdir the last value popped.")
  (mext::popdir n))

(defmfun1 ($dirstack :doc) ()
  "Return a list of the directories on the directory stack. This list is
 manipulated with 'chdir', 'updir', and 'popdir'."
  (cons '(mlist simp) mext::*pwd-directory-stack*))

(defmfun1 ($pwd :doc) ()
  "Return the current working directory."
  (mext::pwd))

(defmfun1 ($mext_test :doc) ( &optional (dists :or-string-symbol-or-listof))
  "Run the test suites for a mext distribution or list of distributions. With
 no argument, a subfolder named 'rtests' is searched for in the current directory."
  (mext::mext-test dists))

(defmfun1 ($mext_list :doc) ()
  "Returns a list of all installed mext distributions."
    (mext::mext-list))

(defmfun1 ($mext_info :doc) ((distname :or-string-symbol))
  :desc ("Print information about installed mext distribution " arg "distname"
    ". The list of installed  distributions is built by calling " code "mext_list" ".")
    (or (mext::mext-info distname)
        (merror1 (intl:gettext "mext_info: Unknown distribtuion '~a'.~%") ($sconcat distname))))

(defmfun1 ($require :doc) ((distname :or-string-symbol) &optional force)
  :desc ( "Load the mext pacakge " arg "distname" " and register that it has been loaded."
 " " code "require('all)" " will load all installed mext packages. If " arg "force" " is true,
 then " arg "distname" " is loaded even if it has been loaded previously.")
  (mext:mext-require distname force))
