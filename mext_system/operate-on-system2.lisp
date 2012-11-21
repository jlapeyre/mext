(in-package :mk)

; (declaim (optimize (speed 3) (space 0) (safety 0) #-gcl (debug 0)))

;;; oos2: copies of data and functions for oos with
;;; addition arguments passed to operation functions

(defvar *component-operations2* (make-hash-table :test #'equal)
  "Hash table of (operation-name function) pairs.")
(defun component-operation2 (name &optional operation)
  (if operation
      (setf (gethash name *component-operations2*) operation)
      (gethash name *component-operations2*)))

(defun operate-on-system2 (name operation
			       &key
                               data   ;  NEW gjl
			       force
			       (version *version*)
			       (test *oos-test*) (verbose *oos-verbose*)
                               (load-source-instead-of-binary
				*load-source-instead-of-binary*)
                               (load-source-if-no-binary
				*load-source-if-no-binary*)
			       (bother-user-if-no-binary
				*bother-user-if-no-binary*)
			       (compile-during-load *compile-during-load*)
			       dribble
			       (minimal-load *minimal-load*)
			       (override-compilation-unit t)
			       )
  (declare #-(or :cltl2 :ansi-cl) (ignore override-compilation-unit))
  (unwind-protect
      ;; Protect the undribble.
      (#+(and (or :cltl2 :ansi-cl) (not :gcl)) with-compilation-unit
	 #+(and (or :cltl2 :ansi-cl) (not :gcl)) (:override override-compilation-unit)
	 #-(and (or :cltl2 :ansi-cl) (not :gcl)) progn
	(when *reset-full-pathname-table* (clear-full-pathname-tables))
	(when dribble (dribble dribble))
	(when test (setq verbose t))
	(when (null force)		; defaults
	  (case operation
	    ((load :load) (setq force :all))
	    ((compile :compile) (setq force :new-source-and-dependents))
	    (t (setq force :all))))
	;; Some CL implementations have a variable called *compile-verbose*
	;; or *compile-file-verbose*.
	(multiple-value-bind (*version-dir* *version-replace*)
	    (translate-version version)
	  ;; CL implementations may uniformly default this to nil
	  (let ((*load-verbose* #-common-lisp-controller t
				#+common-lisp-controller nil) ; nil
		#-(or MCL CMU CLISP ECL :sbcl lispworks scl)
		(*compile-file-verbose* t) ; nil
		#+common-lisp-controller
		(*compile-print* nil)
		#+(and common-lisp-controller cmu)
		(ext:*compile-progress* nil)
		#+(and common-lisp-controller cmu)
		(ext:*require-verbose* nil)
		#+(and common-lisp-controller cmu)
		(ext:*gc-verbose* nil)

		(*compile-verbose* #-common-lisp-controller t
				   #+common-lisp-controller nil) ; nil
		(*version* version)
		(*oos-verbose* verbose)
		(*oos-test* test)
		(*load-source-if-no-binary* load-source-if-no-binary)
		(*compile-during-load* compile-during-load)
		(*bother-user-if-no-binary* bother-user-if-no-binary)
		(*load-source-instead-of-binary* load-source-instead-of-binary)
		(*minimal-load* minimal-load)
		(system (if (and (component-p name)
                                 (member (component-type name)
					 '(:system :defsystem :subsystem)))
                            name
                            (find-system name :load))))
	    #-(or CMU CLISP :sbcl :lispworks :cormanlisp scl)
	    (declare (special *compile-verbose* #-MCL *compile-file-verbose*)
		     #-openmcl (ignore *compile-verbose*
				       #-MCL *compile-file-verbose*)
		     #-openmcl (optimize (inhibit-warnings 3)))
	    (unless (component-operation2 operation)
	      (error "Operation ~A undefined." operation))
	    (operate-on-component2 name system operation force :data data)))) ; changed gjl
    (when dribble (dribble))))

(defun operate-on-component2 (name component operation force &key data  &aux changed) ; NEW gjl
  ;; Returns T if something changed and had to be compiled.
;  (format t "name of component ~a~%" (string (component-name component)))
;  (format t "  name system ~a~%" (string name))
;  (format t "   data is  ~a~%" data)
  (let ((type (component-type component))
	(old-package (package-name *package*)))

    (unwind-protect
	;; Protect old-package.
	(progn
	  ;; Use the correct package.
	  (when (component-package component)
	    (tell-user-generic (format nil "Using package ~A"
				       (component-package component)))
	    (unless *oos-test*
	      (unless (find-package (component-package component))
		;; If the package name is the same as the name of the system,
		;; and the package is not defined, this would lead to an
		;; infinite loop, so bomb out with an error.
		(when (string-equal (string (component-package component))
				    (component-name component))
		  (format t "~%Component ~A not loaded:~%"
			  (component-name component))
		  (error  "  Package ~A is not defined"
			  (component-package component)))
		;; If package not found, try using REQUIRE to load it.
		(new-require (component-package component)))
	      ;; This was USE-PACKAGE, but should be IN-PACKAGE.
	      ;; Actually, CLtL2 lisps define in-package as a macro,
	      ;; so we'll set the package manually.
	      ;; (in-package (component-package component))
	      (let ((package (find-package (component-package component))))
		(when package
		  (setf *package* package)))))

	  ;; Marco Antoniotti 20040609
	  ;; New feature.  Try to FIND-SYSTEM :system components if
	  ;; they have no local :components definition.
	  ;; OPERATE-ON-SYSTEM-DEPENDENCIES should still work as
	  ;; advertised, given the small change made there.

	  (when (or (eq type :system) (eq type :subsystem))
	    (ensure-external-system-def-loaded component))

	  (when (or (eq type :defsystem) (eq type :system))
	    (operate-on-system-dependencies2 name component operation :force force :data data))

	  ;; Do any compiler proclamations
	  (when (component-proclamations component)
	    (tell-user-generic (format nil "Doing proclamations for ~A"
				       (component-name component)))
	    (or *oos-test*
		(proclaim (component-proclamations component))))

	  ;; Do any initial actions
	  (when (component-initially-do component)
	    (tell-user-generic (format nil "Doing initializations for ~A"
				       (component-name component)))
	    (or *oos-test*
		(eval (component-initially-do component))))

	  ;; If operation is :compile and load-only is T, this would change
	  ;; the operation to load. Only, this would mean that a module would
	  ;; be considered to have changed if it was :load-only and had to be
	  ;; loaded, and then dependents would be recompiled -- this doesn't
	  ;; seem right. So instead, we propagate the :load-only attribute
	  ;; to the components, and modify compile-file-operation so that
	  ;; it won't compile the files (and modify tell-user to say "Loading"
	  ;; instead of "Compiling" for load-only modules).
	  #||
	  (when (and (find operation '(:compile compile))
		     (component-load-only component))
	    (setf operation :load))
	  ||#

	  ;; Do operation and set changed flag if necessary.
	  (setq changed
		(case type
		  ((:file :private-file)
                   (funcall (component-operation2 operation) name component force data)) ; NEW gjl
;                   (if (member operation (list :compile 'compile :load 'load))  
;                       (funcall (component-operation operation) component force)
;                     (funcall (component-operation operation) name component force data))
		  ((:module :system :subsystem :defsystem)
		   (operate-on-components2 name component operation force changed :data data))))

	  ;; Do any final actions
	  (when (component-finally-do component)
	    (tell-user-generic (format nil "Doing finalizations for ~A"
				       (component-name component)))
	    (or *oos-test*
		(eval (component-finally-do component))))

	  ;; add the banner if needed
	  #+(or cmu scl)
	  (when (component-banner component)
	    (unless (stringp (component-banner component))
	      (error "The banner should be a string, it is: ~S"
	             (component-banner component)))
	    (setf (getf ext:*herald-items*
			(intern (string-upcase  (component-name component))
				(find-package :keyword)))
		  (list
		     (component-banner component)))))

      ;; Reset the package. (Cleanup form of unwind-protect.)
      ;;(in-package old-package)
      (setf *package* (find-package old-package)))

    ;; Provide the loaded system
    (when (or (eq type :defsystem) (eq type :system) (eq type :subsystem))
      (tell-user-generic (format nil "Providing system ~A~%"
				 (component-name component)))
      (or *oos-test*
	  (provide (canonicalize-system-name (component-name component))))))

  ;; Return non-NIL if something changed in this component and hence had
  ;; to be recompiled. This is only used as a boolean.
  changed)

(defvar *force* nil)
(defvar *providing-blocks-load-propagation* t
  "If T, if a system dependency exists on *modules*, it is not loaded.")

(defun operate-on-system-dependencies2 (name component operation &key force data ) ; NEW gjl
  (declare (ignore name))
  (when *system-dependencies-delayed*
    (let ((*force* force))
      (dolist (system (component-depends-on component))
	;; For each system that this system depends on, if it is a
	;; defined system (either via defsystem or component type :system),
	;; and propagation is turned on, propagates the operation to the
	;; subsystem. Otherwise runs require (my version) on that system
	;; to load it (needed since we may be depending on a lisp
	;; dependent package).
	;; Explores the system tree in a DFS manner.

	;; Do not try to do anything with non system components.
        (cond ((and *operations-propagate-to-subsystems*
                    (not (listp system))
		    (or (stringp system) (symbolp system))
                    ;; The subsystem is a defined system.
                    (find-system system :load-or-nil))
               ;; Call OOS on it. Since *system-dependencies-delayed* is
               ;; T, the :depends-on slot is filled with the names of
               ;; systems, not defstructs.
               ;; Aside from system, operation, force, for everything else
               ;; we rely on the globals.
               (unless (and *providing-blocks-load-propagation*
                            ;; If *providing-blocks-load-propagation* is T,
                            ;; the system dependency must not exist in the
                            ;; *modules* for it to be loaded. Note that
                            ;; the dependencies are implicitly systems.
                            (find operation '(load :load))
                            ;; (or (eq force :all) (eq force t))
                            (find (canonicalize-system-name system)
                                  *modules* :test #'string-equal))

                 (operate-on-system2 system operation :force force :data data)))

              ((listp system)
               ;; If the SYSTEM is a list then its contents are as follows.
               ;;
               ;;    (<name> <definition-pathname> <action> &optional <version>)
               ;;

               (destructuring-bind (system-name definition-pathname action
                                                &optional version)
                   system
                 (tell-user-require-system
                  (if (and (null system-name)
                           (null definition-pathname))
                      action
                      system)
                  component)
                 (or *oos-test* (new-require system-name
                                             nil
                                             (eval definition-pathname)
                                             action
                                             (or version *version*)))))
              ((and (component-p system)
                    (not (member (component-type system)
                                 '(:defsystem :subsystem :system))))
               ;; Do nothing for non system components.
               )
              (t
               (tell-user-require-system system component)
               (or *oos-test* (new-require system))))
        ))))

(defun operate-on-components2 (name component operation force changed &key data) ; NEW gjl
  (with-tell-user (operation component)
    (if (component-components component)
	(dolist (module (component-components component))
	  (when (operate-on-component2 name module operation ; changed gjl
		  (cond ((and (module-depends-on-changed module changed)
			      #||(some #'(lambda (dependent)
					(member dependent changed))
				    (component-depends-on module))||#
			      (or (non-empty-listp force)
				  (eq force :new-source-and-dependents)))
			 ;; The component depends on a changed file
			 ;; and force agrees.
			 (if (eq force :new-source-and-dependents)
			     :new-source-all
			   :all))
			((and (non-empty-listp force)
			      (member (component-name module) force
				      :test #'string-equal :key #'string))
			 ;; Force is a list of modules
			 ;; and the component is one of them.
			 :all)
			(t force))  :data data)
	    (push module changed)))
	(case operation
	  ((compile :compile)
	   (eval (component-compile-form component)))
	  ((load :load)
	   (eval (component-load-form component))))))
  ;; This is only used as a boolean.
  changed)

;; gjl: Compilers complain because this is an eval-when situation that has no
;; use. But this is copied from defsystem.
(eval-when (load eval)
(component-operation2 :compile  'compile-and-load-operation2)
(component-operation2 'compile  'compile-and-load-operation2)
(component-operation2 :load     'load-file-operation2)
(component-operation2 'load     'load-file-operation2)
)

(defun compile-and-load-operation2 (name component force data)
  ;; FORCE was CHANGED. this caused defsystem during compilation to only
  ;; load files that it immediately compiled.
  (let ((changed (compile-file-operation2 name component force data)))
    ;; Return T if the file had to be recompiled and reloaded.
    (if (and changed (component-compile-only component))
	;; For files which are :compile-only T, compiling the file
	;; satisfies the need to load.
	changed
	;; If the file wasn't compiled, or :compile-only is nil,
	;; check to see if it needs to be loaded.
	(and (load-file-operation2 name component force data) ; FORCE was CHANGED ???
	     changed))))


(defun compile-file-operation2 (name component force data)
  ;; Returns T if the file had to be compiled.
  (declare (ignore name data))  
  (let ((must-compile
	 ;; For files which are :load-only T, loading the file
	 ;; satisfies the demand to recompile.
	 (and (null (component-load-only component)) ; not load-only
	      (or (find force '(:all :new-source-all t) :test #'eq)
		  (and (find force '(:new-source :new-source-and-dependents)
			     :test #'eq)
		       (needs-compilation component nil)))))
	(source-pname (component-full-pathname component :source)))

    (cond ((and must-compile (probe-file source-pname))
	   (with-tell-user ("Compiling source" component :source)
	     (let ((output-file
		    #+:lucid
		     (unmunge-lucid (component-full-pathname component
							     :binary))
		     #-:lucid
		     (component-full-pathname component :binary)))

	       ;; make certain the directory we need to write to
	       ;; exists [pvaneynd@debian.org 20001114]
	       ;; Added PATHNAME-HOST following suggestion by John
	       ;; DeSoi [marcoxa@sourceforge.net 20020529]

	       (ensure-directories-exist
		(make-pathname :name nil
			       :type nil
			       :version nil
			       :defaults output-file))

	       (or *oos-test*
		   (apply (compile-function component)
			  source-pname
			  :output-file
			  output-file
			  #+(or :cmu :scl) :error-file
			  #+(or :cmu :scl) (and *cmu-errors-to-file*
						(component-full-pathname component
									 :error))
			  #+CMU
			  :error-output
			  #+CMU
			  *cmu-errors-to-terminal*
			  (component-compiler-options component)
			  ))))
	   must-compile)
	  (must-compile
	   (tell-user "Source file not found. Not compiling"
		      component :source :no-dots :force)
	   nil)
	  (t nil))))

;;; Need to completely rework this function...
(defun load-file-operation2 (name component force data)
  ;; Returns T if the file had to be loaded
  (let* ((binary-pname (component-full-pathname component :binary))
	 (source-pname (component-full-pathname component :source))
	 (binary-exists (probe-file binary-pname))
	 (source-exists (probe-file source-pname))
	 (source-needs-loading (needs-loading component t nil))
	 (binary-needs-loading (needs-loading component nil t))
	 ;; needs-compilation has an implicit source-exists in it.
	 (needs-compilation (if (component-load-only component)
				source-needs-loading
				(needs-compilation component force)))
	 (check-for-new-source
	  ;; If force is :new-source*, we're checking for files
	  ;; whose source is newer than the compiled versions.
	  (find force '(:new-source :new-source-and-dependents :new-source-all)
		:test #'eq))
	 (load-binary (or (find force '(:all :new-source-all t) :test #'eq)
			  binary-needs-loading))
	 (load-source
	  (or *load-source-instead-of-binary*
	      (and load-binary (component-load-only component))
	      (and check-for-new-source needs-compilation)))
	 (compile-and-load
	  (and needs-compilation
               (or load-binary check-for-new-source)
	       (compile-and-load-source-if-no-binary component)))
         )
    ;; When we're trying to minimize the files loaded to only those
    ;; that need be, restrict the values of load-source and load-binary
    ;; so that we only load the component if the files are newer than
    ;; the load-time.
    (when (and *minimal-load*
               (not (find force '(:all :new-source-all)
		          :test #'eq)))
      (when load-source (setf load-source source-needs-loading))
      (when load-binary (setf load-binary binary-needs-loading)))

    (when (or load-source load-binary compile-and-load)
      (cond (compile-and-load
	     ;; If we're loading the binary and it is old or nonexistent,
	     ;; and the user says yes, compile and load the source.
	     (compile-file-operation2 name component t data)
	     (with-tell-user ("Loading binary"   component :binary)
	       (or *oos-test*
		   (progn
		     (funcall (load-function component) binary-pname)
		     (setf (component-load-time component)
			   (file-write-date binary-pname)))))
	     t)
	    ((and source-exists
		  (or (and load-source	; implicit needs-comp...
			   (or *load-source-instead-of-binary*
			       (component-load-only component)
			       (not *compile-during-load*)))
		      (and load-binary
                           (not binary-exists)
			   (load-source-if-no-binary component))))
	     ;; Load the source if the source exists and:
	     ;;   o  we're loading binary and it doesn't exist
	     ;;   o  we're forcing it
	     ;;   o  we're loading new source and user wasn't asked to compile
	     (with-tell-user ("Loading source" component :source)
	       (or *oos-test*
		   (progn
		     (funcall (load-function component) source-pname)
		     (setf (component-load-time component)
			   (file-write-date source-pname)))))
	     t)
	    ((and binary-exists load-binary)
	     (with-tell-user ("Loading binary"   component :binary)
	       (or *oos-test*
		   (progn
		     (funcall (load-function component) binary-pname)
		     (setf (component-load-time component)
			   (file-write-date binary-pname)))))
	     t)
	    ((and (not binary-exists) (not source-exists))
	     (tell-user-no-files component :force)
	     (when *files-missing-is-an-error*
	       (cerror "Continue, ignoring missing files."
		       "~&Source file ~S ~:[and binary file ~S ~;~]do not exist."
		       source-pname
		       (or *load-source-if-no-binary*
			   *load-source-instead-of-binary*)
		       binary-pname))
	     nil)
	    (t
	     nil)))))

(eval-when (load eval)
(component-operation2 :clean    'delete-binaries-operation2)
(component-operation2 'clean    'delete-binaries-operation2)
(component-operation2 :delete-binaries     'delete-binaries-operation2)
(component-operation2 'delete-binaries     'delete-binaries-operation2)
)

(defun delete-binaries-operation2 (name component force data)
  (declare (ignore name data))
  (when (or (eq force :all)
	    (eq force t)
	    (and (find force '(:new-source :new-source-and-dependents
					   :new-source-all)
		       :test #'eq)
		 (needs-compilation component nil)))
    (let ((binary-pname (component-full-pathname component :binary)))
      (when (probe-file binary-pname)
	(with-tell-user ("Deleting binary"   component :binary)
			(or *oos-test*
			    (delete-file binary-pname)))))))

(setf (symbol-function 'oos2) (symbol-function 'operate-on-system2))
(export 'operate-on-system2)
(export 'oos2)
;(export 'operate-on-component2)
;(export 'operate-on-system-dependencies2)
;(export 'operate-on-components2)
