(use-package :max-doc)

(max-doc::set-cur-sec 'max-doc::runtime-fandv)

(add-doc-entry '( :name "truename"
                  :contents "Truename tries to find a canonical pathanme for a file
specified by the string <filespec>."))

(max-doc::add-call-desc '( "truename" ("filespec") 
                           ("returns a string representing a canonical pathname to the file
 specified by " arg "filespec")))

(add-doc-entry '( :name "probe_file"
                  :contents "Probe_File tries to find a canonical pathanme for a file
specified by the string <filespec>."))

(max-doc::add-call-desc '( "probe_file" ("filespec") 
                           ("returns a string representing a canonical pathname to the file
 specified by " arg "filespec" ". False is returned if the file can't be found.")))

(examples::clear-add-example "probe_file"
        '(:code-res ( "probe_file(\"a/b.txt\")" "\"/home/username/c/a/b.txt\"")))

(add-doc-entry '( :name "pwd" ))
(max-doc::add-call-desc '( "pwd" ()
                           ("returns the current working directory.")))

;; (add-doc-entry '( :name "chdir" ))
;; (max-doc::add-call-desc '( "chdir" ()
;;                            ("Sets the current working directory to the directory that was
;;  current when mext was loaded."))
;;                         '( "chdir" ("pathspec")
;;                            ("Sets the current working directory to the directory 
;;  specified by " arg "pathspec" ".")))
