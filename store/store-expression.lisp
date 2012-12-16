(in-package :maxima)
(use-package :gjl.lisp-util)
(use-package :max-doc)
(set-cur-sec 'max-doc::io-fandv)
(defmfun1:set-mext-package "store")

;; Write and read maxima expressions to a file. This is done via a serialization library, cl-store.
;; TODO: cl-store has various settings that that can greatly affect efficiency. We should support these.

(defmfun1 ($store :doc)  ( (file :string) &rest exprs )
 :desc ("Stores maxima expressions " :arg "exprs" " in " :arg "file" 
 " in binary format. Many types of lisp expressions and subexpressions
 are supported: numbers,strings,list,arrays,hashtables,structures,....")
  (let ((cl-store::*check-for-circs* t))
    (cl-store::store (if (length1p exprs) (car exprs) (cons '(mlist simp) exprs)) file)))

(defmfun1 ($store_fast :doc) ( (file :string) &rest exprs )
  :desc ("Stores maxima expressions " :arg "exprs" " in " :arg "file" 
 " in binary format. This is like "
  :mref "store" ", except that no checks for circular references are done.")
  (let ((cl-store::*check-for-circs* nil))
    (cl-store::store (if (length1p exprs) (car exprs) (cons '(mlist simp) exprs)) file)))

(defmfun1 ($restore :doc) ((file :string) )
  :desc ("Reads maxima expressions from file " :arg "file" 
  " created by the function " :mrefdot "store")
  (let ((cl-store::*check-for-circs* t))
    (cl-store::restore file)))

(defmfun1 ($restore_fast :doc) ((file :string) )
  :desc ("Reads maxima expressions from file " :arg "file" " created by the function " 
   :mref "store" ", or " :mref "store_fast" ". No checks for circular references are done.")
  (let ((cl-store::*check-for-circs* nil))
    (cl-store::restore file)))

(clear-call-desc "store" "restore" "store_fast" "restore_fast")
(add-call-desc '( "store" ("file" "expr1" "expr2" "...") ("stores the expressions to the file " :arg "file" ".")))
(add-call-desc '( "restore" ("file") ("Reads and returns expressions from the file " :arg "file" ".")))
(add-call-desc '( "store_fast" ("file" "expr1" "expr2" "...") ("stores the expressions to the file " :arg "file" 
              ". No checking for circular references is done.")))
(add-call-desc '( "restore_fast" ("file") ("Reads and returns expressios from the file " :arg "file"
              ". No checking for circular references is done.")))
(see-also-group '( "store" "restore" "store_fast" "restore_fast"))

(max-doc:implementation "store" "store uses the cl-store library. See the cl-store documentation for more information.")

(examples:clear-add-example "store" '(:pretext "Save a graph to a file. This cannot be done with the command <save>."
                       :code-res ( ("load(graphs)$" nil)
                                   ("c : petersen_graph();" "GRAPH(10 vertices, 15 edges)")
                                   ("factor(graph_charpoly(c,x));" "(x-3)*(x-1)^5*(x+2)^4")
                                   ("store(\"graph.cls\",c)$" nil)
                                   ("factor(graph_charpoly( restore(\"graph.cls\"), x));" "(x-3)*(x-1)^5*(x+2)^4"))))

