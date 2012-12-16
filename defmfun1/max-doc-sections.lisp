;;; Register max-doc sections here.
;;; To be clear: These are not the standard maxima info documentation sections,
;;; but rather a separate database. We also modified the documentation printing
;;; via ? and ?? to use both (as well as other) databases simultaneously.

;;; We use in-package so we don't qualify symbols here.
;;; In the function definition files, we choose not to use-package :max-doc,
;;; but rather use qualied symbols. I think it makes the code easier to understand.
;;; Trying to redfine with add-doc-sec is currently set to fail silently, but this
;;; could be changed. For now, we only use add-doc-sec in this file. Elsewhere, we
;;; use set-cur-sec to choose one of these

;;; We could put these definitions in the files that define the
;;; functions. Eg., put the Lists section definition in the file
;;; lists.lisp. But list functions are defined in other files
;;; too, such as table.lisp. If we were to do this, then the
;;; files would have to be loaded in a particular order. Of
;;; course, they have to be loaded in a particular order anyway,
;;; but this this adds one more thing to the dependency puzzle.
;;; So one solution is to put all the section definitions here and
;;; to load them immediately after max-doc.lisp, so that the files
;;; that use them can be loaded in any order.

(in-package :max-doc)

;;(add-doc-sec '( :tag maxima::$misc :name  "Miscellaneous Functions"))

(add-doc-sec '( :tag misc-fandv 
                :name  "Miscellaneous Functions"
                :shortname "fvmisc"))

(add-doc-sec '( :tag lists-fandv 
                :name "Functions and Variables for Lists"
                :shortname "fvlists"
  :contents
  "These functions manipulate lists. They build lists, take them apart, select elements, etc."))

(add-doc-sec '( :tag aex-core 
                :name "Array Representation For Expressions"
                :shortname "aex"
   :contents
  "Maxima expressions are normally implemented internally as lisp lists,
   but they may also be represented by lisp arrays. Each representation has
   advantages."))

(add-doc-sec '( :tag aex-fandv 
                :name "Functions and Variables for Array Represention for Expressions"
                :shortname "fvaex"
   :contents
 "These functions operate on the the array expression data structure."))

;; combinatorics.lisp
(add-doc-sec '( :tag combinatorics-fandv :name "Functions and Variables for Combinatorics"
                :shortname "fvcombinatorics"))

;; descr1.lisp
(add-doc-sec '( :tag doc-fandv :name "Functions and Variables for Documentation"
                :shortname "fvdocumentation"))

;; lisp-numeric.lisp
(add-doc-sec '( :tag numerics-fandv :name "Functions and Variables for Numerics"
                :shortname "fvnumerics"
     :contents "These are mathematical functions--- cos,sin,etc. ---that accept only
 numerical arguments. Tests of loops in untranslated code show that these are much
 more efficient than using the standard maxima versions. But, for most applications, the
 standard maxima versions are probably ok."))

;; max-doc-entries.lisp
(add-doc-sec '( :tag io-fandv :name "Functions and Variables for Input and Output"
                :shortname "fvio"))

;; max-doc-entries.lisp
(add-doc-sec '( :tag options :name "Options" :shortname "options"
    :contents "Options to a function in the aex-maxima distribution are passed as follows:

    funcname(x,y, [optname -> optval, optname2 -> optval2])
    or
    funcname(x,y, optname -> optval, optname2 -> optval2)

 The standard options described in this section are some options that are supported by
 many functions in the aex-maxima distribution."))

(add-doc-sec '( :tag attributes :name "Attributes" :shortname "attributes"
    :contents "A function may possess a list of attributes. The attributes control how the arguments
 to the function are evaluated and how errors are handled."))

;; strings.lisp
(add-doc-sec '( :tag strings-fandv :name "Functions and Variables for Strings"
                :shortname "fvstrings"))

;; misc-util.lisp.  Don't know if we are using this
(add-doc-sec '( :tag misc-util :name "Miscellaneous utilities"
                         :shortname "miscutils"))

(add-doc-sec '( :tag func-def-fandv :name "Functions and Variables for Function Definition"
                         :shortname "fvfunctiondefinition"))

(add-doc-sec '( :tag program-flow-fandv :name "Functions and Variables for Program Flow"
                         :shortname "fvflow"))

(add-doc-sec '( :tag predicates-fandv :name "Functions and Variables for Predicates"
                         :shortname "fvpredicates"))

(add-doc-sec '( :tag number-theory-fandv :name "Functions and Variables for Number Theory"
                :shortname "fvnumbertheory"))

(add-doc-sec '( :tag equations-fandv :name "Functions and Variables for Equations"
                :shortname "fvequations"))

(add-doc-sec '( :tag runtime-fandv :name "Functions and Variables for Runtime Environment"
                :shortname "fvruntime"))

(add-doc-sec '( :tag quicklisp-fandv :name "Functions and Variables for Quicklisp"
                :shortname "fvquicklisp"))
