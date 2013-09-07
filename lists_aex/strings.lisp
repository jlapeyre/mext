(in-package :maxima)

(mext:mext-optimize)
(max-doc::set-cur-sec 'max-doc::strings-fandv)
(defmfun1:set-file-and-package "strings.lisp" "lists_aex")

(if (find-package :maxima-take ) t (defpackage :maxima-take (:use :common-lisp )))
(in-package :maxima-take)
(use-package :gjl.lisp-util)

;; copy a substring skipping characters and stepping either up or down

(defmacro mk-string-sub (name sign)
  (let ((np (if (eq 'neg sign) t nil)))
    `(defun ,name (s i1 i2 i3)
       ,@(when np `((setf i3 (- i3))))
       (let* ((n1 (truncate (1+ (- ,@(if np `(i1 i2) `(i2 i1)))) i3))
              (s1 (make-string n1)))
         (loop for i from ,@(if np `((1- i1) downto i2) `(i1 to i2)) by i3
            for j from 0 to (1- n1)  do
            (setf (aref s1 j) (aref s i)))
         s1))))

(mk-string-sub string-sub-pos pos)
(mk-string-sub string-sub-neg neg)

;; TODO in both string_take and string_drop. Check that sequence specification is
;; valid. Currently a lisp error is raised by subseq
(maxima::defmfun1 ( maxima::$string_take :doc) ((s :string :thread) (spec :seq-spec))
  (dbind (i1 i2 i3 n) (maxima-take::process-seq-spec-0 s spec)
         (declare (ignore n))
         (cond ((= 1 i3)
                (subseq s i1 i2))
               ((= -1 i3)
                (reverse (subseq s i2 i1)))
               ((< i3 0)
                (string-sub-neg s i1 i2 i3))
               (t  ; i3 is positive
                (string-sub-pos s i1 i2 i3)))))

(max-doc::add-call-desc '("string_take" ("s" "n") ("returns a string of the first " :arg "n"
                          " characters of the string " :arg "s" "."))
                        '("string_take" ("s" ("lit" "-n" )) ("returns a string of the last " :arg "n"
                                " characters of " :arg "s" ".")))

(examples::clear-examples "string_take")
(examples::add-example "string_take" '( :code "string_take(\"dog-goat-pig-zebra\",[5,12])"))


(maxima::defmfun1 ( maxima::$string_reverse :doc) ((s :string :thread))
  (reverse s))

(max-doc::add-call-desc '("string_reverse" ("s") ("returns a copy of string " :arg "s"
                          " with the characters in reverse order.")))

;  "string_drop is only partially implemented, but the following examples work."
(maxima::defmfun1 ( maxima::$string_drop :doc) ((s :string :thread) (spec :seq-spec))
  (dbind (i1 i2 i3 n) (maxima-take::process-seq-spec-0 s spec)
         (declare (ignore n))
         (cond ((= 1 i3)
                (concatenate 'string (subseq s 0 i1) (subseq s i2 (length s))))
               (t
                (maxima::merror1 "string_drop: increments not equal to one not supported.")))))

(examples::clear-examples "string_drop")
(examples::add-example "string_drop" 
                       '( :code " string_drop(\"abracadabra\",1)")
                       '( :code " string_drop(\"abracadabra\",-1)")
                       '( :code " string_drop(\"abracadabra\",[2,10])"))
