;; This file must be loaded as uncompiled lisp, at least with gcl.
;; Otherwise, the infix notation is not recognized by maxima

;; copied from translation of maxima code
;; Use of Rule is just to be compatible with mixima
;;infix("->")$
;;"->"(a,b) ::= buildq([a:a,b:b],Rule('a,b));
(in-package :maxima)

(meval '(($infix simp) "->"))
(meval '((MDEFMACRO SIMP) (($->) $A $B)
 (($BUILDQ) ((MLIST) ((MSETQ) $A $A) ((MSETQ) $B $B))
  ((|$Rule|) ((MQUOTE) $A) $B))))

(ddefun rule-opt (opt-name val)
 "Make an option specification as a Rule. For use when calling from lisp code.
  ***!! Note. We need to learn how to set the precedence."
;; (format t "Settign rule ~s~%" `((|$Rule| simp) ,opt-name ,val))
 `((|$Rule| simp) ,opt-name ,val))

(defprop |$Rule| msize-infix grind)
(defprop |$Rule| (#\- #\>) strsym)
