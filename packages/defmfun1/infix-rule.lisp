;;;  Copyright (C) (2012,2013) John Lapeyre. Licensed under GPL, v3 or greater. See the file
;;;  `LICENSE' in this directory.

;; This file must be loaded as uncompiled lisp, at least with gcl.
;; Otherwise, the infix notation is not recognized by maxima

(in-package :maxima)

;; copied from translation of maxima code
;; Use of Rule is just to be compatible with mixima
;;infix("->")$
;;"->"(a,b) ::= buildq([a:a,b:b],Rule('a,b));
(meval '(($infix simp) "->"))
(meval '((MDEFMACRO SIMP) (($->) $A $B)
 (($BUILDQ) ((MLIST) ((MSETQ) $A $A) ((MSETQ) $B $B))
  ((|$Rule|) ((MQUOTE) $A) $B))))

(mext::no-warning
(ddefun rule-opt (opt-name val)
 "Make an option specification as a Rule. For use when calling from lisp code.
  ***!! Note. We need to learn how to set the precedence."
;; (format t "Settign rule ~s~%" `((|$Rule| simp) ,opt-name ,val))
 `((|$Rule| simp) ,opt-name ,val)))

(defprop |$Rule| msize-infix grind)
(defprop |$Rule| (#\- #\>) strsym)
