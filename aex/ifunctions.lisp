(in-package :maxima)
(mext:mext-optimize)
(max-doc:set-cur-sec 'max-doc::aex-fandv)
(defmfun1:set-mext-package "aex")

(defmfun-ae ($iapply :doc)  ( (fun :function)  (arg :non-atom :ensure-lex))
  :desc ( :mref "iapply" " is like maxima " :emref "apply" ", but it supports aex lists. "
  :arg "arg" " is converted to an ml if it is an aex expression. By default, output is ml 
 regardless of the input representation.")
  (unless ($listp arg)
    (merror1 (intl:gettext "iapply: second argument must be a list; found: ~M") arg))
  (defmfun-final-to-ae 
    (let ((fun-opr (getopr fun)))
            (autoldchk fun-opr)
            (mapply1 fun-opr (cdr arg) fun `(($apply) ,fun ,arg)))))

(examples::clear-examples "iapply")
(examples::add-example "iapply" 
                       '( :code "iapply(%%ff,lrange(4))")
                       '( :code "iapply(%%ff,lrange(4,[ot->ar]))")
                       '( :code "iapply(%%ff,lrange(4,[ot->ar]), [ot->ar] )")
                       '( :code "iapply(%%ff,lrange(4), [ot->ar] )"))


(defmfun-ae ($ireverse :doc) ( (e :non-atom) )
 "ireverse is like maxima reverse, but is works on both aex and list
  objects. ireverse is tries to be identical to maxima reverse for a non-aex argument."
  (defmfun-final-to-ae
    (if-aex e (progn
                (let ((oe ($aex_cp e)))
                  (setf (aex-arr oe) (nreverse (aex-arr oe))) 
                  oe))
            (progn
;;              (atomchk (setq e (format1 e)) '$reverse nil) ; we are now checking via defmfun1
              (mcons-exp-args e (reverse (margs e)))))))

(examples::clear-examples "ireverse")
(examples::add-example "ireverse" 
                       '( :code "ireverse(lrange(4))")
                       '( :code "ireverse(lrange(4), [ot->ar] )")
                       '( :code "ireverse(lrange(4, [ot->ar]) )")
                       '( :code "ireverse(lrange(4, [ot->ar]), [ot->ml] )"))

;;(defmfun1 ($icons :doc) (x (e :non-atom-list))
(defmfun $icons (x e)
;  :desc ( "This is like maxima " :mref "cons" ", but less general, and much, much faster.
;   It is suitable at a minimum, for pushing a number or list or string onto
;   a list of numbers, or strings or lists. If you find buggy behavior that you
;   are not interested in investigating, use " :mref "cons" " instead.")
  (cons (car e) (cons x (cdr e))))

(max-doc::set-cur-sec 'max-doc::lists-fandv)
(add-doc-entry1 :e  '(:name "icons"
                      :protocol "icons(x,e)"
                      :protocol-list ( "icons" ("X" "E") nil nil)
                      :contents
  ( :mref "icons" 
  " is like maxima " :emref "cons" ", but less general, and much, much faster. "
  :arg "x" " is a maxima object. " :arg "e" " is a maxima list or list-like object, such as "
  :code "[a]" ", or " :code "f(a)" ". It is suitable at a minimum, for pushing a number or list or string onto
 a list of numbers, or strings or lists. If you find " :mref "icons" " gives buggy behavior that you
 are not interested in investigating, use " :emref "cons" " instead.")))

(max-doc::implementation "icons" 
   "In a function that mostly only does icons in a loop,
   icons defined with defmfun rather than defmfun1 runs almost twice as fast. So icons is
   defined with defmfun rather than defmfun1. icons does no argument checking.")
