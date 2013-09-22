;; These hacks allow the tex() command and imaxima to print
;; more objects that otherwise give an error.

;; GJL -- copied from imaxima.lisp by
;; Jesper Harder, Yasuaki Honda
(mext::no-warning
(defun verb-quote (str)
  (let ((var "") (charlist
		  '((#\Newline . "| \\\\ \\verb| "))))
    (dotimes (i (length str))
      (let ((chari (elt str i)))
	(setq var (concatenate 'string var 
			       (or (cdr (assoc chari charlist :test #'eql))
				   (string chari))))))
  var)))

;; GJL -- hacked from src/mactex.lisp by RJF to
;; print verbatim objects that we don't really know how to print:
;; arrays, structs, etc.
(mext::no-warning
(defun old-tex-atom (x l r)
  (append l
	  (list (cond ((numberp x) (texnumformat x))
		      ((and (symbolp x) (or (get x 'texword) (get (get x 'reversealias) 'texword))))
                      ((stringp x)
                       (tex-string (quote-% (if $stringdisp (concatenate 'string "``" x "''") x))))
                      ((characterp x) (tex-char x))
                      ((not ($mapatom x))
                       (verb-quote ($sconcat x)))
		      (t (tex-stripdollar (or (get x 'reversealias) x)))))
	  r)))

;(defvar *old-tex-atom* #'tex-atom)

;; GJL -- hacked from imaxima.lisp
(mext::no-warning
(defun tex-atom (x l r &aux other-case)
  (let ((result (append l
			(list (cond ((mstringp x) (texstring x))
				    ((characterp x) (texchar x))
				    (t (setq other-case t))))
			r)))
    (if other-case
        (if (not ($mapatom x))
            (progn
              (setf x (list (verb-quote ($sconcat x))))
              (tex-list x l r 'mparen))
;            (funcall *old-tex-atom* x l r))
            (old-tex-atom x l r))
      result))))

(mext::no-warning
(defun texstring (x)
  (let ((sym-name
	 (if (symbolp x)
	     (print-case-sensitive x)
	   x)))
    (cond ((equal sym-name "") "")
	  ((eql (elt sym-name 0) #\\) sym-name)
	  ((memq (elt sym-name 0) '(#\$ #\&))
	   (setq sym-name (subseq sym-name 1))
	   (concatenate 'string "\\verb|   " (verb-quote sym-name) "|"))
	  (t (concatenate 'string "\\verb|" (verb-quote sym-name) "|"))))))
