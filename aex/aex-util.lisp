;;; Copyright (C) 2012 John Lapeyre
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities for loading and compiling and testing aex

(in-package :maxima)
(mext:mext-optimize)

;; These are probably obsolete

(max-doc::set-cur-sec 'max-doc::misc-fandv)
(defmfun1:set-file-and-package "aex-util.lisp" "aex")

;; Fix hard coded paths here!!
;; run testsuite
(defmfun1 ($aetest :doc)  ()
 "Run a test suite for the aex-maxima functions"
  (setf $testsuite_files
        '((mlist simp)
          "./rtest_aex.mac"
          "./rtest_table.mac"
          "./rtest_take.mac"
          "./rtest_list.mac"
          "./rtest_afuncs.mac"))
  ($run_testsuite))

;; cat all files into one lisp file for compilation
;; Note that function is not defined anywhere aex-append-lisp-ext
(defmfun $cat_aex_files ()
  (let ((fst))
    (dolist (f *aex-lisp-files*)
      (setf f (concatenate 'string " " (aex-append-lisp-ext f) " "))
      (setf fst (cons f fst)))
    (setf fst (nreverse fst))
    (setf fst (apply #'concatenate (cons 'string fst)))
    ($system (concatenate 'string "cat " fst " > all-aex.lisp "))))

;; cat all files together, compile, and load
(defmfun $aex_compile ()
  ($cat_aex_files)
  ($comp_load "./all-aex"))

(defmfun1 ($aex_compile_each :doc) ()
 "Compile and load all of the aex-maxima code. This is probably
 most  useful when running maxima with gcl. If this command is run
 before installing the aex-maxima code, then the compiled code will be
 loaded when load(aex) is run."
  (dolist (f *aex-lisp-files*)
    (if (null ($comp_load f))
      (progn
        (format t "Compiling and loading '~a' failed.~%" f) (return nil)))))
