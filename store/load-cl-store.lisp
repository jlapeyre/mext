(in-package :maxima)

;; obsolete

;; gcl can't use any of this yet.
;; ccl understands  (require :asdf), but not maxima built with ccl

(max-ql:load-asdf)

;; following line seems must be present with sbcl
;; unless the following line is present,
;;   (asdf:load-system :asdf),
;; but this latter line is uneccesary, it seems
#-gcl (asdf::clear-configuration)

;; rutils can't handle nested some nested structures. cl-store does serialization to
;; binary files of  everything we need
#-gcl (asdf:load-system :cl-store)
