;;;; pg-text.asd
(defpackage #:pg-text
  (:use #:cl #:asdf)
  (:export #:rank
           #:make-suffix-array
           #:bwt
           #:wavelet-tree
           #:restore-wavelet-tree
           #:levenshtein-distance))

(in-package :pg-text)

(asdf:defsystem #:pg-text
  :serial t
  :description "Text processing library implementing useful functions"
  :version "0.0.1"
  :author "Picogram <picogram3@gmail.com>"
  :license "MIT"
  :components (
               (:file "bwt")
	       (:file "wavelet-tree")
	       (:file "levenshtein")))

