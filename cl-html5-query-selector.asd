;;; Copyright (c) 2024 Bruno Dias


(asdf:defsystem #:cl-html5-query-selector
  :description "A query selector for cl-html5-parser."
  :author "Bruno Dias"
  :license  "Unlicense"
  :version "0.0.1"
  :serial t
  :depends-on (#:str
	       #:cl-ppcre
	       #:cl-html5-parser)
  :components ((:file "package")))
