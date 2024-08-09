;;; Copyright (c) 2024 Bruno Dias


(asdf:defsystem #:cl-html5-query-selector.test
  :description "cl-html5-query-selector tests."
  :author "Bruno Dias"
  :license  "Unlicense"
  :version "0.0.1"
  :serial t
  :depends-on (#:fiveam
               #:cl-html5-query-selector)
  :components ((:file "test")))
