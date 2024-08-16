;;; Copyright (c) 2024 Bruno Dias


(defpackage #:cl-html5-query-selector.test
  (:use #:cl))

(in-package :cl-html5-query-selector.test)

(5am:def-suite cl-html5-query-selector.suite
  :description "cl-html5-query-selector test suite.")

(5am:in-suite cl-html5-query-selector.suite)

(5am:def-test no-match-on-empty-tree ()
  (5am:is (null (cl-html5-query-selector:query-selector ".no-element" (html5-parser:make-document)))))

(5am:def-test match-with-a-simple-class-name ()
  (let ((tree (html5-parser:parse-html5 "<div class=\"element\"></div>")))
    (5am:is (equal (car (cl-html5-query-selector:query-selector ".element" tree))
		   (html5-parser:node-last-child
		    (html5-parser:node-last-child
		     (html5-parser:node-last-child tree)))))))

(5am:def-test match-with-a-simple-id-name ()
  (let ((tree (html5-parser:parse-html5 "<div id=\"element\"></div>")))
    (5am:is (equal (html5-parser:node-last-child
		    (html5-parser:node-last-child
		     (html5-parser:node-last-child tree)))
		   (car (cl-html5-query-selector:query-selector "#element" tree))))))

(5am:def-test match-with-composed-class-name ()
  (let ((tree (html5-parser:parse-html5 "<div class=\"c1 c2\"></div>")))
    (5am:is (equal (html5-parser:node-last-child
		    (html5-parser:node-last-child
		     (html5-parser:node-last-child tree)))
		   (car (cl-html5-query-selector:query-selector ".c1.c2" tree))))))

(5am:def-test must-find-2-element-with-a-class-name ()
  (let ((tree (html5-parser:parse-html5 "<div><div class=\"element\"></div><div class=\"element\"></div></div>")))
    (5am:is (= 2 (length (cl-html5-query-selector:query-selector ".element" tree))))))

(5am:def-test must-find-element-with-tag-name ()
  (let ((tree (html5-parser:parse-html5 "<div><div class=\"element\"><strong>Ok</strong></div><div class=\"element\"><strong>Ok</strong></div></div>")))
    (5am:is (= 2 (length (cl-html5-query-selector:query-selector "strong" tree))))))
