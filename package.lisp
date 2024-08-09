;;; Copyright (c) 2024 Bruno Dias


(defpackage #:cl-html5-query-selector
  (:use #:cl)
  (:export
   #:query-selector))

(in-package :cl-html5-query-selector)

(defun match-selector (selector target)
  (when target
    (case (car selector)
      (:and (reduce (lambda (acc selector)
                      (and acc (match-selector selector target)))
                    (cdr selector)
                    :initial-value t))
      (t (destructuring-bind (kind . value)
             selector
           (case kind
             (:id
              (string-equal value
                            (html5-parser:element-attribute target "id")))
             (:class
              (let ((class-names (str:words (html5-parser:element-attribute target "class"))))
                (member value class-names :test #'string-equal)))))))))

(defun reduce-tree (fn tree &key initial-value)
  (etypecase tree
    (html5-parser::document
     (reduce (lambda (acc tree) (reduce-tree fn tree :initial-value acc))
             (html5-parser::%node-child-nodes tree)
             :initial-value nil))
    (html5-parser::element
     (reduce (lambda (acc tree) (reduce-tree fn tree :initial-value acc))
             (html5-parser::%node-child-nodes tree)
             :initial-value (funcall fn initial-value tree)))
    (html5-parser::text-node (funcall fn initial-value tree))
    (t initial-value)))

(defun make-selector (selector)
  (cons (case (char selector 0) (#\. :class) (#\# :id))
        (subseq selector 1)))

(defun specialized-selector (name)
  (let ((selectors (cl-ppcre:all-matches-as-strings "[\\#\\.]([\\w\\-\\_0-9]+)" name)))
    (reduce (lambda (acc selector)
              (append acc (list (make-selector selector))))
            (cdr selectors)
            :initial-value (list (make-selector (car selectors))))))

(defun parse-selector (name)
  (let ((groups (str:words name)))
    (cons :and (reduce (lambda (acc selector)
                         (list* acc (specialized-selector selector)))
                       (cdr groups)
                       :initial-value (specialized-selector (car groups))))))

(defun query-selector (selector tree)
  (let ((matcher (parse-selector selector)))
    (reduce-tree (lambda (acc element)
                   (etypecase element
                     (html5-parser::text-node acc)
                     (t (progn
                          (when (match-selector matcher element)
                            (setf acc (append acc (list element))))
                          acc))))
                 tree)))
