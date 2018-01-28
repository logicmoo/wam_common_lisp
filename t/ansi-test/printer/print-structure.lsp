;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed May 26 22:19:52 2004
;;;; Contains: Printing tests for structures

(in-package :cl-test)


(defstruct print-struct-1
  foo bar)

(deftest print-structure.1
  (let ((s (make-print-struct-1 :foo 1 :bar 2)))
    (with-standard-io-syntax
      (let ((*tst-pkg* (find-package "CL-TEST"))
            (*kwd-pkg* (find-package "KEYWORD")))
       (let ((str (write-to-string s :readably nil :case :upcase :escape nil)))
         (assert (string= (subseq str 0 3) "#S("))
         (let ((vals (read-from-string (subseq str 2))))
           (assert (listp vals))
           (assert (= (length vals) 5))
           (assert (eq (car vals) 'print-struct-1))
           (assert (symbolp (second vals)))
           (assert (symbolp (fourth vals)))
           (assert (eql *tst-pkg* (symbol-package (first vals))))
           (assert (eql *kwd-pkg* (symbol-package (second vals))))
           (assert (eql *kwd-pkg* (symbol-package (fourth vals))))
           (cond
            ((string= (symbol-name (second vals)) "FOO")
             (assert (string= (symbol-name (fourth vals)) "BAR"))
             (assert (= (third vals) 1))
             (assert (= (fifth vals) 2)))
            (t
             (assert (string= (symbol-name (second vals)) "BAR"))
             (assert (string= (symbol-name (fourth vals)) "FOO"))
             (assert (= (third vals) 2))
             (assert (= (fifth vals) 1))))
           nil)))))
  nil)
