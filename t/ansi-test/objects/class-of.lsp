;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jun 16 19:40:32 2003
;;;; Contains: Tests of CLASS-OF



;;; Most tests of CLASS-OF are in other files

(deftest class-of.error.1
  (signals-error (class-of) program-error)
  t)

(deftest class-of.error.2
  (signals-error (class-of nil nil) program-error)
  t)
