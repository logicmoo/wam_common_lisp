;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Sep  1 07:57:02 2003
;;;; Contains: Aux. functions for testing /



(defun divide-by-zero-test (&rest args)
  (handler-case
   (progn (apply #'/ args) (values))
   (division-by-zero () (values))
   (condition (c) c)))
