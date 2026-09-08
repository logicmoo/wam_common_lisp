;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Oct 17 20:08:18 2004
;;;; Contains: Tests of the ~? and ~@? format directives


(defun formatter-nothing (stream &rest args)
  (declare (ignore stream))
  args)

(defun formatter-aesthetic (stream arg &rest args)
  (princ arg stream)
  args)

(defun formatter-bra-aesthetic-ket (stream arg &rest args)
  (write-char #\< stream)
  (princ arg stream)
  (write-char #\> stream)
  args)

(def-format-test format.?.1
  "~?" ("" nil) "")

(def-format-test format.?.2
  "~?" ("~A" '(1)) "1")

(def-format-test format.?.3
  "~?" ("" '(1)) "")

(def-format-test format.?.4
  "~? ~A" ("" '(1) 2) " 2")

(def-format-test format.?.5
  "a~?z" ("b~?y" '("c~?x" ("~A" (1)))) "abc1xyz")

(def-format-test format.?.6
  "~?" (#'formatter-nothing nil) "")

(def-format-test format.?.7
  "~?" (#'formatter-aesthetic '(1)) "1")

(def-format-test format.?.8
  "~?" (#'formatter-nothing '(1)) "")

(def-format-test format.?.9
  "~? ~A" (#'formatter-nothing '(1) 2) " 2")

(def-format-test format.?.10
  "a~?z" ("b~?y" (list "c~?x" (list #'formatter-aesthetic '(1)))) "abc1xyz")

;;; Tests of ~@?

(def-format-test format.@?.1
  "~@?" ("") "")

(def-format-test format.@?.2
  "~@?" ("~A" 1) "1")

(def-format-test format.@?.3
  "~@? ~A" ("<~A>" 1 2) "<1> 2")

(def-format-test format.@?.4
  "a~@?z" ("b~@?y" "c~@?x" "~A" 1) "abc1xyz")

(def-format-test format.@?.5
  "~{~A~@?~A~}" ('(1 "~4*" 2 3 4 5 6)) "16")

(def-format-test format.@?.6
  "~@?" (#'formatter-nothing) "")

(def-format-test format.@?.7
  "~@?" (#'formatter-aesthetic 1) "1")

(def-format-test format.@?.8
  "~@? ~A" (#'formatter-bra-aesthetic-ket 1 2) "<1> 2")

(def-format-test format.@?.9
  "a~@?z" ("b~@?y" "c~@?x" #'formatter-aesthetic 1) "abc1xyz")
