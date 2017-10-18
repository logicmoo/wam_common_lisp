;;; -*- Mode: LISP; Syntax: Common-Lisp; Base:10; Package BABYLON  -*- 

(in-package "BABYLON")

(defpackage "TIME"
  (:USE #+:AKCL "LISP" #-:AKCL "COMMON-LISP")
  (:EXPORT "PRINT-UNIVERSAL-TIME"
           "PRINT-CURRENT-TIME"
           "PARSE-UNIVERSAL-TIME"
           "TIME-DIFFERENCE"))

(bab-require 'time)

;;; ------------------------------------------------------------------------------
;;; Knowledge-base configuration extrac
;;; ------------------------------------------------------------------------------

(def-kb-configuration extrac
  (:procs normal-frame-mixin
          normal-rule-mixin
          lisp-mixin)
  (:interface normal-interface-mixin))

;;; eof


