;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

(eval-when (eval load compile)
  (bab-require 'mktrans))

;;; the floorc configuration file

(def-kb-configuration floorc
  (:procs normal-frame-mixin 
          ;normal-rule-mixin 
          normal-constraint-mixin 
          lisp-mixin 
          normal-prolog-mixin)
  (:interface normal-interface-mixin)
  (:special import-export-mixin))

;;; eof

