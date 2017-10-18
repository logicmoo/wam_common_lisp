;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")


(bab-require 'textrans)

(def-kb-configuration k3c
  (:procs normal-frame-mixin 
          normal-rule-mixin
	  normal-constraint-mixin 
          lisp-mixin free-text-mixin)
  (:interface normal-interface-mixin)
  (:special import-export-mixin))

;;; eof

