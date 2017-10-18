;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

(eval-when (eval load compile)
  (bab-require 'mktrans))

;;; the kads configuration

(def-kb-configuration kadsc
  (:procs normal-frame-mixin 
          ;normal-rule-mixin 
          ;normal-constraint-mixin 
          lisp-mixin 
          normal-prolog-mixin
          ;free-text-mixin
          )
  (:interface normal-interface-mixin)
  (:special import-export-mixin))


;;; eof

