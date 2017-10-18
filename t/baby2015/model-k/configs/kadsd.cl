;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;; load model-k specific translations

(eval-when (eval compile load)
  (bab-require 'mktrans))

;;; the kads dummy interface configuration

(def-kb-configuration kadsd
  (:procs normal-frame-mixin 
          ;normal-rule-mixin 
          ;normal-constraint-mixin 
          lisp-mixin 
          normal-prolog-mixin
          ;free-text-mixin
          )
  (:interface dummy-interface-mixin)
  (:special import-export-mixin))


;;; eof

