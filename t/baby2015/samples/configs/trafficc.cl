;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

(def-kb-configuration trafficc
  (:procs normal-frame-mixin normal-rule-mixin normal-constraint-mixin
	  lisp-mixin normal-prolog-mixin free-text-mixin)
  (:interface normal-interface-mixin))


#-:FMCS(compile-$flavor-$methods trafficc)

;;; eof

