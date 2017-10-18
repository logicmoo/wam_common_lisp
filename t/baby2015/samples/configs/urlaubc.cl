;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

(def-kb-configuration urlaubc
  (:procs normal-frame-mixin normal-rule-mixin normal-prolog-mixin
	  lisp-mixin free-text-mixin)
  (:interface normal-interface-mixin))


#-:FMCS(compile-$flavor-$methods urlaubc)

;;; eof

