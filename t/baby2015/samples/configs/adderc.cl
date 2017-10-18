;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")


(def-kb-configuration adderc
  (:procs mini-constraint-mixin lisp-mixin)
  (:interface normal-interface-mixin))


#-:FMCS(compile-$flavor-$methods adderc)

;;; eof

