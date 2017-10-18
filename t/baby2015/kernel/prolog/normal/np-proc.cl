;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHOR:   E. G R O S S

(def$flavor normal-prolog-processor
           ()
	(proc-explain-mixin
	 ax-develop-mixin
	 mini-prolog-processor)  
  :settable-instance-variables
  (:documentation "Flavor combining the prolog-interpreter with optional components.
The available mixins include prolog-trace-mixin for tracing, proc-explain-mixin for
explanations and ax-develop-mixin for development support."))


(def$method (normal-prolog-processor :after :init) (&rest plist)
  (declare (ignore plist))
  (setf root-type 'mini-goalbox))



#-:FMCS(compile-$flavor-$methods normal-prolog-processor)

;;; eof

