;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-


(in-package "BABYLON")


(def$flavor mini-constraint-processor
	((trace nil))
	(basic-constraint-processor)
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables
  (:required-instance-variables meta-processor)
  (:documentation " Version des Constraint-Prozessors der Trace unterstuetzt"))


(def$method (mini-constraint-processor :trace-status) ()
  (if trace
      (format nil (getentry trace-on-fstr  babylon-io-table) "Consat")
      (format nil (getentry trace-off-fstr babylon-io-table) "Consat")))



#-:FMCS(compile-$flavor-$methods mini-constraint-processor)


;;; eof

