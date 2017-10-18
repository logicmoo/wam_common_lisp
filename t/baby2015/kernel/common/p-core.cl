;;; -*- Mode: Lisp; Base:10; Syntax: Common-Lisp; Package: BABYLON -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHOR:   J. W A L T H E R, E. G R O S S

(def$flavor processor-core
	   ((meta-processor nil)
	    (alternate-meta-processor nil))
	   ()
  :settable-instance-variables
  (:documentation "This is the standard processor-core flavor.
It provides the interface to the meta-processor or meta-processor-stub."))

(def$method (processor-core :switch-mode) ()
  "Switch between standalone and integrated application of the processor."
  (let ((temp alternate-meta-processor))
    (when temp
      (setf alternate-meta-processor meta-processor)
      (setf meta-processor temp))))
