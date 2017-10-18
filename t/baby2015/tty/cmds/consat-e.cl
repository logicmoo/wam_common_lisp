;;;  -*-  Mode: Lisp; Syntax: Common-Lisp; Base: 10. ;Package: BABYLON -*-

(in-package "BABYLON")

;;------------------------------------------------------------------------
;;          englische kommandos fuer basic-constraint-mixin
;;------------------------------------------------------------------------


(defbabylon-entry consat cmd-table english 
  '(" Constraint Operations " :value (:open-menu :consat)
     :documentation "Constraint Operations "))


(defbabylon-entry consat-commands cmd-table english
	  '(("Define Constraint"
	     :funcall read-constraint
	     :documentation "defines a new constraint")
	    ("Display Constraint"
	     :funcall display-constraint
	     :documentation "displays description of a defined constraint")
	    ("Satisfy Constraint Locally"
	     :funcall satisfy-constraint-locally
	     :documentation "computes the maximal locally consistent solution")
	    ("Satisfy Constraint Globally"
	     :funcall satisfy-constraint-globally
	     :documentation "computes globally consistent solutions")))



;;------------------------------------------------------------------------
;;          englische kommandos fuer mini-constraint-mixin
;;------------------------------------------------------------------------



(defbabylon-entry consat-trace-commands cmd-table english 
  '(("Trace Mode"
     :funcall trace-constraints
     :documentation "choose trace mode")))

;; eof
