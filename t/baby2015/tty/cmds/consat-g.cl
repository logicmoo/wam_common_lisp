;;;  -*-  Mode: Lisp; Syntax: Common-Lisp; Base: 10. ;Package: BABYLON -*-

(in-package "BABYLON")

;;------------------------------------------------------------------------
;;          deutsche kommandos fuer basic-constraint-mixin
;;------------------------------------------------------------------------


(defbabylon-entry consat cmd-table german 
  '(" Constraint-Operationen " :value (:open-menu :consat)
     :documentation "Constraint-Operationen "))


(defbabylon-entry consat-commands cmd-table  german
	  '(("Constraint-Definition"
	     :funcall read-constraint
	     :documentation "definiert ein neues Constraintes")
	    ("Constraint-Beschreibung"
	     :funcall display-constraint
	     :documentation "beschreibt ein definiertes Constraint")
	    ("lokale Constraint-Aktivierung"
	     :funcall satisfy-constraint-locally
	     :documentation "bewertet die lokale Aktivierung eines Constraints")
	    ("globale Constraint-Aktivierung"
	     :funcall satisfy-constraint-globally
	     :documentation "bewertet die globale Aktivierung eines Constraints")))


;;------------------------------------------------------------------------
;;          englische kommandos fuer mini-constraint-mixin
;;------------------------------------------------------------------------



(defbabylon-entry consat-trace-commands cmd-table german 
  '(("Trace Modus"
     :funcall trace-constraints
     :documentation "waehle Trace Modus")))

;; eof
