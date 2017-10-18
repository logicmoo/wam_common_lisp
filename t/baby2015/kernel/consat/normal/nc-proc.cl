;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-


(in-package "BABYLON")




(def$flavor normal-constraint-processor
	()
	(restriction-base mini-constraint-processor)
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables
  (:required-instance-variables meta-processor)
  (:documentation " Version des Constraint-Prozessors
                    der Restrictions unterstuetzt"))



(def$method (normal-constraint-processor :after :reset-proc) ()

    "setzt alle Restrictions zurueck"

  (mapc (function
	  (lambda (constraint-assoc)
	    ($send (get-object-of-c-assoc constraint-assoc)
                          :redefine-one)))
	restriction-nets))



(def$method (normal-constraint-processor :after :print)
	    (&optional (stream *default-dialog-stream*))
  
  "gibt alle definierten Restrictions in wiedereinlesbarer Form aus."
  
  (print-constraint-list restriction-nets stream))


(def$method (normal-constraint-processor :after :kb-inform) (stream)
  
  "gibt die Zahl der Restrictions aus"

  (terpri stream)
  (format stream (getentry number-of-restrictions
		      constraint-io-table)
	  (length restriction-nets)))



(def$method (normal-constraint-processor :get) (c-name)

  "ermittelt das primitive oder zusammengesetzte Constraint mit
   dem angegebenen Namen
    (Beachte: ein Netz und ein primitives Constraint duerfen nicht
    	      den gleichen Namen besitzen)"

  (let ((primitive-c-assoc (assoc c-name constraints))
	(compound-c-assoc (assoc c-name constraint-nets)))

    (cond ((get-object-of-c-assoc primitive-c-assoc))
	  ((get-object-of-c-assoc compound-c-assoc))
	  (($send self :get-restrictions c-name))
	  (t nil))))

#-:FMCS(compile-$flavor-$methods normal-constraint-processor)

;;; eof

