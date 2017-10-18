;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-


(in-package "BABYLON")

;
;
;		constraint processor
;
;


;
;	CONSTRAINT-PROCESSOR
;


(def$flavor basic-constraint-processor
	(meta-processor)
	(constraint-base
	 processor-core)
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables
  (:documentation "Umgebung fuer Constraint-Prozessor:
	           enthaelt eine Liste mit primitiven und
	           mit zusammengesetzten Constraints"))


(def$method (basic-constraint-processor :reset-proc) ()

  "setzt alle Constraint-Netze zurueck."

  (mapc (function
	  (lambda (constraint-assoc)
	    ($send (get-object-of-c-assoc constraint-assoc)
                          :reset-state)))
	constraint-nets))


(def$method (basic-constraint-processor :print)
	    (&optional (stream *default-dialog-stream*))
  
  "gibt alle definierten Constraints in wiedereinlesbarer Form aus."
  
  (terpri stream)
  (princ  ";; ************ C O N S T R A I N T S ************" stream)
  (terpri stream)
  (terpri stream)
  (print-constraint-list constraints stream)
  (print-constraint-list constraint-nets stream))


(def$method (basic-constraint-processor :kb-inform) (stream)
  
  "gibt die Zahl der primitiven und zusammengesetzten Constraints aus." 
  
  (terpri stream)
  (format stream (getentry number-of-primitives constraint-io-table)
          (length constraints))
  (terpri stream)
  (format stream (getentry number-of-nets constraint-io-table)
          (length constraint-nets)))




(def$method (basic-constraint-processor :get) (c-name)

  " ermittelt das primitive oder zusammengesetzte Constraint mit
    dem angegebenen Namen
    (Beachte: ein Netz und ein primitives Constraint duerfen nicht
    	      den gleichen Namen besitzen)"

  (let ((primitive-c-assoc (assoc c-name constraints))
	(compound-c-assoc (assoc c-name constraint-nets)))

    (cond ((get-object-of-c-assoc primitive-c-assoc))
	  ((get-object-of-c-assoc compound-c-assoc))
	  (t nil))))


#-:FMCS(compile-$flavor-$methods basic-constraint-processor)


;;; eof

