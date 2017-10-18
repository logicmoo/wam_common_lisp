;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-


(in-package "BABYLON")

;
;	TRACER
;


;
;	TRACE-MIXIN
;


(def$flavor constraint-trace-mixin	
	(name)
	()  
  :settable-instance-variables
  :initable-instance-variables	
  (:documentation "Flavor mit spezifischen Trace-Operationen"))


(def$method (constraint-trace-mixin :traced-p) ()
  "dient zum Test, ob ein gegebenes Constraint den trace-Mode besitzt oder nicht"
  t)


(def$method (constraint-trace-mixin :evaluate-expression)
	    (constraint-expr global-net-spec consistency-level)
  
  "sendet vor und nach der Evaluierung eine entsprechende
Protokollierungsnachricht an den Constraint-Prozessor
 
leider konnte kein :after-daemon  benutzt werden,
da dieser nicht auf das Ergebnis der aufgerufenen
Funktion zugreifen kann"
  
  (send-kb :protocol :enter
	   (cons name (make-local-value-ass
			(get-parameters constraint-expr)
			(get-parameters constraint-expr)
			global-net-spec)))
  
  (let* ((local-value-ass (global-to-local-subst
			    constraint-expr
			    global-net-spec))
	 (new-value-ass
	   (local-to-global-subst
	     constraint-expr
	     ($send self
		    :activate
		    local-value-ass
		    'initialize
		    (adapt-consistency-level consistency-level)))))
    
    (send-kb :protocol :exit (cons name new-value-ass))
    new-value-ass))



(def$method (constraint-trace-mixin :before :test-choices)
	    (variable value-set number-of-results)
  
  "sendet Protokollnachricht darueber, welche Wahl fuer die
Variable getroffen wurde"
  
  (declare (ignore number-of-results))
  (if (null value-set)
      (send-kb :protocol :fail variable)
      (send-kb :protocol :choice
	       (make-value-assoc variable value-set))))


;
;	TRACED CONSTRAINTS
;


(def$flavor traced-constraint
	()
	(constraint-trace-mixin
	 constraint)
 (:documentation "primitives Constraint mit Trace-Mixin"))


(def$flavor traced-constraint-net
	()
	(constraint-trace-mixin
	 constraint-net)
        (:documentation "Constraint-Netz mit Trace-Mixin")
        )


(def$method (constraint :trace-on) (c-name)
  
  "erzeugt ein Traced-Constraint, dass mit dem
Empfaenger in allen Komponenten (ausser name) uebereinstimmt"
  
  (make-$instance
    'traced-constraint
    :name c-name
    :interface interface
    :relation relation
    :condition condition))


(def$method (constraint :trace-off) (c-name)
  (declare (ignore c-name))
  self)


(def$method (traced-constraint :trace-on) (c-name)
  (declare (ignore c-name))
  self)


(def$method (traced-constraint :trace-off) (c-name)

  "erzeugt ein Constraint, dass mit dem Empfaenger in allen
Komponenten (ausser name) uebereinstimmt"
  
  (declare (ignore c-name))
  (make-$instance
    'constraint
    :interface interface
    :relation relation
    :condition condition))
  

(def$method (constraint-net :trace-on) (c-name)
  
  "erzeugt ein Traced-Constraint-Netz, dass mit dem
Empfaenger in allen Komponenten (ausser name) uebereinstimmt"

  (make-$instance
    'traced-constraint-net
    :name c-name
    :interface interface
    :net-spec net-spec
    :agenda agenda
    :stack stack))
  

(def$method (constraint-net :trace-off) (c-name)
  (declare (ignore c-name))
  self)
  

(def$method (traced-constraint-net :trace-on) (c-name)
  (declare (ignore c-name))
  self)


(def$method (traced-constraint-net :trace-off) (c-name)
  
  "erzeugt ein Constraint-Netz, dass mit dem
Empfaenger in allen Komponenten (ausser name) uebereinstimmt"

  (declare (ignore c-name))
  (make-$instance
    'constraint-net
    :interface interface
    :net-spec net-spec
    :agenda agenda
    :stack stack))




;;; eof

