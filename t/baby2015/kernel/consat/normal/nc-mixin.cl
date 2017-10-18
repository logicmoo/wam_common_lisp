;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-


(in-package "BABYLON")

;
;	mini-constraint-mixin
;


(def$flavor normal-constraint-mixin
	    ()
  (restriction-base mini-constraint-mixin)
  
  :settable-instance-variables
  (:required-instance-variables procs kb-name)
;  (:required-flavors kb-processor-core)
  (:documentation "Anteil des Constraint-Systems am Metaprozessor")
  )



(def$method (normal-constraint-mixin :generate-constraint-processor) ()
  
  " erzeugt einen Constraint-Prozessor "
  
  (setf constraint-processor
	(make-$instance 'normal-constraint-processor
			:meta-processor self)))

(def$method (normal-constraint-mixin :after :new&delete-restriction) (&rest ignore)
  "runterreichen der definierten Restrictions "
  (declare (ignore ignore))
  ($send constraint-processor :set-restriction-nets  restriction-nets))

(assign-typefkt 'constraint-type 'normal-constraint-mixin)


;;; eof

