;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Base:10; Package: BABYLON -*-

(in-package "BABYLON")

(def-kb-instance reactor reactorc)

;; ************* O B J E C T S ***********

(defframe cooling-system
    (slots (pressure -
            :possible-values (:one-of decreasing increasing))
           (integrity -
            :possible-values (:one-of challenged ok))
           (temperature -
            :possible-values (:one-of decreasing increasing))
           (heat-transfer -
            :possible-values (:one-of adequate inadequate)
	    :aspect - )))



(definstance primary-cooling-system of cooling-system)

(definstance secondary-cooling-system of cooling-system)

(defframe high-pressure-injection-system
    (slots (status -
            :possible-values (:one-of on off))))



(definstance hpis of high-pressure-injection-system)

(defframe steam-generator
    (slots (level -
            :possible-values (:one-of decreasing increasing))
           (inventory -
            :possible-values (:one-of adequate inadequate))
           (steam-flow -
            :possible-values (:one-of high low))))



(definstance steam-generator1 of steam-generator)

(defframe containment-vessel
    (slots (radiation -
            :possible-values (:one-of high low))
           (pressure -
            :possible-values (:one-of high low))
           (integrity -
            :possible-values (:one-of challenged ok))))



(definstance containment-vessel1 of containment-vessel)

(defframe feedwater-pump
    (slots (flow -
            :possible-values (:one-of low high))))



(definstance feedwater-pump1 of feedwater-pump)

(defframe accident
    (slots (type -
            :possible-values (:one-of loss-of-feedwater
                                      loss-of-coolant
                                      steam-generator-tube-rupture
                                      steam-line-break))))


(definstance accident1 of accident)


;; ************** R U L E S ************

(defrule-set :check
             (rule1 ($and (primary-cooling-system pressure = decreasing)
			  (hpis status = on))
                    ($conclude (primary-cooling-system integrity = challenged)))
             (rule2 ($and (primary-cooling-system temperature = increasing))
                    ($conclude (secondary-cooling-system heat-transfer = inadequate)))
             (rule3 ($and (steam-generator1 level = decreasing))
                    ($conclude (steam-generator1 inventory = inadequate)))
             (rule4 ($and (containment-vessel1 radiation = high)
                          (containment-vessel1 pressure = high))
                    ($conclude (containment-vessel1 integrity = challenged)))
             (rule5 ($and (secondary-cooling-system heat-transfer = inadequate)
                          (feedwater-pump1 flow = low))
                    ($conclude (accident1 type = loss-of-feedwater)))
             (rule6 ($and (steam-generator1 inventory = inadequate)
                          (feedwater-pump1 flow = low))
                    ($conclude (accident1 type = loss-of-feedwater)))
             (rule7 ($and (primary-cooling-system integrity = challenged)
                          (containment-vessel1 integrity = challenged))
                    ($conclude (accident1 type = loss-of-coolant)))
             (rule8 ($and (primary-cooling-system integrity = challenged)
                          (steam-generator1 level = increasing))
                    ($conclude (accident1 type = steam-generator-tube-rupture)))
             (rule9 ($and (steam-generator1 inventory = inadequate)
                          (steam-generator1 steam-flow = high))
                    ($conclude (accident1 type = steam-line-break))))

(defrule-set :output
	     (rule1 ($and (accident1 type = loss-of-feedwater))
		    ($conclude (say "~%the type of accident is loss of feedwater.")))
             (rule2 ($and (accident1 type = loss-of-coolant))
		    ($conclude (say "~%the type of accident is loss of coolant.")))
	     (rule3 ($and (accident1 type = steam-generator-tube-rupture))
		    ($conclude
		      (say "~%the type of accident is steam generator tube rupture.")))
	     (rule4 ($and (accident1 type = steam-line-break))
		    ($conclude (say "~%the type of accident is steam line break."))))



;; ********* I N S T R U C T I O N S ************

(instructions ($send $self :obtain 1. '(accident1 type) :check)
	      ($send $self :print-true-facts)
              ($send $self :find-implications :output))

;;; eof

