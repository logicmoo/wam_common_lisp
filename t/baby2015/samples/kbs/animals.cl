;;; -*- Mode:Lisp; Package:BABYLON; Base:10; Syntax:Common-Lisp -*-

(in-package "BABYLON")

;; ************* KNOWLEDGE BASE DECLARATION ************* 

(def-kb-instance animals animalsc)

;; ************** R U L E S ************

(defrule-set identify
  (identify1 ($and (animal has hair))
	     ($conclude (animal is mammal)))
  (identify2 ($and (animal gives milk))
	     ($conclude (animal is mammal)))
  (identify3 ($and (animal has feathers))
	     ($conclude (animal is bird)))
  (identify4 ($and (animal flies)
		   (animal lays eggs))
	     ($conclude (animal is bird)))
  (identify5 ($and (animal eats meat))
	     ($conclude (animal is carnivore)))
  (identify6 ($and (animal has pointed teeth)
		   (animal has claws)
		   (animal has forward eyes))
	     ($conclude (animal is carnivore)))
  (identify7 ($and (animal is mammal)
		   (animal has hoofs))
	     ($conclude (animal is ungulate)))
  (identify8 ($and (animal is mammal)
		   (animal chews cud))
	     ($conclude (animal is ungulate)
			(even toed)))
  (identify9 ($and (animal is mammal)
		   (animal is carnivore)
		   (animal has tawny color)
		   (animal has dark spots))
	     ($conclude (animal is cheetah)))
  (identify10 ($and (animal is mammal)
		    (animal is carnivore)
		    (animal has tawny color)
		    (animal has black stripes))
	      ($conclude (animal is tiger)))
  (identify11 ($and (animal is ungulate)
		    (animal has long neck)
		    (animal has long legs)
		    (animal has dark spots))
	      ($conclude (animal is giraffe)))
  (identify12 ($and (animal is ungulate)
		    (animal has black stripes))
	      ($conclude (animal is zebra)))
  (identify13 ($and (animal is bird)
		    (animal does not fly)
		    (animal has long neck)
		    (animal has long legs)
		    (animal is black and white))
	      ($conclude (animal is ostrich)))
  (identify14 ($and (animal is bird)
		    (animal does not fly)
		    (animal swims)
		    (animal is black and white))
	      ($conclude (animal is penguin)))
  (identify15 ($and (animal is bird)
		    (animal flys well))
	      ($conclude (animal is albatross))))



;; ********* I N S T R U C T I O N S ************

(instructions (send-kb :test-hypotheses
		     1.
		     '((animal is tiger)
		       (animal is cheetah)
		       (animal is giraffe)
		       (animal is ostrich)
		       (animal is penguin)
		       (animal is zebra)
		       (animal is albatross))
		     'identify)
	      (send-kb :print-hypotheses-verified)
	      (send-kb :print-true-facts))

;;; eof

