;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10 -*-

(in-package "BABYLON")

;;           COPYRIGHT   1984, 1985, 1986    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHORS:  Erich Rome, Eckehard Gross

;;    provisorisch   rest in np-expl-rest


;;;;;;;;;;;;;;;;; MIXIN PROVIDING EXPLANATION FACILITIES ;;;;;;;;;;;;;;;

(def$flavor proc-explain-mixin
	   ()()	
  :settable-instance-variables
  (:required-instance-variables meta-processor root)
  (:documentation "mixin for prolog-processor providing explanation facilities."))


;;;;;;;;;;;;;;; CONTEXT EXPLANATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def$method (proc-explain-mixin :get-current) ()
  "returns current goalbox."
  ($send root :get-current))

(def$method (proc-explain-mixin :why-goal) (goalbox)
  "displays the goal and the clause currently used to prove it."
  ($send meta-processor :send-explanation-window :format
		(getentry needed-to-prove-fstr prolog-io-table)
		($send goalbox :get-goal-on-init 'normal))
  (let ((clause-used (ext-rep-clause ($send goalbox :clause-used))))
    (if clause-used
	($send meta-processor :send-explanation-window :format
		      (getentry by-clause-fstr prolog-io-table)
		      (first clause-used)
		      (second clause-used)
		      (cddr clause-used)))))

(def$method (proc-explain-mixin :explain-kontext) ()
  "explains what the current goal is used for."
  (prog ((current-goal ($send self :get-current))
	 (ask t) choice)

	($send meta-processor :send-explanation-window :format
		      (getentry subgoals-fstr prolog-io-table)
		      (subst-prolog-vars ($send current-goal :goal) 'normal))
	
     A  (setq current-goal ($send current-goal :parent-goal))
	(cond ((equal ($send current-goal :goal) '(%top))
	       ($send meta-processor :send-explanation-window :format 
			     (getentry topgoal-reached-fstr prolog-io-table))
	       (return 'done)   ;; zuvor: (go B)
	       ))
	($send self :why-goal current-goal)
	(cond (ask
	       (setq choice
		     ($send meta-processor :choose-from-menu
				   (getentry why-item-list prolog-io-table)
				   (getentry further-explanation-str prolog-io-table)))
	       (case choice
		 (why-goal (go A))		 
		 (why-path (setq ask nil)(go A))
		 (exit (return 'done)  
		       )))
	      (t (go A)))))


;;;;;;;;; CONTEXT EXPLANATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ==> basic>xx

(def$method (basic-goalbox :get-current) ()
  "returns current goalbox."
  (cond (curr-subgoal ($send curr-subgoal :get-current))
	(t self)))

(def$method (basic-goalbox :get-goal-on-init) (mode)
  (rest-subst-prolog-vars goal mode init-env-depth))


(def$method (basic-goalbox :clause-used) ()
  (unless (member (first goal)  *prolog-syspreds*)
    (first clauses)))