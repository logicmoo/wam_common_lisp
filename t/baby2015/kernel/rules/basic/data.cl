;;; -*- Mode: Lisp; Syntax:  Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; DATE:     uralt
;; AUTHORS:  Franco di Primio, Juergen Walther

;; This file depends on:  common>*

;; contens: a flavor managing the dynamic data base of the rule interpreter.



;;-----------------------------------------------------------------------------
;;                   FLAVOR DATA-BASE 
;;-----------------------------------------------------------------------------


(def$flavor data-base 
	   ((rules-tried nil)
	    (rules-used nil)
	    (hypotheses-verified nil)
	    (justification-list nil))		
	   (processor-core)
  :settable-instance-variables
  (:documentation "This flavor manages the dynamic data base for the rule-processor.
It uses facilities of the meta-processor via the flavor processor-core to store and
retrieve data and it stores basic information for rule-interpreter execution
and explanation.")
  )


(def$method (data-base :reset-data-base) ()
  "Reset dynamic data base to initial state."
  (setf	rules-tried nil)
  (setf rules-used nil)
  (setf hypotheses-verified nil)
  (setf justification-list nil))


;;-----------------------------------------------------------------------------
;;                   METHODS TO STORE AND RETRIEVE FACTS
;;-----------------------------------------------------------------------------


(def$method (data-base :recall) (fact)
  "Recall the status of a (negated) fact."
  (let ((success t) (failure nil))
    (cond ((IS-NEGATED-TERM fact)
	   (setq fact (GET-POSITIVE-TERM fact))
	   (setq success nil)
	   (setq failure t)))
    (let ((meta-processor-answer 
	    ($send meta-processor :eval fact :RECALL 'rule)))
      (if (IS-UNDETERMINED meta-processor-answer)
	  (UNDETERMINED)
	  (if (not (null meta-processor-answer))
	      success
	      failure)))))


(def$method (data-base :recall-without-asking) (fact) 
  "To avoid full evaluation of premises for explanations."
  (let ((success t) (failure nil))
    (cond ((IS-NEGATED-TERM fact)
	   (setq fact (GET-POSITIVE-TERM fact))
	   (setq success nil)
	   (setq failure t)))
    (let ((meta-processor-answer  
	    ($send meta-processor :eval fact :RECALL-IMMEDIATE 'rule)))
      (if (IS-UNDETERMINED meta-processor-answer)
	  (UNDETERMINED)
	  (if (not (null meta-processor-answer))
	      success
	      failure)))))

(def$method (data-base :remember) (action rule-set rule)
  "Remember the status of a fact. Returns nil, if already known, fact otherwise."
  (cond (($send meta-processor :eval action :REMEMBER 'rule)
	 ($send self :add-direct-deduced
	       action `(,(rule-set-name rule-set) ,rule))
	 action)))

;; store not used yet

(def$method (data-base :store) (action rule-set rule)
  "Store the status of a fact. Returns the fact in any case."
  ($send meta-processor :eval action :STORE 'rule)
  ($send self :add-direct-deduced action `(,(rule-set-name rule-set) ,rule))
  action)


(def$method (data-base :why) (fact current-rule rule-set fact-type)
  "Substitute for true explanations, which are provided by rule-explain-mixin"
  (declare (ignore fact current-rule rule-set fact-type))
  ($send meta-processor :choose-from-menu
		'((" No Explanation available " :no-select t))))


(def$method (data-base :ask-user)
	   (action rule rule-set flag type)
  ; type :PREMISE or :ACTION
  "Ask user for an undetermined action."
  (let ((answer ($send meta-processor :eval action :ask 'rule flag)))
    (case answer 
      (TRUE    ($send self :add-as-positive action) T)
      (FALSE   ($send self :add-as-negative action) NIL)
      (UNKNOWN ($send self :add-as-unknown action) NIL)
      (WHY     ($send self :why action rule rule-set type)
	       ($send self :ask-user action rule rule-set flag type))
      (t (baberror (getentry ask-user-wrong-answer-error-str rule-io-table))))))


;;-----------------------------------------------------------------------------
;;          METHODS TO STORE HOW INFORMATION WAS GATHERED
;;-----------------------------------------------------------------------------



(def$method (data-base :add-rule-tried) (rule)
  "Add rule to tried rules."
  (setf rules-tried (cons rule rules-tried)))


(def$method (data-base :add-rule-used) (rule)
  "Add rule to used rules."
  (if (not (member rule rules-used :test 'equal))
      (setf rules-used (cons rule rules-used))))


(def$method (data-base :add-hypotheses-verified) (term)
  "Add term to list of verified hypotheses."
  (setf hypotheses-verified (cons term hypotheses-verified)))


;;-----------------------------------------------------------------------------


(defstruct justification
  justificand			; a term
  justificans			; the support (a rule or user-answer ...)
  time-tag)                     ; actually not used (for reason maintenance)


;<justificand> is a term (premise or action)
;
;<justificans> is the reason for the status of <justificand>
;
; :USER-YES       <justificans> confirmed by a question
; :USER-NO        <justificans> rejected by a question
; :UNKNOWN        <justificans> left unknown after a question
; (:UNPROVABLE <rule-set>)
;                 <justificans> is unprovable in <rule-set>
; (:RULE-ACTION (<rule-set> <rule>)) 
;                 <justificans> was an action of (<rule-set> <rule>)


(def$method (data-base :add-as-positive) (term)
  "Add term to list of positive terms."
  (setf justification-list
	(cons  (make-justification :justificand term
				   :justificans :USER-YES)
	       justification-list)))


(def$method (data-base :add-as-negative) (term)
  "Add term to list of negative terms."
  (setf justification-list
	(cons (make-justification :justificand term
				  :justificans :USER-NO)
	      justification-list)))


(def$method (data-base :add-as-unknown) (term)
  "Add term to list of unknown terms."
  (setf justification-list
	(cons (make-justification :justificand term
				  :justificans :UNKNOWN)
	      justification-list)))


(def$method (data-base :add-direct-deduced) (term rule-set-and-rule)
  "Add term to list of directly deduced terms of a given rule of rule set."
  (setf justification-list
	(cons  (make-justification :justificand term
				   :justificans `(:RULE-ACTION ,rule-set-and-rule))
	       justification-list)))


(def$method (data-base :add-unprovable) (term rule-set)
  "Add term to list of not provable terms of a given rule set."
    (setf justification-list
	  (cons (make-justification :justificand term
				    :justificans `(:UNPROVABLE ,rule-set))
		justification-list)))


;(def$method (data-base :is-unprovable) (term rule-set)
;  "Look up if term is known to be not provable by a given rule set."
;  (let ((justification (assoc term justification-list :test 'equal))) 
;    ;; find last justification
;    (if justification
;	(and (consp (second justification))
;	     (eq :UNPROVABLE (first (second justification)))
;	     (eq (second (second justification)) rule-set))
;	nil)))

(def$method (data-base :is-unprovable) (term rule-set)
  "Look up if term is known to be not provable by a given rule set."
  (let* ((justification
	   (first (member term justification-list
			  :test #'(lambda (term element)
				    (equal term (justification-justificand element))))))
	 (justificans (if justification (justification-justificans justification))))
    ;; find last justification
    (and (consp justificans)
	 (eq :UNPROVABLE (first justificans ))
	 (eq (second justificans) rule-set))))

;;-----------------------------------------------------------------------------
;;                    GETTING RESULTS
;;-----------------------------------------------------------------------------

(def$method (data-base :get-true-facts) ()
  "Get all true facts."
  (mapcan #'(lambda (a-justification)
	      (let ((justificans (justification-justificans a-justification)))
		(if (or (eq justificans :USER-YES)
			(and (consp justificans)
			     (eq (first justificans) :RULE-ACTION)))
		    (list (justification-justificand a-justification)))))
	  justification-list))


(def$method (data-base :get-unprovable-facts) ()
  "Get all unprovable facts."
  (mapcan #'(lambda (a-justification)
	      (let ((justificans (justification-justificans  a-justification)))
		(if (and (consp justificans)
			 (eq (first justificans) :UNPROVABLE))
		    (list (justification-justificand  a-justification)))))
	  justification-list))


(def$method (data-base :get-all-facts) ()
  "Get all facts."
  (mapcar #'(lambda (a-justification)
	      (justification-justificand  a-justification))
	  justification-list))


;;-----------------------------------------------------------------------------

(def$method (data-base :print-hypotheses-verified) ()
  "Print hypotheses verified on dialog-window."
  (if hypotheses-verified
      ($send meta-processor :babylon-format
	    (getentry hypotheses-verified-fstr rule-io-table)
	    hypotheses-verified)
      ($send meta-processor :babylon-format 
	    (getentry no-hypothesis-verified-fstr rule-io-table))))  

(def$method (data-base :print-true-facts) ()
  "Print all true facts on dialog-window." 
  (let ((true-facts ($send self :get-true-facts)))
    (if true-facts
	($send meta-processor :babylon-format 
	      (getentry true-facts-fstr rule-io-table)
	      true-facts)
	($send meta-processor :babylon-format 
	      (getentry no-true-facts-fstr rule-io-table)))))

