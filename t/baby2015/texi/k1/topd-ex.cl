;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")


;;; adopted to non SYMBOLICS machines for BABYLON 2.1 by Juergen Walther 10.5.89


(def-kb-instance top-down-refine-examples k1c :pkg :ks)

;; the kb uses the definition of top-down-refine knowledge base

(do ()
    ((member 'top-down-refine *known-knowledge-bases*)
     (progn
       ($send top-down-refine-examples :make-yourself-current)
       (send-kb :send-if-handles :import-kb top-down-refine)
       "top-down-refine imported"))
  (declare (special TOP-DOWN-REFINE-EXAMPLES TOP-DOWN-REFINE))
  (if (not (member 'top-down-refine *known-knowledge-bases*))
    (cerror "Load << top-down-refine >> before proceeding!"
            "Unknown Knowledge Base ~S" 'top-down-refine)))

;; _________________________________________________________________________


;; **************************** EXAMPLE **********************************

;; An abstract example!

#||
;                                      TOP-DIAGNOSE
;                                           |
;                                           |
;                   ------------------------------------------------------
;                   |                       |                            |
;                   |                       |                            |
;                   |                       |                            |
;                  D-1                     D-2                          D-3
;                   |                       |                            
;                   |                       |                            
;          ------------------      ---------------------
;          |                |      |                   |
;          |                |      |                   |
;          |                |      |                   |
;         D-11             D-12  D-21                D-22 
;

||#


;; ************************** ASKING-DIAGNOSE ***************************** 

(DEFFRAME asking-diagnose
  (SUPERS DIAGNOSE-UNIT))

;;__________________________________________________________________________

(DEFBEHAVIOR (asking-diagnose :ask-for-focus) (diagnoses &optional rule-type ignore)
  "this is used in the examples. 
it asks for the focus of the diagnoses and adds the answer to the value of the focus so far."
  (declare (ignore ignore))
  (if diagnoses
    (case rule-type
      (SELF-TEST-RULES
       (send-kb :babylon-format "~%EVAL SELF TEST RULES of ~s " (first diagnoses)))
      (REFINEMENT-RULES
       (send-kb :babylon-format "~%EVAL REFINEMENT RULES of ~s for ~s"
                ($send self :object-name) diagnoses))
      (t (error "~S: Wrong rule type in ~S."
                rule-type ($send self :object-name)))))
  (dolist (a-diagnose  diagnoses)
    (let ((old-focus (<- a-diagnose :get 'focus)))
      (<- a-diagnose :ask 'focus)
      (<- a-diagnose :add-to-focus old-focus)
      (send-kb :babylon-format "~%New Focus of ~s = ~s"
               a-diagnose (<- a-diagnose :get 'focus)))))


;; ************************** INSTANCES ************************************ 

(definstance top-diagnose of asking-diagnose
  with diagnose-name = top-diagnose
  father =   nil
  children = ((d-1 d-2 d-3))
  self-test-rules   = nil
  refinement-rules  = :start-refine)

(definstance d-1 of asking-diagnose
  with diagnose-name = d-1
  father =   top-diagnose 
  children = ((d-11 d-12))
  self-test-rules   = :d-1-self-test
  refinement-rules  = :d-1-refine)

(definstance d-2 of asking-diagnose
  with diagnose-name = d-2
  father = top-diagnose
  children = ((d-21 d-22))
  self-test-rules   = :d-2-self-test
  refinement-rules  = :d-2-refine)

(definstance d-3 of asking-diagnose
  with diagnose-name = d-3
  father =  top-diagnose
  children = nil
  self-test-rules   = :d-3-self-test
  refinement-rules  = :d-3-refine)

(definstance d-11 of asking-diagnose
  with diagnose-name = d-11
  father =   d-1
  children = nil
  self-test-rules   = :d-11-self-test
  refinement-rules  = :d-11-refine)

(definstance d-12 of asking-diagnose
  with diagnose-name = d-12
  father =   d-1
  children = nil
  self-test-rules   = :d-12-self-test
  refinement-rules  = :d-12-refine)

(definstance d-21 of asking-diagnose
  with diagnose-name = d-21
  father =   d-2
  children = nil
  self-test-rules   = :d-21-self-test
  refinement-rules  = :d-21-refine)

(definstance d-22 of asking-diagnose
  with diagnose-name = d-22
  father =   d-2
  children = nil
  self-test-rules   = :d-22-self-test
  refinement-rules  = :d-22-refine)

;; ************************** RULE-SETS ************************************

(defrule-set :start-refine
  (rule1 ($TRUE)
         ($EXECUTE
          (top-diagnose
           :ask-for-focus (<- top-diagnose children) 'refinement-rules))))


(defrule-set :d-1-self-test
  (rule1 ($TRUE)
         ($EXECUTE (d-1 :ask-for-focus '(d-1) 'self-test-rules))))

(defrule-set :d-1-refine
  (rule1 ($TRUE)
         ($EXECUTE
          (d-1 :ask-for-focus (<- d-1 children) 'refinement-rules))))


(defrule-set :d-2-self-test
  (rule1 ($TRUE)
         ($EXECUTE (d-2 :ask-for-focus '(d-2) 'self-test-rules))))

(defrule-set :d-2-refine
  (rule1 ($TRUE)
         ($EXECUTE
          (d-2 :ask-for-focus (<- d-2 children) 'refinement-rules))))


(defrule-set :d-3-self-test
  (rule1 ($TRUE)
         ($EXECUTE (d-3 :ask-for-focus '(d-3) 'self-test-rules))))

(defrule-set :d-3-refine
  (rule1 ($TRUE)
         ($EXECUTE
          (d-3 :ask-for-focus (<- d-3 children) 'refinement-rules))))


(defrule-set :d-11-self-test
  (rule1 ($TRUE)
         ($EXECUTE (d-11 :ask-for-focus '(d-11) 'self-test-rules))))

(defrule-set :d-11-refine
  (rule1 ($TRUE)
         ($EXECUTE
          (d-11 :ask-for-focus (<- d-11 children) 'refinement-rules))))


(defrule-set :d-12-self-test
  (rule1 ($TRUE)
         ($EXECUTE (d-12 :ask-for-focus '(d-12) 'self-test-rules))))

(defrule-set :d-12-refine
  (rule1 ($TRUE)
         ($EXECUTE
          (d-12 :ask-for-focus (<- d-12 children) 'refinement-rules))))


(defrule-set :d-21-self-test
  (rule1 ($TRUE)
         ($EXECUTE (d-21 :ask-for-focus '(d-21) 'self-test-rules))))

(defrule-set :d-21-refine
  (rule1 ($TRUE)
         ($EXECUTE
          (d-21 :ask-for-focus (<- d-21 children) 'refinement-rules))))


(defrule-set :d-22-self-test
  (rule1 ($TRUE)
         ($EXECUTE (d-22 :ask-for-focus '(d-22) 'self-test-rules))))

(defrule-set :d-22-refine
  (rule1 ($TRUE)
         ($EXECUTE
          (d-22 :ask-for-focus (<- d-22 children) 'refinement-rules))))


;; ************************** INSTRUCTIONS ******************************* 


;;; Instructions for normal-Babylon


(INSTRUCTIONS (<- top-diagnose :start))

;;;  Task and instruction part for tex-i-bylon :
;;;  you can start this task after PRESET with
;;;      (send-kb :reset-kb)
;;;      (send-kb :start-kb)

;;; eof

