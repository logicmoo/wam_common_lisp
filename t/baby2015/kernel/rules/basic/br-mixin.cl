;;;  -*-  Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10. -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; DATE:     uralt
;; AUTHORS:  Franco di Primio, Juergen Walther

;; This file depends on:  common>*
;;                        meta>kb-stub
;;                        rules>basic>rules
;;                        rules>basic>br-inter
;;                        rules>basic>br-proc

;; contents: a mixin making the facilities of basic-rule-processor available
;;           for a knowledge base.

;;-----------------------------------------------------------------------------
;;                   FLAVOR BASIC-RULE-MIXIN
;;-----------------------------------------------------------------------------

(def$flavor basic-rule-mixin
	   (rule-processor
	    (rules nil)
	    (hypotheses nil))
	   ()
  :settable-instance-variables
  (:required-instance-variables kb-name procs system-trace system-trace-window language)
  (:documentation "this mixin makes the facilities of basic-rule-processor available.")) 


(def$method (basic-rule-mixin :after :init) (&rest plist)
  "Create rule processor."
  (declare (ignore plist))
  ($send self :generate-rule-processor)
  (setf procs (cons rule-processor procs)))

(def$method (basic-rule-mixin :generate-rule-processor) ()
  "Make an instance of basic-rule-processor and associate it with kb."
  (setf rule-processor (make-$instance 'basic-rule-processor 
                                       :meta-processor self
                                       :alternate-meta-processor				      
                                       (make-$instance 'kb-stub
                                                       :meta-processor self))))


(def$method (basic-rule-mixin :set-up-rule-cmds) ()
  (let ((table (get 'cmd-table ($send self :language))))
    (when (and table ($send self :operation-handled-p :add-operations))
      ($send self :add-sub-operations
	     :top (gethash 'rule table)
	     :rule (gethash 'rule-commands table)))))


;;--------------------------------------------------------------------------
;;                  RULE SET CONSTRUCTION (CONSTRUCTORS)
;;--------------------------------------------------------------------------

(def$method (basic-rule-mixin :before :add-to-rules) (a-rule-set)
  "This is a check of the rule syntax."
  (if (not (symbolp (first a-rule-set)))
      (baberror (getentry rule-set-name-error-fstr rule-io-table)
	     (first a-rule-set)
	     kb-name)
      (let ((where (format nil (getentry rule-set-name-error-spot-fstr rule-io-table)
			   (first a-rule-set) kb-name)))
	(mapc #'(lambda (a-rule)
		  (CHECK-RULE-SYNTAX a-rule where))
	      (rule-set-rules a-rule-set)))))


(def$method (basic-rule-mixin :add-to-rules) (a-rule-set)	
  "Add a rule set to rules."
  (let ((previous-set (assoc (first a-rule-set) rules)))
    (cond ((null rules)
	   (setf rules `(,a-rule-set))
	   ($send rule-processor :set-rules rules))
	  ((null previous-set)
	   (setf rules (nconc rules `(,a-rule-set))))
	  (t (setf (rest previous-set) (rest a-rule-set))))))



(defmacro DEFRULE-SET (rule-set-name &rest rules)
  `(and (current-kb-typep 'basic-rule-mixin)
	(send-kb :add-to-rules '(,rule-set-name . ,rules))
	'(RULE-SET ,rule-set-name DEFINED FOR ,(send-kb :kb-name))))

(defmacro RULE-SET (rule-set-name &rest rules)
  `(DEFRULE-SET ,rule-set-name . ,rules))


(defmacro HYPOTHESES (&rest hypotheses)
  `(and (current-kb-typep 'basic-rule-mixin)
	(send-kb :set-hypotheses ',hypotheses)
	`(HYPOTHESES DEFINED FOR ,(send-kb :kb-name))))


;;--------------------------------------------------------------------------
;;                  PRINTING RESULTS 
;;--------------------------------------------------------------------------

(def$method (basic-rule-mixin :print-hypotheses-verified) ()
  "Print hypotheses verified."
  ($send rule-processor :print-hypotheses-verified))

(def$method (basic-rule-mixin :print-true-facts) ()
  "Print true facts."
  ($send rule-processor :print-true-facts))

;;--------------------------------------------------------------------------
;;                  EXPLAINING EVALUATION 
;;--------------------------------------------------------------------------

(defrequest rule-meta-reference		
	    :prolog  :eval-rule-meta-reference-for-prolog)

(defun is-rule-meta-predicate (x)	
  (member x *rule-meta-predicates*))

(defmacro rule-reference-type (request)	
  `(if (and (listp ,request)
	    (is-rule-meta-predicate (first ,request)))
       'RULE-META-REFERENCE))

(assign-typefkt 'rule-reference-type 'basic-rule-mixin)


(def$method (basic-rule-mixin :eval-rule-meta-reference-for-prolog) (request mode)
  (when system-trace
    ($send system-trace-window :format
	   (getentry meta-rule-reference-trace-fstr rule-io-table)
	   mode request))
  (let ((clauses nil))
    (case (first request)
      (RULE-SET (let ((rule-set-names ($send rule-processor :get-rule-set-names)))
		  (if (IS-VARIABLE (second request))
		      (dolist (a-rule-set-name rule-set-names (nreverse clauses))
			(setf clauses
			      (cons `((,(first request) ,a-rule-set-name)) clauses)))
		      (if (member (second request) rule-set-names)
			  t
			  nil))))
      (RULE-SET-DEF (let ((rule-set-names ($send rule-processor :get-rule-set-names))
			  (rule-set-defs nil))
		      (dolist (a-rule-set-name rule-set-names t)
			(setf rule-set-defs
			      (cons `(DEFRULE-SET . ,($send rule-processor
							    :get-rule-set
							    a-rule-set-name))
				    rule-set-defs)))		    
		      (if (CONTAINS-VARS (second request))
			  (dolist (a-rule-set-def (nreverse rule-set-defs) (nreverse clauses))
			    (setf clauses
				  (cons `((,(first request) ,a-rule-set-def)) clauses)))
			  (if (member (second request) rule-set-defs :test #'equal)
			      t
			      nil))))
      ((HAS-RULE RULE)
       (if (IS-VARIABLE (second request))
	   nil
	   (let ((rule-set ($send rule-processor :get-rule-set (second request))))
	     (if (CONTAINS-VARS (third request))
		 ;; CONTAIN-VARS checkt auch wie IS-VARIABLE
		 (dolist (a-rule (rest rule-set) (nreverse clauses))
		   (setf clauses
			 (cons `((,(first request) ,(second request) ,a-rule)) clauses)))
		 (if (member (third request) rule-set :test #'equal)
		     t
		     nil)))))
      (t ;; signal error
	nil))))

(def$method (basic-rule-mixin :find-implications)
	   (&optional (rule-set-name nil)
	    (control-structure :DO-ALL)		      		      
	    (condition T)
	    (bindings nil))
  "Find implications (forward evaluation)."
  ($send rule-processor :start-forward
	rule-set-name control-structure condition bindings))


(defun find-implications (&optional
			     (rule-set-name nil)
			     (control-structure :DO-ALL)
			     (condition T)
			     (bindings nil))
  (send-kb :find-implications
	    rule-set-name control-structure condition bindings))

(def$method (basic-rule-mixin :test-hypotheses) (&optional
                                                 (number-of-hypotheses-to-verify 1.)
                                                 (list-of-hypotheses nil)
                                                 (rule-set-name nil)
                                                 (bindings nil))
  "Test hypotheses (backward evaluation)."

  (if (not list-of-hypotheses)
    (setq list-of-hypotheses hypotheses))
  (if (not (integerp number-of-hypotheses-to-verify))
    (if (eq :ALL number-of-hypotheses-to-verify)
      (setq number-of-hypotheses-to-verify (length list-of-hypotheses))
      (baberror (getentry hypotheses-spec-number-error-fstr
                          rule-io-table)
                number-of-hypotheses-to-verify
                list-of-hypotheses
                kb-name)))
  (if (not (listp list-of-hypotheses))
    (baberror (getentry hypotheses-spec-list-error-fstr
                        rule-io-table) kb-name))
  ($send rule-processor :test-hypotheses
         number-of-hypotheses-to-verify
         list-of-hypotheses
         rule-set-name
         bindings))


(defun test-hypotheses (&optional (number-of-hypotheses-to-verify 1.)
			(list-of-hypotheses nil)
			(rule-set-name nil)
			(bindings nil))
  (send-kb :test-hypotheses
	   number-of-hypotheses-to-verify
	   list-of-hypotheses 
	   rule-set-name
	   bindings))


(def$method (basic-rule-mixin :obtain) (number-of-hypotheses-to-verify
				 goal-specification
				 &optional (rule-set-name nil)
				 (bindings nil))
  "Obtain (backward evaluation) with a goal specification."
  (if (not (integerp number-of-hypotheses-to-verify))
      (if (eq :ALL number-of-hypotheses-to-verify)
	  (setq number-of-hypotheses-to-verify 1000.)   ;  sollte reichen
	  (baberror (getentry hypotheses-spec-number-error-fstr
			   rule-io-table)
		  number-of-hypotheses-to-verify
		  goal-specification
		  kb-name)))
  ($send rule-processor :obtain
	number-of-hypotheses-to-verify
	goal-specification
	rule-set-name
	bindings))


(defun obtain (number-of-hypotheses-to-verify goal-specification
	       &optional (rule-set-name nil) (bindings nil))
  (send-kb :obtain
	   number-of-hypotheses-to-verify
	   goal-specification
	   rule-set-name
	   bindings))


;;--------------------------------------------------------------------------
;;                  CONFLICT RESOLUTION 
;;--------------------------------------------------------------------------

(def$method (basic-rule-mixin :set-conflict-resolution) (function)
  "Sets the rule interpreter conflict resolution strategy to function.
Function get a list-of-rules, the goal and the current rule-interpreter
instance as parameters and should return a list-of-rules, which are used
in that order to prove goal. The standard-conflict-resolution function
simply returns the list-of-rules argument. It is reassigned if the rule
interpreter is reset."
  ($send rule-processor :set-conflict-resolution function))

;;--------------------------------------------------------------------------
;;                  LIST RULES
;;--------------------------------------------------------------------------

(def$method (basic-rule-mixin :list-rule) (rule-name rule-set-name &optional window)
  (let ((rule ($send rule-processor :get-rule rule-set-name rule-name)))
    (setf window (or window self))
    ($send window :format
		  "~&~%Rule: ~S of Rule Set: ~S" (rule-name rule) rule-set-name)
    ($send window :format 
		  "~%IF    ~S" (get-junctor (rule-left-hand-side rule)))
    ($send window :format 
		  "~{~%      ~S~}" (get-rule-conditions (rule-left-hand-side rule)))
    ($send window :format 
		  "~%THEN  ~S" (get-action-type (rule-right-hand-side rule)))
    ($send window :format 
		  "~{~%      ~S~}" (get-rule-actions (rule-right-hand-side rule)))
    ($send window :format "~%")))


(def$method (basic-rule-mixin :select-rule-set-name) ()
  (let ((rule-set-names ($send rule-processor :get-rule-set-names)))
    (cond ((null (rest rule-set-names)) (first rule-set-names))
	  (t (do* ((items (append rule-set-names 
				  `(,(getentry exit-menu-item  rule-io-table))))
		   (label (getentry select-rule-set-str rule-io-table))
		   (rule-set ($send self :choose-from-menu items label)
			     ($send self :choose-from-menu items label)))
		  ((not (null rule-set)) rule-set))))))



(def$method (basic-rule-mixin :select-list-rule) (&optional window)
  (let ((rule-set-name ($send self :select-rule-set-name)))
    (setf window (or window self))
    (when (and rule-set-name (not (eq rule-set-name 'exit)))
      (do* ((items (append
		     ($send rule-processor :get-rule-names rule-set-name)
		     `(,(getentry suspend-item  rule-io-table)
		       ,(getentry exit-menu-item  rule-io-table))))
	    (label (format nil
			   (getentry which-rule-fstr rule-io-table)
			   rule-set-name))
	    (rule-name ($send self :choose-from-menu items label)
		       ($send self :choose-from-menu items label)))
	   ((eq rule-name 'exit) t)
	(cond ((eq rule-name 'all))
	      ((eq rule-name 'suspend)
	       ($send self :type-end-to-continue
		      (getentry type-end-to-continue-str rule-io-table)))
	      ((not (null rule-name))
	       ($send self :list-rule rule-name rule-set-name window)))))))

(def$method (basic-rule-mixin :list-rules) ()
  ($send self :select-list-rule))

;;--------------------------------------------------------------------------
;;                  AUX STUFF 
;;--------------------------------------------------------------------------


(defun send-rule (selector &rest args)
  "Send to current rule-processor."
  (lexpr-$send (send-kb :rule-processor) selector args))