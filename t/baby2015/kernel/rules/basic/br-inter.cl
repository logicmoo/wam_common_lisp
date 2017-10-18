;;; -*- Mode: Lisp; Base:10; Syntax: Common-Lisp; Package: BABYLON -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; DATE:     uralt
;; AUTHORS:  Franco di Primio, Juergen Walther

;; This file depends on:  common>*
;;                        rules>basic>data
;;                        rules>basic>rules

;; Contents: A flavor providing main functionality for rule oriented programming.
;;           The implementation of the rule interpreter is data driven.
;;           For every junctor type of the if-part and every action type of the
;;           then-part of a rule the method  realizing the evaluation according
;;           to forward or backward control strategy is put on the property list
;;           of the junctor symbol using macro defjunctor.
;;
;;           For Example:
;;
;;           Symbol    index=Evaluation Strategy   Method
;;
;;           $TRUE     :FORWARD                    :true
;;           $TRUE     :BACKWARD                   :true
;;           $AND      :FORWARD                    :and-forward

;;-----------------------------------------------------------------------------
;;                     AUXILARY FUNCTIONS
;;-----------------------------------------------------------------------------


(defmacro defjunctor (name rule-interpreter-method mode)
  "Associates operators in rules with mode and method for execution."
  `(progn (setf (get ',name ',mode) ',rule-interpreter-method)
	  ',name))


(defun standard-conflict-resolution (list-of-rules goal processor)
  "Defines the standard conflict resolution strategy."
  (declare (ignore goal processor))
  list-of-rules)


(defun stop-execution (&optional (result 'done))
  (throw 'rule-interpreter-stop-tag result))


(defun is-undetermined-or-null (value)
  "Returns t if value is nil or undetermined, nil otherwise."
  (cond ((or (is-undetermined value) (null value)) t)
	(t nil)))

;;-----------------------------------------------------------------------------
;;                   RULE INTERPRETER 
;;-----------------------------------------------------------------------------

(def$flavor rule-interpreter
	   ((rhs-one-shot-list
	      nil)				; for ONE-SHOT rules a la LOOPS
	    (conflict-resolution
	      #'standard-conflict-resolution))	; conflict resolution
	   (rule-base				; rule manager
	     data-base)				; dynamic data base manager
  :settable-instance-variables
  (:documentation "This flavor  provides  main  functionality  for  rule
oriented programming.  It uses flavor  rule-base for handling rules  and
flavor data-base as the dynamic data base.  Rule-base and data-base only
provide the functionality needed for rule-interpreter.  Data-base  rests
on other processors via meta-processor to store and retrieve data.")
  )


(def$method (rule-interpreter :before :reset-proc) ()
  "Reset rule interpreter to initial state."
  (setf rhs-one-shot-list nil)
  (setf	conflict-resolution #'standard-conflict-resolution))


;;-----------------------------------------------------------------------------
;;             METHOD TO EVALUATE A SINGLE RULE
;;-----------------------------------------------------------------------------


(defun get-op-def (symbol index)
  "Retrieves selector-names attached with operators which may appear in rules."
  (let ((fn (get symbol index)))
    (if (null fn)
	(baberror (getentry method-property-error-fstr rule-io-table)
	       symbol index)
	fn)))


(defmacro testif (rule left-hand-side rule-set mode)
  `($send self (get-op-def (get-junctor ,left-hand-side) ,mode)
		 ,rule
		 (get-rule-conditions ,left-hand-side)
		 ,rule-set))


(defmacro usethen (rule right-hand-side rule-set mode)
  `($send self (get-op-def (get-action-type ,right-hand-side) ,mode)
		 ,rule
		 (get-rule-actions ,right-hand-side)
		 ,rule-set))



(defmacro instantiate-pattern (alist pattern)
  `(sublis ,alist ,pattern))


(def$method (rule-interpreter :try-rule) (rule rule-set mode)
  "Internal method."
  ($send self :add-rule-tried `(,(rule-set-name rule-set) ,rule))
  (let ((lhs-evaluation-result (testif rule (rule-left-hand-side rule) rule-set mode)))
    (cond ((null lhs-evaluation-result) nil)
	  ((eq lhs-evaluation-result T)
	   (prog1 (usethen rule (rule-right-hand-side rule) rule-set mode)
		  ($send self :add-rule-used
			`(,(rule-set-name rule-set) ,rule))))
	  (t ;; the result is a stream of environments:
	   ;; environment-stream ::= (<env1> ... <envN>)
	   ;; <env> ::= ((<var1> . <value1>) ... (<varN> . <valueN>))
	   (do ((envs lhs-evaluation-result (rest envs))
		(result nil))
	       ((null envs) result)
	     (let* ((rule-instance (instantiate-pattern (first envs) rule))
		    (use-result (usethen rule-instance
					 (rule-right-hand-side rule-instance)
					 rule-set
					 mode)))
	       (when use-result
		 (setq result use-result)
		 ($send self :add-rule-used
		       `(,(rule-set-name rule-set) ,rule-instance)))))))))
	       


;;----------------------------------------------------------------------------- 
;;                 BACKWARD CHAINING EVALUATION METHODS
;;----------------------------------------------------------------------------- 

(def$method (rule-interpreter :test-hypotheses) (verification-limit
                                                 hypotheses
                                                 rule-set-or-rule-set-name
                                                 &optional (bindings nil))
  "Start rule evaluation by backward chaining.
Hypotheses are given explicitly."
  (declare (fixnum verification-limit) (list hypotheses bindings))
  (catch 'RULE-INTERPRETER-STOP-TAG
    (prog ((rule-set
            (if (atom rule-set-or-rule-set-name)
              ($send self :get-rule-set
                     rule-set-or-rule-set-name nil bindings)
              rule-set-or-rule-set-name)))
      loop
      (if (null hypotheses) (go exit))
      (if (< verification-limit 1.) (go exit))
      (cond (($send self :verify-hypothesis
                    (first hypotheses) nil rule-set)
             ($send self :add-hypotheses-verified (first hypotheses))
             (decf verification-limit)))
      (setq hypotheses (rest hypotheses))	     
      (go loop)
      exit
      (return ($send self :hypotheses-verified)))))

;;----------------------------------------------------------------------------- 

(def$method (rule-interpreter :obtain) (verification-limit
                                        goal-specification
                                        a-rule-set-name
                                        &optional (bindings nil))
  "Start rule evaluation by backward chaining.
Hypotheses are given implicitly."
  (declare (fixnum verification-limit) (list goal-specification bindings))
  (if (atom goal-specification)
    (setq goal-specification `(,goal-specification)))
  (let* ((rule-set ($send self :get-rule-set a-rule-set-name nil bindings))
         (goals
          (case (length goal-specification)
            (1 ($send self :find-matching-conclusions
                      goal-specification rule-set
                      #'match-first))
            (2 ($send self :find-matching-conclusions
                      goal-specification rule-set
                      #'match-first-and-second))
            (t ($send self :find-matching-conclusions
                      goal-specification rule-set
                      #'equal)))))
    ($send self :test-hypotheses
           verification-limit goals rule-set bindings)))

;;----------------------------------------------------------------------------- 

(def$method (rule-interpreter :in-then-part) (fact rule-set current-rule)
  "Defines a different name for method :inthen of rule-base.
This method is  used as  the primary  method of  a before  demon used to
trace actions of the interpreter, and seperates the use of :inthen within
the context of the interpreter from the use of :inthen in  other context,
which should not be traced."
  (declare (ignore current-rule))
  ($send self :inthen fact rule-set))


(def$method (rule-interpreter :verify-hypothesis)
	   (hypothesis rule rule-set)
  "Defines a different name for method :verify.
This method is  used as  the primary  method of  a before  demon used to
trace verification of hypotheses. :verify is called recursively"
  ($send self :verify hypothesis rule rule-set))


  
(def$method (rule-interpreter :verify) (fact current-rule rule-set)
  "Internal method.
a modified version of WINSTONS VERIFY."
  (prog (data-base-answer relevant1 relevant2 (SUCCESS t) (FAILURE nil))	
	(cond ((is-negated-term fact)
	       (setq fact (get-positive-term fact))
	       (setq SUCCESS nil)
	       (setq FAILURE t)))	
     RECALL
	(setq data-base-answer ($send self :recall fact))	
	(cond ((IS-UNDETERMINED data-base-answer)
	       (if ($send self :is-unprovable fact rule-set)
		   (return FAILURE)
		   t))		       ; try to deduce or ask user
	      ((null data-base-answer) (return failure))
	      (t (return SUCCESS)))
	(setq relevant1
	      (funcall CONFLICT-RESOLUTION
		       ($send self :in-then-part fact rule-set current-rule)
		       fact
		       self))
	(setq relevant2 relevant1)
	(cond ((null relevant1)	       ; the fact is an input premise
	                               ; (still undetermined)
	       ($send self :ask-user
		     fact current-rule rule-set failure :PREMISE)
	       (go RECALL)))
	
     FORWARD ;; try forward rules with fact in right-hand-side	
	(cond ((null relevant1) (go BACKWARD)))	
	(cond (($send self :try-rule (pop relevant1) rule-set 'FORWARD)
	       (return SUCCESS)))
	(go FORWARD)
	
     BACKWARD ;; try backward rules with fact in right-hand-side	
	(cond ((null relevant2)	       ; mark fact as unprovable in <rule-set>
	       ($send self :add-unprovable fact rule-set)
	       (return FAILURE)))	
	(cond (($send self :try-rule (pop relevant2) rule-set 'BACKWARD)
	       (return SUCCESS)))
	(go BACKWARD)))


;;-----------------------------------------------------------------------------
;;             METHODS FOR JUNCTORS WITH BACKWARD CHAINING
;;-----------------------------------------------------------------------------
 

(defjunctor $and :and-backward BACKWARD)

(def$method (rule-interpreter :and-backward) (rule conditions rule-set)
    "Internal method. Backward evaluation of AND junctor in premisses."
  (dolist (condition conditions t)
    (if (not ($send self :verify condition rule rule-set)) (return nil))))

;;-----------------------------------------------------------------------------

(defjunctor $or :or-backward BACKWARD)

(def$method (rule-interpreter :or-backward) (rule conditions rule-set)
    "Internal method. Backward evaluation of OR junctor in premisses."
  (dolist (condition conditions nil)
    (if ($send self :verify condition rule rule-set) (return t))))


;;-----------------------------------------------------------------------------
;;               FORWARD CHAINING EVALUATION
;;-----------------------------------------------------------------------------

(def$method (rule-interpreter :start-forward) 
	    (&optional rule-set-name
		       (control-structure :do-all)
		       (condition t)
		       (bindings nil))
  "Start rule evaluation by forward chaining."
  (catch 'RULE-INTERPRETER-STOP-TAG
    ($send self control-structure
	   (or rule-set-name current-rule-set) condition bindings)))


(def$method (rule-interpreter :do-one) (rule-set-name
                                        &optional condition (bindings nil))
  "Internal method. Forward evaluation of a DO-ONE rule."
  (declare (ignore condition))
  (prog ((rule-set ($send self :get-rule-set rule-set-name nil bindings))
         trules next-rule result)
    (setq trules (rule-set-rules rule-set))
    A  (if (null trules) (return result))
    (setq next-rule (pop trules))	
    (setq result ($send self :try-rule next-rule rule-set 'FORWARD))
    (if result (return result))
    (go A)))


(def$method (rule-interpreter :do-all) (rule-set-name
                                        &optional condition
                                        (bindings nil))
  "Internal method. Forward evaluation with DO-ALL control strategy."
  (declare (ignore condition))
  (prog ((rule-set ($send self :get-rule-set rule-set-name nil bindings))
         trules next-rule result last-result)
    (setq trules (rule-set-rules rule-set))
    A  (if (null trules) (return result))
    (setq next-rule (pop trules))
    (setq last-result ($send self :try-rule next-rule rule-set 'FORWARD))
    (if last-result (setq result last-result))
    (go A)))



(def$method (rule-interpreter :while-one) (rule-set-name 
                                           &optional (while-condition t) 
                                           (bindings nil))
  "Internal method. Forward evaluation with WHILE-ONE control strategy."
  (prog (result last-result)
    A  (cond ((eval while-condition)
              (setq last-result ($send self :do-one rule-set-name nil bindings))
              (cond (last-result (setq result last-result) (go A))
                    (t (return result))))
             (t (return result)))))



(def$method (rule-interpreter :while-all) (rule-set-name 
                                           &optional (while-condition t) 
                                           (bindings nil))
  "Internal method. Forward evaluation with WHILE-ALL control strategy."
  (prog (result last-result)
    A  (cond ((eval while-condition)
              (setq last-result ($send self :do-all rule-set-name nil bindings))
              (cond (last-result (setq result last-result) (go A))
                    (t (return result))))
             (t (return result)))))


;;-----------------------------------------------------------------------------
;;           METHODS FOR JUNCTORS WITH FORWARD CHAINING 
;;-----------------------------------------------------------------------------

(defjunctor $one-shot :one-shot FORWARD)

(def$method (rule-interpreter :one-shot) (rule conditions rule-set)
  "One shot control strategy for rule evaluation."
  (declare (ignore conditions rule-set))
  (let ((rhs (rule-right-hand-side rule)))
    (cond ((member rhs rhs-one-shot-list) nil)	; Test auf EQ!!!!
          (t (setf rhs-one-shot-list (cons rhs rhs-one-shot-list))
             t))))

;;-----------------------------------------------------------------------------

(defjunctor $and :and-forward FORWARD)

(def$method (rule-interpreter :and-forward) (rule conditions rule-set)
  "AND forward control strategy for rule evaluation."
  (declare (ignore rule rule-set))
  (dolist (condition conditions t)
    (if (is-undetermined-or-null ($send self :recall condition))
      (return nil))))

;;-----------------------------------------------------------------------------

(defjunctor $or :or-forward FORWARD)

(def$method (rule-interpreter :or-forward) (rule conditions rule-set)
  "OR forward control strategy for rule evaluation."
  (declare (ignore rule rule-set))
  (dolist (condition conditions nil)
    (if (not (is-undetermined-or-null ($send self :recall condition)))
      (return t))))

;;-----------------------------------------------------------------------------

(defjunctor ?and :and-forward-asking-if-undetermined FORWARD)

(def$method (rule-interpreter :and-forward-asking-if-undetermined) (rule 
                                                                    conditions 
                                                                    rule-set)
  "AND forward with asking if undetermined strategy for rule evaluation.
   Do not ask any questions to the user, if there is any premisse,
   that can be shown to be failing."
  (prog (data-base-answer condition conditions-to-ask (to-ask t))
    A
    (if (null conditions)
      (if (and to-ask (not (null conditions-to-ask)))
        (setq conditions (reverse conditions-to-ask)
              to-ask nil)
        (return t)))
    (setq condition (pop conditions)) 
    B
    (setq data-base-answer
          ($send self :recall condition))
    (cond ((IS-UNDETERMINED data-base-answer)
           (cond (to-ask (push condition conditions-to-ask) (go A))
                 (t ($send self :ask-user
                           (compute-term condition)
                           rule
                           rule-set
                           (is-negated-term condition)
                           :PREMISE)
                    (go B))))
          ((null data-base-answer) (return nil))
          (t (go A)))))

;;-----------------------------------------------------------------------------

(defjunctor ?or :or-forward-asking-if-undetermined FORWARD)

(def$method (rule-interpreter :or-forward-asking-if-undetermined) (rule 
                                                                   conditions 
                                                                   rule-set)
  "OR forward with asking if undetermined strategy for rule evaluation."
  (prog (data-base-answer condition conditions-to-ask (to-ask t))
    
    A  (if (null conditions)
         (if (and to-ask (not (null conditions-to-ask)))
           (setq conditions (reverse conditions-to-ask)
                 to-ask nil)
           (return nil)))
    (setq condition (pop conditions))
    
    B  (setq data-base-answer
             ($send self :recall condition))
    (cond ((IS-UNDETERMINED data-base-answer)
           (cond (to-ask (push condition conditions-to-ask) (go A))
                 (t ($send self :ask-user
                           (compute-term condition)
                           rule
                           rule-set
                           (is-negated-term condition)
                           :PREMISE)
                    (go B))))		
          ((null data-base-answer) (go A))
          (t (return t)))))

;;-----------------------------------------------------------------------------

(defjunctor and :variable-and-forward FORWARD)

(def$method (rule-interpreter :variable-and-forward) (rule conditions rule-set)
  "AND forward control strategy for rule evaluation with variables."
  (declare (ignore rule rule-set))
  ($send meta-processor :eval
         `(AND . ,conditions) :RECALL 'rule))
 
;;-----------------------------------------------------------------------------

(defjunctor or :variable-or-forward FORWARD)

(def$method (rule-interpreter :variable-or-forward) (rule conditions rule-set)
  "OR forward control strategy for rule evaluation with variables."
  (declare (ignore rule rule-set))
  ($send meta-processor :eval
         `(OR . ,conditions) :RECALL 'rule))


;;-----------------------------------------------------------------------------
;;               METHODS FOR JUNCTORS $TRUE AND $FALSE 
;;-----------------------------------------------------------------------------


(defjunctor $true :true FORWARD)
(defjunctor $true :true BACKWARD)

(def$method (rule-interpreter :true) (rule conditions rule-set)
  "Unconditional rule."
  (declare (ignore rule conditions rule-set))
  t)

;;-----------------------------------------------------------------------------

(defjunctor $false :false FORWARD)
(defjunctor $false :false BACKWARD)

(def$method (rule-interpreter :false) (rule conditions rule-set)
  "Rule which never succeeds. No operation."
  (declare (ignore rule conditions rule-set))
  nil)

;;-----------------------------------------------------------------------------
;;               METHODS FOR EVALUATION OF RULE CONCLUSIONS 
;;-----------------------------------------------------------------------------


(defjunctor $conclude :conclude FORWARD)
(defjunctor $conclude :conclude BACKWARD)

(def$method (rule-interpreter :conclude) (rule actions rule-set)
  "Performs rule conclusions of action type $CONCLUDE.
The value of :conclude is the value of the last action executed successfully."
  (let (result)
    (dolist (action actions result)
      (setq result (or ($send self :remember action rule-set rule)
                       result)))))

;;-----------------------------------------------------------------------------

(defjunctor $execute :execute FORWARD)
(defjunctor $execute :execute BACKWARD)

(def$method (rule-interpreter :execute) (rule actions rule-set)
  "Performs rule conclusions of action type $EXECUTE.
The value of :execute is always true."
  (dolist (action actions t)
    ($send self :remember action rule-set rule)))

;;-----------------------------------------------------------------------------

(defjunctor $ask :execute-ask FORWARD)
(defjunctor $ask :execute-ask BACKWARD)

(def$method (rule-interpreter :execute-ask) (rule actions rule-set)
  "Performs rule conclusions of action type $ASK.
   While some action is undetermined ask user for verification."
  (dolist (action actions t)
    (do ((result ($send self :recall action)
                 ($send self :recall action)))
        ((not (IS-UNDETERMINED result)) t)
      ($send self :ask-user action rule rule-set nil :ACTION))))


;;; eof

