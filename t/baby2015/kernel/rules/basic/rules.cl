;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; DATE:     uralt
;; AUTHORS:  Franco di Primio, Juergen Walther

;; This file depends on:  common>*

;; contens: a handler for rule sets.

;;-----------------------------------------------------------------------------
;;                   DEFINING RULE SETS 
;;-----------------------------------------------------------------------------


;; Syntax  (VARIABLES not yet documented)

;; (<rule-set-name> {(:VARIABLES ...)} 
;;    . <rule1> <rule2> ... <ruleN>)


(defun rule-set-name (rule-set)
  (first rule-set))

(defun is-rule-variable (x)
  (and (symbolp x)(char-equal (aref (string x) 0) #\_)))

(defun is-variables-specification (x)
  (and (consp x)
       (eq (first x) :VARIABLES)))

(defun check-rule-set-variables-specification (variables-specification where)
  (or (every #'is-rule-variable (rest variables-specification))
      (baberror (getentry rule-variables-error-fstr rule-io-table) 
	     variables-specification
	     where)))

(defun is-rule-set-with-variables (rule-set)
  (is-variables-specification (second rule-set)))

(defun get-rule-set-variables (rule-set)
  (second rule-set))

(defun get-variables (variables-specification)
  (rest variables-specification))

(defun rule-set-rules (rule-set)
  (if (is-rule-set-with-variables rule-set)
      (rest (rest rule-set))
      (rest rule-set)))

(defun get-bindings (bindings)
  (rest bindings))

(defun is-bindings (x)
  (and (consp x)
       (eq (first x) :BINDINGS)))

(defun check-bindings (rule-set-name variables-specification bindings)
  (declare (list variables-specification bindings))
  (or (and (is-bindings bindings)
           (= (length variables-specification) (length bindings))
           (every #'(lambda (a-pair)
                      (and (consp a-pair)
                           (member (first a-pair)
                                   (get-variables variables-specification))))
                  (get-bindings bindings)))
      (baberror (getentry bindings-spec-error-fstr rule-io-table)
                bindings 
                variables-specification
                rule-set-name)))

(defun substitute-variables-in-rule-set (rule-set bindings)
  (let ((variables-specification (get-rule-set-variables rule-set)))
    (check-bindings (rule-set-name rule-set) variables-specification bindings)
    (sublis (get-bindings bindings)
	    (cons (rule-set-name rule-set) (rest (rest rule-set)))))) 


;;-----------------------------------------------------------------------------
;;                   DEFINING RULES 
;;-----------------------------------------------------------------------------

;; Syntax
;;
;; (<rule-name>
;;    (<junctor> <condition1>
;;               ...
;;               <conditionN>)
;;    (<action-type>
;;               <action1>
;;               ...
;;               <actionN>))


(defun rule-name (rule)
  (first rule))

(defun rule-left-hand-side  (rule)
  (second rule))

(defun rule-right-hand-side  (rule)
  (third rule))

(defmacro get-rule-conditions (left-hand-side)
  `(rest ,left-hand-side))

(defmacro get-rule-actions (right-hand-side)
  `(rest ,right-hand-side))

(defmacro get-junctor (left-hand-side)
  `(first ,left-hand-side))

(defmacro get-action-type (right-hand-side)
  `(first ,right-hand-side))

(defmacro rule-body (rule)
  `(rest ,rule))

(defun make-lhs-example ()
  (format nil (getentry rule-lhs-example-str rule-io-table)))

(defun make-rhs-example ()
  (format nil (getentry rule-rhs-example-str rule-io-table)))

(defun make-full-rule-example ()
  (format nil (getentry rule-example-fstr rule-io-table)
	  (make-lhs-example)
	  (make-rhs-example)))


(defun check-rule-set-syntax (a-rule-set knowledge-base-name)
  (if (not (symbolp (rule-set-name a-rule-set)))
      (baberror (getentry rule-set-name-error-fstr rule-io-table)
	     (rule-set-name a-rule-set)
	     knowledge-base-name))
  (let ((where (format nil (getentry rule-set-name-error-spot-fstr
				     rule-io-table)
		       (rule-set-name a-rule-set) knowledge-base-name)))
    (if (is-rule-set-with-variables a-rule-set)
	(check-rule-set-variables-specification
	  (get-rule-set-variables a-rule-set)
	  where))
    (mapc #'(lambda (a-rule)
	      (CHECK-RULE-SYNTAX a-rule where))
	  (rule-set-rules a-rule-set))))


(defun check-rule-syntax (rule &optional (where ""))
  ;; WHERE is a string saying where the rule is
  (cond ((atom rule)
	 (baberror (getentry rule-syntax-error-fstr rule-io-table)
		 rule where (make-full-rule-example))))
  (let ((error-descriptions nil))
    (cond ((not (symbolp (rule-name rule)))
	   (setq error-descriptions
		 `("The rule name must be a symbol." . ,error-descriptions))))
    (cond ((not (consp (rule-left-hand-side rule)))
	   (setq error-descriptions
		 `("The left-hand-side must be a list." . ,error-descriptions))
	   ))
    (cond ((and (consp (rule-left-hand-side rule))
		(not (symbolp (get-junctor (rule-left-hand-side rule)))))
	   (setq error-descriptions
		 `("The junctor must be a symbol." . ,error-descriptions))))
    (cond ((not (consp (rule-right-hand-side rule)))
	   (setq error-descriptions
		 `("The right-hand-side must be a list." . ,error-descriptions))
	   ))
    (cond ((and (consp (rule-right-hand-side rule))
		(not (symbolp (get-action-type (rule-right-hand-side rule)))))
	   (setq error-descriptions
		 `("The action type must be a symbol." . ,error-descriptions))))
    (cond (error-descriptions
	   (format t (getentry rule-error-description-fstr
			       rule-io-table) rule where)
	   (mapc #'(lambda (an-error-description)
		     (format t "~%~A" an-error-description))
		 (reverse error-descriptions))
	   (baberror (getentry rule-correct-description-fstr rule-io-table) 
		  (make-full-rule-example)))
	  (t t))))


;;-----------------------------------------------------------------------------
;;                   FLAVOR RULE-BASE 
;;-----------------------------------------------------------------------------

(def$flavor rule-base
	   ((rules nil)
	    ;; ((<rule-set-name1> ....) (<rule-set-name2> ...))
	    (current-rule-set nil)
	    ;; (<rule-set-name1> ...) | (<rule-set-name2>  ...)
	    )
	   ()
  (:required-instance-variables meta-processor)
  :settable-instance-variables
  (:documentation "This flavor manages rule sets and rules for the rule interpreter.     
One of the rule sets is the current rule set."))

;
;(def$method (rule-base :reset-pointer) ()
;  "Reset the rules to the initial state."
;  (setf rules ($send meta-processor :rules)))
;  

(def$method (rule-base :get-rule) (rule-set-name rule-name)
  "Returns the rule for a rule-set-name rule-name combination."
  (assoc rule-name
	 (rule-set-rules ($send self :get-rule-set rule-set-name rules))))


(def$method (rule-base :get-rule-set)
	   (rule-set-name &optional (rule-sets nil) (bindings nil))
  "Returns the rule set for a rule set name."
  (let ((rule-set (assoc rule-set-name (or rule-sets rules))))
    (if rule-set
	(cond ((not (null bindings))
	       (if (is-rule-set-with-variables rule-set)
		   (substitute-variables-in-rule-set rule-set bindings)
		   rule-set))
	      (t rule-set))
	(baberror (getentry rule-set-not-found-error-fstr rule-io-table)
	       rule-set-name))))


(def$method (rule-base :get-current-rule-set-name) ()
  "Returns the name of the current rule set."
  (rule-set-name current-rule-set))


(def$method (rule-base :get-rule-set-names) ()
  "Returns a list of all rule set names."
  (mapcar #'rule-set-name rules))


(def$method (rule-base :get-rule-names) (rule-set-name)
  "Returns a list of all rule names of a rule set."
  (mapcar #'rule-name
	  (rule-set-rules ($send self :get-rule-set rule-set-name))))


(def$method (rule-base :add-rule) (rule rule-set)
  "Add a rule to a given rule set."
  (nconc rule-set `(,rule)))


(def$method (rule-base :modify-rule) (rule-set-name new-rule)
  "Replace a given rule by a new one."
  (let* ((rule-name (rule-name new-rule))
	 (old-rule ($send self :get-rule rule-set-name rule-name)))
    (if old-rule
	(setf (rest old-rule) (rest new-rule))
	(baberror (getentry rule-does-not-exist-error-fstr rule-io-table)
	       rule-set-name rule-name))))


(def$method (rule-base :kill-rule) (rule rule-set)
  "Delete a given rule of a rule set."
  (setf (rest rule-set) (delete rule (rule-set-rules rule-set))))


;(def$method (rule-base :edit-rule-set) (rule-set-name)
;  "Edit a given rule set."
;  (let ((rule-set ($send self :get-rule-set rule-set-name)))  
;    (eval `(EDIT-TEMPORARY ,rule-set-name (PRINT-RULE-SET ',rule-set)))))


;(def$method (rule-base :edit-rule-in-buffer) (rule-set-name rule)
;  "Edit a given rule of a rule set."
;  (eval `(EDIT-TEMPORARY ,(rule-name rule)
;			 (print-rule-for-editing ',rule-set-name ',rule))))


;(defun print-rule-for-editing (rule-set-name rule)
;  (babpprint `(MODIFY-RULE ,rule-set-name . ,rule)))


(def$method (rule-base :inthen) (term &optional rule-set (test #'EQUAL))
  "Returns a list of all rules, which have term in their action parts."
  (setq rule-set (or rule-set current-rule-set))
  (mapcan #'(lambda (rule)
	      (cond ((member term
			     (get-rule-actions (rule-right-hand-side rule))
			     :test test)
		     (list rule))))
	  (rule-set-rules rule-set)))


(def$method (rule-base :inif) (term &optional rule-set  (test #'EQUAL))
  "Returns a list of all rules, which have term in their premisse parts."
  (setq rule-set (or rule-set current-rule-set))
  (mapcan #'(lambda (rule)
	      (cond ((member term
			     (get-rule-conditions (rule-left-hand-side rule))
			     :test test)
		     (list rule))))
	  (rule-set-rules rule-set)))



;;-----------------------------------------------------------------------------
;;                   FUNCTIONS NEEDED FOR :OBTAIN 
;;-----------------------------------------------------------------------------

(defun match-first (x y)
    (and (consp x)
         (consp y)
         (eq (first x) (first y))))

(defun match-second (x y)
    (and (consp x)
         (consp y)
         (eq (second x) (second y))))

(defun match-first-and-second (x y)
    (and (consp x)
         (consp y)
         (eq (first x) (first y))
	 (eq (second x) (second y))))

(def$method (rule-base :find-matching-conclusions)
	   (goal-specification &optional rule-set  (test #'EQUAL))
  "Returns a list of conclusions conforming to goal specification."
  (setq rule-set (or rule-set current-rule-set))
  (REMOVE-DOUBLES
    (mapcan #'(lambda (rule)
		(mapcan #'(lambda (an-action)
			    (if (funcall test goal-specification an-action)
				(list an-action)))
			(get-rule-actions (rule-right-hand-side rule))))
	    (rule-set-rules rule-set))))

;;-----------------------------------------------------------------------------
;;                   METHODS FOR PRINTING 
;;-----------------------------------------------------------------------------

(defun print-rule-set (a-rule-set &optional (stream *default-dialog-stream*))
  (babpprint `(DEFRULE-SET . ,a-rule-set) stream))


(def$method (rule-base :unparse-rules) (&optional (stream *default-dialog-stream*))
  "Lists rules and hypotheses on <stream>."
  (let ((kb-rules ($send meta-processor :rules))
	(kb-hypotheses ($send meta-processor :hypotheses)))
    (cond (kb-rules
	   (format stream (getentry rule-statistics-header-str rule-io-table))
	   (cond (kb-hypotheses
		  (babpprint `(hypotheses . ,kb-hypotheses) stream)
		  (format stream "~2%")))
	   (mapc #'(lambda (a-rule-set) 
		     (print-rule-set a-rule-set)
		     (format stream "~2%"))
		 kb-rules)))
    t))


(def$method (rule-base :rule-statistics) (&optional (stream *default-dialog-stream*))
  "Lists a statistical description of rules and hypotheses on <stream>."
  (let ((kb-rules ($send meta-processor :rules)))
    (declare (list kb-rules))
    (format stream (getentry rule-sets-header-fstr rule-io-table) (length kb-rules))
    (cond (kb-rules
           (mapc #'(lambda (a-rule-set)
                     (format stream (getentry rule-set-header-fstr rule-io-table) 
                             (first a-rule-set)
                             (length (the list (rest a-rule-set)))))
                 kb-rules)
           (format stream  (getentry rule-statistics-trailer-str rule-io-table) 
                   (apply #'+ (mapcar #'(lambda (a-rule-set)
                                          (length (the list (rest a-rule-set))))
                                      kb-rules)))))
    t))



;;; eof

