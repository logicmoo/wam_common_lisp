;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base:10 -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; DATE:     uralt
;; AUTHORS:  Eckehard Gross 

;; This file depends on:  common>*
;;                        prolog>basic>axioms
;;                        prolog>basic>ax-sc

;; contents: a flavor representing a single goal provided by the user
;;           or emerging as a subgoal in course of a proof.
;;           it provides a proof method for each system predicate
;;           and a common method for user defined predicates.
;;           the selector of this method is held on the property list
;;           of the predicate using the indicator PROLOG-METHOD

;;--------------------------------------------------------------------------
;;                  FLAVOR BASIC-GOALBOX 
;;--------------------------------------------------------------------------


(def$flavor basic-goalbox
	(goal-level
	  parent-goal
	  (next-goal nil)
	  (previous-goal nil)
	  (subgoals nil)
	  (curr-subgoal nil))
	(goalbox-sc-mixin)
  :settable-instance-variables
  (:documentation "Flavor representing a single goal provided by the user
or emerging as a subgoal in course of a proof.
the flavor provides a proof method for each system predicate and a common
method for user defined predicates."))


;;---------------------------------------------------------------------------
;;              GETTING AND EXECUTING THE RIGHT PROOF METHOD
;;---------------------------------------------------------------------------

; Defined in file global-variables
;(defvar *prolog-syspreds* nil "all system predicates of prolog")

(defmacro defprolog-method (pred method)
  "assigns a proof method to a pred(icate)."
  `(progn (setf (get ',pred 'prolog-method) ,method)
	 ; (putprop ',pred ,method 'curr-prolog-method) ;now in goalbox-trace-mixin
	  (push ',pred *prolog-syspreds*)
	  ',pred))

(defmacro get-method-for-pred (pred)
  "provides the right proof method for pred(icate)."
  `(or (get ,pred 'prolog-method)
       (get '%normal 'prolog-method)))

(defmacro get-prove-method (goal)
  "provides the right proof method for goal."
  `(cond ((varcell-p ,goal) :prove-var)
	((consp ,goal)
	 (let ((pred (pred ,goal)))
	   (cond ((varcell-p pred) :prove-varpred)
		 ((symbolp pred)
		  (get-method-for-pred pred))
		 (t :abort))))
	(t :abort)))


(def$method (basic-goalbox :prove-goal) (mode)
  "fetches the right method to prove the goal and executes that method."
  ($send self (get-prove-method goal) mode))


;;---------------------------------------------------------------------------
;;               METHODS TO PROVE GOALS WITH USER DEFINED PREDICATES 
;;---------------------------------------------------------------------------

(def$method (basic-goalbox :prove-normal) (mode)
  "provides first or next proof of a goal with user defined predicate."
  (prog (next-clause)
        (case mode
	  (try   (prepare-reset)
	         (setf clauses ($send prolog-processor :get-clauses goal)))
	  (retry (cond ((null subgoals)
			(normal-reset)
			(setf clauses (cdr clauses))
			(setq mode 'try))
		       (t (go B)))))
	
     A (cond ((first clauses)
	      (setq next-clause 
		    ($send prolog-processor :trans-unify goal (first clauses)))
	      (case (clause-type next-clause)
		(rule ($send self :generate-subgoals (body next-clause))
		      (go B))
		(fact (return 'succ))
		(none (setf clauses (rest clauses))
		      (go A))))
	     (t (return 'fail)))
	
     B (case ($send self :prove-subgoals mode)
	 (succ 	(return 'succ))
	 (fail  (normal-reset)	       
		(setf clauses (cdr clauses))
		(setf subgoals nil)
		(setq mode 'try)
		(go A))
	 (cut   (cut-reset)
		(setf subgoals nil)
		(return 'cfail))  
	 (error (return 'error)))))

(def$method (basic-goalbox :generate-subgoals) (sgoals)
  "generates goalboxes according to s(ub)goals."
  (setf subgoals
    (mapcar #'(lambda (subgoal)
		(make-$instance (flavor-type-of self)
			       :prolog-processor prolog-processor
			       :parent-goal self
			       :goal-level (1+ goal-level)
			       :goal subgoal))
	    sgoals))  
  (mapl #'(lambda (rsubgoals)
	   (cond ((cdr rsubgoals)
	          ($send (first  rsubgoals) :set-next-goal (second rsubgoals))
		  ($send (second rsubgoals) :set-previous-goal (first rsubgoals)))))
	   subgoals)
  (setf curr-subgoal (first subgoals)))

(def$method (basic-goalbox :prove-subgoals) (mode)
  "provides first or next proof of all subgoals of the goal
giving a proof of the goal itself using the last unifying rule."
  (prog ((ngoal nil) (pgoal nil))	
     A  (case ($send curr-subgoal :prove-goal mode)
	  (succ  (cond ((setq ngoal ($send curr-subgoal :next-goal))
			(setf curr-subgoal ngoal)
			(setq mode 'try)
			(go A))
		       (t (return 'succ))))
	  ((fail cfail)  (cond ((setq pgoal ($send curr-subgoal :previous-goal))
				(setf curr-subgoal pgoal)
				(setq mode 'retry)
				(go A))
			       (t (return 'fail))))
	  (cut   (return 'cut))
	  (error (return 'error)))))

(defprolog-method %normal :prove-normal)


;;---------------------------------------------------------------------------
;;               PROVE-TOP, ABORT 
;;---------------------------------------------------------------------------

(def$method (basic-goalbox :prove-top) (mode)
  "proves the pseudo system predicate %top i.e. the user provided goals."
  (cond ((eq mode 'try)
	 (prepare-reset)
	 ($send self :generate-subgoals
		       (if (consp (car clauses))
			   clauses        	;for goal %top clauses is
			   (list clauses)))))	;used to remember topgoal
  (case ($send self :prove-subgoals mode)
    (succ   'succ)
    (fail  (normal-reset) 'fail)
    (cut   (cut-reset)    'cfail)
    (error 'error)))

(defprolog-method %top :prove-top)

(def$method (basic-goalbox :abort) (ignore)
  "signals syntax error for a goal."
  (declare (ignore ignore))
  ($send prolog-processor :babylon-format
         (getentry abort-fstr prolog-io-table)
         (subst-prolog-vars goal 'ext))
  'error)


;;---------------------------------------------------------------------------
;;               METHODS TO PROVE SOME BASIC SYSTEM PREDICATES 
;;---------------------------------------------------------------------------

(def$method (basic-goalbox :prove-cut) (mode)
  "proves system predicate cut."
  (case mode
    (try (prepare-reset)
	 'succ)
    (retry  'cut)))

(def$method (basic-goalbox :prove-fail) (mode)
  "proves system predicate fail."
  (declare (ignore mode))
  'fail)

(def$method (basic-goalbox :prove-true) (mode)
  "proves system predicate true."
  (case mode
    (try (prepare-reset)
	 'succ)
    (retry 'fail)))

(def$method (basic-goalbox :prove-call) (mode)
  "proves system predicate call."
  (let ((gl goal) res)
    (setf goal (deref (cadr goal)))
    (cond ((varcell-p goal)
	   ($send prolog-processor :babylon-format 
		 (getentry not-instant-fstr prolog-io-table)
		 (subst-prolog-vars goal 'ext))
	   (setq res 'error))
	  (t (setq res ($send self :prove-goal mode)))) 
    (setf goal gl)
    res))

(def$method (basic-goalbox :prove-var) (mode)
  "proves a goal which is an instantiated variable."
  (let ((gl goal) res)
    (setf goal (deref goal))
    (cond ((varcell-p goal)
	   ($send prolog-processor :babylon-format 
		 (getentry not-instant-fstr prolog-io-table)
		 (subst-prolog-vars goal 'ext))
	   (setq res 'error))
	  (t (setq res ($send self :prove-goal mode))))
    (setf goal gl)
    res))

(def$method (basic-goalbox :prove-callpred) (mode)
  "proves system predicate callpred."
  (let ((gl goal)(pred (deref (second goal))) res)
    (cond ((or (null pred)(varcell-p pred))
	   ($send prolog-processor :babylon-format 
		 (getentry illegal-pred-fstr prolog-io-table)
		 (subst-prolog-vars goal 'ext))
	   (setq res 'error))
	  (t (setf goal (cons pred (rest (rest goal))))
	     (setq res ($send self :prove-goal mode))))
    (setf goal gl)
    res))

(def$method (basic-goalbox :prove-varpred) (mode)
  "proves a goal whose predicate is an instantiated varaible."
  (let ((gl goal)(pred (deref (first goal))) res)
    (cond ((varcell-p pred)
	   ($send prolog-processor :babylon-format 
		 (getentry illegal-pred-fstr prolog-io-table)
		 (subst-prolog-vars goal 'ext))
	   (setq res 'error))
	  (t (setf goal (cons pred (rest goal)))
	     (setq res ($send self :prove-goal mode))))
    (setf goal gl)
    res))

(defmacro eval-lisp-call (goal)
  `(eval (subst-prolog-vars ,goal 'normal)))
	
(def$method (basic-goalbox :prove-lisp) (mode)
  "proves system predicate lisp."
  (case mode
    (try  (let ((result (eval-lisp-call goal)))
	    (prepare-reset)
	    (if result
		'succ
		'fail)))		 
    (retry 'fail)))

(def$method (basic-goalbox :prove-is) (mode)
  "proves system predicate is."
  (case mode
    (try  (let ((result (eval-lisp-call (third goal))))
	    (prepare-reset)
	    (setf clauses result)
	    (if ($send prolog-processor
			      :unify (deref (second goal)) clauses)
		'succ
		'fail)))	   
    (retry (normal-reset)
	   'fail)))

(defprolog-method cut :prove-cut)

(defprolog-method ! :prove-cut)

(defprolog-method fail :prove-fail)

(defprolog-method true :prove-true)

(defprolog-method call :prove-call)

(defprolog-method callpred :prove-callpred)

(defprolog-method is :prove-is)

;(defmacro lisp (&rest formlist)
;  `(progn . ,formlist))

(defprolog-method lisp :prove-lisp)

;;---------------------------------------------------------------------------
;;               METHODS TO PROVE SYSTEM PREDICATES AND/OR ET AL 
;;---------------------------------------------------------------------------

(def$method (basic-goalbox :prove-and) (mode)
  "proves system predicate and."
  (BLOCK NIL
	(case mode
	  (try (prepare-reset)
	       (if (rest goal)
		   ($send self :generate-subgoals (rest goal))
		   (return 'succ)))
	  (retry (if (rest goal)
		     nil
		     (return 'fail)))) 
	
	(case ($send self :prove-subgoals mode)
	  (succ	(return 'succ))
	  (fail (normal-reset)
		(setf subgoals nil)
		(return 'fail))
	  (cut  (cut-reset)   ;to reset side-effects
	        (setf subgoals nil)
		(return 'cfail))
	  (error  (return 'error)))))

(def$method (basic-goalbox :generate-subgoal) (gl)
  (setf subgoals
    	(list (make-$instance (flavor-type-of self)
			     :prolog-processor prolog-processor
			     :goal gl
			     :parent-goal self
			     :goal-level (1+ goal-level))))
  (setf curr-subgoal (first subgoals)))


(def$method (basic-goalbox :prove-or) (mode)
  "proves system predicate or."
  (prog ()	
        (case mode
	  (try (prepare-reset)
	       (setf clauses (rest goal))
	       ($send self :generate-subgoal nil))
	  (retry (go B)))
	
     A  (cond ((null clauses)(return 'fail))
	      (t nil))	
	($send curr-subgoal :set-goal (first clauses))
	
     B  (case ($send curr-subgoal :prove-goal mode)
	  (succ	(return 'succ))
	  ((fail cfail)			
	   (normal-reset)	       
	   (setf clauses (cdr clauses))
	   (setq mode 'try)
	   (go A))
	  (cut (cut-reset)
	       (setf subgoals nil)
	       (return 'cfail))
	  (error (return 'error)))))

(def$method (basic-goalbox :prove-not) (mode)  
  "proofs system predicate not."
  (case mode
    (try (prepare-reset)
	 ($send self :generate-subgoals (rest goal))
	 (case ($send self :prove-subgoals mode)
	   (succ (normal-reset)
		 (setf subgoals nil)
		 'fail)
	   (fail (normal-reset)
		 (setf subgoals nil)
		 'succ)
	   (cut (cut-reset)
		(setf subgoals nil)
		'succ)
	   (error  'error)))
    (retry (normal-reset)
	   'fail)))


(def$method (basic-goalbox :prove-once) (mode)
  "proves system predicate once."
  (case mode
    (try (prepare-reset)
	 ($send self :generate-subgoals (rest goal))
	 (case ($send self :prove-subgoals mode)
	   (succ 'succ)
	   (fail (normal-reset)
		 (setf subgoals nil)
		 'fail)
	   (cut (cut-reset)
		(setf subgoals nil)
		'cfail)
	   (error  'error)))
    (retry (normal-reset) ; side effects?
	   'fail)))

(def$method (basic-goalbox :prove-cond) (mode)
  "proves system predicate cond."
  (cond ((eq mode 'try)
	 (prepare-reset)
	 ($send self :generate-subgoals
	       `(,(second goal) (cut) ,@(rest (rest goal))))))
  
  (case ($send self :prove-subgoals mode)
    (succ  'succ)
    (fail (normal-reset)
	  (setf subgoals nil)
	  'fail)
    (cut (cut-reset)
	 (setf subgoals nil)
	 'cfail)
    (error  'error)))


(def$method (basic-goalbox :prove-repeat) (mode)
  "proves system predicate repeat."
  (case mode
    (try (prepare-reset)
	 (setf clauses 0)
	 'succ)
    (retry (setf clauses (1+ clauses))
	   'succ)))


(def$method (basic-goalbox :prove-bagof) (mode) 
  "proves system predicate bagof."
  (prog ((mod mode) (var (second goal)))
	
        (case mod
	  (try (prepare-reset)
	       (setf clauses nil)
	       ($send self :generate-subgoal (third goal)))	  
	  (retry (normal-reset)
		 (return 'fail)))
	
     A   (case ($send curr-subgoal :prove-goal mod)
	   (succ (setf clauses (cons (subst-prolog-vars var 'normal) clauses))
		 (setq mod 'retry)
		 (go A))
	   ((fail cfail)		
	    (cond (($send prolog-processor
			 :unify
			 (deref (fourth goal))
			 (nreverse clauses))
		   (setf subgoals nil)
		   (return 'succ))
		  (t (normal-reset)  
		     (setf subgoals nil)
		     (return 'fail))))
	   (error (return 'error)))))

(defprolog-method and    :prove-and)

(defprolog-method or     :prove-or)

(defprolog-method not    :prove-not)
  
(defprolog-method once   :prove-once)

(defprolog-method cond   :prove-cond)

(defprolog-method repeat :prove-repeat)

(defprolog-method bagof  :prove-bagof)


;;---------------------------------------------------------------------------
;;               METHODS TO PROVE SYSTEM PREDICATES FOR COMPARING 
;;---------------------------------------------------------------------------

(def$method (basic-goalbox :prove-equal) (mode)
  "proves system predicate equal."
  (case mode
    (try   (prepare-reset)
	   (cond (($send prolog-processor
			:unify (deref (second goal))(deref (third goal)))
		  'succ)
		 (t (normal-reset)
		    'fail)))
    (retry (normal-reset)
	   'fail)))

(def$method (basic-goalbox :prove-noequal) (mode)
  "proves system predicate noequal."
  (case mode
    (try   (prepare-reset)
	   (cond (($send prolog-processor :unify
			(deref (second goal)) (deref (third goal)))
		  (normal-reset)
		  'fail)
		 (t (normal-reset)
		    'succ)))
    (retry (normal-reset)
	   'fail)))


(defprolog-method = :prove-equal)

(defprolog-method /= :prove-noequal)

(defmacro == (arg1 arg2)
  `(equal ',arg1 ',arg2))

(defmacro /== (arg1 arg2)
  `(not (equal ',arg1 ',arg2)))

(defprolog-method == :prove-lisp)

(defprolog-method /== :prove-lisp)

(defmacro =.= (arg1 arg2)
  `(= ,arg1 ,arg2))

(defmacro =< (arg1 arg2)
  `(<= ,arg1 ,arg2))

(defmacro =/= (arg1 arg2)
  `(not (= ,arg1 ,arg2)))

(defprolog-method =.= :prove-lisp)

(defprolog-method <   :prove-lisp)

(defprolog-method >   :prove-lisp)

(defprolog-method >=  :prove-lisp)

(defprolog-method =<  :prove-lisp)

(defprolog-method =/= :prove-lisp)


;;---------------------------------------------------------------------------
;;               METHODS TO PROVE SYSTEM PREDICATES FOR READ/WRITE 
;;---------------------------------------------------------------------------

(def$method (basic-goalbox :prove-read) (mode)
  "proves system predicate read."
   (case mode
    (try  (setf clauses ($send prolog-processor :babylon-read))
	  (prepare-reset)
	    (if ($send prolog-processor :unify (deref (second goal)) clauses)
		'succ
		'fail))
    (retry (normal-reset)
	   'fail)))

(def$method (basic-goalbox :prove-write) (mode)
  "proves system predicate write."
   (case mode
    (try  (setf clauses (subst-prolog-vars (second goal) 'ext))
	  (prepare-reset)
	  ($send prolog-processor :babylon-format "~%~S" clauses)
	  'succ)
    (retry  'fail)))

(def$method (basic-goalbox :prove-format) (mode)
  "proves system predicate format."
   (case mode
    (try  (setf clauses (subst-prolog-vars (rest goal) 'ext))
	  (prepare-reset)
	  (eval `($send ',prolog-processor :babylon-format ,@clauses))
	  'succ)
    (retry  'fail)))

(defprolog-method read :prove-read)

(defprolog-method write :prove-write)

(defprolog-method format :prove-format)


;;---------------------------------------------------------------------------
;;               METHODS TO PROVE SYSTEM PREDICATES FOR TYPE CHECKING 
;;---------------------------------------------------------------------------

(def$method (basic-goalbox :prove-type) (mode)
  "proves system predicates atom atomic integer var."
  (case mode
    (try (prepare-reset)
	 (setf clauses (subst-prolog-vars (second goal) 'normal))
	 (if (case (first goal)
	       (atom    (and (symbolp clauses)
			     (not (is-var clauses))))
	       (integer (integerp clauses))
	       (atomic  (or (and (symbolp clauses)
				 (not (is-var clauses)))
			    (integerp clauses)))
	       (var     (is-var clauses)))
	     'succ
	     'fail))
    (retry 'fail)))

(defprolog-method atom :prove-type)

(defprolog-method atomic :prove-type)

(defprolog-method integer :prove-type)

(defprolog-method var :prove-type)


;;---------------------------------------------------------------------------
;;               METHODS TO PROVE SYSTEM PREDICATES FOR CLAUSE MANAGEMENT 
;;---------------------------------------------------------------------------

(def$method (basic-goalbox :prove-asserta) (mode)
  "proves system predicate asserta."
  (case mode
    (try (let* ((clause
		  (normalize-clause (subst-prolog-vars (cadr goal) 'ext)))
		(pred (pred (head clause)))
		(axiom-set nil))
	   (cond ((is-var pred)
		  ($send prolog-processor :babylon-format 
			(getentry wrong-argument-fstr prolog-io-table)
			(subst-prolog-vars (second goal) 'normal)
			'asserta)
		  'error)
		 (t (setq axiom-set
			  ($send prolog-processor :select-axiom-set pred))
		    (prepare-reset)
		    (prolog-assert clause axiom-set #'xpush)
		    (setf clauses (list clause axiom-set))
		    'succ))))
    (retry 'fail)))

(def$method (basic-goalbox :prove-assertz) (mode)
  "proves system predicate assertz."
  (case mode
	(try (let* ((clause
		      (normalize-clause (subst-prolog-vars (cadr goal) 'ext)))
		    (pred (pred (head clause)))
		    (axiom-set nil))
	       (cond ((is-var pred)
		      ($send prolog-processor :babylon-format 
			    (getentry wrong-argument-fstr prolog-io-table)
			    (subst-prolog-vars (second goal) 'normal)
			    'assertz)
		      'error)
		     (t (setq axiom-set
			      ($send prolog-processor :select-axiom-set pred))
			(prepare-reset)
			(prolog-assert clause axiom-set #'nconc)
			(setf clauses (list clause axiom-set))
			'succ))))
	(retry 'fail)))

(def$method (basic-goalbox :prove-assume) (mode)
  "proves system predicate assume."
  (case mode
    (try (let* ((clause
		  (normalize-clause (subst-prolog-vars (cadr goal) 'ext)))
		(pred (pred (head clause)))
		(axiom-set nil))
	   (cond ((is-var pred)
		  ($send prolog-processor :babylon-format
			(getentry wrong-argument-fstr prolog-io-table)
			(subst-prolog-vars (second goal) 'normal)
			'assume)
		  'error)
		 (t (setq axiom-set
			  ($send prolog-processor :select-axiom-set pred))
		    (prepare-side-reset)
		    (prolog-assert clause axiom-set #'nconc)
		    (setf clauses (list clause axiom-set))	
		    'succ))))
    (retry (side-reset)
	   (rem-clause (first clauses) (second clauses))
	   'fail)))

(defun check-for-clause (clause)
  (cond ((is-simple-clause clause)(list clause))
	((is-rule-clause clause)(normalize-clause clause))))

(def$method (basic-goalbox :prove-retract) (mode)
  "proves system predicate retract."
  (prog (clause pred axiom-set next-clause)	
	
        (case mode
	  (try   (prepare-reset)
		 (setq clause
		       (check-for-clause (subst-prolog-vars (second goal) 'int)))
		 (cond ((null clause)
			($send prolog-processor :babylon-format
				      (getentry wrong-argument-fstr prolog-io-table)
				      (subst-prolog-vars (second goal) 'normal)
				      'retract)
			(return 'error)))
		 (setq pred (pred (head clause)))
		 (setq axiom-set
		       ($send prolog-processor :find-axiom-set pred))	
		 (if (null axiom-set)
		     (return 'fail))
		 (setf clauses (get pred axiom-set)))
	  (retry (normal-reset)
		 (setq clause
		       (check-for-clause (subst-prolog-vars (second goal) 'int)))
		 (setq pred (pred (head clause)))
		 (setq axiom-set ($send prolog-processor :find-axiom-set pred))
		 (setf clauses (cdr clauses))
		 (setq mode 'try)))
	
     A (cond ((first clauses)
	      (setq next-clause 
		    ($send prolog-processor
				  :clause-trans-unify clause (first clauses)))
	      (cond ((null next-clause)
		     (setf clauses (rest clauses))
		     (go A))
		    (t (rem-clause (first clauses) axiom-set)
		       (return 'succ))))
	     (t (return 'fail)))))

(def$method (basic-goalbox :prove-abolish) (mode)
  "proves system predicate abolish."
  (case mode
    (try (prepare-reset)
	 (let ((pred (subst-prolog-vars (second goal) 'ext)) axiom-set)
	   (cond ((or (null pred)(is-var pred)(not (symbolp pred)))
		  ($send prolog-processor :babylon-format
				(getentry wrong-argument-fstr prolog-io-table)
				pred 'abolish)
		  'error)
		 (t (setq axiom-set
			  ($send prolog-processor :find-axiom-set pred))
		    (rem-clauses pred axiom-set)
		    (setf clauses (cons pred axiom-set))	;for trace
		    'succ))))
    (retry 'fail)))


(def$method (basic-goalbox :prove-clause) (mode)
  "proves system predicate clause."
  (prog (clause pred axiom-set next-clause)	
	
        (case mode
	  (try (prepare-reset) 	       
	       (setq clause
		     (check-for-clause (subst-prolog-vars (second goal) 'int)))
	       (cond ((null clause)
		      ($send prolog-processor :babylon-format
				    (getentry illegal-clause-fstr prolog-io-table)
				    (subst-prolog-vars (second goal) 'normal))
		      (return 'error)))
	       (setq pred (pred (head clause)))
	       (setq axiom-set ($send prolog-processor :find-axiom-set pred))
	       (if (null axiom-set)
		   (return 'fail)
		   (setf clauses (get pred axiom-set))))
	  (retry (normal-reset)		 
		 (setq clause
		       (check-for-clause (subst-prolog-vars (second goal) 'int)))
		 (setq pred (pred (head clause)))
		 (setq axiom-set ($send prolog-processor :find-axiom-set pred))
		 (setf clauses (cdr clauses))
		 (setq mode 'try)))
	
     A (cond ((first clauses)
	      (setq next-clause 
		    ($send prolog-processor
				  :clause-trans-unify clause (first clauses)))
	      (cond ((null next-clause)
		     (setf clauses (rest clauses))
		     (go A))
		    (t (return 'succ))))
	     (t (return 'fail)))))

(defprolog-method asserta :prove-asserta)

(defprolog-method assertz :prove-assertz)

(defprolog-method assume  :prove-assume)

(defprolog-method abolish :prove-abolish)

(defprolog-method retract :prove-retract)

(defprolog-method clause  :prove-clause)

