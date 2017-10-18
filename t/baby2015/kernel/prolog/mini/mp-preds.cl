;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; DATE:     uralt
;; AUTHORS:  Eckehard Gross 

;; This file depends on:  common>*
;;                        prolog>basic>*

;; contents: a mixin for the flavor goalbox-core offering trace facilities. 
;;           it provides an encapsulated version of each proof method
;;           of goalbox-core which produces trace information in addition.
;;           the selector of this method is held on the property list
;;           of the predicate using the indicator PROLOG-TRACE-METHOD.
;;           the selector of the method to be used in course of a proof
;;           i.e. the plain or the encapsulated version is held on the
;;           same property list using the indicator CURR-PROLOG-METHOD.

;;--------------------------------------------------------------------------
;;                  FLAVOR GOALBOX-TRACE-MIXIN
;;--------------------------------------------------------------------------


(def$flavor goalbox-trace-mixin
	()
	()
  (:required-instance-variables prolog-processor goal goal-level clauses init-env-depth)
  (:documentation "mixin for goalbox-core providing trace facilities"))



;;---------------------------------------------------------------------------
;;              GETTING AND EXECUTING THE RIGHT PROOF METHOD
;;---------------------------------------------------------------------------

(defmacro defprolog-trace-methods (pred before after)
  (let ((base-method (get pred 'prolog-method))
	(trace-method (intern (format nil "PROVE-TRACE-~S" pred) :keyword)))
    `(progn
       (def$method (goalbox-trace-mixin ,trace-method) (mode)
	 ,(format nil
		  "encapsulated method to prove and trace system predicate ~(~S~)"
		  pred)
	 ,(if before
	      `($send self ,before mode))
	 (let ((res ($send self ,base-method mode)))
	   ($send self ,after mode res)
	   res))
       (setf (get ',pred 'curr-prolog-method) ,base-method)
       (setf (get ',pred 'prolog-trace-method) ,trace-method))))


(defmacro get-curr-method-for-pred (pred)
  "provides the right proof method for pred(icate)."
  `(or (get ,pred 'curr-prolog-method)
       (get '%normal 'curr-prolog-method)))

(defmacro get-curr-method (goal)
  "provides the right proof method for goal."
  `(cond ((varcell-p ,goal) :prove-var)
	((consp ,goal)
	 (let ((pred (pred ,goal)))
	   (cond ((varcell-p pred) :prove-varpred)
		 ((symbolp pred)
		  (get-curr-method-for-pred pred))
		 (t :abort))))
	(t :abort)))


(def$method (goalbox-trace-mixin :prove-goal) (mode)
  "fetches the right method to prove the goal and executes that method."
  ($send self (get-curr-method goal) mode))


;;---------------------------------------------------------------------------
;;               METHODS TO TRACE GOALS WITH USER DEFINED PREDICATES 
;;---------------------------------------------------------------------------


(def$method (goalbox-trace-mixin :trace-top-before) (mode)
  (case mode
    (try   ($send prolog-processor :format-trace
		 0 (getentry first-proof-fstr prolog-io-table)
		 (subst-prolog-vars clauses 'normal)))
    (retry ($send prolog-processor :format-trace
		 0 (getentry next-proof-fstr prolog-io-table)
		 (rest-subst-prolog-vars clauses 'normal init-env-depth)))))

(def$method (goalbox-trace-mixin :trace-top-after) (mode res)
  (declare (ignore mode))
  (if (eq res 'cfail)
      ($send prolog-processor :format-trace
	    0 (getentry top-cut-fail-fstr prolog-io-table))))

(def$method (goalbox-trace-mixin :trace-normal-before) (mode)
  (case mode
    (try ($send prolog-processor :format-trace
	       '+ (getentry normal-try-fstr prolog-io-table)
	       (subst-prolog-vars goal 'normal)))
    (retry ($send prolog-processor :format-trace
		 '+ (getentry normal-retry-fstr prolog-io-table)
		 (rest-subst-prolog-vars goal 'normal init-env-depth)))))

(def$method (goalbox-trace-mixin :trace-normal-after) (mode res)
  (declare (ignore mode))
  (case res
    (succ ($send prolog-processor :format-trace
		'- (getentry normal-succ-fstr prolog-io-table)
		(subst-prolog-vars goal 'normal)))
    (fail ($send prolog-processor :format-trace
		'- (getentry normal-fail-fstr prolog-io-table)
		(pred goal)))
    (cfail (case goal-level
	     (0 ($send prolog-processor :format-trace
		      0 (getentry top-cut-fail-fstr prolog-io-table)))
	     (t ($send prolog-processor :format-trace
		      '- (getentry cut-fail-fstr prolog-io-table)
		      (pred goal)))))))

(defprolog-trace-methods %normal :trace-normal-before :trace-normal-after)

(defprolog-trace-methods %top :trace-top-before :trace-top-after)

;;---------------------------------------------------------------------------
;;               METHODS TO TRACE SOME BASIC SYSTEM PREDICATES 
;;---------------------------------------------------------------------------

(def$method (goalbox-trace-mixin :trace-cut) (mode res)
  (declare (ignore mode))
  (if (eq res 'succ)
      (case goal-level
	(1 ($send prolog-processor :format-trace
		 0 (getentry top-cut-fstr prolog-io-table)))
	(t ($send prolog-processor :format-trace
		 '-+ (getentry cut-fstr prolog-io-table))))))


(def$method (goalbox-trace-mixin :trace-fail) (mode res)
  (declare (ignore mode res))
  ($send prolog-processor :format-trace
	0 (getentry forced-fail-fstr prolog-io-table)))


(def$method (goalbox-trace-mixin :trace-lisp) (mode res)
  (if (eq mode 'try)
      (case res
	(succ ($send prolog-processor :format-trace
		    0 (getentry succ-lisp-fstr prolog-io-table)
		    (subst-prolog-vars goal 'normal)))
	(fail ($send prolog-processor :format-trace
		    0 (getentry fail-lisp-fstr prolog-io-table)
		    (subst-prolog-vars goal 'normal))))))

(def$method (goalbox-trace-mixin :trace-is) (mode res)
  (if (eq mode 'try)
      (case res
	(succ ($send prolog-processor :format-trace
		    0 (getentry succ-is-fstr prolog-io-table)
		    (rest-subst-prolog-vars (second goal) 'normal init-env-depth)
		    (subst-prolog-vars (third goal) 'normal)
		    clauses))
	(fail ($send prolog-processor :format-trace
		    0 (getentry fail-is-fstr prolog-io-table)
		    (rest-subst-prolog-vars (second goal) 'normal init-env-depth)
		    (subst-prolog-vars (third goal) 'normal)
		    clauses)))))

(def$method (goalbox-trace-mixin :trace-nothing) (mode res)
  (declare (ignore mode res))
  t)

(defprolog-trace-methods cut nil :trace-cut)

(defprolog-trace-methods ! nil :trace-cut)

(defprolog-trace-methods fail nil :trace-fail)

(defprolog-trace-methods true nil :trace-nothing)

(defprolog-trace-methods call nil :trace-nothing)

(defprolog-trace-methods callpred nil :trace-nothing)

(defprolog-trace-methods is nil :trace-is)

(defprolog-trace-methods lisp nil :trace-lisp)



;;---------------------------------------------------------------------------
;;               METHODS TO TRACE SYSTEM PREDICATES AND/OR ET AL 
;;---------------------------------------------------------------------------


(def$method (goalbox-trace-mixin :trace-try-before) (mode)
  (cond ((eq mode 'try)
	 ($send prolog-processor :format-trace
	       '+ (getentry normal-try-fstr prolog-io-table)
	       (subst-prolog-vars goal 'normal)))))

(def$method (goalbox-trace-mixin :trace-try-after) (mode res)
  (cond ((eq mode 'try)
	 (case res
	   (succ ($send prolog-processor :format-trace
		       '- (getentry normal-succ-fstr prolog-io-table)
		       (subst-prolog-vars goal 'normal)))
	   (fail ($send prolog-processor :format-trace
		       '- (getentry normal-fail-fstr prolog-io-table)
		       (pred goal)))))))


(def$method (goalbox-trace-mixin :trace-repeat) (mode res)
  (declare (ignore mode res))
  ($send prolog-processor :format-trace
	0
	(getentry repeat-fstr prolog-io-table) 
	clauses))


(defprolog-trace-methods and :trace-normal-before :trace-normal-after)

(defprolog-trace-methods or :trace-normal-before :trace-normal-after)

(defprolog-trace-methods not :trace-try-before :trace-try-after)

(defprolog-trace-methods once :trace-try-before :trace-try-after)

(defprolog-trace-methods cond :trace-normal-before :trace-normal-after)

(defprolog-trace-methods repeat nil :trace-repeat)

(defprolog-trace-methods bagof :trace-try-before :trace-try-after)


;;---------------------------------------------------------------------------
;;               METHODS TO TRACE SYSTEM PREDICATES FOR COMPARING 
;;---------------------------------------------------------------------------


(def$method (goalbox-trace-mixin :trace-equal) (mode res)
  (let ((arg1 (rest-subst-prolog-vars (second goal) 'normal init-env-depth))
	(arg2 (rest-subst-prolog-vars (third goal)  'normal init-env-depth)))
    (if (eq mode 'try)
	(case res
	  (succ ($send prolog-processor :format-trace
		      0 (getentry succ-equal-fstr prolog-io-table)
		      arg1 arg2))
	  (fail ($send prolog-processor :format-trace
		      0 (getentry fail-equal-fstr prolog-io-table)
		      arg1 arg2))))))

(def$method (goalbox-trace-mixin :trace-noequal) (mode res)
  (let ((arg1 (subst-prolog-vars (second goal) 'normal))
	(arg2 (subst-prolog-vars (third goal) 'normal)))
    (if (eq mode 'try)
	(case res
	  (succ ($send prolog-processor :format-trace
		      0 (getentry succ-noequal-fstr prolog-io-table)
		      arg1 arg2))
	  (fail ($send prolog-processor :format-trace
		      0 (getentry fail-noequal-fstr prolog-io-table)
		      arg1 arg2 ))))))

(defprolog-trace-methods = nil :trace-equal)

(defprolog-trace-methods /= nil :trace-noequal)

(defprolog-trace-methods == nil :trace-lisp)

(defprolog-trace-methods /== nil :trace-lisp)

(defprolog-trace-methods =.= nil :trace-lisp)

(defprolog-trace-methods <   nil :trace-lisp)

(defprolog-trace-methods >   nil :trace-lisp)

(defprolog-trace-methods >=  nil :trace-lisp)

(defprolog-trace-methods =<  nil :trace-lisp)

(defprolog-trace-methods =/= nil :trace-lisp)


;;---------------------------------------------------------------------------
;;               METHODS TO TRACE SYSTEM PREDICATES FOR READ/WRITE 
;;---------------------------------------------------------------------------


(def$method (goalbox-trace-mixin :trace-read) (mode res)
  (if (eq mode 'try)
      (case res
	(succ ($send prolog-processor :format-trace
		    0 (getentry succ-read-fstr prolog-io-table)
		    (rest-subst-prolog-vars (second goal) 'normal init-env-depth)
		    clauses))
	(fail ($send prolog-processor :format-trace
		    0 (getentry fail-read-fstr prolog-io-table)
		    (rest-subst-prolog-vars (second goal) 'normal init-env-depth)
		    clauses)))))

(def$method (goalbox-trace-mixin :trace-write) (mode res)
  (declare (ignore res))
  (if (eq mode 'try)
      ($send prolog-processor :format-trace
	    0 (getentry write-fstr prolog-io-table)
	    clauses)))


(def$method (goalbox-trace-mixin :trace-format) (mode res)
  (declare (ignore res))
  (if (eq mode 'try)
      ($send prolog-processor :format-trace
	    0 (getentry format-fstr prolog-io-table)
	    (first clauses)(rest clauses))))


(defprolog-trace-methods read nil :trace-read)

(defprolog-trace-methods write nil :trace-write)

(defprolog-trace-methods format nil :trace-format)


;;---------------------------------------------------------------------------
;;               METHODS TO TRACE SYSTEM PREDICATES FOR TYPE CHECKING 
;;---------------------------------------------------------------------------


(def$method (goalbox-trace-mixin :trace-type) (mode res)
  (if (eq mode 'try)
      (case res
	(succ ($send prolog-processor :format-trace
		    0 (getentry succ-type-fstr prolog-io-table)
		    clauses (first goal)))
	(fail ($send prolog-processor :format-trace
		    0 (getentry fail-type-fstr prolog-io-table)
		    clauses (first goal))))))


(defprolog-trace-methods atom nil :trace-type)

(defprolog-trace-methods atomic nil :trace-type)

(defprolog-trace-methods integer nil :trace-type)

(defprolog-trace-methods var nil :trace-type)


;;---------------------------------------------------------------------------
;;               METHODS TO TRACE SYSTEM PREDICATES FOR CLAUSE MANAGEMENT 
;;---------------------------------------------------------------------------

(def$method (goalbox-trace-mixin :trace-assert) (mode res)
  (if (and (eq mode 'try)
	   (eq res 'succ))
      ($send prolog-processor :format-trace
	    0 (getentry assert-fstr prolog-io-table)
	    (second clauses) (first clauses))))

(def$method (goalbox-trace-mixin :trace-assume) (mode res)
  (case mode
    (try (if (eq res 'succ)
	     ($send prolog-processor :format-trace
		   0 (getentry assert-fstr prolog-io-table)
		   (second clauses) (first clauses))))
    (retry ($send prolog-processor :format-trace
		 0 (getentry remove-fstr prolog-io-table)
		 (second clauses) (first clauses)))))

(def$method (goalbox-trace-mixin :trace-abolish) (mode res)
  (if (and (eq mode 'try)
	   (eq res 'succ))
      (cond ((cdr clauses)
	     ($send prolog-processor :format-trace
		   0 (getentry pred-remove-fstr prolog-io-table)
		   (car clauses) (cdr clauses)))
	    (t ($send prolog-processor :format-trace
		     0 (getentry pred-not-def-fstr prolog-io-table)
		     (car clauses))))))

(def$method (goalbox-trace-mixin :trace-ext-after) (mode res)
  (declare (ignore mode))
  (case res
    (succ ($send prolog-processor :format-trace
		'- (getentry normal-succ-fstr prolog-io-table)
		(subst-prolog-vars goal 'ext)))
    (fail ($send prolog-processor :format-trace
		'- (getentry normal-fail-fstr prolog-io-table)
		(pred goal)))))

(defprolog-trace-methods asserta nil :trace-assert)

(defprolog-trace-methods assertz nil :trace-assert)

(defprolog-trace-methods assume nil :trace-assume)

(defprolog-trace-methods abolish nil :trace-abolish)

(defprolog-trace-methods retract :trace-normal-before :trace-ext-after)

(defprolog-trace-methods clause :trace-normal-before :trace-ext-after)


