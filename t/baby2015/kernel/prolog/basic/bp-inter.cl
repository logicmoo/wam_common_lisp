;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base:10 -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;; DATE:     uralt
;; AUTHOR:   Eckehard Gross

;; This file depends on:  common>*
;;                        prolog>basic>axioms
;;                        prolog>basic>ax-sc
;;                        prolog>basic>bp-preds

;; Contents: a prolog interpreter



(def$flavor prolog-interpreter
	(root
	 (root-type 'basic-goalbox)
	 (status 'noinit))
	(axset-basic proc-sc-mixin)
  :settable-instance-variables
  (:documentation "Flavor providing main functionality for logic programming.
it uses flavor axset-basic for handling clauses and proc-sc-mixin for
transforming clauses according to the structure copying approach.
to prove user provided goals a prooftree is built, whose nodes represent
single subgoals. the root of the prooftree represents a fictive goal %top,
whose successors are nodes representing the user provided goals
thus allowing user requests consisting of several goals.
the nodes are instances of the flavor bound to root-type."))

(def$method (prolog-interpreter :after :setgoal) (goals)
  "generates root goalbox."
  (declare (ignore goals))
  (setf root (make-$instance root-type
			    :prolog-processor self
			    :goal '(%top)
			    :goal-level 0
			    :clauses topgoal))
  (setf status 'init))

(def$method (prolog-interpreter :prove-topgoals) (&optional (mode 'try))
  "provides first/next proof of last user request according to <mode> try/retry."
  (case mode
    (try   (cond ((eq status 'init)
		  (setf status ($send root :prove-goal mode)))))
    (retry (cond ((eq status 'succ)
		  (setf status ($send root :prove-goal mode)))
		 ((eq status 'fail)  status)
		 ((eq status 'cfail) status)))))


(def$method (prolog-interpreter :prove-return) (mode &optional dispform)
  "provides first/next proof of last user request according to <mode> try/retry.
<dispform> determines what to return:
form: the topgoal is returned after substitution of all varcells by their values,
if the last proof succeded, and NIL otherwise.
vars:  an alist is returned consisting of all nonanonymous variables of the topgoal
paired with their values, if the last proof succeded, and NIL otherwise.
bound: like vars but variables whose values are variables are omitted.
if variables are missing, YES is returned instead."
  (if ($send self :prove-topgoals mode)
      ($send self :return-result dispform)
      (baberror (getentry wrong-status prolog-io-table) status mode)))

(def$method (prolog-interpreter :prolog-prove)
	   (&optional goals  (dispform 'form))
  "attemps the next proof of <goals> and returns the result.
if <goals> is nil, the current topgoal is used instead.
<dispform> determines how to display the result,
possible values and effects are those described for :prove-return."
  (cond ((not (null goals))
	 ($send self :setgoal goals)
	 ($send self :prove-return 'try dispform))
	(t ($send self :prove-return 'retry dispform))))


(def$method (prolog-interpreter :first-answer) (goals)
  "provides first proof of <goals>.
returns the topgoal after substitution of all varcells by their values,
if the last proof succeded and NIL otherwise."
  ($send self :setgoal goals)
  ($send self :prove-return 'try 'form))

(defun is-yes-list (list)		
  (and (listp list)
       (member 'yes list)))

(def$method (prolog-interpreter :some-answers)
	   (goals &optional (nr -1) (dispform 'vars))
  "provides at most <nr> proofs of <goals>. 
the results of each proof are collected in a list.
<dispform> determines the representation of the results,
possible values and effects are those described for :prove-return
with the exeption, that T is returned, if variables are missing
and <dispform> is VARS or BOUND."
  ($send self :setgoal goals)
  (do ((res ($send self :prove-return 'try dispform)
	    ($send self :prove-return 'retry dispform))
       (i nr (1- i))
       (results nil (cons res results)))
      ((or (null res) (= i 0))
       (if (is-yes-list results) 
         t
         (nreverse results)))))

(def$method (prolog-interpreter :why) ()
  "provides context explanations"
  (or ($send self :send-if-handles :explain-kontext)
      ($send meta-processor :choose-from-menu
	    `(,(getentry no-explanation-entry prolog-io-table)))))
