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

;; contents: a mixin providing trace facilities for the prolog processor.
;;           predicates currently to be traced are held on the variable
;;           *prolog-preds-traced*
;;           

;;--------------------------------------------------------------------------
;;                  FLAVOR PROLOG-TRACE-MIXIN 
;;--------------------------------------------------------------------------

(def$flavor prolog-trace-mixin
	((prolog-trace nil)  
	 (trace-list nil)
	 (trace-mode 'normal)
	 (trace-level nil)
	 (indent-level 0))
	()
  (:required-instance-variables meta-processor axioms)
  :settable-instance-variables
  (:documentation "this mixin provides tracing facilities for the prolog processor."))


;(defvar *prolog-preds-traced* nil) 

;;; ersetzt

(def$method (prolog-trace-mixin :trace-status) ()
  (if prolog-trace 
      (format nil (getentry trace-on-fstr  babylon-io-table) "Prolog")
      (format nil (getentry trace-off-fstr babylon-io-table) "Prolog")))


(def$method (prolog-trace-mixin :toggle-prolog-trace) ()
  "Toggles prolog trace mode."
  (setf prolog-trace (if prolog-trace nil t))
  ($send self :synchronize-trace))


(def$method (prolog-trace-mixin :get-preds-with-mark) (axset-name)
  (mapcar #'(lambda (pred)
	      (cons pred (not (null (or (eq trace-list 'all)
					(member pred trace-list))))))
	  (get-predicates axset-name)))

;;--------------------------------------------------------------------------

;;each method proving a goal corresponds a method proving and tracing that goal.

(defun mark-pred (pred)
  (let ((tmethod (or (get pred 'prolog-trace-method)
		     (get '%normal 'prolog-trace-method))))
    (setf (get pred 'curr-prolog-method) tmethod)))

(defun unmark-pred (pred)
  (let ((nmethod (get pred 'prolog-method)))
    (if nmethod
	(setf (get pred 'curr-prolog-method) nmethod)
	(remprop pred 'curr-prolog-method))))

(def$method (prolog-trace-mixin :synchronize-trace) ()
  (cond ((null prolog-trace)
	 (cond ((null *prolog-preds-traced*))
	       ((eq *prolog-preds-traced* 'all)
		(mapc #'unmark-pred *prolog-syspreds*))
	       (t (mapc #'unmark-pred *prolog-preds-traced*)))
	 (setf *prolog-preds-traced* nil))	
	((eq *prolog-preds-traced* trace-list))
	(t (cond ((null *prolog-preds-traced*))
		 ((eq *prolog-preds-traced* 'all)
		  (mapc #'unmark-pred *prolog-syspreds*))
		 (t (mapc #'unmark-pred *prolog-preds-traced*)))
	   (cond ((null trace-list))
		 ((eq trace-list 'all)
		  (mapc #'mark-pred *prolog-syspreds*))
		 (t (mapc #'mark-pred trace-list)))
	   (setf *prolog-preds-traced* trace-list))))


;;--------------------------------------------------------------------------

(def$method (prolog-trace-mixin :after :set-axioms) (axsets)
  (declare (ignore axsets))
  (setf trace-list 
	(if (or (eq trace-list 'all)
		(equal trace-list '(%top)))
	    trace-list
	    (cons '%top (remove-if-not
			  #'(lambda (pred)
			      (get-properties (symbol-plist pred) axioms))
			  trace-list))))
  ($send self :synchronize-trace))

(def$method (prolog-trace-mixin  :before :prove-topgoals) (&rest goals)
  (declare (ignore goals))
  ($send self :synchronize-trace))
  
;;;------------------------------------------------------------------------

(def$method (prolog-trace-mixin :after :setgoal) (goals)
  (declare (ignore goals))
  (setf indent-level 0))

(defun normal-indent (level)
  (declare (fixnum level))
  (format nil "~V@T" (* 3 level)))

(def$method (prolog-trace-mixin :before :trans-unify) (goal clause)
  "traces clause tried to unify in case of full mode."
  (and prolog-trace
       (eq trace-mode 'full)
       (or (eq trace-list 'all)
	   (member (first goal) trace-list))
       ($send meta-processor :send-prolog-trace-window :format
		     "~Ausing: ~S"
		     (normal-indent indent-level)
		     (ext-rep-clause clause))))

(def$method (prolog-trace-mixin :before :clause-trans-unify) (goal clause)
  "traces clause tried to unify for system-predicates clause, retract in case of full mode."
  (and prolog-trace
       (eq trace-mode 'full)
       (or (eq trace-list 'all)
	   (member (first goal) trace-list))
       ($send meta-processor :send-prolog-trace-window :format 
	      "~Ausing: ~S"
	      (normal-indent indent-level)
	      (ext-rep-clause clause))))

(def$method (prolog-trace-mixin :format-trace) (ind-pattern fstring &rest args)
  (if (member ind-pattern '(- -+))
      (setf indent-level (1- indent-level)))
  (lexpr-$send meta-processor :send-prolog-trace-window :format 
		      fstring (normal-indent indent-level) args)
  (if (member ind-pattern '(+ -+))
      (setf indent-level (1+ indent-level))))

;;; eof

