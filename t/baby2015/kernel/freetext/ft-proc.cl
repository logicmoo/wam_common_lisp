;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHORS:  Eckehard Gross, Juergen Walther



(def$flavor basic-free-text-processor
	(meta-processor
	 (true-facts nil)
	 (false-facts nil))
	()
  (:initable-instance-variables meta-processor)
  :settable-instance-variables)

(def$method (basic-free-text-processor :reset-proc) ()
  "Reset free text data base to initial state."
  (setf true-facts nil)
  (setf	false-facts nil))


(def$method (basic-free-text-processor :add) (fact mode)
  "Store fact as true or false"
  (case mode
    (true  (setf true-facts  (cons fact true-facts)))
    (false (setf false-facts (cons fact false-facts)))))

(def$method (basic-free-text-processor :recall) (fact)
  "Recall the status of a (negated) fact."
  (let ((success t) (failure nil))
    (cond ((IS-NEGATED-TERM fact)
	   (setq fact (GET-POSITIVE-TERM fact))
	   (setq success nil)
	   (setq failure t)))
    (cond ((member fact true-facts  :test 'equal) success)
	  ((member fact false-facts :test 'equal) failure)
	  (t (UNDETERMINED)))))

(def$method (basic-free-text-processor :remember) (fact)
  "Remember the status of a fact.
Returns nil if already known, fact otherwise."
  (cond ((member fact true-facts :test 'equal) nil)
	(t (setf true-facts  (cons fact true-facts))
	   fact)))

(def$method (basic-free-text-processor :store) (fact)
  "Store the status of a fact.
Returns the fact in any case."
  (if (not (member fact true-facts :test 'equal))
      (setf true-facts  (cons fact true-facts))) 
  fact)


(defun format-translate-true-or-false (fact)
  (format nil
	  (getentry is-it-true-question-fstr free-text-io-table)
	  (if (consp fact) fact `(,fact))))



(def$method (basic-free-text-processor :ask-user) (fact &optional (negation-flag nil))
  (let ((item-list
	  `(,(if negation-flag
		 (getentry expected-answer-no-str free-text-io-table)
		 (getentry expected-answer-yes-str free-text-io-table))
	    ("" :no-select t)
	    ,@(getentry ask-item-list free-text-io-table)))
	(label (getentry choose-one-of-str free-text-io-table)))
    ($send meta-processor :babylon-format "~%~A"                 ;;; <- 
		  (format-translate-true-or-false fact))
    (do ((answer (normalize-answer
		   ($send meta-processor :babylon-read (list *help-key*)))
		 ($send meta-processor :choose-from-menu item-list label))
	 (echo nil t))
	((member answer '(yes no unknown help *help-key*))
	 (if echo
	     ($send meta-processor :babylon-format 
			   "~(~S~)" (translate-answer answer)))	 
	 (cond
	   ((eq answer 'yes) ($send self :add fact 'true) 'true)
	   ((eq answer 'no)  ($send self :add fact 'false) 'false)
	   ((eq answer 'unknown) ($send self  :add fact 'false) 'unknown)
	   ((eq answer 'help) 'help)	 
	   ((eql answer *help-key*)
	    ($send meta-processor :babylon-format "?") 'help))))))



(def$method (basic-free-text-processor :ask-user-without-adding) (fact)
  (let ((item-list `(,(getentry prompt-item free-text-io-table)
		     ,@(getentry ask-item-list free-text-io-table)))
	(label (getentry choose-one-of-str free-text-io-table)))
    ($send meta-processor :babylon-format "~%~A"
		  (format-translate-true-or-false fact))
    (do ((answer (normalize-answer
		   ($send meta-processor :babylon-read (list *help-key*)))
		 ($send meta-processor :choose-from-menu item-list label))
	 (echo nil t))
	((member answer '(yes no unknown help *help-key*))
	 (if echo
	     ($send meta-processor :babylon-format 
			   "~(~S~)" (translate-answer answer)))
	 (cond
	   ((eq answer 'yes) t)
	   ((eq answer 'no)  nil)
	   ((eq answer 'unknown) nil)
	   ((eq answer 'prompt) 'prompt)
	   ((eq answer 'help) 'help)	 
	   ((eql answer *help-key*)
	    ($send meta-processor :babylon-format "?") 'help))))))

(def$method (basic-free-text-processor :get-true-facts-for) 
	   (predicate &optional (test #'(lambda (atom list)
					  (and (consp list)
					       (eq atom (first list))))))
  "Yields a list of all non atomic true facts whose first element equals predicate."
  (let ((facts nil))
    (dolist (a-true-fact true-facts (nreverse facts))
      (if (funcall test predicate a-true-fact)
	  (setf facts (cons a-true-fact facts))))))