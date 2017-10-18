;;  -*- Mode: Lisp; Syntax: Common-Lisp; BASE: 10. -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHOR:   E. G R O S S

;; provisorisch   rest in np-devel-rest

(def$flavor ax-develop-mixin
	   ()
	   ()
  (:required-instance-variables meta-processor axioms)
  :settable-instance-variables
  (:documentation "mixin providing a development environment for clauses."))

;;;;;;;;;;;;;;; DISPLAYING CLAUSES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-rec-clause-items
       (clause axiom-sets &optional previous-predicate (item-len 60))
  (declare (fixnum item-len))
  (let ((head-str (if (is-fact clause)
                    (from-list-to-string clause)
                    (concatenate 'string
                                 "("
                                 (from-list-to-string (first clause))
                                 "  <- "))))
    (declare (simple-string head-str))
    `((,(make-string-of-length item-len "-") :no-select t)
      (,(complete-to-n head-str (- item-len (length head-str)))
       :no-select t)     
      ,@(let ((premises (body clause)))
          (if premises
            (do ((p premises (rest p))
                 (result nil))
                ((null p)(reverse result))
              (push (let ((str (concatenate 'string
                                            "    "
                                            (from-list-to-string (first p)))))
                      `(,(complete-to-n str (- item-len (length str)))
                        :value (:mom-show-premise ,(first p)
                                                  ,axiom-sets
                                                  ,previous-predicate
                                                  ,item-len)))
                    result))
            )))))


(defun exception-comment (goal)
  (cond ((is-var goal)
         (format nil (getentry is-metavar-fstr prolog-io-table) goal))
        ((listp goal)
         (let ((pred (pred goal)))
           (cond ((is-var pred)
                  (format nil (getentry is-varpred-fstr prolog-io-table) goal))
                 ((symbolp pred)
                  (if (get pred 'prolog-method)
                    (format nil (getentry is-system-pred-fstr prolog-io-table) pred)))
                 (t (format nil (getentry syntax-error-fstr prolog-io-table))))))
        (t (format nil (getentry syntax-error-fstr prolog-io-table)))))

(def$method (ax-develop-mixin :mom-show-premise)
	   (goal axiom-sets &optional previous-predicate (item-len 60))
  (prog ((string (exception-comment goal)) clauses item-list choice)
	(cond (string
	       ($send meta-processor :notify string)
	       (return :repeat)))
	(setq clauses (get-clauses-direct goal axiom-sets))
	(cond ((not clauses)
	       ($send meta-processor :notify 
		     (format nil
			     (getentry no-clauses-for-pred-fstr prolog-io-table)
			     (first goal)))
	       (return :repeat)))
	(setq item-list
	      `(("" :no-select t)
		(,(format nil
			  (getentry clauses-for-pred prolog-io-table)
			  (first goal))
		 :no-select t
		 #+:lispm :font #+:lispm fonts:medfnb)
		("" :no-select t)
		,@(mapcan #'(lambda (a-clause)
			      (make-rec-clause-items a-clause
						     axiom-sets
						     (first goal)
						     item-len))
			  clauses)
		(,(make-string-of-length item-len "-") :no-select t)
		("" :no-select t)
		,@(if previous-predicate
		      `((,(format nil
				  (getentry back-to-fstr prolog-io-table)
				  previous-predicate)
			 :value :back
			 #+:lispm :font #+:lispm fonts:medfnb)))
		(" EXIT " :value :exit
		 #+:lispm :font #+:lispm fonts:medfnb)
		("" :no-select t)))
	
     A  (setq choice ($send meta-processor :choose-from-menu item-list))
	(cond ((null choice) (go A))
	      ((eq choice :back) (return :repeat))
	      ((eq choice :exit) (return :exit))
	      ((eq (first choice) :mom-show-premise)
	       (case (lexpr-$send self :mom-show-premise (rest choice))
		 (:repeat (go A))
		 (:exit (return :exit)))))))


(def$method (ax-develop-mixin :inspect-axioms)
	   (&optional (axiom-sets  (get-known-axiom-sets)))
  "displays all clauses of selected predicates in a momentary window.
pops up a menu to select an axiom set from the list axiom-sets and a menu
to select a predicate from the selected axiom set."
   (prog ((axiom-item-list (append axiom-sets
				   `(,(getentry exit-inspect-item prolog-io-table))))
	  axiom-choice preds pred-choice)
      loop
	 (setq axiom-choice
	       ($send meta-processor :choose-from-menu
			     axiom-item-list " List Which Axioms ?"))
	 (cond ((eq axiom-choice 'exit)(return t))
	       ((null axiom-choice)(go loop)))
	 (setq preds
	       (append  (cdr (get-preds axiom-choice))
		       `(,(getentry exit-menu-item prolog-io-table)
			 ,(getentry exit-inspect-item prolog-io-table))))
      loop2
	 (setq pred-choice
	       ($send meta-processor :choose-from-menu
		     preds
		     (format nil
			     (getentry which-pred-fstr prolog-io-table)
			     axiom-choice)))
	 (cond ((eq pred-choice 'exit) (return t))
	       ((eq pred-choice 'loop) (go loop))
	       ((null pred-choice) (go loop2))
	       (t ($send self :mom-show-premise (list pred-choice) axiom-sets)
		  (go loop2)))))


(def$method (ax-develop-mixin :display-predicate)
	   (predicate axiom-set)
  "displays in a momentary menu the clauses
   of predicate <predicate> in axiom set <axiom-set>"
  ($send self :mom-show-premise (list predicate) (list axiom-set)))

;;; eof

