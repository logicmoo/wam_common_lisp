;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base:10; Package: BABYLON -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHOR:   F. di P R I M I O, J. W A L T H E R



;;  ted required
;;

;;-----------------------------------------------------------------------------
;;                   FLAVOR RULE-EXPLAIN-MIXIN
;;-----------------------------------------------------------------------------

(def$flavor rule-explain-mixin
	   ()
	   ()
  (:required-instance-variables
   meta-processor rules-tried rules-used justification-list)
  (:documentation "This is the rule-explain-mixin.
The rule-explain-mixin provides the explanation facilities for the
rule-interpreter."))


(def$method (rule-explain-mixin :get-status) (term)
  "Provides a list of justificandes for term."
  (mapcan #'(lambda (a-justification)
	      (let ((justificand (justification-justificand a-justification)))
		(if (equal justificand term)
		    (list (justification-justificans a-justification)))))
	  justification-list))


(def$method (rule-explain-mixin :get-last-status) (term)
  "Get last status for term."
  (do ((jl justification-list (cdr jl)))
      ((null jl) nil)
    (let* ((a-justification (first jl))
	   (justificand (justification-justificand a-justification)))
      (if (equal justificand term)
	  (return (justification-justificans a-justification))))))

(def$method (rule-explain-mixin :get-last-status-identifier) (term)
  "Get last status identifier for term."
  (let ((last-status ($send self :get-last-status term)))
    (if last-status
	(if (atom last-status) last-status (first last-status))
	:UNDETERMINED)))

(def$method (rule-explain-mixin :translate-status-into-string) (term)
  "Translate status of a term into a string."
  (let ((justificantes ($send self :get-status term)))
    (if (not justificantes)
	`(,(format nil (getentry justifications-missing-error-fstr rule-io-table)
		   term))
	(mapcar #'(lambda (a-justificans)
		    (case a-justificans
		      (:USER-YES
			(format nil (getentry said-to-be-true-fstr rule-io-table)
				term))
		      (:USER-NO
			(format nil (getentry said-to-be-false-fstr rule-io-table)
				term))
		      (:UNKNOWN
			(format nil (getentry said-to-be-unknown-fstr rule-io-table)
				term)) 
		      (t (if (listp a-justificans)
			     (case (first a-justificans)
			       (:RULE-ACTION
				(let* ((rule-set-name-and-rule (second  a-justificans))
				       (rule (second rule-set-name-and-rule))
				       (right-hand-side (rule-right-hand-side rule)))
				  (format nil (getentry how-description-fstr rule-io-table)
					  term
					  (get-action-type right-hand-side)
					  (first rule-set-name-and-rule)
					  (rule-name rule))))
			       (:UNPROVABLE
				 (format nil (getentry not-provable-fstr rule-io-table)
					 term
					 (rule-set-name (second a-justificans))))
			       (t (baberror
				    (getentry justifications-type-error-str rule-io-table)
				    (first a-justificans))))
			     (baberror
			       (getentry justifications-type-error-str rule-io-table)
			       a-justificans)))))
		justificantes))))




;;
;;methodendefinitionen, die die schnittstelle des regelprozessors
;;zur wissensbasis definiert. sie besteht im wesentlichen aus leistungen,
;;die fuer die erklaerungskomponente wichtig sind.




(def$method (rule-explain-mixin :explain-results) ()
  "Explain all the results of rule evaluation."
;  ($send meta-processor :send-explanation-window :expose)
  ($send self :how))

(def$method (rule-explain-mixin :why) (fact current-rule rule-set fact-type)
  "Explain why a fact has some status."
  (case fact-type
    (:PREMISE  ($send self :explain-premise fact current-rule rule-set))
    (:ACTION   ($send self :explain-action fact current-rule rule-set))
    (t (baberror (getentry fact-type-error-fstr rule-io-table)))))


(def$method (rule-explain-mixin :explain-fact) (fact)
  "Explain a facts status."
  (let ((status-string-list ($send self :translate-status-into-string fact)))
    (dolist (a-string status-string-list t)
      ($send meta-processor :send-explanation-window :format " ~A" a-string))))




(def$method (rule-explain-mixin :explain-premise) (fact current-rule rule-set)
  "Explain a premisse."
  (let ((left-hand-side  (rule-left-hand-side current-rule))
	(right-hand-side (rule-right-hand-side current-rule))
	(known-facts ($send self :get-true-facts))
	(all-numbered-facts (make-numbered-facts ($send self :get-all-facts)))
	(negated-conditions nil)
	(known-conditions nil)
	(numbered-facts nil))
    (setq negated-conditions
	  (mapcan #'(lambda (a-condition)
		      (if (is-negated-term a-condition)
			  (list (get-positive-term a-condition))))
		  (get-rule-conditions left-hand-side)))
    (setq known-conditions
	  (mapcan #'(lambda (a-condition)
		      (let ((condition-status
			      ($send self :recall-without-asking a-condition)))
			(if (and condition-status
				 (not (IS-UNDETERMINED condition-status)))
			    (list a-condition))))
		  (get-rule-conditions left-hand-side)))
    ;; This would print all known facts
;        (cond (known-facts
;	       (setq numbered-facts (make-numbered-facts known-facts))
;	       ($send meta-processor :send-explanation-window :format 
;                 "~%~1TIt is already established that:")
;               ($send self :print-numbered-facts numbered-facts)))
    (cond (known-conditions
	   (setq numbered-facts (make-numbered-facts known-conditions))))
    ($send meta-processor :send-explanation-window :format " ")  ;;empty line
    ($send meta-processor :send-explanation-window :format
	  (getentry evaluation-msg-fstr rule-io-table)
	  (rule-set-name rule-set)
	  (rule-name current-rule))	
    ;; This prints the known premises
    (cond (known-conditions
	   ($send meta-processor :send-explanation-window :format 
			 (getentry already-established-msg-str rule-io-table))
	   ($send self :print-numbered-facts numbered-facts)))
    
;;	(if (member fact negated-conditions)
;;	    ($send meta-processor :send-explanation-window :format
;;                   "~%~%~1TIf ~S is false, " fact)
;;	    ($send meta-processor :send-explanation-window :format
;;                  "~%~%~1TIf ~S is true, " fact))

    ($send meta-processor :send-explanation-window :format
		  (getentry false-true-msg-fstr rule-io-table)
		  fact
		  (not (member fact negated-conditions :test #'equal)))
    (mapc #'(lambda (c)
	      (if (is-negated-term c)
		  (setq c (get-positive-term c)))
	      (cond ((and (member c negated-conditions :test #'equal)
			  (not (equal c fact)))
		     ($send meta-processor :send-explanation-window :format 
			   (getentry is-false-msg-fstr rule-io-table)
			   (get-junctor left-hand-side)
			   c))
		    ((or (equal c fact)
			 (member c known-conditions :test #'equal))) 
		    (t ($send meta-processor :send-explanation-window :format 
			     (getentry is-true-msg-fstr rule-io-table)
			     (get-junctor left-hand-side)
			     c))))
	  (get-rule-conditions left-hand-side))
    ($send meta-processor :send-explanation-window :format 
	  (getentry then-msg-fstr rule-io-table)
	  (get-action-type right-hand-side))
    (mapc #'(lambda (a)
	      (or (member a known-facts :test #'equal)
		  ($send meta-processor
			 :send-explanation-window :format  " ~S" a)))
	  (get-rule-actions right-hand-side))
    (if (or known-conditions all-numbered-facts)
	($send self :ask-for-how numbered-facts all-numbered-facts)
	($send meta-processor :type-end-to-continue
	       (getentry type-end-to-continue-str babylon-io-table) ))
    ))


(def$method (rule-explain-mixin :explain-action) (fact current-rule rule-set)
  "Explain an action."
  fact      ;; for the compiler
  (let ((left-hand-side (rule-left-hand-side current-rule))
	(right-hand-side (rule-right-hand-side current-rule))
	(numbered-facts nil)
	(all-numbered-facts
	  (make-numbered-facts ($send self :get-all-facts))))
    ($send meta-processor :send-explanation-window :format
	  (getentry evaluation-msg-fstr rule-io-table)
	  (rule-set-name rule-set)
	  (rule-name current-rule))
    (cond ((get-rule-conditions left-hand-side)
	   ($send meta-processor :send-explanation-window :format
		 (getentry since-msg-fstr rule-io-table)
		 (get-junctor left-hand-side)) 
	   (mapc #'(lambda (c)
		     ($send meta-processor
			    :send-explanation-window :format  " ~S" c))
		 (get-rule-conditions left-hand-side))))
    ($send meta-processor :send-explanation-window :format
	  (getentry i-have-to-msg-fstr rule-io-table)
	  (get-action-type right-hand-side))
    (mapc #'(lambda (a)
	      ($send meta-processor :send-explanation-window :format  " ~S" a))
	  (get-rule-actions right-hand-side))
    (cond ((get-rule-conditions left-hand-side)
	   (setq numbered-facts
		 (make-numbered-facts (get-rule-conditions left-hand-side)))))
    (if (or numbered-facts  all-numbered-facts)
        ($send self :ask-for-how numbered-facts all-numbered-facts)
        ($send meta-processor :type-end-to-continue       ;;; hier um rueckschalten
	       (getentry type-end-to-continue-str babylon-io-table)))   ;; zu verhindrn
    ))


(defun make-facts-choice (numbered-facts &optional (item-len *item-width*))
  (declare (list numbered-facts) (fixnum item-len))
  (if (null numbered-facts)
    nil
    (let* ((numbered-fact (first numbered-facts))
           (str (FROM-LIST-TO-STRING (second numbered-fact)))
           (strlen (length str)))
      (cons `(,(COMPLETE-TO-N str (- item-len strlen))
              :value ,(second numbered-fact))
            (make-facts-choice (rest numbered-facts) item-len)))))


(def$method (rule-explain-mixin :print-numbered-facts) (numbered-facts)	       
  (mapc #'(lambda (f)
            ($send meta-processor :send-explanation-window :format
                   " ~S ~S" (first f) (second f)))
        numbered-facts))


(defun make-numbered-facts (facts &aux (fact-counter 0))
  (mapcar #'(lambda (f)
              (list (setq fact-counter (1+ fact-counter))
                    f))
          facts))


(defun generate-how-menu-items (true-facts all-facts unprovable-facts)
  (declare (ignore unprovable-facts))
  `((,(getentry no-point-str rule-io-table)
     :value EXIT
     :documentation
     ,(getentry exit-label-str rule-io-table))
    ,@(if true-facts
        `((,(getentry how-str rule-io-table)
           :value HOW
           :documentation
           ,(getentry how-str-mouse-doc rule-io-table))))
    ,@(if all-facts
        `((,(getentry how-all-str rule-io-table)
           :value HOW-ALL
           :documentation
           ,(getentry how-all-str-mouse-doc rule-io-table))))
    ;    ,@(if true-facts
    ;	  `((,(getentry how-ultimately-str rule-io-table)
    ;	     :value HOW-ULTIMATELY
    ;	     :documentation
    ;	     ,(getentry how-ultimately-str-mouse-doc rule-io-table))))
    ;    ,@(if unprovable-facts
    ;	  `((,(getentry why-not-str rule-io-table)
    ;	     :value WHY-NOT
    ;	     :documentation
    ;	     ,(getentry why-not-str-mouse-doc rule-io-table))))
    (,(getentry print-rule-item-str rule-io-table)
     :value PRINT-RULE
     :documentation ,(getentry print-rule-item-str rule-io-table))))



(def$method (rule-explain-mixin :ask-for-how)
  (numbered-facts all-numbered-facts &optional unprovable-facts)
  "Make Selection menu and explain selected facts."
  (prog ((menu-list (generate-how-menu-items
                     ;; true-facts all-facts unprovable-facts
                     numbered-facts all-numbered-facts unprovable-facts))
         answer)
    A	(if (setq answer
                  ($send meta-processor :choose-from-menu menu-list "further explanations"))
          (case answer
            (EXIT (return t))
            (HOW (do ((fact ($send self :which-fact numbered-facts)
                            ($send self :which-fact numbered-facts)))
                     ((null fact) t)
                   ($send self :explain-fact fact)))		
            (HOW-ALL (do ((fact ($send self :which-fact all-numbered-facts)
                                ($send self :which-fact all-numbered-facts)))
                         ((null fact) t)
                       ($send self :explain-fact fact)))
            (HOW-ULTIMATELY (let ((fact ($send self :which-fact numbered-facts)))
                              (if (not (null fact))
                                ($send self  :how-ultimately fact))))
            (WHY-NOT (let ((fact ($send self :which-fact unprovable-facts)))
                       (if (not (null fact))
                         ($send self  :why-not fact))))
            (PRINT-RULE ($send self  :print-rule))
            (t (return nil))))
    (go A)))
		
				
(def$method (rule-explain-mixin :which-fact)
  (numbered-facts &optional (item-len *item-width*))
  (prog (choice menu)
    (setq menu
          `((" ****  E X I T **** " :value EXIT)
            ("" :no-select t)
            . ,(make-facts-choice numbered-facts item-len)))
    A  (setq choice ($send meta-processor :choose-from-menu
                           menu
                           (getentry how-which-fact-str rule-io-table)))
    (if (eq choice 'EXIT)
      (return nil)
      (if (null choice) (go A)))
    (return choice)))


(def$method (rule-explain-mixin :how) (&optional (facts nil))
  "Provides how explanations."
  (let ((numbered-facts
         (make-numbered-facts (or facts ($send self :get-true-facts))))
        (all-numbered-facts
         (make-numbered-facts ($send self :get-all-facts)))
        (unprovable-facts
         (make-numbered-facts ($send self :get-unprovable-facts))))
    (cond (numbered-facts
           ($send meta-processor :send-explanation-window :format
                  (getentry results-header-msg-str rule-io-table))
           ($send self :print-numbered-facts numbered-facts))
          (t ($send meta-processor :send-explanation-window :format 
                    (getentry no-positive-results-msg-str rule-io-table))))
    ($send self :ask-for-how numbered-facts all-numbered-facts unprovable-facts)))


;;;;;;;;;;;;;;;;;;;;;;;;  ted-interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(def$method (rule-explain-mixin :how-ultimately) (fact)
  "Provides how ultimately explanations using the ted."
  ;; method requires ted-interface
  (when rules-used
    ($send self :send-if-handles :display-term-tree
           fact
           `(NIL . ,(mapcar #'second rules-used)))
    ;; rule-set-name kann nicht uebergeben werden
    ;; weil mehrere rule sets evaluiert worden sein koennen
    ;; this to get rid of the rule-set-name
    ;; see format of rules used
    ($send meta-processor :type-end-to-continue
           (getentry type-end-to-continue-str babylon-io-table))))


(def$method (rule-explain-mixin :why-not) (fact)
  "Provides why not explanations using the ted."
  ;; method requires ted-interface
  (when rules-tried
    ($send self :send-if-handles :display-unprovable-term-tree fact
           `(NIL . ,(REMOVE-DOUBLES (mapcar #'second rules-tried)))
           self)))

;;;;;;;;   This computes a string list tree for the ted



(def$method (rule-explain-mixin :get-input-type) (term)
  "internal method."
  (let ((term (compute-term term)))
    (case term
      (PROLOG-GOAL `((,(getentry prolog-goal-spec-str rule-io-table))))
      ;; to have a reference on horn clauses
      (t (let ((rule-sets-where-term-is-action
                (mapcan #'(lambda (a-rule-set)
                            (if ($send self :inthen term a-rule-set)
                              `((,(format nil
                                          (getentry input-spec-fstr
                                                    rule-io-table)
                                          (first a-rule-set))))))
                        ($send self :rules))))
           (if rule-sets-where-term-is-action
             rule-sets-where-term-is-action
             `((,(getentry input-spec-str rule-io-table)))))))))


(def$method (rule-explain-mixin :compute-tree) (term rule-set)
  "internal method."
  (let ((rules-with-term-in-then-part
         ($send self :inthen (compute-term term) rule-set)))
    (if (not rules-with-term-in-then-part)
      (if (is-negated-term term)
        `(,(format nil "~S" (get-negation term))
          (,(FROM-LIST-TO-STRING (get-positive-term term))
           . ,($send self :get-input-type  term)))
        `(,(FROM-LIST-TO-STRING term)
          . ,($send self :get-input-type  term)))
      (let ((next-nodes
             (mapcar #'(lambda (a-rule)
                         (let* ((left-hand-side (rule-left-hand-side a-rule))
                                (junctor (get-junctor left-hand-side)))
                           `(,(format nil
                                      "<~S> ~S" (rule-name a-rule) junctor)
                             . ,(mapcar #'(lambda (a-premise)
                                            ($send self :compute-tree
                                                   a-premise rule-set))
                                        (get-rule-conditions left-hand-side)))))
                     rules-with-term-in-then-part)))
        (if (is-negated-term term)
          `(,(format nil "~S" (get-negation term))
            (,(FROM-LIST-TO-STRING (get-positive-term term)) . ,next-nodes))
          `(,(FROM-LIST-TO-STRING term) . ,next-nodes))))))


(def$method (rule-explain-mixin :display-term-tree) (a-term rule-set)
  "Internal method."
  (let* ((string-tree ($send self :compute-tree a-term rule-set))
         (root-name
          (format nil
                  "~A~A" (if (rule-set-name rule-set) ;; rule-set-name
                           (format nil "~S " (first rule-set)) "")
                  (first string-tree) ;; the goal
                  )))
    ; (choose-ted (getentry rule-ted-str rule-io-table))
    ($send meta-processor :send-rule-ted :display
           (cons (concatenate 'string
                              (getentry rule-ted-str rule-io-table)
                              ": "
                              root-name)
                 (cons root-name (rest string-tree))))))


(def$method (rule-explain-mixin :compute-why-not-tree)
  (term rule-set trace-base)
  "internal method."
  (let ((rules-with-term-in-then-part
         ($send self :inthen (compute-term term) rule-set)))
    (if (not rules-with-term-in-then-part)
      (if (is-negated-term term)
        `(,(format nil "~S" (get-negation term))
          (,(format nil "~A ~S"
                    (FROM-LIST-TO-STRING (get-positive-term term))
                    (funcall trace-base :get-last-status-identifier term))))
        `(,(format nil "~A ~S"
                   (FROM-LIST-TO-STRING term)
                   (funcall trace-base :get-last-status-identifier term))))
      (let ((next-nodes
             (mapcar #'(lambda (a-rule)
                         (let* ((left-hand-side (rule-left-hand-side a-rule))
                                (junctor (get-junctor left-hand-side)))
                           `(,(format nil "<~S> ~S" (rule-name a-rule) junctor)
                             . ,(mapcar #'(lambda (a-premise)
                                            ($send self :compute-why-not-tree
                                                   a-premise rule-set
                                                   trace-base))
                                        (get-rule-conditions left-hand-side)))))
                     rules-with-term-in-then-part)))
        (if (is-negated-term term)
          `(,(format nil "~S" (get-negation term))
            (,(format nil "~A ~S"
                      (FROM-LIST-TO-STRING (get-positive-term term))
                      (funcall trace-base
                               :get-last-status-identifier term))
             . ,next-nodes))
          `(,(format nil "~A ~S"
                     (FROM-LIST-TO-STRING term)
                     (funcall trace-base :get-last-status-identifier term))
            . ,next-nodes))))))


(def$method (rule-explain-mixin :display-unprovable-term-tree)
  (a-term rule-set trace-base)
  "internal method."
  (let* ((string-tree
          ($send self :compute-why-not-tree a-term rule-set trace-base))
         (root-name (format nil "~A~A"
                            (if (rule-set-name rule-set) ;; rule-set-name
                              (format nil "~S " (first rule-set))
                              "")
                            (first string-tree) ;; the goal
                            )))
    ($send meta-processor :send-rule-ted :display
           (cons (concatenate 'string
                              (getentry rule-ted-str rule-io-table) ": " root-name)
                 (cons root-name (rest string-tree))))))

;;; eof

