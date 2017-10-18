;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10. -*-

(in-package "BABYLON") 

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;; DATE:     uralt
;; AUTHOR:   Franco di Primio, Eckehard Gross

;; This file depends on:  common>*

;; Contents: a handler for axiom sets.
;;           an axiom set consists of a list of clauses.
;;           all predicates occuring in the head of one of these clauses are held
;;           on the property list of the axiom set name using PREDS as indicator.
;;           the defining clauses of a predicate are held on the property
;;           list the predicate name using the axioms set name as indicator.
;;           axiom sets are free or associated with a kb. in the latter case
;;           the kb name is stored as KB-NAME property of the axiom set name.
;;           

;;---------------------------------------------------------------------------
;;               FLAVOR FOR CLAUSE MANAGEMENT 
;;---------------------------------------------------------------------------


(def$flavor axset-basic
	((axioms nil))
	(processor-core)
  :settable-instance-variables
  (:documentation "flavor managing axiom sets and clauses for the prolog interpreter.
axiom sets currently available are bound to axioms."))


;;---------------------------------------------------------------------------
;;           GENERATING INTERNAL REPRESENTATIONS OF AXIOM SETS 
;;---------------------------------------------------------------------------


; Defined in file global-variables
;(defvar *AXIOM-SETS* nil "list of known axiom sets.")


(defun get-known-axiom-sets ()
  *axiom-sets*)


(defmacro get-preds (axiom-set)
  "gets the list of predicates of <axiom-set>."
  `(get ,axiom-set 'preds))

(defun get-predicates (axset-name)
  (rest (get-preds axset-name)))

(defmacro rem-preds (axiom-set)
  "removes the list of predicates of <axiom-set>."
  `(remprop ,axiom-set 'preds))

(defmacro rem-pred (pred axiom-set)
  "removes <pred> from the list of predicates of <axiom-set>."
  `(setf (get-preds ,axiom-set)
	 (delete ,pred (get-preds ,axiom-set))))

(defmacro add-pred (pred axiom-set)
  "adds <pred> to the list of predicates of <axiom-set>."
  `(setf (get ,axiom-set 'preds)
	 (nconc (get ,axiom-set 'preds) (list ,pred))))



;;--------------------------------------------------------------------------------

(defmacro head (rule)
  `(car ,rule))

(defmacro body (rule) 
  `(cdr ,rule))

(defmacro pred (head)
  `(car ,head))


(defun normalize-clause (clause)
  "removes <- from rules and transforms facts into rules without body."
  (if (atom clause)
      (baberror (getentry clause-syntax-error-fstr prolog-io-table)))
  (if (atom (first clause))
      (setq clause (list clause)))
  (if (and (consp (cdr clause))
	   (eq (second clause) '<-))
      (cons (first clause) (rest (rest clause)))
      clause))

(defun get-subgoal-predicates (clause)
  "yields the predicates used in the subgoals of <clause>."
  (let ((subpreds nil))
    (dolist (a-subgoal (body (normalize-clause clause)) (nreverse subpreds))
      (setf subpreds (if (not (is-variable (pred a-subgoal)))
			 (cons (pred a-subgoal) subpreds)
			 subpreds)))))


(defun get-clauses (predicate axiom-set)
  (get predicate axiom-set))


(defun rem-clause (clause axiom-set)
  "removes <clause> from the defining clauses of <axiom-set>.
the predicate of <clause> is removed from the list of predicates of <axiom-set>,
if no clauses remain."
  (let* ((head (head clause))
	 (pred (pred head))
	 (rest-clauses (delete clause (get pred axiom-set))))
    (if (null rest-clauses)
	(rem-pred pred axiom-set))
    (setf (get pred axiom-set) rest-clauses)))


(defmacro rem-clauses (pred axiom-set)
  "removes <pred> from the list of predicates of <axiom-set> with all defining clauses."
  `(progn (remprop ,pred ,axiom-set)
	  (rem-pred ,pred ,axiom-set)
	  ,pred))

(defun remove-all-clauses (axiom-set) 
  "removes all clauses in <axiom-set> without deleting <axiom-set>."
  (mapc #'(lambda (pred)
	    (rem-clauses pred axiom-set))
	(cdr (get-preds axiom-set)))
  axiom-set)


(defun prolog-assert (clause axiom-set xconc)
  "adds <clause> to the clauses of <axiom-set> using <xconc> for the placement."

   (let* ((nclause (normalize-clause clause))
	  (head (head nclause))
	  (pred (pred head))
	  (clauses-sofar (get pred axiom-set))
	  (pred-sofar (get-preds axiom-set)))
     (if (not (member nclause clauses-sofar :test 'equal))
	 (setf (get pred axiom-set)
	       (funcall xconc clauses-sofar (list nclause))))
     (if (not (member pred pred-sofar :test 'equal))
	 (add-pred pred axiom-set))
     pred))

(defun xpush (list x)
  (nconc x list))

(defmacro asserta (clause axiom-set)    
  `(prolog-assert ',clause ',axiom-set #'xpush))

(defmacro assertz (clause axiom-set)    
  `(prolog-assert ',clause ',axiom-set #'nconc))

(defun add-axioms (axiom-set clauses)
  "adds <clauses> to the clauses of <axiom-set>."
  (if clauses
      (mapc #'(lambda (clause)
	    (prolog-assert clause axiom-set #'nconc))
	clauses))
  axiom-set)

;;-------------------------------------------------------------------------------------------
;;                           AXIOM SET GENERATION  & RESETTING
;;-------------------------------------------------------------------------------------------

(defun init-axset (axset-name &optional kb-name)
  "builds an empty axiom set named <axset-name>.
if <kb-name> is not NIL, <axset-name> is marked to be associated with <kb-name>."
  (cond ((member axset-name *axiom-sets*)
	 (remove-all-clauses axset-name))
	(t (push axset-name *axiom-sets*)
	   (setf (get axset-name 'preds) (list '$preds))
	   (if kb-name
	       (setf (get axset-name 'kb-name) kb-name))))
  axset-name)


(defun assert-axioms (axset-name clauses &optional kb-name)
  "builds an axiom set named <axset-name> consisting of the predicates defined by <clauses>.
if <kb-name> is not NIL, <axset-name> is marked to be associated with <kb-name>."

  (init-axset axset-name kb-name)
  (add-axioms axset-name clauses)
  (setf (get axset-name 'clauses) clauses)
  axset-name)


(defmacro defaxiom-set (axset-name &rest clauses)
  "constructor macro for free axiom sets."
  `(assert-axioms ',axset-name ',clauses))

(defun reset-axiom-set (axiom-set  &optional (axiom-sets (get-known-axiom-sets)))
  "resets <axiom-set> if it belongs to the list of axiom sets <axiom-sets>."

    (cond ((member axiom-set axiom-sets)
	   (remove-all-clauses axiom-set)
	   (add-axioms axiom-set (get axiom-set 'clauses))
	   axiom-set)
	  (t (send-kb :babylon-format
		      (getentry unknown-axset-fstr prolog-io-table) axiom-set))))

(def$method (axset-basic :reset-axiom-sets) (&rest axiom-sets)
  "resets <axiom-sets> if specified or all currently available axiom sets.
all modifications made by consult, reconsult, edit clauses or by prolog programs
are reset."

  (let ((axiom-sets-copy (or (copy-list axiom-sets)
			     axioms)))
    (mapc #'(lambda (axiom-set)
	      (reset-axiom-set axiom-set axioms))
	  axiom-sets-copy)
    axiom-sets-copy))

;;---------------------------------------------------------------------------
;;               PRINTING OF CLAUSES
;;---------------------------------------------------------------------------

(defun ext-rep-clause (clause)
  "introduces <- in rules."
  (if (and (rest clause)
	   (not (eq (second clause) '<-)))
      `(,(first clause) <- ,@(rest clause))
      clause))


(defun print-clause (clause &optional (label "") (stream *default-dialog-stream*))
  "prints <clause> to <stream> headed by <label>.
<clause> might be NIL producing an empty line.
if <stream> is NIL, a string is returned that contains the output,
otherwise an empty string is returned."
 
 (let ((head (first clause))
       (body (rest clause)))
   (or (cond (body (format stream
			   #+:SYMBOLICS "~%~A~(~S <- ~{~%   ~S~}~)"
			   #-:SYMBOLICS "~%~A(~S <- ~{~%     ~S~})"
			   label head body))
	     (clause (format stream "~%~A~S" label clause))
	     (t (format stream "~%")))
       "")))

(defun print-clauses (clauses &optional (label "") (stream *default-dialog-stream*))
  "prints <clauses> to <stream>, each clause headed by <label>.
if <stream> is NIL, a string is returned that contains the output,
otherwise an empty string is returned."

  (apply #'concatenate
	 'string
	 (mapcar #'(lambda (clause)
		     (print-clause clause label stream))
		 clauses)))

(defun print-pred (axset pred &optional (label "") (stream *default-dialog-stream*))
  "prints the defining clauses of <pred> in <axset> to <stream>.
each clause is headed by <label>. if <stream> is NIL, a string is returned
that contains the output, otherwise an empty string is returned."

  (print-clauses (get-clauses-direct (list pred) (list axset))
		 label
		 stream))

(defun collect-clauses (axset preds)
  "provides a list of the defining clauses for the predicates <preds> in <axset>. 
clauses for different predicates are separated by NIL."

  (rest (mapcan #'(lambda (a-pred)
		     (cons nil (copy-list (get-clauses-direct
					    (list a-pred)
					    (list axset)))))
		     preds)))

(defun print-preds (axset preds &optional (label "") (stream *default-dialog-stream*))
  "prints the defining clauses of all <preds> in <axset> to <stream>.
each clause is headed by <label>. clauses for different predicates are
separated by an empty line. if <stream> is NIL, a string is returned
that contains the output, otherwise an empty string is returned."
  (print-clauses (collect-clauses axset preds) label stream))


(defun print-axiom-set (axiom-set &optional (stream *default-dialog-stream*))
  (let ((preds (cdr (get-preds axiom-set))))
    (format stream "~2%(~A ~S"
	    (if (get axiom-set 'kb-name) "DECLAUSES" "DEFAXIOM-SET")
	    axiom-set)   
    (print-preds axiom-set preds "   " stream)
    (format stream  ")")))


(def$method (axset-basic :print) (&optional (stream *default-dialog-stream*))  
  "prints all clauses in the associated kb to <stream>."
 
  (let ((relname ($send meta-processor :relations-name)))
    (when (get relname 'clauses)
      (format stream (getentry relations-fstr prolog-io-table))
      (print-axiom-set relname stream))))

(def$method (axset-basic :kb-inform) (&optional (stream *default-dialog-stream*))
  "displays the number of clauses in the associated kb on <stream>."
  (let ((clause-nbr 0))
    (dolist (axiom-set  ($send self :axioms))
      (setf clause-nbr (+ clause-nbr (length (get axiom-set 'clauses)))))
    (format stream 
	    (getentry number-of-relations-fstr prolog-io-table)
	    clause-nbr)))
#|

(def$method (axset-basic :kb-inform) (&optional (stream *default-dialog-stream*))
  "displays the number of clauses in the associated kb on <stream>."
  
  (let ((kb-clauses (the list 
                         (get ($send meta-processor :relations-name) 'clauses))))
    (format stream 
            (getentry number-of-relations-fstr prolog-io-table)
            (length kb-clauses))))

|#

;;---------------------------------------------------------------------------
;;               LOCATING CLAUSES 
;;---------------------------------------------------------------------------


(defmacro clause-type (clause)
  `(cond ((rest ,clause) 'rule)
	 (,clause 'fact)
	 (t 'none)))

(defmacro is-fact (clause)
  `(null (body ,clause)))

(defmacro is-simple-clause (clause)
  `(and (consp ,clause)
	(symbolp (first ,clause))))

(defmacro is-rule-clause (clause)
  `(and (consp ,clause)
	(consp (first ,clause))
	(symbolp (first (first ,clause)))))

(defun get-clauses-direct (goal axiom-sets)
  "gets the relevant clauses for <goal> from <axiom-sets>."
  (do ((pred (pred goal))
       (clauses nil)
       (r-axiom-sets axiom-sets (cdr r-axiom-sets)))
      ((null r-axiom-sets) nil)       
    (setq clauses (get pred (car r-axiom-sets)))
    (if clauses
	(return clauses))))


(def$method (axset-basic :find-axiom-set) (pred)
  "searches for the first axiom set containing clauses for <pred>.
returns nil if none is found."
  (do ((raxioms axioms (cdr raxioms)))
      ((null raxioms) nil)
    (if (get pred (car raxioms))
	(return (car raxioms)))))

(def$method (axset-basic :select-axiom-set) (pred)
  "selects an axiom set where to place clauses for <pred>.
returns the first axiom set containing clauses for <pred> or the first axiom set,
if none contains clauses for <pred>."
  (or ($send self :find-axiom-set pred)
      (first axioms)))


;;---------------------------------------------------------------------------
;;               MAKING AXIOM SETS AVAILABLE 
;;---------------------------------------------------------------------------



(defun get-known-free-axiom-sets ()
  (remove-if #'(lambda (axset)
		 (get axset 'kb-name))
	     (get-known-axiom-sets)))

(defun known-axiom-set (axset-name)
  "checks whether <axset-name> is the name of a known axiom set.
returns <axset-name>, if it is known and NIL otherwise."
  (if (member axset-name (get-known-free-axiom-sets))
      axset-name 
      (send-kb :babylon-format
	       (getentry unknown-axset-fstr prolog-io-table) axset-name)))                    

(def$method (axset-basic :use-axiom-sets) (&rest axiom-sets)
  "makes <axiom-sets> currently available in addition to the own axiom set.
checks whether all axiom sets are known. if not nothing is changed."
  (if (dolist (axset axiom-sets t)
	(unless (known-axiom-set axset)
	  (send-kb :babylon-format
		   (getentry unknown-axset-fstr prolog-io-table) axset)
	    (return nil)))
      ($send self :set-axioms (cons (first axioms) (copy-list axiom-sets)))))

(def$method (axset-basic :remax) (axiom-set)
  "makes the free axiom set <axiom-set> unavailable."
  (unless (eq axiom-set (first axioms))
    (let ((new-axioms (delete axiom-set axioms :count 1)))
      ($send self :set-axioms new-axioms))))

(defun splice (new before list)
  "adds item <new> to <list> before item <before>."
  (do ((newlist nil (cons (first restlist) newlist))
       (restlist list (rest restlist)))
      ((null restlist) (nreverse (cons new newlist)))
    (if (eq before (first restlist))
	(return (append (nreverse newlist) (list new) restlist)))))

(def$method (axset-basic :addax)
	    (axiom-set &optional (mode 'first) (before-set nil) (check t))
  "adds <axiom-set> to the currently available axiom sets.
according to <mode> <axiom-set> becomes the first axiom set, the last axiom set
or the axiom set before <before-set>.
if <check> is not nil, it is checked whether <axiom-set> is known."
  (if (or (null check)
	  (known-axiom-set axiom-set))
      (let ((free-axioms
	      (case mode
		(first  (cons axiom-set (rest axioms)))
		(last   (append (rest axioms) (list axiom-set)))
		(before (splice axiom-set before-set (rest axioms)))
		(t ($send meta-processor :babylon-format
			  (getentry unknown-mode-fstr prolog-io-table) mode)
		   (rest axioms)))))
	($send self :set-axioms (cons (first axioms) free-axioms))
	axiom-set)))

;;; eof

