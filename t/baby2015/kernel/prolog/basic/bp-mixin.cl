;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; DATE:     uralt
;; AUTHORS:  Eckehard Gross 

;; This file depends on:  common>*
;;                        meta>kb-stub
;;                        prolog>basic>axioms
;;                        prolog>basic>ax-sc
;;                        prolog>basic>bp-inter
;;                        prolog>basic>bp-proc

;; contents: a mixin making the facilities of basic-prolog-processor available
;;           for a knowledge base.


;;--------------------------------------------------------------------------
;;                  FLAVOR BASIC-PROLOG-MIXIN 
;;--------------------------------------------------------------------------


(def$flavor basic-prolog-mixin
        (prolog-processor
	 (relations-name nil))
	()
  :settable-instance-variables
  (:required-instance-variables
   kb-name procs active-proc system-trace system-trace-window)
  (:documentation "this mixin makes the facilities of basic-prolog-processor available."))

(def$method (basic-prolog-mixin :after :init) (&rest plist)
  "internal method. triggers generation of a prolog processor instance."
  (declare (ignore plist))
  ($send self :generate-prolog-processor)
  (setf procs (cons prolog-processor procs))
  ($send self :init-kbaxset))
 
(def$method (basic-prolog-mixin :generate-prolog-processor) ()
  "generates an instance of basic-prolog-processor associated with the kb."
  (setf prolog-processor (make-$instance 'basic-prolog-processor 
					:meta-processor self
					:alternate-meta-processor
					(make-$instance 'kb-stub
						       :meta-processor self))))


(def$method (basic-prolog-mixin :init-kbaxset) ()
  (setf relations-name
	(or relations-name (intern (format nil "~S-CLAUSES" kb-name))))
  (init-axset relations-name kb-name)
  ($send prolog-processor :set-axioms (list relations-name))
  relations-name)

(def$method (basic-prolog-mixin :set-up-prolog-cmds) ()
  (let ((table (get 'cmd-table ($send self :language))))
    (when (and table ($send self :operation-handled-p :add-operations))
      ($send self :add-sub-operations
	     :top (gethash 'prolog table)
	     :prolog (gethash 'prolog-commands table)))))


;;---------------------------------------------------------------------------
;;               AUXILIARY FUNCTIONS
;;---------------------------------------------------------------------------


(defun send-prolog (method &rest args)
  "passes messages to the prolog processor of the current kb."
  (lexpr-$send (send-kb :prolog-processor) method args))


(defun warn-if-no-prolog ()
  "checks whether the current kb contains  a prolog processor."
  (current-kb-typep 'basic-prolog-mixin (getentry no-prolog-fstr prolog-io-table)))

;;---------------------------------------------------------------------------
;;               CONSTRUCTION OF KB AXIOM SETS 
;;---------------------------------------------------------------------------


(defun remove-default-axset (name)
  (remprop name 'preds)
  (remprop name 'kb-name)
  (setf  *axiom-sets* (remove name *axiom-sets*)))


(def$method (basic-prolog-mixin :assert-clauses) (clauses)
  "builds up an internal representation for <the-relations>."
  (when  (symbolp (first clauses))
    (let ((new-name (first clauses)))
      (unless (eq new-name relations-name)
        (remove-default-axset relations-name)
        (setf relations-name new-name)
        ($send self :init-kbaxset)))
    (setf clauses (rest clauses)))
  (assert-axioms relations-name clauses kb-name))

(defmacro defclauses (&rest clauses)
  "constructor for kb-relations."
  `(and (warn-if-no-prolog)
	(send-kb :assert-clauses ',clauses)))

(defmacro defrelations (&rest relations)
  `(defclauses ,@relations))


;;---------------------------------------------------------------------------
;;               METHODS FOR GOAL EVALUATION 
;;---------------------------------------------------------------------------


(defrequest prolog-goal
	    :recall           :eval-goal
            :recall-immediate :return-nil
	    :prolog           :return-nil)		; defined in meta-mixin

(defun is-prolog-junctor-for-rules (x)
  (member x *prolog-junctor-for-rules*))

#+:SABN(defmacro prolog-type (request)
  "type checking macro."
  `(and (listp ,request)
	(symbolp (first ,request))
	(or (get-properties (symbol-plist (first ,request))
			    ($send prolog-processor :axioms))
	    (is-prolog-junctor-for-rules (first ,request)))
	'prolog-goal))


#-:SABN(defmacro prolog-type (request)
  "type checking macro."
  `(and (listp ,request)
	(symbolp (first ,request))
	(or (get-properties (symbol-plist (first ,request))
			    ($send ($slot 'prolog-processor) :axioms))
	    (is-prolog-junctor-for-rules (first ,request)))
	'prolog-goal))


(assign-typefkt 'prolog-type 'basic-prolog-mixin)


(def$method (basic-prolog-mixin :prolog-why) ()
  "provides context explanations."
  ($send prolog-processor :why))

(def$method (basic-prolog-mixin :read-clauses) ()
  "reads clauses prompting the user."
  (or ($send prolog-processor :send-if-handles
		    :read-clauses (getentry answer-prompt-fstr prolog-io-table))
      ($send self :choose-from-menu
		    `(,(getentry no-develop-entry prolog-io-table)))))

(def$method (basic-prolog-mixin :eval-goal) (goal mode)
  "internal method. evals the prolog goal <goal> passed to the meta-processor."
  (when system-trace
    ($send self :send-system-trace-window :format
	   (getentry sys-trace-fstr prolog-io-table) mode goal))
  (setf active-proc prolog-processor)
  (cond ((is-prolog-junctor-for-rules (first goal))
	 ($send prolog-processor :some-answers goal -1 'VARS))
	(t ($send prolog-processor :first-answer goal))))


;;---------------------------------------------------------------------------
;;               METHODS FOR SELECTING AXIOM-SETS
;;---------------------------------------------------------------------------



(def$method (basic-prolog-mixin :show-axioms) ()
  "displays the names of all axiom sets currently available."
  (let ((current-axioms ($send prolog-processor :axioms)))
    (if (null current-axioms)
	($send self :babylon-format
		      (getentry no-ax-fstr prolog-io-table))
	($send self :babylon-format 
		      (getentry list-ax-fstr prolog-io-table)
		      current-axioms))))


(def$method (basic-prolog-mixin :show-status) ()
  "displays the names of all axiom sets currently available.
intended to be overwritten by specializations of basic-prolog-mixin." 
  ($send self :show-axioms))


(defun generate-current-item-list (current)
  (if current
      `((current
	  ,(format nil " Current = ~S" current)
	  (t)))))

(defun gen-mult-axset-item-list (axiom-sets)
  (mapcar #'(lambda (axset)
	      `(,axset ,(format nil " ~S" axset) (t)))
	  axiom-sets))

(defun gen-choose-axioms-item-list (axiom-sets current-axsets)
  (append (generate-current-item-list current-axsets)
	  (gen-mult-axset-item-list axiom-sets)))

(def$method (basic-prolog-mixin :select-load-axioms) ()
  "makes axiom sets available.
pops up a menu to select one or more axiom sets out of the known axiom sets."

  (let* ((current-axsets ($send prolog-processor :axioms))
	 (axioms-item-list
	   (gen-choose-axioms-item-list (get-known-free-axiom-sets) current-axsets))
	 (axiom-choice
	   ($send self :mult-choose-from-menu axioms-item-list)))
    (cond ((member 'current axiom-choice)
	   ($send prolog-processor :set-axioms
		  (remove-duplicates (append current-axsets
					     (remove 'current axiom-choice))
				     :from-end t)))
	  (t ($send prolog-processor :set-axioms
		    (cons (first current-axsets) axiom-choice))))))

;;---------------------------------------------------------------------------

(def$method (basic-prolog-mixin :add-axiom-set)
	    (axset-name &optional (mode 'first) (before-set nil) (check t))
  "adds the axiom set named <axset-name> to the currently available axiom sets.
if <mode> is first or last, the axiom-set becomes the first or last axiom set, 
if <mode> is before and <before-set> is specified, the axiom-set is added immediately
before it. if <check> is not nil, it is checked whether <axset-name> is known."
  
  ($send prolog-processor :addax axset-name mode before-set check))


;;---------------------------------------------------------------------------

(def$method (basic-prolog-mixin :remove-axiom-set) (axset-name)
  "makes the axiom set named <axset-name> unavailable."
  ($send prolog-processor :send-if-handles :remax axset-name))

;;---------------------------------------------------------------------------

(def$method (basic-prolog-mixin :reset-axiom-sets) (&rest axset-names)
  "resets the specified axiom sets or all currently available axiom sets.
all modifications made by consult, reconsult, edit clauses or by prolog programs
are reset."
  (lexpr-$send prolog-processor :reset-axiom-sets axset-names))



;;---------------------------------------------------------------------------
;;               LISTING AXIOM SETS OR PREDICATES
;;---------------------------------------------------------------------------


(def$method (basic-prolog-mixin :list-predicate)
	   (predicate axset-name &optional window)
  "prints <predicate> from <axset-name> to <window>.
predicate and axiom set are to be specified by their names.
if <window> is omitted, the dialog-stream is used."

  (setf window (or window self))
  ($send window :format
		(getentry clauses-header-fstr prolog-io-table)
		predicate axset-name)
  ($send window :format (print-pred axset-name predicate "" nil))
  ($send window :format "~%"))


(def$method (axset-basic :display-predicate)
	   (predicate axset-name)
  "prints a predicate from an axiom set to the dialog-stream."  
  ($send self :list-predicate predicate axset-name))



(def$method (basic-prolog-mixin :list-axset)
	   (axset-name &optional window)
  "prints the axiom set named <axset-name> to <window>.
if <window> is omitted the dialog-stream is used."
 
  (setf window (or window self))
  ($send window :format
		"~% ------- ~S ------- " axset-name)
  (mapc #'(lambda (pred)
	    ($send window :format "~% ")
	    ($send window :format (print-pred axset-name pred "" nil)))
	(get-predicates axset-name))
  ($send window :format "~%"))


(def$method (basic-prolog-mixin :select-axset-name) (&optional axset-names)
  "presents a menu to select an axiom set.
if specified <axset-names> are presented, otherwise all currently available axiom sets."

  (let ((axnames (or axset-names
		     ($send prolog-processor :axioms))))
    (cond ((null (rest axnames)) (first axnames))
	  (t (do* ((items (append axnames
				  `(,(getentry exit-menu-item prolog-io-table))))
		   (label (getentry select-axset-str prolog-io-table))
		   (axset ($send self :choose-from-menu items label)
			  ($send self :choose-from-menu items label)))
		  ((not (null axset)) axset))))))


(def$method (basic-prolog-mixin :select-list-predicate)
	   (&optional axset-names window)
  "prints a predicate selected via menu to <window>.
only predicates from the specified axiom sets are selectable
or from the currently available axiom sets, if <axset-names> is nil.
if <window> is nil, the dialog-stream is used."

  (let ((axset ($send self :select-axset-name axset-names)))
    (setf window (or window self))
    (when (and axset (not (eq axset 'exit)))
      (do* ((items (append
		     `(,(getentry all-item prolog-io-table))
		     (get-predicates axset)
		     `(,(getentry suspend-item prolog-io-table)
		       ,(getentry exit-menu-item prolog-io-table))))
	    (label (format nil
			   (getentry which-pred-fstr prolog-io-table)
			   axset))
	    (pred ($send self :choose-from-menu items label)
		  ($send self :choose-from-menu items label)))
	   ((eq pred 'exit) t)
	(cond ((eq pred 'all)
	       ($send self :list-axset axset window))
	      ((eq pred 'suspend)
	       ($send self :type-end-to-continue
		      (getentry type-end-to-continue-str prolog-io-table)))
	      ((not (null pred))
	       ($send self :list-predicate pred axset window)))))))


(def$method (basic-prolog-mixin :list-axioms) ()
  "prints a predicate selected via menu to the dialog-stream.
only predicates from the currently available axiom sets are selectable."
  ($send self :select-list-predicate (get-known-axiom-sets)))


;;---------------------------------------------------------------------------
;;               READ GOALS & PROVIDE RESULTS 
;;---------------------------------------------------------------------------

  
(def$method (basic-prolog-mixin :ask-set-goal) ()
  "prompts for a goal and initializes it."

  ($send self :babylon-format
		(getentry goals-prompt-fstr prolog-io-table))
  (let ((answer ($send self :babylon-read
			      (list *help-key* *c-help-key* *end-key*))))
    (cond
      ((member answer (list *help-key* *c-help-key* 'help '?))
       (unless (member answer '(help ?))
	 ($send self :babylon-format "help"))
       ($send self :babylon-format 
	     (getentry explain-goal-format-fstr prolog-io-table))
       ($send self :ask-set-goal))
      ((eq answer 'end)  ($send self :babylon-format "~%") nil)
      ((eql answer *end-key*) ($send self :babylon-format  "end~%") nil)
      (t (unless (null answer)
	   ($send prolog-processor :setgoal answer)
	   answer)))))


(def$method (basic-prolog-mixin :select-format) ()
  "pops up a menu to select the display format."

  (let ((format-option ($send self :choose-from-menu
				     (getentry format-item-list prolog-io-table)
				     (getentry choose-format-str prolog-io-table))))
    (if format-option
	($send prolog-processor :set-format-option format-option))))


(def$method (basic-prolog-mixin :show-form) ()
  "internal method.
displays the topgoal after substitution of all varcells by their values
if the last proof succeded and a NO otherwise."

  (let ((status ($send prolog-processor :status))
	(topgoal ($send prolog-processor :topgoal)))
    (case status
      (succ ($send self :babylon-format
			  (getentry result-fstr prolog-io-table)
			  (subst-prolog-vars topgoal 'ext))
	    t)
      ((fail cfail) ($send self :babylon-format 
				  (getentry no-fstr prolog-io-table)))
      (t ($send self :babylon-format 
		       (getentry status-fstr prolog-io-table) status)))))


(def$method (basic-prolog-mixin :show-vars) (type)
  "internal method.
displays the variables of the topgoal together with their values,
if the last proof succeded, and a NO otherwise.
<type> prescribes, which variables to display:
all means all nonanonymous variables
bound means omit all variables whose values are variables.
if there are no variables, YES is displayed instead."

  (let ((status ($send prolog-processor :status))
	(top-varcells ($send prolog-processor :top-varcells)))
    (case status
      (succ (let ((tlist (gen-var-value-list top-varcells type)))
	      (if (null tlist)
		  ($send self :babylon-format 
				(getentry yes-fstr prolog-io-table))
		  (mapc #'(lambda (var-value)
			    ($send self :babylon-format 
					  "~%~S = ~S"
					  (car var-value)
					  (cdr var-value)))
			tlist))
	      t))
      ((fail cfail) ($send self :babylon-format 
				  (getentry no-fstr prolog-io-table)))
      (t ($send self :babylon-format 
		       (getentry status-fstr prolog-io-table)
		       status)))))

(def$method (basic-prolog-mixin :display-result) (&optional dispform redisplay)
  "displays the result of a proof according to the specified format <dispform>:
form: the topgoal is displayed after substitution of all varcells by their values,
if the last proof succeded, and a NO otherwise.
vars:  all nonanonymous variables are displayed together with their values,
if the last proof succeded, and a NO otherwise.
bound: like vars but variables whose values are variables are omitted,
status: a status message like succ, fail, error is displayed,
no: like status, if <redisplay> is not nil, otherwise nothing is displayed."  

  (let ((status ($send prolog-processor :status))
	(format-option (or dispform
			   ($send prolog-processor :format-option))))
    (case format-option
      (no     (if redisplay
		  ($send self :babylon-format 
				(getentry status-fstr prolog-io-table) status)))
      (form   ($send self :show-form))
      (bound  ($send self :show-vars 'bound))
      (vars   ($send self :show-vars 'all))
      (status ($send self :babylon-format 
			    (getentry status-fstr prolog-io-table) status)))
    status))



;;---------------------------------------------------------------------------
;;               EVALUATION METHODS 
;;---------------------------------------------------------------------------


(def$method (basic-prolog-mixin :prove-display) (&optional goals dispform)
  "attemps the next proof for <goals> and displays the result.
if <goals> is nil, the user is prompted for goals.
if <goals> is *, the current topgoal is used instead.
<dispform> determines how to display the result,
possible values and effects are those described for :display-result."

  (prog ((mode 'retry))
    (cond ((eq goals  '*))
	  ((not (null goals))
	   ($send prolog-processor :setgoal goals)
	   (setq mode 'try))
	  (t (setq goals ($send self :ask-set-goal))
	     (if (null goals)
		 (return nil)
		 (setq mode 'try))))
      (if ($send prolog-processor :prove-topgoals mode)
	  (return ($send self :display-result dispform))
	  ($send self :babylon-format
			(getentry cr-wrong-status-fstr prolog-io-table)
			($send prolog-processor :status) mode))))


(def$method (basic-prolog-mixin :prolog-prove-loop)
	   (&optional goals cont dispformat)
   "attempts one or more proofs for goals and displays the results.
if <goals> is nil, the user is prompted for goals.
if <goals> is *, the current topgoal is used instead.
if <cont> is one, all or a number, one proof, all proofs or as many proofs
as number prescribes are tried, otherwise the user is asked how to proceed:
if he enters ; the next proof is tried, otherwise the loop is stopped.
<dispform> determines how to display the result,
possible values and effects are those described for :display-result."

  (do ((status ($send self :prove-display  goals dispformat)
	       ($send self :prove-display  '* dispformat))
       (answer nil))
      ((member status '(nil fail cfail error)) 'done)
    (cond ((eq cont 'one)(return 'done))
	  ((eq cont 'all))
	  ((numberp cont)(if (<= cont 1)
			     (return 'done)
			     (decf cont)))
	  (t (setf answer
		   ($send self :babylon-read (list #\; #\. #\return)))
	     (cond ((eql answer #\;))
		   (t (return 'done)))))))


(defmacro ?- (&rest goals)
  `(and (warn-if-no-prolog)
	(send-kb  :prolog-prove-loop ',goals)))


(def$method (basic-prolog-mixin :prolog-prove)
	   (&optional goal (disp-format 'vars))
    "attemps the next proof of <goal> and returns the result.
if <goal> is nil, the current topgoal is used instead.
<disp-format> determines what to return:
form: the topgoal is returned after substitution of all varcells by their values,
if the last proof succeded, and NIL otherwise.
vars:  an alist is returned consisting of all nonanonymous variables of the topgoal
paired with their values, if the last proof succeded, and NIL otherwise.
bound: like vars but variables whose values are variables are omitted.
if variables are missing, YES is returned instead."
  ($send prolog-processor :prolog-prove goal disp-format))

(def$method (basic-prolog-mixin :prolog-mult-prove)
	   (goal &optional (number 'all) (disp-format 'vars))
    "provides at most <nr> or all proofs of <goals>. 
the results of each proof are collected in a list.
<dispform> determines the representation of the results,
possible values and effects are those described for :prove-return
with the exeption, that T is returned, if variables are missing
and <dispform> is VARS or BOUND."
    (unless (numberp number) (setf number -1))
    ($send prolog-processor :some-answers goal number disp-format))

;;; eof

