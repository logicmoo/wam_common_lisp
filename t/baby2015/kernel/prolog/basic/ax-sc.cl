;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

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

;;---------------------------------------------------------------------------

(def$flavor proc-sc-mixin
	(topgoal
	 top-varcells
	 (format-option 'vars)
	 maxvar
	 env
	 env-depth
	 reset-ops)			
	()
  (:required-instance-variables meta-processor axioms status)
  :settable-instance-variables
  (:documentation "Mixin for prolog-interpreter to handle clauses in course of a proof.
according to the structure copying approach clauses are copied before unification,
their prolog variables beeing replaced by newly generated variables internally
represented by arrays called varcells.
varcells getting instantiated by unification are stacked on the instance variable 
env to allow backtracking. the depth of that stack is stored in env-depth.
reset-ops is a stack for those goals, that caused backtrackable side effects.
the goals provided by the user are copied and stored in topgoal.
all varcells in topgoal representing nonanonymous variables are collected
in top-varcells.
the instance variable format-option controlls the display format of results."))


;;---------------------------------------------------------------------------
;;               INTERNAL REPRESENTATION OF CLAUSES 
;;---------------------------------------------------------------------------


; Defined in file global-variables

;(defvar *maxvar* 0 "next free number for a varcell")

(defvar *vars nil)

(defvar *tenv nil)

(defvar *tenv-depth  0)

(declaim (type list *vars *tenv))
(declaim (type fixnum *tenv-depth *maxvar*))

(defmacro is-var (x)
  `(and (symbolp ,x)
	(char-equal (aref (string ,x) 0) #\_)))


(defstruct (varcell (:type vector)
		    :named
		    (:conc-name nil))
  ;;internal representation of a prolog variable
  varname	;prolog variable varcell is substituted for				    
  varnr		;internal number of varcell
  envnr         ;position in stack env
  varvalue      ;value of varcell
  )

;(defmacro is-varcell (x)
;  `(typep ,x 'varcell))

;(defmacro is-varcell (x)
;  `(varcell-p ,x))

(defmacro gen-varcell (var)
  "generates a varcell for var using *maxvar* as internal number."
  `(prog1 (make-varcell :varname ,var
			:varnr *maxvar*
			:envnr nil)
	  (incf *maxvar*)))

(defun trans-clause (term)
  "copies term substituting prolog variables by associated varcells.
nonanymous variables are collected in *vars to avoid generating
a new varcell for each occurrence of the same variable."
  (let ((*vars nil))
    (trans-clause1 term)))

(defun trans-clause1 (term)
  (cond ((is-var term)
	 (cond ((eq term '_)(gen-varcell term))
	       ((not (member term *vars :test 'equal))
		(push term *vars)
		(set term (gen-varcell term)))
	       (t (symbol-value term))))
	((atom term) term)
	((consp term)(cons (trans-clause1 (first term))
			   (trans-clause1 (rest term))))))

(defmacro is-bound (varcell)
  `(envnr ,varcell))

(defmacro is-rest-bound (varcell nr)
  `(and (envnr ,varcell)
	(<= (envnr ,varcell) ,nr)))

(defun deref (x)
  (cond ((and (varcell-p x)(is-bound x))
	 (deref (varvalue x)))
	(t x)))

(defun rest-deref (x nr)
  (cond ((and (varcell-p x)(is-rest-bound x nr))
	 (rest-deref (varvalue x) nr))
	(t x)))

(defun subst-prolog-vars (term mode)
  "substitutes all varcells in term by their values.
not instantiated varcells are replaced by their internal name, their external
name or by itself according to the values normal ext int of mode."
  (cond ((varcell-p term)
	 (cond ((is-bound term)
		(subst-prolog-vars (varvalue term) mode))
	       (t (case mode
		    (normal (intern (format nil "_~S" (varnr term))))
		    (ext    (varname term))
		    (int     term)))))
	((atom term) term)		  
	((consp term)
	 (cons (subst-prolog-vars (first term) mode)
	       (subst-prolog-vars (rest term) mode)))))

(defun rest-subst-prolog-vars (term mode nr)
  "substitutes all varcells in term by their values neglecting last instantiations.
instantiations are considered if their position in env is <= nr.
varcells not instantiated till then are replaced by their internal name, their
external name or by itself according to the value normal ext int of mode."
  (cond ((varcell-p term)
	   (cond ((is-rest-bound term nr)
		   (rest-subst-prolog-vars (varvalue term) mode nr))
		 (t (case mode
		    (normal (intern (format nil "_~S" (varnr term))))
		    (ext    (varname term))
		    (int    term)))))
	((atom term) term)
	((consp term)
	 (cons (rest-subst-prolog-vars (first term) mode nr)	       
	       (rest-subst-prolog-vars (rest term) mode nr)))))


;;---------------------------------------------------------------------------
;;               SET GOALS & PROVIDE RESULTS 
;;---------------------------------------------------------------------------


(def$method (proc-sc-mixin :setgoal) (goals)
  "initializes processor to prove goals."
  (let ((*vars nil))
    (check-type goals cons (getentry a-list-str prolog-io-table))
    (setq *maxvar* 0)
    (setf topgoal (trans-clause1 goals))
    (setf top-varcells (mapcar #'symbol-value (nreverse *vars)))
    (setf env nil)			
    (setf env-depth 0)			
    (setf reset-ops nil)))   


(def$method (proc-sc-mixin :return-form) ()
  "internal method.
returns topgoal after substitution of all varcells by their values,
if the last proof succeded, and NIL otherwise."
  (case status
    (succ (subst-prolog-vars topgoal 'ext))
    ((fail cfail) nil)
    (t (baberror "~S ~S" (getentry status-str prolog-io-table) status))))


(defun value-is-var (pair)
  (is-var (cdr pair)))

(defun gen-var-value-list (varcells type)
  (let ((list (mapcar #'(lambda (varcell)
			  (cons (varname varcell)
				(subst-prolog-vars varcell 'ext)))
		       varcells)))
    (case type
      (all list)
      (bound (remove-if #'value-is-var list)))))

(def$method (proc-sc-mixin :return-vars) (type)
  "internal method.
returns an alist consisting of variables of the topgoal paired with their values,
if the last proof succeded, and NIL otherwise.
<type> might be ALL or BOUND. in the former case all nonanonymous variables
are considered, in the latter case variables bound to a variable are ommitted.
if variables are missing, YES is returned instead."
  (case status
    (succ (or (gen-var-value-list top-varcells type)
	      'yes))
    ((fail cfail) nil)
    (t (baberror "~S ~S" (getentry status-str prolog-io-table) status))))


(def$method (proc-sc-mixin :return-result) (&optional rform)
  "returns the result of a proof according to <rform> or the current format-option:
form: the topgoal is returned after substitution of all varcells by their values,
if the last proof succeded, and NIL otherwise.
vars:  an alist is returned consisting of all nonanonymous variables of the topgoal
paired with their values, if the last proof succeded, and NIL otherwise.
bound: like vars but variables whose values are variables are omitted.
if variables are missing, YES is returned instead."
  (case (or rform format-option)
    (form  ($send self :return-form))
    (bound ($send self :return-vars 'bound))
    (vars  ($send self :return-vars 'all))
    (t (baberror (getentry wrong-format-fstr prolog-io-table)
	      (or rform format-option)))))


;;---------------------------------------------------------------------------
;;               GET RELEVANT CLAUSES 
;;---------------------------------------------------------------------------


(defmacro is-t (assertions)  ; returned by meta-processor
  `(eq ,assertions t))

(def$method (proc-sc-mixin :get-clauses) (goal)
  "provides the relevant clauses to prove goal.
if there aren't any clauses the meta-processor is asked."
  (let ((clauses (get-clauses-direct goal axioms)))
    (cond (clauses clauses)
	  (meta-processor
	   (let* ((goal-with-substs (subst-prolog-vars goal 'ext))
		  (meta-answer ($send meta-processor :eval
					     goal-with-substs
					     :prolog
					     'prolog-processor)))
	       (cond ((null meta-answer)
		      (setq clauses `((,goal-with-substs (cut) (fail)))))
		     ((is-t meta-answer)
		      (setq clauses `((,goal-with-substs))))
		     (t (setq clauses meta-answer)))
	       clauses)))))


;;---------------------------------------------------------------------------
;;               UNIFICATION & BACKTRACKING 
;;---------------------------------------------------------------------------


(defmacro setvar (varcell term)
  "instantiates varcell with term pushing varcell on the environment stack."
  `(progn (push ,varcell *tenv)
	  (incf *tenv-depth)
	  (setf (varvalue ,varcell) ,term)
	  (setf (envnr ,varcell) *tenv-depth)))

(defun unify (term1 term2)
  "tries to unify term1 term2.
instantiated varcells are stacked in *tenv."
  (cond ((eq term1 term2))	
	((varcell-p term1) (setvar term1 term2))
	((varcell-p term2) (setvar term2 term1))
	((and (consp term1) (consp term2))        ;; statt lisp !!!!
	 (and (unify (deref (first term1)) (deref (first term2)))
	      (unify (deref (rest term1)) (deref (rest term2)))))
	((equal term1 term2))))

(def$method (proc-sc-mixin :unify) (goal clause)	;patch 4.6
  "tries to unify clause with goal. 
returns t if successfull and nil otherwise. instantiated varcells are
stacked on env."
  (let ((*tenv nil)
	(*tenv-depth env-depth))
    (cond ((unify goal clause)
	   (setf env (nconc *tenv env))
	   (setf env-depth *tenv-depth))
	  (t (mapc #'(lambda (varcell)
		       (setf (envnr varcell) nil))
		   *tenv)
	     nil))))

(def$method (proc-sc-mixin :trans-unify) (goal clause)
  "copies clause and tries to unify its head with goal.
returns the transformed clause if successfull and nil otherwise.
instantiated varcells are stacked on env."
  (let ((*tenv nil)
	(*tenv-depth env-depth)
	(nclause (trans-clause clause)))    
    (cond ((unify goal (head nclause))
	   (setf env (nconc *tenv env))
	   (setf env-depth *tenv-depth)
	   nclause)
	  (t (mapc #'(lambda (varcell)
		       (setf (envnr varcell) nil))
		   *tenv)
	     nil))))

(def$method (proc-sc-mixin :clause-trans-unify) (goal clause)
  "copies clause and tries to unify it with goal.
returns the transformed clause if successfull and nil otherwise.
instantiated varcells are stacked on env."
  (let ((*tenv nil)
	(*tenv-depth env-depth)
	(nclause (trans-clause clause)))
    (cond ((unify goal nclause)
	   (setf env (nconc *tenv env))
	   (setf env-depth *tenv-depth)
	   nclause)
	  (t (mapc #'(lambda (varcell)
		       (setf (envnr varcell) nil))
		   *tenv)
	     nil))))

(def$method (proc-sc-mixin :reset-env) (n)
  "resets env stack."
  (declare (fixnum n))
  (do ((var))
      ((>= n (the fixnum env-depth)))
    (setq var (prog1 (first env) (setf env (rest env))))
    (setf (envnr var) nil)
    (setf env-depth (1- (the fixnum env-depth)))))


;;---------------------------------------------------------------------------
;;               RESETTING BACKTRACKABLE SIDE EFFECTS 
;;---------------------------------------------------------------------------


(def$method (proc-sc-mixin :cut-reset) (nr)
  "resets env stack and reset-ops stack."
  (declare (fixnum nr))
  (do ((box (first reset-ops) (first reset-ops)))
      ((or (null box)
           (> nr ($send box :init-env-depth)))
       ($send self :reset-env nr))
    ($send box :prove-goal 'retry)))

(def$method (proc-sc-mixin :push-goalbox) (box)
  (setf reset-ops (cons box reset-ops))
  (setf env-depth (1+ (the fixnum env-depth))))

(def$method (proc-sc-mixin :side-reset) (nr)
  "resets env stack and pops reset-ops stack."
  (declare (fixnum nr))
  (do ((var))
      ((>= (1+ nr) env-depth))
    (setq var
          (prog1 (first env) (setf env (rest env))))
    (setf (envnr var) nil)
    (setf env-depth (1- env-depth)))
  (setf reset-ops (rest reset-ops))
  (setf env-depth (1- env-depth)))


;;---------------------------------------------------------------------------
;;               MIXIN FOR GOALBOX-BASIC 
;;---------------------------------------------------------------------------


(def$flavor goalbox-sc-mixin
	(prolog-processor
	  goal
          clauses
	  init-env-depth)
	()
  :settable-instance-variables
  (:documentation "Mixin for goalbox-basic corresponding to proc-sc-mixin.
goalbox-basic represents a single (sub)goal emerging during the proof of
a user provided goal. clauses contains all relevant clauses not yet tried
to proof goal and the currently used clause. 
to be able to reset instantiations on backtracking init-env-depth
remembers the depth of the instantiations stack at the beginning of
a proof of goal."))


;;---------------------------------------------------------------------------
;;               MACROS FOR RESETTING 
;;---------------------------------------------------------------------------

#+:SABN(defmacro prepare-reset ()
  "prepares a goal for backtracking."
  `(setf init-env-depth ($send prolog-processor :env-depth)))


#-:SABN(defmacro prepare-reset ()
  "prepares a goal for backtracking."
  `(setf ($slot 'init-env-depth) ($send ($slot 'prolog-processor) :env-depth)))


#+:SABN(defmacro prepare-side-reset ()
  "prepares a goal causing backtrackable side effects for backtracking."
  `(progn (setf init-env-depth ($send prolog-processor :env-depth))
	  ($send prolog-processor :push-goalbox self)))


#-:SABN(defmacro prepare-side-reset ()
  "prepares a goal causing backtrackable side effects for backtracking."
  `(progn (setf ($slot 'init-env-depth)
		($send ($slot 'prolog-processor) :env-depth))
	  ($send ($slot 'prolog-processor) :push-goalbox self)))


#+:SABN(defmacro normal-reset ()
  "resets a goal."
  `($send prolog-processor :reset-env init-env-depth))


#-:SABN(defmacro normal-reset ()
  "resets a goal."
  `($send ($slot 'prolog-processor) :reset-env ($slot 'init-env-depth)))


#+:SABN(defmacro cut-reset ()
  "resets a goal in case of cut."
  `($send prolog-processor :cut-reset init-env-depth))


#-:SABN(defmacro cut-reset ()
  "resets a goal in case of cut."
  `($send ($slot 'prolog-processor) :cut-reset ($slot 'init-env-depth)))

#+:SABN(defmacro side-reset ()
  "resets a goal causing backtrackable side effects."
  `($send prolog-processor :side-reset init-env-depth))


#-:SABN(defmacro side-reset ()
  "resets a goal causing backtrackable side effects."
  `($send ($slot 'prolog-processor) :side-reset ($slot 'init-env-depth)))




;;; eof

