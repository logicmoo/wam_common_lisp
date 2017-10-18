;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

  
;;; adopted to non SYMBOLICS machines for BABYLON 2.1 by Juergen Walther 10.5.89


(def-kb-instance top-down-refine k1dummyc :pkg :ks)

;; the kb uses the definitions of tasks&agendas knowledge base

(do ()
    ((member 'tasks&agendas *known-knowledge-bases*)
     (progn
       ($send top-down-refine :make-yourself-current)
       (send-kb :send-if-handles :import-kb tasks&agendas)
       "tasks&agendas imported"))
  (if (not (member 'tasks&agendas *known-knowledge-bases*))
    (cerror "Load << tasks&agendas >> before proceeding!"
            "Unknown Knowledge Base ~S" 'tasks&agendas)))

;; _________________________________________________________________________


;; can be used to implement a top down refine strategy 
;; a la CSRL (BYLANDER et al. 83)
;; Bylander, T.; Mittal, S.; Chandrasekaran, B.
;; CSRL: A language for Expert Systems for Diagnosis
;; in: Proc. IJCAI-83, 218-221
;; Franco di Primio, Karl Wittur

;; ****************** RULE-PROCESSOR PREDICATES and FUNTIONS ******************

(defun is-rule-set-forward-control-structure (x)
  (member x '(:DO-ONE :DO-ALL :WHILE-ONE :WHILE-ALL)))

(defun default-rule-set-forward-control-structure ()
  :DO-ALL)

(defun is-known-rule-set-name (x)
  (assoc x (send-current-knowledge-base :rules)))

;;________________________________________________________________

;; a diagnose-unit is a special task
;; for the user the following slots of a diagnose-unit are
;; relevant:
;;
;; a) DIAGNOSE-NAME
;;    the name of the diagnose-unit for tracing/explanation
;; b) FATHER
;;    the father of this diagnose-unit
;; c) CHILDREN
;;    a list of the children of this diagnose-unit,
;;    each child must be a diagnose-unit) the property :sort-behavior of
;;    this slot is to specify that behavior which should be used
;;    in :activate-child-with-max-focus
;; d) FOCUS
;;    the numeric weight of this diagnose-unit. this slot should be changed
;;    during the self-test-rules. Default init value is 0
;; e) SELF-TEST-RULES
;;    rules whose actions can change the focus of this diagnose-unit
;; f) REFINEMENT-RULES
;;    rules whose actions can change the focus of children
;;
;; The syntax for the value of slot SELF-TEST-RULES and REFINEMENT-RULES is:
;; 
;; <rules-specification> ::= <single-rules-specification>
;;                           | "List of <single-rules-specification>"
;;                           
;; <single-rules-specification> ::= <rule-set-name> | (<do-control-structure> <rule-set-name>)
;;                                  | (<while-control-structure> <rule-set-name> <condition>)
;;                                  
;; <do-control-structure> ::= :DO-ONE | :DO-ALL
;; 
;; <while-control-structure> ::= :WHILE-ONE | :WHILE-ALL
;; 
;; As usual if for a <while-control-structure> no condition is specified,
;; then the condition is by default T (true) 
;; In case that only a <rule-set-name> is specified the interpretation
;; is: (:DO-ALL <rule-set-name>)
;;
;; the op-stack contains the actions to perform on a diagnose-unit this slot shouldn't be
;; accessed by the user.


(DEFFRAME diagnose-unit
  (SUPERS task) 
  (SLOTS (diagnose-name nil :trace-on t)
         (father nil)
         (children nil :sort-behavior nil)
         (focus 0 :possible-values :number)
         (self-test-rules nil)
         (refinement-rules nil)
         (op-stack (:eval-self-test-rules :give-up-control
                                          :eval-refinement-rules 
                                          :activate-child-with-max-focus) 
                   :doc "operations to be carried out")))

;;________________________________________________________________

(DEFBEHAVIOR (diagnose-unit :after :initialize) (&rest ignore)
  
  "saves focus and op-stack for resetting after abort."
  
  (declare (ignore ignore))
  (setf ($value 'focus :default-value) ($value 'focus))
  (setf ($value 'op-stack :default-value) ($value 'op-stack)))

;;________________________________________________________________


(DEFBEHAVIOR (diagnose-unit :after :reset) ()
  
  "restores the initial focus and op-stack."
  
  (setf ($value 'focus) ($value 'focus :default-value))
  (setf ($value 'op-stack) ($value 'op-stack :default-value))
  (send-kb :babylon-format "~%~s" ($send self :get 'children))
  (dolist (a-child ($send self :get 'children))
    (<- a-child :reset)))

;;________________________________________________________________


(DEFBEHAVIOR (diagnose-unit :change-focus) (behavior-name val &optional (mode :REMEMBER))
  
  " USER  this is the interface for changing the focus of a diagnose-unit.
user should only supply the arguments behavior-name and val. 
this behavior sets the focus of self to the result of
funcalling behavior-name to self with val as argument."
  
  (if (eq mode :REMEMBER)
    (setf ($value 'focus)
          ($send self behavior-name val) ;; using $send to compute behavior-name
          )))

;;________________________________________________________________

;;  EXAMPLE 

(DEFBEHAVIOR (diagnose-unit :added-focus) (val)
  (+ ($value 'focus) val))

;; A frame reference: (diagnose1 :add-to-focus 40)
;; A frame reference: (diagnose1 :add-to-focus -40)

(DEFBEHAVIOR (diagnose-unit :add-to-focus) (val &optional (mode :REMEMBER))
  
  "this can be used as an example for using :change-focus."
  
  (<- self :change-focus :added-focus val mode))

;;________________________________________________________________


(DEFBEHAVIOR (diagnose-unit :set-diagnose-trace) (t-or-nil)
  
  " USER   switching trace on or off."
  
  (setf ($value 'name :trace-on) t-or-nil))


;;________________________________________________________________


(DEFBEHAVIOR (diagnose-unit :is-traced-diagnose) ()
  
  "predicate for testing if diagnose-trace is set."
  
  ($value 'diagnose-name :trace-on))

;;________________________________________________________________


(DEFBEHAVIOR (diagnose-unit :empty-stack?) ()
  (null ($value 'op-stack)))


;;________________________________________________________________

(DEFBEHAVIOR (diagnose-unit :get-root) ()
  
  " USER   yields the root of the tree this diagnose-unit is contained in."
  
  (let ((father-diagnose ($value 'father)))
    (if father-diagnose
      (<- father-diagnose :get-root)
      ($send self :object-name))))

;;________________________________________________________________


(DEFBEHAVIOR (diagnose-unit :stop-search) (diagnose)
  
  " USER   stops the search at diagnose."
  
  (<- (<- self :get-root) :stop diagnose))


;;________________________________________________________________


(DEFBEHAVIOR (diagnose-unit :before :activate) ()
  
  "tracing is done before a diagnose-unit is activated."
  
  (if (<- self :is-traced-diagnose)
    (<- self :trace-diagnose)))

;;________________________________________________________________


(DEFBEHAVIOR (diagnose-unit :trace-diagnose) ()
  
  "default tracing-behavior."
  
  (let* ((prefix (format nil "  DIAGNOSE")) 
         (object-line (format nil " ~S " ($send self :object-name)))
         (focus-line (format nil " [FOCUS = ~S] " ($value 'focus)))
         (constant-line (format nil "ACTIVATED."))) 
    (send-kb :send-system-trace-window :format
             `(:string ,prefix)       ;; LIST  entfernt 21.7.88 Ca
             `(:mouse
               (:mouse-click :eval
                             (,self :get-diagnose-trace-information)
                             :documentation "Click here to show some information")
               :string ,object-line)
             `(:string ,focus-line)
             `(:string ,constant-line))))


;;________________________________________________________________


(DEFBEHAVIOR (diagnose-unit :get-diagnose-trace-information) ()
  
  "pops up a menu for selecting sibling an father of that diagnose-unit."
  
  (send-kb :choose-from-menu 
           `(("Get siblings" :eval
              (,self :show-list-as-menu :get-siblings-items
                     ,(format nil "Siblings of diagnose ~S" ($send self :object-name))))
             ("Get children" :eval
              (,self :show-list-as-menu :get-children-items
                     ,(format nil "Children of diagnose ~S" ($send self :object-name)))))
           " Select Diagnose Operation: "))

;;________________________________________________________________


(DEFBEHAVIOR (diagnose-unit :get-siblings) ()
  
  " USER   gets the siblings of this diagnose-unit."
  
  (remove ($send self :object-name) (<- ($value 'father) :get 'children) :test 'eql))


;;________________________________________________________________


(DEFBEHAVIOR (diagnose-unit :get-siblings-items) ()
  
  "produces items of the form "diagnose focus" for explanation menu."
  
  (mapcar #'(lambda (a-diagnose)
              (format nil "~S  FOCUS = ~S" a-diagnose (<- a-diagnose :get 'focus)))
          (<- self :get-siblings)))
              
  ;;________________________________________________________________


(DEFBEHAVIOR (diagnose-unit :get-children) ()
  ($value 'children))


;;________________________________________________________________


(DEFBEHAVIOR (diagnose-unit :get-children-items) ()
  "produces items of the form "diagnose focus" for explanation menu."
  (mapcar #'(lambda (a-diagnose)
              (format nil "~S  FOCUS = ~S" a-diagnose (<- a-diagnose :get 'focus)))
          (<- self :get-children)))

;;________________________________________________________________


(DEFBEHAVIOR (diagnose-unit :start) (&optional arg (mode :REMEMBER))
  
  " USER   start behavior for diagnose-unit."
  
  (declare (ignore arg))
  (when (eq mode :REMEMBER)
    (<- self :activate)))


;;________________________________________________________________


(DEFBEHAVIOR (diagnose-unit :activate) ()
  
  "realizes the sequential processing of the op-stack."
  
  (prog (op stack)
    loop
    (setq stack ($value 'op-stack))
    (if (null stack)
      (return `(end at ,($value 'diagnose-name))))
    (setq op (pop stack))
    (setf ($value 'op-stack) stack)
    ($send self op)
    (go loop)))


;;________________________________________________________________

;; ***************** THE RULE EVALUATOR IN DIAGNOSE-UNITS *****************

;; The syntax for the value of slot SELF-TEST-RULES and REFINEMENT-RULES is:
;; 
;; <rules-specification> ::= <single-rules-specification>
;;                           | "List of <single-rules-specification>"
;;                           
;; <single-rules-specification> ::= <rule-set-name> | (<do-control-structure> <rule-set-name>)
;;                                  | (<while-control-structure> <rule-set-name> <condition>)
;;                                  
;; <do-control-structure> ::= :DO-ONE | :DO-ALL
;; 
;; <while-control-structure> ::= :WHILE-ONE | :WHILE-ALL
;; 
;; As usual if for a <while-control-structure> no condition is specified,
;; then the condition is by default T (true) 
;; In case that only a <rule-set-name> is specified the interpretation
;; is: (:DO-ALL <rule-set-name>)


(DEFBEHAVIOR (diagnose-unit :normalize-rules-specification) (rules-specification)
  
  "here the syntax of the rules-specification is normalized.
no complete check is made!"
  
  (cond ((null rules-specification) nil)
        ((atom rules-specification)
         `((,(default-rule-set-forward-control-structure)
            ,rules-specification)))
        ((is-rule-set-forward-control-structure (first rules-specification))
         rules-specification)
        (t (mapcar #'(lambda (a-rule-spec)
                       (cond ((atom a-rule-spec)
                              `(,(default-rule-set-forward-control-structure)
                                ,rules-specification))
                             ((is-rule-set-forward-control-structure (first a-rule-spec))
                              a-rule-spec)
                             (t (error "~S: WRONG RULE SPECIFICATION IN ~S"
                                       a-rule-spec ($send self :object-name)))))
                   rules-specification))))

(DEFBEHAVIOR (diagnose-unit :eval-rules) (rules-specification)
  
  "This is the RULE-Interpreter."
  
  (dolist (a-rule-spec (<- self :normalize-rules-specification rules-specification))
    (let ((rule-set-name (second a-rule-spec))
          (control-structure (first a-rule-spec)))
      (if (is-known-rule-set-name rule-set-name)
        (if (rest (rest a-rule-spec))                   ;; a condition has been specified
          (send-current-knowledge-base
           :find-implications rule-set-name control-structure (third a-rule-spec))
          (send-current-knowledge-base
           :find-implications rule-set-name control-structure))
        (error "~S: WRONG RULE-SET-NAME IN ~S" rule-set-name ($send self :object-name))))))

(DEFBEHAVIOR (diagnose-unit :eval-self-test-rules) ()
  
  "evaluation of the self-test-rules."
  
  (<- self :eval-rules ($value 'self-test-rules)))


(DEFBEHAVIOR (diagnose-unit :eval-refinement-rules) ()
  
  "evaluation of the refinement-rules."
  
  (<- self :eval-rules ($value 'refinement-rules)))


;;________________________________________________________________


(DEFBEHAVIOR (diagnose-unit :give-up-control) ()
  
  "gives up control to the father of this diagnose-unit."
  
  (if ($value 'father)
    (<- ($value 'father) :check-focus ($send self :object-name))))


;;________________________________________________________________


(DEFBEHAVIOR (diagnose-unit :sort-children) (&optional (children-list nil))
  
  " USER   Sorting the children-list using the specified behavior 
of (children :sort-behavior) or :sort-children-greaterp if not specified. 
if a sort-behavior is specified, it must have an optional argument. 
the behavior should return the sorted list of the args if supplied or of the children."
  
  (let ((children-sorting-method ($value 'children :sort-behavior)))
    (if children-sorting-method
      ;; here $send must be used because the selector has to be computed (bad style)
      ($send self children-sorting-method (or children-list ($value 'children)))
      (<- self :sort-children-greaterp children-list))))

;;________________________________________________________________

(DEFBEHAVIOR (diagnose-unit :sort-children-greaterp) (&optional (children-list nil))
  
  "default sorting behavior."
  
  (sort (remove nil (mapcar #'(lambda (a-child)
                                (unless (<- a-child :empty-stack?) a-child))
                            (or children-list ($value 'children))))
        #'(lambda (c1 c2)
            (> (<- c1 :get 'focus)
               (<- c2 :get 'focus)))))


;;________________________________________________________________


(DEFBEHAVIOR (diagnose-unit :check-focus) (sender-diagnose)
  
  "Ueberpruefung des focus der children.
falls sender-diagnose den hoechsten focus hat -> t (Bearbeitung fortsetzen)
sonst child mit hoechstem focus aktivieren
nach Abarbeitung -> Frage nach vollstaendiger Suche.
Ja -> children-list neu berechnen und nach selbigem Schema abarbeiten."
  
  (prog ((children-list ($value 'children)))
    A (setq children-list (<- self :sort-children children-list))
    (cond ((eq sender-diagnose (first children-list)) ; sender-diagnose already active
           (return t))
          (t                                          ; activate most interesting child
           (<- (pop children-list) :activate)
           (if children-list	                      ; do exaustive search
             (go A))))))

;;________________________________________________________________


(DEFBEHAVIOR (diagnose-unit :continue-from-terminal-diagnose?) (diagnose)
  
  " USER   default behavior for deciding what to do when a terminal diagnose is reached.
The user is asked for continuing the search.
if no search will be stopped, if yes the search will be continued.
 The user must override this to specify a different kind of behavior. "
  
  (send-kb :babylon-format
           "~%You are now at TERMINAL DIAGNOSE ~S [FOCUS = ~S]." diagnose ($value 'focus))
  (send-kb :babylon-format "~%Shall I stop the search ? ")
  (cond ((member (send-kb :babylon-read '(#\Y #\y))  '(#\Y #\y))
         (send-kb :babylon-format " Yes")
         (<- self :stop-search diagnose))
        (t (send-kb :babylon-format " No"))))



;;________________________________________________________________


(DEFBEHAVIOR (diagnose-unit :activate-child-with-max-focus) ()
  
  "behavior which decides which child is to activate next.
here the results of :sort-children (which the user can manipulate) is used."
  
  (let ((children-list ($value 'children)))
    (cond ((null children-list)	       ; terminal diagnose reached. Ask user to continue.
           (<- self :continue-from-terminal-diagnose? ($send self :object-name)))
          (t                           ; activate most interesting child
           (prog ()
             A  (setq children-list ($send self :sort-children children-list)) 
             (<- (pop children-list) :activate)
             (if children-list
               (go A)                 ;  do exaustive search
               ))))))


;;________________________________________________________________


(DEFBEHAVIOR (diagnose-unit :next-operation) ()
  
  "selecting the next operation of op-stack."
  
  (if ($value 'op-stack) (first ($value 'op-stack))))


;;________________________________________________________________


(DEFBEHAVIOR (diagnose-unit :tree) (&optional slot-names)
  
  "produces the data-structure for displaying the tree.
starting at diagnose-unit with the arc children
slot-names optionally can be given and must be specified
as slot-names in all the diagnose-units of the tree."
  
  (let ((the-name ($send self :object-name))
        (the-children ($value 'children)))
    (if (null the-children)
      (if slot-names
        `(,(format nil "~S" the-name)
          . ,(mapcar #'(lambda (a-slot-name)
                         `(,(format nil "==> ~S ~S"
                                    a-slot-name (<- self :get a-slot-name))))
                     slot-names))
        `(,(format nil "~S" the-name)))
      (let ((children-tree (mapcar #'(lambda (child)
                                       (<- child :tree slot-names))
                                   the-children)))
        (if slot-names
          `(,(format nil "~S" the-name)
            ,@(mapcar #'(lambda (a-slot-name)
                          `(,(format nil "==> ~S ~S"
                                     a-slot-name (<- self :get a-slot-name))))
                      slot-names)
            . ,children-tree)
          `(,(format nil "~S" the-name) . ,children-tree))))))


;;________________________________________________________________


;(DEFBEHAVIOR (diagnose-unit :describe-tree)
;	     (&rest slot-names)
;
;  ;;  USER  
;  ;; behavior for displaying the tree from self
;  
;  (TREE-LIST-BROWSER (<- self :tree slot-names)))


;;________________________________________________________________


(DEFBEHAVIOR (diagnose-unit :link-down) (descendant &optional slot-names)
  (prog (the-father end result)
    loop (push (if slot-names
                 `(,descendant . ,(mapcar #'(lambda (a-slot-name)
                                              `(==> ,a-slot-name 
                                                    ,(<- descendant
                                                         :get a-slot-name)))
                                          slot-names))
                 descendant)
               result)
    (if end (return result))
    (setq the-father (<- descendant :get 'father))
    (if (null the-father)
      (return nil)
      (progn
        (if (eq the-father ($send self :object-name))
          (setq end t))
        (setq descendant the-father)
        (go loop)))))


;;________________________________________________________________


(DEFBEHAVIOR (diagnose-unit :link-up) (ancestor &optional slot-names)
  (reverse (<- ancestor :link-down ($send self :object-name) slot-names)))


;;________________________________________________________________


(DEFBEHAVIOR (diagnose-unit :describe-link-to) (diagnose &rest slot-names)
  (let ((result-down (<- self :link-down diagnose slot-names)))
    (if result-down
      (pprint `(link-down . ,result-down))
      (let ((result-up (<- self :link-up diagnose slot-names)))
        (if result-up
          (pprint `(link-up . ,result-up)))))))



(if (member 'top-down-refine *known-knowledge-bases*)
  ($send top-down-refine :send-if-handles :export-kb))


;;; eof

