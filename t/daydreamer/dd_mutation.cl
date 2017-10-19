;*******************************************************************************
;
; Daydreamer
; Version 3.5
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
; 1/12/86: Began adding code for action mutation
; 7/22/86: Redid mutations to use serendipity mechanism
; 9/25/86: Got rid of flavors
;
;*******************************************************************************

(defun mutations (action context)
 (yloop (initial (result nil)
                 (bd nil))
        (yfor mut in *mutations*)
        (ydo (if (setq bd (ob$unify (car mut) action *empty-bd*))
                 (setq result (cons (ob$instantiate (cadr mut) bd)
                                    result))))
        (yresult result)))

;
; Returns NIL or T.
;
(defun action-mutations (top-level-goal backtrack-wall)
  ; For now, only attempt mutations once for a given top-level goal.
  (if (null? (ob$get top-level-goal 'run-mutations?))
      (progn
       (ob$add top-level-goal 'run-mutations? t)
  (ndbg-roman-nl *gate-dbg* rule "Action mutations for ~A" top-level-goal)
  (yloop
   (initial (mutated-actions nil) (result nil))
   (yfor leaf in (cx$leaf-descendants backtrack-wall))
   (yuntil result)
   (ydo
    (ndbg-roman-nl *gate-dbg* rule "Trying leaf context ~A" leaf)
    (if (null? (ob$get leaf 'mutations-tried?))
        (progn
         (ob$set leaf 'mutations-tried? t)
         (yloop
          (yfor ob in (cx$get-all-ty leaf *active-goal-ob*))
          ; Note that planning loops do not result in failed goals.
          ; They just leave unplanned active goals.
          (ydo
           (if (ty$instance? (ob$get ob 'obj) 'action)
               (progn
                (ndbg-roman-nl *gate-dbg* rule "Mutating action goal ~A" ob)
                (setq mutated-actions (mutations (ob$get ob 'obj)
                                                leaf))
                (yloop
                 (yfor mutated-action in mutated-actions)
                 (ydo (if (action-mutation top-level-goal leaf mutated-action
                                           ob)
                          (setq result t)))))))))))
   (yresult result)))
      nil))

;
; See if a given action mutation pans out (via serendipity mechanism).
; First try serendipity from the supergoal of the action goal.
; If that doesn't work, try serendipity from the top (well, actually
; the subgoal of the daydreaming goal).
; Todo: we might also want to invoke serendipity for other tasks as well!
;
; Returns T or NIL.
;
(setq *action-mutations?* nil)

(defun action-mutation (daydreaming-goal leaf mutated-action
                         mutated-action-goal)
 (ndbg-roman-nl *gate-dbg* rule "Trying mutated action ~A" mutated-action)
(unwind-protect
 (let ((bottom-goal (ob$fcreate `(SUCCEEDED-GOAL obj ,mutated-action))))
  (setq  *action-mutations?* t)
  (if (serendipity-recognize-apply daydreaming-goal
                                   (goal-supergoal mutated-action-goal leaf)
                                   (bottom-rules mutated-action) bottom-goal)
      t
      (let ((subgoal (dd-goal-subgoal daydreaming-goal)))
           (if nil ; was subgoal
               (if (serendipity-recognize-apply daydreaming-goal subgoal
                                                (bottom-rules mutated-action)
                                                bottom-goal)
                   t
                   nil)
               nil))))
  (setq *action-mutations?* nil)))

;
; What follows is code that is not currently being used.
;

;
; Action mutation generation
;

(defun type-mutations (action)
 (ndbg-roman-nl *gate-dbg* rule "Find type mutations for ~A" action)
 (let ((mutation1 (ob$copy action))
       (mutation2 (ob$copy action))) ; need to copy w/o links
  (cond
   ((ty$instance? action 'ptrans)
    (ob$set mutation1 'type *mtrans-ob*)
    (ob$set mutation2 'type *atrans-ob*)
    (list action mutation1 mutation2))
   ((ty$instance? action 'mtrans)
    (ob$set mutation1 'type *ptrans-ob*)
    (ob$set mutation2 'type *atrans-ob*)
    (list action mutation1 mutation2))
   ((ty$instance? action 'atrans)
    (ob$set mutation1 'type *mtrans-ob*)
    (ob$set mutation2 'type *ptrans-ob*)
    (list action mutation1 mutation2))
   (else nil))))

(defun normalize-action! (action context)
  (cond
   ((ty$instance? action 'ptrans)
    (normalize-ptrans! action context))
   ((ty$instance? action 'mtrans)
    (normalize-mtrans! action context))
   ((ty$instance? action 'atrans)
    (normalize-atrans! action context))
   (else action)))

(defun normalize-ptrans! (action context)
  (let ((from (ob$get action 'from))
        (to (ob$get action 'to))
        (obj (ob$get action 'obj)))
       (cond
        ((eq? from 'some-object)
         (ob$set action from *location-var*))
        ((and from (not (ty$instance? from 'location))
                   (ty$instance? from 'person))
         (ob$set action 'from (object->location from context))))
       (cond
        ((eq? to 'some-object)
         (ob$set action to *location-var*))
        ((and to (not (ty$instance? to 'location))
              (ty$instance? to 'person))
         (ob$set action 'to (object->location to context))))
       (cond
        ((eq? obj 'some-object)
         (ob$set action to *phys-obj-var*))
        ((and obj (not (ty$instance? obj 'phys-obj))
              (ty$instance? obj 'mental-obj))
         (ob$set action 'obj
               (ob$fcreate `(PHYS-OBJ obj ,obj)))))
       action))

(defun normalize-mtrans! (action context)
  (let ((from (ob$get action 'from))
        (to (ob$get action 'to))
        (obj (ob$get action 'obj)))
       (cond
        ((eq? from 'some-object)
         (ob$set action from *person-var*))
        ((and from (not (ty$instance? from 'person))
              (ty$instance? from 'location))
         (ob$set action 'from (location->object from
               context))))
       (cond
        ((eq? to 'some-object)
         (ob$set action to *person-var*))
        ((and to (not (ty$instance? to 'person))
              (ty$instance? to 'location))
         (ob$set action 'to (location->object to context))))
       (cond
        ((eq? obj 'some-object)
         (ob$set action to *mental-obj-var*))
        ((and obj (not (ty$instance? obj 'mental-obj))
              (ty$instance? obj 'phys-obj))
         (ob$set action 'obj
               (ob$fcreate `(MENTAL-OBJ obj ,obj)))))
       action))

(defun normalize-atrans! (action context)
  (let ((from (ob$get action 'from))
        (to (ob$get action 'to))
        (obj (ob$get action 'obj)))
       (cond
        ((eq? from 'some-object)
         (ob$set action from *person-var*))
        ((and from (not (ty$instance? from 'person))
              (ty$instance? from 'location))
         (ob$set action 'from (location->object from context))))
       (cond
        ((eq? to 'some-object)
         (ob$set action to *person-var*))
        ((and to (not (ty$instance? to 'person))
              (ty$instance? to 'location))
         (ob$set action 'to (location->object to context))))
       (cond
        ((eq? obj 'some-object)
         (ob$set action to *phys-obj-var*))
        ((and obj (not (ty$instance? obj 'phys-obj))
              (ty$instance? obj 'mental-obj))
         (ob$set action 'obj
               (ob$fcreate `(PHYS-OBJ obj ,obj)))))
       action))

; generates list of substitution binding lists for each possible permutation
; of a list of objects
(defun permutation-substs (objs)
  (yloop (initial (result nil)
                  (perms (permute-list objs)))
         (yfor perm in perms)
         (ydo (yloop (initial (bd nil))
                     (yfor elem1 in objs)
                     (yfor elem2 in perm)
                     (ydo (setq bd (cons (list elem1 elem2) bd)))
                     (yresult (setq result (cons (cons 't bd) result)))))
         (yresult result)))

(defun permute-list (lst)
  (cond
   ((null? (cdr lst)) (list lst))
   (else (yloop (initial (result1 nil)
                         (result2 nil))
                (yfor elem1 in lst)
                (ydo (setq result2 (permute-list (delq elem1 lst)))
                     (yloop (yfor elem2 in result2)
                            (ydo (setq result1 (cons (cons elem1 elem2)
                                               result1)))))
                (yresult result1)))))

(defun permutation-mutations (action)
 (ndbg-roman-nl *gate-dbg* rule "Find permutation mutations for ~A" action)
  (yloop (initial (result nil))
        (yfor subst in (cdr (permutation-substs (objects-in action))))
        ; cdr is intended to remove the identity substitution--it
        ; may end up as last, though.
        (ydo (setq result (cons (ob$subst action subst nil nil nil) result)))
        (yresult result)))

(defun substitution-mutations (action)
 (ndbg-roman-nl *gate-dbg* rule "Find substitution mutations for ~A" action)
  (if (ob$literal? action)
      (list action)
  (yloop (initial (result (list (ob$create-empty)))
                  (ob1 nil)
                  (ob2 nil)
                  (temp nil))
         (yfor sv in (ob$pairs action))
         ; have to add other instan code to handle literal and type obs right?
         (ydo (if (ty$instance? (slots-value sv) 'object)
                  (progn
;                  (setq ob1 (ob$copy (tlast result)))
                   (setq ob2 (ob$copy (tlast result)))
                   (yloop (yfor ob in result)
                          (ydo (ob$add ob (slots-name sv) (slots-value sv))))
;                  (ob$add ob1 (slots-name sv) *me*)
                   (ob$add ob2 (slots-name sv) 'some-object)
                   (setq result (cons ob2 result)))
                  (progn
                   (setq temp (substitution-mutations (slots-value sv)))
                   (yloop (initial (new-result nil))
                          (yfor ob1 in temp)
                          (ydo (yloop (yfor ob2 in result)
                                      (ydo (setq ob2 (ob$copy ob2))
                                           (ob$add ob2 (slots-name sv) ob1)
                                           (setq new-result (append! new-result
                                                             (list ob2))))))
                          (yresult (setq result new-result))))))
         (yresult result))))

(setq *mutation-timeout* 4)

(defun replan-mut (goal)
  (let ((sprout (cx$sprout (ob$get goal 'top-context))))
       (ob$set sprout 'mutations-tried? t)
       (list sprout)))

(defun redo-plans-with-mutations? (top-level-goal leaf)
  (ndbg-roman-nl *gate-dbg* rule "Redo plans with mutations")
  (yloop (initial (sprouted-contexts nil))
         (yfor ob in (cx$get-all-ty leaf *active-goal-ob*))
         (ydo (if (eq? (ob$get ob 'top-level-goal) top-level-goal)
                  (setq sprouted-contexts (append
                    (run-mutation-plans ob top-level-goal
                     (ob$get ob 'top-context)) sprouted-contexts))))
         (yresult sprouted-contexts)))

(defun mutation-result? (fact context)
  (ol-path fact nil *dependency-ob* 'backward
           context (lambda (dummy ob) (mutation-action? ob context)) nil))

(defun mutation-action? (ob context)
  (ob$get ob 'mutant))

(defun run-mutation-plans (goal top-level-goal context)
  (ndbg-roman-nl *gate-dbg* rule "Trying mutation plans for ~A in ~A"
                 goal context)
  (let ((goal-obj (ob$get goal 'obj)) (bds nil)
        (sprouted-context nil) (sprouted-contexts nil))
       (yloop
        (initial (bds nil))
        (yfor mutated-plan-context in (ob$get top-level-goal
                                       'mutation-plan-contexts))
        (ydo (setq bds (cx$retrieve mutated-plan-context goal-obj))
;  was retrieve-all; why, I don't know--retrieve always retrieves all
             (yloop
              (yfor bd in bds)
              (ydo (if (mutation-result? (car bd) mutated-plan-context)
                       (progn
                        (ndbg-roman *gate-dbg* rule "Mutation plan")
                        (ndbg-roman *gate-dbg* rule " for ~A in ~A"
                                    goal context)
                        (ndbg-newline *gate-dbg* rule)
                        (setq sprouted-context (cx$sprout context))
                        (delay-dbgs sprouted-context
                         (ob$set sprouted-context 'mutations-tried? t)
                         ; the above is to prevent mutations being tried on
                         ; any leaves which already involve mutations.
                         (ob$removes sprouted-context 'timeout)
                         (setq sprouted-contexts (cons sprouted-context
                                                      sprouted-contexts))
                         ; The below splices in a plan resulting from an
                         ; inference chain from another context!
                         (inference-chain->plan-trc mutated-plan-context
                                                    sprouted-context (car bd)
                                                    goal bd top-level-goal
                                                    *active-goal-ob*)))))))
        (yresult sprouted-contexts))))

(defun inference-chain->plan-trc (inf-context plan-context fact goal
                                     bd top-level-goal goal-type)
  ; Plan instantiate now returns nil if goal equals top-level-goal
  ; so this will have to be rewritten.
  (let ((root-goal (plan-instantiate goal bd plan-context top-level-goal
                                     *me-belief-path* nil))
        (dependencies (ol-get fact *dependency-ob* 'backward inf-context))
        (intends nil))
       (yloop (yfor dependency in dependencies)
              (ydo (setq intends
                         (ob$fcreate `(INTENDS linked-from ,root-goal
                                               linked-to
                                                 ,(inference-chain->plan-trc1
                                                 inf-context plan-context
                                                 (ob$get dependency 'linked-to)
                                                 top-level-goal goal-type)
                                               rule ,(ob$get dependency 'rule)
                                               seq? t)))
                   (cx$assert plan-context intends)))
       root-goal))

(defun inference-chain->plan-trc1 (inf-context plan-context fact
                                      top-level-goal goal-type)
  (let ((goal (ob$fcreate `(NOTYPE obj ,fact)))
        (dependencies (ol-get fact *dependency-ob* 'backward inf-context))
        (intends nil))
       (ob$add goal 'type goal-type)
       (yloop (yfor dependency in dependencies)
              (ydo (setq intends
                         (ob$fcreate `(INTENDS linked-from ,goal
                                               linked-to
                                                 ,(inference-chain->plan-trc1
                                                 inf-context plan-context
                                                 (ob$get dependency 'linked-to)
                                                 top-level-goal goal-type)
                                               rule ,(ob$get dependency 'rule)
                                               seq? t)))
                   (cx$assert plan-context intends)))
       (cx$assert plan-context goal)
       goal))

; End of file.
