
; Treating multiple failed-goals required a cross product of the
; negated leaf causes with which I am not prepared to deal.

(defun reverse-undo-causes (failed-goals context top-level-goal rule bd goal)
 (let ((sprouted-contexts nil) (sprouted-context nil)
        (intends nil)
       (leaf-causes (get-leaf-causes (car failed-goals)
                                     (ob$get (car failed-goals)
                                             'termination-context)))
       (failed-goal-obj (ob$get (car failed-goals) 'obj))
       (old-top-level-goal (ob$get (car failed-goals) 'top-level-goal))
       (backwards-planning-path nil) (old-context nil) (rand nil)
       (new-rule nil)
       (uor-obj nil) (predictor nil) (p-goal-uid nil) (input-states nil)
       (cfg-term-ctxt (ob$get (car failed-goals) 'termination-context))
       (path nil) (prev-context nil))
  (ndbg-roman-nl *gate-dbg* rule "Reverse undo causes for ~A in ~A top = ~A"
                 (car failed-goals) context top-level-goal)
  (setq old-context (ob$get old-top-level-goal 'activation-context))
  (setq backwards-planning-path
       (reverse (memq old-context
                      (reverse (cons cfg-term-ctxt
                                     (cx$ancestors cfg-term-ctxt))))))
  (if (null? backwards-planning-path)
      (error "Null backwards planning path."))
  (ndbg-roman-nl *gate-dbg* rule "Bckwds plng path = ~A"
                 backwards-planning-path)
  (yloop
   (yfor leaf-cause in leaf-causes)
   (ydo
    (ndbg-roman-nl *gate-dbg* rule "Considering leaf cause ~A" leaf-cause)
    (if (ty$instance? leaf-cause 'not)
     (progn
      (if (ty$instance? (ob$get leaf-cause 'obj) 'long-term-state)
          (progn
           (setq *new-personal-goals*
                (cons (ob$get leaf-cause 'obj) *new-personal-goals*))
       (activate-top-level-goal
        (ob$fcreate `(ACTIVE-GOAL obj ,(ob$get leaf-cause 'obj)))
        *reality-lookahead* *empty-bd*
        (ob$fcreate `(RULE emotion (POS-EMOTION strength ,(strength goal) )))))
       (progn
        ; set up rand object
        (setq rand (ob$fcreate '(RAND)))
        (yloop
         (yfor leaf-cause1 in leaf-causes)
         (ydo
          (if (neq? leaf-cause1 leaf-cause)
           (progn
            (setq uor-obj (ob$fcreate '(ROR))) ; was UOR, but gets killed by vblz
            (ob$add uor-obj 'obj leaf-cause1)
            (ob$add uor-obj 'obj (ob$fcreate `(ACTIVE-GOAL obj ,leaf-cause1)))
            (setq predictor (predicting-state leaf-cause1))
            (if predictor
             (progn
              (ob$add uor-obj 'obj predictor)
              (ob$add uor-obj 'obj (ob$fcreate `(ACTIVE-GOAL obj ,predictor)))))
            (ob$add rand 'obj uor-obj)))))
        ; set up rules
        (setq p-goal-uid
             (string->symbol (string-append "PRESERVATION"
                                            (fixnum->string
                                             *next-prule-number*))))
        (setq new-rule
         (ob$fcreate `(RULE subgoal ,rand
                            goal (ACTIVE-GOAL
                                  obj (PRESERVATION obj ,failed-goal-obj
                                                    uid ',p-goal-uid))
                            is 'inference-only
                            plausibility 0.9)))
        (setq new-rule
             (ob$variabilize new-rule #'varize-object? nil *link-slots* nil))
        (ob$add-unique-name new-rule
                            (string->symbol
                             (string-append "PRESERVATION-INF."
                                            (fixnum->string
                                             *next-prule-number*))))
        (add-rule-print new-rule)
        (setq new-rule
         (ob$fcreate `(RULE subgoal ,(ob$get leaf-cause 'obj)
                            goal (PRESERVATION obj ,failed-goal-obj
                                               uid ',p-goal-uid)
                            is 'plan-only
                            plausibility 0.9)))
        (setq new-rule
             (ob$variabilize new-rule #'varize-object? nil *link-slots* nil))
        (ob$add-unique-name new-rule
                            (string->symbol
                             (string-append "PRESERVATION-PLAN."
                                            (fixnum->string
                                             *next-prule-number*))))
        (add-rule-print new-rule)
        (increment-me *next-prule-number*)
        ; replan
        (setq input-states nil)
        (setq path backwards-planning-path)
        (setq old-context nil)
        (yloop (ydo (setq prev-context old-context)
                  (setq old-context (car path))
                  (setq input-states (union input-states
                                           (cx$input-states old-context)))
                  (setq path (cdr path)))
              (ywhile path))
;              (yuntil 
;               (prog1
;                (progn
;                 (cx$assert-many old-context input-states)
;                 (not (show rand old-context *empty-bd* *me-belief-path*)))
;                (cx$retract-many old-context input-states)))
;        (if (null? prev-context)
;            (progn
;             (error "null prev context")
;             (setq prev-context (ob$get (car failed-goals)
;                                       'termination-context))))
        ; This line added for new alg.
        (setq old-context (ob$get old-top-level-goal 'activation-context))
        ; Sprout an alternative past context
        (delay-dbgs 'to-be-set
         (rule-fire-msg rule "coded plan" context bd nil goal)
         (ndbg-roman-nl *gate-dbg* rule "Reverse undo cause")
         (setq sprouted-context
              (reversal-sprout-alternative old-context old-top-level-goal
                                           context top-level-goal
                                           1.0 t))
         (setq xxcontext sprouted-context)
         (no-gen (cx$assert-many sprouted-context input-states))
         (setq sprouted-contexts (cons sprouted-context
                                      sprouted-contexts)))))))))
      sprouted-contexts))

