;*******************************************************************************
;
; Daydreamer
; Version 3.5
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
; 1/26/86: Reversal algorithm
; 9/24/86: Removed sends
; 9/28/86: Rewrote for new undo-causes
;
;*******************************************************************************

;
; Failure Reversal
;

(defun reversal (goal rev context top-level-goal rule bd)
 (let ((failed-goals (ob$gets rev 'obj)))
  (ndbg-roman-nl *gate-dbg* rule
                 "Reversal for ~A in ~A, top-level-goal = ~A"
                 failed-goals context top-level-goal)
  (if (inferred-top-level-goal? (car failed-goals))
      (reverse-undo-causes failed-goals context top-level-goal rule bd goal)
      nil)))

(defun reverse-alterns (failed-goal context top-level-goal rule bd goal)
  (let ((sprouted-contexts nil)
        (intends nil)
        (old-top-level-goal (ob$get failed-goal 'top-level-goal))
        (planning-path nil)
        (activation-context nil)
        (termination-context nil))
       (ndbg-roman-nl *gate-dbg* rule "Reverse alterns for ~A in ~A top = ~A"
             failed-goal context top-level-goal)
       (setq activation-context (ob$get old-top-level-goal 'activation-context))
       (setq termination-context (ob$get old-top-level-goal
                                         'termination-context))
       (setq planning-path
            (memq activation-context (reverse (cons termination-context
                                                    (cx$ancestors
                                                     termination-context)))))
       (yloop
        (initial (rest planning-path)
                 (sprouted-context nil))
        (yuntil (null? (cdr rest)))
        (ydo
         (yloop
          (yfor child in (prune-possibilities (cx$children (car rest))))
          (ydo (if (neq? child (cadr rest))
                  (progn
                   (delay-dbgs 'to-be-set
                    (rule-fire-msg rule "coded plan" context bd nil goal)
                    (ndbg-roman-nl *gate-dbg* rule "Reverse alterns")
                    (setq sprouted-context
                         (reversal-sprout-alternative child old-top-level-goal
                          context top-level-goal 1.0 t))
                    (setq xxcontext sprouted-context)
                    (setq sprouted-contexts
                         (cons sprouted-context sprouted-contexts)))))))
         (setq rest (cdr rest))))
       sprouted-contexts))

(defun reversal-sprout-alternative (old-context old-top-level-goal
                                     new-context new-top-level-goal
                                     ordering do-intends?)
  ; Sprout an alternative past context
  (ndbg-roman-nl *gate-dbg* rule "Reversal sprout alternative")
  (let ((result (sprout-alternative-past old-context old-top-level-goal
                                         new-context new-top-level-goal))
        (sprouted-context nil))
       (setq sprouted-context (car result))
       (setq old-top-level-goal (cadr result))
       (set-ordering sprouted-context ordering)
       ; Bring in all emotions?
       ; I don't think this is needed. Emotions are always in *reality*.
       ; (add-emotions sprouted-context new-context)
       ; Bring in the top-level goal (Would be done by above, if above
       ; were done).
       (no-gen (cx$assert sprouted-context new-top-level-goal))
       ;
       ; Make old top-level-goal actually be a subgoal of the
       ; current REVERSAL top-level-goal.
       ; Left over task slots associated with old top-level goal
       ; shouldn't make any difference (except maybe for clarity
       ; in debugging). We can clear them here if we want.
       (if do-intends?
           (cx$assert sprouted-context
                      (ob$fcreate `(INTENDS linked-from ,new-top-level-goal
                                            linked-to ,old-top-level-goal
                                            rule Reversal-Plan))))
       sprouted-context))

(setq *reverse-leaf-thresh* 0.5)

; Ordering of sprouted-contexts is in inverse proportion to
; the realities of the leafs from which those contexts were
; derived. Lower reality assumptions are better candidates
; for replanning.
(defun reverse-leafs (old-top-level-goal context top-level-goal rule bd goal)
  (let ((sprouted-contexts nil)
        (sprouted-context nil)
        (intends nil)
        (old-context nil)
        (leafs (get-leafs old-top-level-goal *intends-ob* 'forward
                          (ob$get old-top-level-goal
                                   'termination-context))))
       (ndbg-roman-nl *gate-dbg* rule "Reverse leafs for ~A in ~A top = ~A"
             old-top-level-goal context top-level-goal)
       (yloop
        (yfor leaf in leafs)
        (ydo (if (fl< (strength (ob$get leaf 'obj)) *reverse-leaf-thresh*)
                (progn
                 (setq old-context (ob$get leaf 'activation-context))
                 ; Sprout an alternative past context
                         (delay-dbgs 'to-be-set
                    (rule-fire-msg rule "coded plan" context bd nil goal)
                  (ndbg-roman-nl *gate-dbg* rule "Reverse leafs")
                  (setq sprouted-context
                   (reversal-sprout-alternative old-context
                                                old-top-level-goal
                                                context
                                                top-level-goal
                                                (fl/ 1.0 (strength
                                                 (ob$get leaf 'obj)))
                                                t))
                 (setq xxcontext sprouted-context))
                 (setq sprouted-contexts (cons sprouted-context
                                              sprouted-contexts))
                 ; Retract the leaf objective (so we have to plan for
                 ; it instead of shakily assuming its truth)
                 (cx$retract sprouted-context (ob$get leaf 'obj))))))
       sprouted-contexts))

;
; REVERSE-UNDO-CAUSES:
;

(setq *next-prule-number* 1)

(setq *new-personal-goals* nil)

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

(defun cx$input-states (cx)
  (yloop (initial (result nil))
         (yfor ob in (cx$get-all cx))
         (ydo (if (ob$get ob 'input-state?)
                  (setq result (cons ob result))))
         (yresult result)))

(defun cx$assert-many (cx obs)
  (yloop (yfor ob in obs)
        (ydo (cx$assert cx ob))))

(defun cx$retract-many (cx obs)
  (yloop (yfor ob in obs)
        (ydo (cx$retract cx ob))))

(defun predicting-state (state)
  (let ((pred (ob$get (ob$ty state) 'predictor)))
       (if pred
           (ob$fcreate `((quote ,pred)))
           nil)))

; Merges emotions and whatever they are connected to into another
; context.
(defun add-emotions (to-context from-context)
  (ndbg-roman-nl *gate-dbg* rule "Add emotions")
  (yloop (initial (deps nil))
        (yfor ob in (cx$get-all from-context))
        (ydo (if (ty$instance? ob 'emotion)
                (progn
                 (cx$assert to-context ob)
                 (setq deps (get-links ob *dependency-ob* from-context))
                 (yloop (yfor dep in deps)
                        (ydo (cx$assert to-context dep)
                             (cx$assert to-context (ob$get dep 'linked-to))))
                 (setq deps (get-links-from ob *dependency-ob* from-context))
                 (yloop (yfor dep in deps)
                        (ydo (cx$assert to-context dep)
                             (cx$assert to-context
                                        (ob$get dep 'linked-from)))))))))

; Returns a fresh alternative past context (an alternative of old-context)
; which includes only planning structure with which we are concerned
; (old-top-level-goal) modified to be part of a new top-level goal
; (new-top-level-goal), state facts true at that time in the past, and
; having the specified context (new-context) as an effective parent.
; My, isn't this description clear!
; ---> This can also be used to sprout an alternative future (as in
; earthquake), so the name is misleading. Todo: change it.
(defun sprout-alternative-past (old-context old-top-level-goal
                                 new-context new-top-level-goal)
  ; Copy the old starting context from which we wish to explore an
  ; alternative past
  (ndbg-roman-nl *gate-dbg* rule "Sprout alternative")
  (no-gen (let ((sprouted-context (cx$copy old-context nil)))
       ; Make this alternative past be a (pseudo) sprout of the
       ; new context in which we wish to carry out this past
       ; exploration.
       (cx$pseudo-sprout-of sprouted-context new-context)
       ; Declare this context as an alternative past (for
       ; inverted emotional responses) if necessary.
       ; Fix generational tense on this context
       ; (There is no analogous 'what if' here?)
       (if (not (dd-goal? old-top-level-goal)) ; criterion for past/future
           (progn
            (set-altern sprouted-context)
            (ob$set sprouted-context 'gen-switches
                    '((tense conditional-present-perfect)))))
       ; Get rid of all emotions.
       (gc-emotions sprouted-context)
       ; Get rid of any planning structure not on behalf of the
       ; old top-level goal which we will be replanning.
       (gc-plans sprouted-context (list old-top-level-goal))
       ; The following is a kludge (as if other things weren't!).
       ; Top-level goal outcomes clobber the goal status globally;
       ; therefore we must recopy it here and set it back to
       ; being an ACTIVE-GOAL.
       (if (or (ty$instance? old-top-level-goal 'succeeded-goal)
               (ty$instance? old-top-level-goal 'failed-goal))
           (progn
;            (ndbg-roman-nl *gate-dbg* rule "The case of the resolved goal.")
            (setq old-top-level-goal
                 (replace-linked-ob old-top-level-goal
                                    sprouted-context *me-belief-path*
                                    *empty-bd*))
            (cx$retract sprouted-context old-top-level-goal)
            (ob$set old-top-level-goal 'type *active-goal-ob*)
            (cx$assert sprouted-context old-top-level-goal)))
       ; Change all remaining planning structure to be on behalf
       ; of new-top-level-goal.
       ; Without replace-linked-ob this would clobber the top-level goals
       ; on things which are shared with the original episode contexts.
       ; Todo: altern would be to have a cx$copy that copies obs and yet
       ; preserves links.
       (yloop (yfor ob in (cx$get-all sprouted-context))
             (ydo (if (ty$instance? ob 'goal)
                      (progn
                       (setq ob (replace-linked-ob ob sprouted-context
                                 *me-belief-path* *empty-bd*))
                       (ob$set ob 'top-level-goal new-top-level-goal)))))
       ; Also, fix up activation contexts to be here. This isn't
       ; strictly necessary (?) because failure reversal (which uses
       ; that slot so far) will never get run on these trcs...
       (yloop (yfor ob in (cx$get-all-ty sprouted-context *active-goal-ob*))
              (ydo (ob$set ob 'activation-context sprouted-context)))
       (list sprouted-context old-top-level-goal))))

;
; Garbage collect away any planning structure not on behalf of any of
; the specified top-level-goals.
; (May want to use this other than from sprout-alternative-past in order
;  to unclutter contexts.)
;
; Note: the INTENDS removal is a bit brute force but it probably works.
;
; Todo: Has to be extended also to get rid of relative planning
; structure--- (BELIEVE MS1 (ACTIVE-GOAL ...))
;
(defun gc-plans (context top-level-goals)
  (ndbg-roman-nl *gate-dbg* rule
   "Gc plans for ~A in ~A" top-level-goals context)
  (yloop (initial (deps nil))
        (yfor ob in (cx$get-all context))
        (ydo (if (and (ty$instance? ob 'goal)
                      (not (memq? (ob$get ob 'top-level-goal)
                                  top-level-goals)))
                 (progn
                  (setq deps (get-links ob *intends-ob* context))
                  (yloop (yfor dep in deps)
                         (ydo (cx$retract context dep)))
                  (cx$retract context ob))))))

(defun gc-plans1 (context top-level-goals)
  (ndbg-roman-nl *gate-dbg* rule
   "Gc plans for ~A in ~A" top-level-goals context)
  (yloop (initial (deps nil))
        (yfor ob in (cx$get-all context))
        (ydo (if (and (ty$instance? ob 'goal)
;                     (not (memq? ob top-level-goals))
                      (memq? (ob$get ob 'top-level-goal) top-level-goals))
                (progn
                 (setq deps (get-links ob *intends-ob* context))
                 (yloop (yfor dep in deps)
                        (ydo (cx$retract context dep)))
                 ; Don't clobber the top-level goal itself.
                 (if (not (memq? ob top-level-goals))
                     (cx$retract context ob)))))))

(defun gc-emotions (context)
  (ndbg-roman-nl *gate-dbg* rule "Gc emotions in ~A" context)
  (yloop (initial (deps nil))
         (yfor ob in (cx$get-all context))
         (ydo (if (ty$instance? ob 'emotion)
                  (progn
                   (setq deps (get-links ob *dependency-ob* context))
                   (yloop (yfor dep in deps)
                          (ydo (cx$retract context dep)))
                   (setq deps (get-links-from ob *dependency-ob* context))
                   (yloop (yfor dep in deps)
                          (ydo (cx$retract context dep)))
                   (cx$retract context ob))))))

; End of file.
