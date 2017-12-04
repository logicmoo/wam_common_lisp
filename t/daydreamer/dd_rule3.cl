; Todo: We need to break this function into several to make it more
; readable.
(defun run-plan (goal top-level-goal context belief-path)
  (ndbg-roman-nl *gate-dbg* rule "Run plan for ~A in ~A" goal context)
  (if (not (planning-loop? goal context top-level-goal belief-path))
  (let ((goal-obj (ob$get goal 'obj))
        (sprouted-contexts nil)
        (candidates nil)
        (existing-analogical-ep? (ob$get goal 'analogical-episode)))
   ; Todo: In fact the below is necessary to give 'fact plans' priority
   ; over generic planning so that action mutations function to
   ; provide new possibilities. Only this isn't implemented clearly.
; But I got rid of this since fact plans are now in here.
;   (if (cx$retrieve-relative context goal-obj belief-path)
;       (progn (ndbg-roman-nl *gate-dbg* rule
;                "Warning: Goal obj ~A already true in ~A"
;                    goal-obj context)
;              nil))
    ;
    ; Highest priority--existing analogical plan.
    ;
    (if existing-analogical-ep?
        (progn
     (ndbg-roman-nl *gate-dbg* rule "Try existing analogical plans")
     (yloop (yfor episode in (ob$gets goal 'analogical-episode))
           (ydo (setq sprouted-contexts
                    (append!
                    ; Ordering is 1.0: There should only be one
                    ; analogical goal at lower levels anyway--I don't know why
                    ; this is a loop.
                    (try-analogical-plan goal goal-obj context
                                         episode
                                         belief-path top-level-goal)
                     sprouted-contexts))))))
    ;
    ; Mutation plans are next highest priority.
    ;
    (if nil ;(and (me-belief-path? belief-path)
             ; (null? sprouted-contexts)
             ; (ob$gets top-level-goal 'mutation-plan-contexts))
        (progn
     (ndbg-roman-nl *gate-dbg* rule "Try mutation plans")
        (setq sprouted-contexts (run-mutation-plans goal
                                                   top-level-goal
                                                   context))))
    ;
    ; Facts are next highest priority.
    ;
    ; Actions cannot be satisfied by fact plans, though.
    ;
    (if (and (null? sprouted-contexts)
             (not (ty$instance? goal-obj 'action)))
        (progn
         (ndbg-roman-nl *gate-dbg* rule "Try fact plans")
         (setq sprouted-contexts (run-fact-plan goal top-level-goal
                                               context belief-path))))
    ;
    ; Otherwise, try analogical plans or generic rules, depending.
    ;
    ; Get candidate rules and episodes
 (if (null? sprouted-contexts)
     (progn
  (ndbg-roman-nl *gate-dbg* rule "Try rules and episodes")
  (setq candidates (find-candidate-rules goal-obj nil
                                        belief-path context))
  ; If there are any episodes whose ordering is greater than
  ; 0.0, go for analogical planning; otherwise use generic
  ; planning.
  ; (Since leaf goals shouldn't be indexed, these analogical plans
  ; WILL go through, and so we don't need to have generic plans
  ; take over if no analogical plans fire).
  (if (order-candidates goal-obj candidates top-level-goal)
      (yloop (yfor episode in (candidates->episodes candidates))
            (ydo (setq sprouted-contexts
                     (append!
                     (run-analogical-plan goal goal-obj context
                                     (ob$get episode 'bd)
                                     (ob$get episode 'goal)
                                     (ob$get episode 'context)
                                     (ob$get episode 'rule)
                                     (ob$get episode 'ordering)
                                     belief-path top-level-goal
                                     episode t)
                      sprouted-contexts))))
      ; Rules generated automatically from input episodes are not used in
      ; generic planning (but other ones, e.g., from reversal, may be?)
      (yloop (yfor candidate in candidates)
            (ydo (if (not (constructed-plan? (candidate-rule candidate)))
                    (progn
                     (setq sprouted-contexts
                          (append!
                           (run-generic-plan goal goal-obj context
                                             (candidate-rule candidate)
                                             belief-path nil
                                             (candidate-bd candidate)
                                             top-level-goal)
                           sprouted-contexts)))))))))
    ;
    ; If still no luck, try believe others.
    ;
    (if (null? sprouted-contexts)
        (progn
         (ndbg-roman-nl *gate-dbg* rule "Try believe others")
        (if (and (ty$instance? goal-obj 'believe)
                 (neq? (car belief-path) (ob$get goal-obj 'actor))
                 (ty$instance? (ob$get goal-obj 'obj) 'mental-state))
;                 (or (ty$instance? (ob$get goal-obj 'obj)
;                                   'mental-state)
;                     (ty$instance? (ob$get goal-obj 'obj)
;                                   'personal-attribute))
            (let ((new-goal-obj (ob$get goal-obj 'obj)))
             (yloop
              (initial (believe-other? (ob$get goal-obj 'actor)))
              (yfor rule in (collect-planning-rules new-goal-obj))
              (ydo (if (plan? rule)
                       (progn
                        (setq sprouted-contexts
                              (append!
                               (try-generic-plan goal new-goal-obj context
                                rule belief-path believe-other?
                                top-level-goal)
                              sprouted-contexts))))))))))
    ;
    ; Eventually apply analogical plans in a relaxed way here?
    ;
    ; If still no luck, employ reality relaxation to a high degree
    ; and assume goal succeeds
    ;
    (if (and (null? sprouted-contexts)
             (progn
              (ndbg-roman-nl *gate-dbg* rule "Try subgoal relaxation")
              t)
             (not (top-goal? goal context belief-path))
             (or (not (real? top-level-goal))
                 (not (vars-in? goal-obj)))  ; for now...
             (not (ty$instance? goal-obj 'action))
             (not (ty$instance? goal-obj 'minimization))
             (me-belief-path? belief-path)
             ; Can only relax goals involving other actors than me for
             ; real planning.
             (cond
              ((or (real? top-level-goal)
                   (imaginary-realistic? top-level-goal))
               (if (ob? (ob$pget goal '(obj actor)))
                   (if (and (ob$pget goal '(obj actor))
                            (not-me? (ob$pget goal '(obj actor))))
                       t nil)
                   t))
              (else ;(imaginary-fanciful? top-level-goal)
               t)))
        (let ((sprouted-context (cx$sprout context)))
         (delay-dbgs sprouted-context
          (set-ordering sprouted-context 0.1)
          (ndbg-roman *gate-dbg* rule "Subgoal relaxation")
          (ndbg-roman-nl *gate-dbg* rule ", ~A succeeds" goal)
          (ndbg-newline *gate-dbg* rule)
          (gen-relaxation sprouted-context
             (cx$assert-relative sprouted-context goal-obj belief-path))
             ; Can only do below if goal is fully instantiated.
            (setq goal (make-goal-success goal sprouted-context nil belief-path
                               *empty-bd*)) ; could just wait..
            ; Todo: Have to copy goal objective!! This setting will affect
            ; other contexts! (If make-goal-success does deep copy, however,
            ; we are OK).
            (ob$pset goal '(obj strength) *goal-relaxation-realism*))
         (setq sprouted-contexts (list sprouted-context))))
    sprouted-contexts)
      nil))


