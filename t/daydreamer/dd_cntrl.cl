;*******************************************************************************
;
; Daydreamer
; Version 3.5
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
;  9/25/86: Removed flavors
; 19990504: ported to Common Lisp
;
;*******************************************************************************

;
; Top-level functions
;

(setq *starting-state* 'daydreaming)

(defun daydreamer ()
  (ndbg-reset)
  (if *typeset?*
      (format *gate-dbg* "\\begin{flushleft}~%"))
  (ndbg-roman-nl *gate-dbg* rule *dd-version*)
  (setq *state* 'suspended)
  (daydreamer-initialize)
  (set-state *starting-state*)
  ; Get off the ground by running inferences which will activate
  ; some top-level goals.
  (run-inferences *reality-lookahead* nil *me-belief-path*)
  ; Run the top-level emotion-directed control loop.
  (daydreamer-control0)
  (ndbg-roman-nl *gate-dbg* rule "DAYDREAMER terminates")
  (if *typeset?*
      (format *gate-dbg* "\\end{flushleft}~%"))
  t)

(defun dd-continue ()
  (daydreamer-control0))

; This will pick up from the first subgoal in the set of subgoals of
; which goal is a part. We need to find the first context in which there
; is a plan for goal and back up one.
(defun pickup (goal)
  (let ((context (ob$get goal 'activation-context))
        (top-level-goal (ob$get goal 'top-level-goal)))
       (ob$set context 'children nil)
       (set-next-context top-level-goal context)
       (ndbg-roman-nl *gate-dbg* rule "DAYDREAMER ~A pickup" goal)
       (daydreamer-control0)))

(defun pickup-in (goal context)
  (let ((top-level-goal (ob$get goal 'top-level-goal)))
       (ob$set context 'children nil)
       (set-next-context top-level-goal context)
       (ndbg-roman-nl *gate-dbg* rule "DAYDREAMER ~A pickup" goal)
       (daydreamer-control0)))

(defun resume (goal)
  (change-tlg-status goal 'runable)
  (daydreamer-control0))

(defun resume-infs ()
  (yloop (yfor fact in *needs*)
        (ydo (cx$touch-fact *reality-lookahead* fact)))
  (run-inferences *reality-lookahead* nil *me-belief-path*)
  (set-state 'performance)
  (daydreamer-control0))

(defun resume-enter ()
  (enter-concepts *reality-lookahead* *me-belief-path*)
  (run-inferences *reality-lookahead* nil *me-belief-path*)
  (set-state 'performance)
  (daydreamer-control0))

(defun update-initial (cx)
  (yloop (yfor f in *initial-facts*)
         (ydo (cx$assert f cx))))

(defun new-daydreamer ()
  (setq *first-time-initialize?* t)
  (daydreamer))

(setq *state* nil)

(defun set-state (state)
  (if (not (memq? state
           '(suspended performance daydreaming)))
      (set-state (error "~A is not a valid state." state))
      (if (neq? state *state*)
          (progn
           (ndbg-roman-nl *gate-dbg* rule "State changes from ~A to ~A"
                          *state* state)
           (setq *state* state)))))

(defun performance-mode? ()
  (eq? *state* 'performance))

(defun daydreaming-mode? ()
  (eq? *state* 'daydreaming))

(setq *reality* nil)
(setq *reality-lookahead* nil)
(setq *primal-reality* nil)
(setq *first-time-initialize?* t)

(setq *initial-reality* nil)

(setq *entered-concepts* nil)

(defun daydreamer-initialize ()
  (ndbg-roman-nl *gate-dbg* rule "Initialize DAYDREAMER")
  (if *first-time-initialize?*
      (first-time-initialize))
  (ndbg-roman-nl *gate-dbg* rule "~%Creating initial reality context...")
  (setq *top-level-goals* nil)
  (setq *top-level-goal* nil)
  (setq *emotions* nil)
  (setq *reality*
        (cx$sprout *primal-reality*))
  (setq *initial-reality* *reality*) ; for debugging
  (setq *entered-concepts* nil)
  (setq *reality-lookahead* *reality*)
  (need-init *reality*)
  (epmem-initialize))

(defun dbg-info ()
  (cx$tree-print *initial-reality*))

(defun first-time-initialize ()
  (ndbg-roman-nl *gate-dbg* rule "Performing first-time initialization")
  (no-gen (initialize-primal-reality))
  (setq *first-time-initialize?* nil))

(setq *initial-facts* nil)

(defun loadable-subsets? (subsets)
  (or (nil? subsets)
      (any? (lambda (x) (memq? x *subsets*)) subsets)))

(defun initialize-primal-reality ()
  (ndbg-roman-nl *gate-dbg* rule "Creating primal reality...")
  (setq *primal-reality* (cx$create))
  (yloop (yfor assertion in *initial-facts*)
         (ydo (cx$assert *primal-reality* assertion))))

;
; Top-level control loop: repeatedly select the most highly motivated
; available top-level goal and run that goal.
;
(defun daydreamer-control0 ()
  (ndbg-roman-nl *gate-dbg* rule "Running emotion-driven control loop...")
  (yloop (initial (candidates nil)
                 (strikes 0)
                 (top-level-goal nil))
        (yuntil (> strikes 2))
        (ydo
            (need-decay)
            (emotion-decay)
            (setq candidates (most-highly-motivated-goals))
            (format (standard-output) ":")
            (force-output (standard-output))
            (cond
             ((null? candidates)
              (if (performance-mode?)
                  (progn
                  (ndbg-roman-nl *gate-dbg* rule
                   "No more goals to run; switching to daydreaming mode")
                   (setq strikes (+ 1 strikes))
                   (set-state 'daydreaming))
                  (progn
                   (if (null? (environmental-object-input))
                       (progn
                        (ndbg-roman-nl *gate-dbg* rule
                         "No more goals to run; switching to performance mode")
                        (setq strikes (+ 1 strikes))
                        (yloop (yfor goal in *top-level-goals*)
                              (ydo (if (and (eq? 'waiting (ob$get goal 'status))
                                           (eq? 'real (ob$get goal
                                                               'planning-type)))
                                      (progn
                                       (change-tlg-status goal 'runable)))))
                        (set-state 'performance))))))
             ((memq? top-level-goal candidates)
              (setq strikes 0)
              (daydreamer-control1 top-level-goal))
             ((= (length candidates) 1)
              (setq strikes 0)
              (setq top-level-goal (car candidates))
              (ndbg-roman *gate-dbg* rule "Switching to new top-level goal")
              (ndbg-roman *gate-dbg* rule " ~A" top-level-goal)
              (ndbg-newline *gate-dbg* rule)
              (daydreamer-control1 top-level-goal))
             (else
              (setq strikes 0)
              (setq top-level-goal (random-element candidates))
              (ndbg-roman *gate-dbg* rule "Switching to new top-level goal
                          (broke tie)")
              (ndbg-roman *gate-dbg* rule " ~A" top-level-goal)
              (ndbg-newline *gate-dbg* rule)
              (daydreamer-control1 top-level-goal))))))

(setq *need-decay-factor* .98)

(defun need-decay ()
  (ndbg-roman-nl *gate-dbg* rule-long "Need decay.")
  (yloop (yfor need in *needs*)
        (ydo (set-strength need (fl* *need-decay-factor* (strength need)))
             (cx$touch-fact *reality* need))))

(setq *emotion-decay-factor* .95)

(setq *emotion-gc-threshold* 0.15)

; Only non-motivating emotions are subject to decay.
(defun emotion-decay ()
  (ndbg-roman-nl *gate-dbg* rule-long "Emotion decay.")
  (yloop
   (yfor emot in *emotions*)
   (ydo
    (if (not (motivating-emotion? emot))
        (progn
         (set-strength emot (fl* *emotion-decay-factor* (strength emot)))
         (cx$touch-fact *reality* emot)
         (if (fl< (strength emot) *emotion-gc-threshold*)
             (progn
              (ndbg-roman-nl *gate-dbg* rule "Emotion ~A below threshold." emot)
              (cx$retract *reality* emot)
              (emotion-delete emot))))))))

(defun emotion-delete (emot)
  (setq *emotions* (delq! emot *emotions*))
  *emotions*)

(defun emotion-add (emot)
  (if (not (memq? emot *emotions*))
      (setq *emotions* (cons emot *emotions*)))
  emot)

(defun motivating-emotion? (emot)
  (any? (lambda (ob) (ty$instance? ob 'goal)) ; or (memq? ob *top-level-goals*)
        (get-dependees emot *reality* *me-belief-path*)))

(setq *top-level-goals* nil)

;
; Find the top-level goals which are most highly motivated and are
; not halted, and if in performance mode, are not imaginary.
;
(defun most-highly-motivated-goals ()
  (yloop (initial (highest-strength 0.0)
;                 (emotion nil)
                  (candidates nil))
         (yfor top-level-goal in *top-level-goals*)
         (ydo (if (and (eq? 'runable
                       (ob$get top-level-goal 'status))
                       (or (daydreaming-mode?)
                           (neq? 'imaginary
                                 (ob$get top-level-goal 'planning-type))))
                  (progn
;                  (setq emotion (ob$get top-level-goal 'emotion))
;                  (if (null? emotion)
;                      (error "No motivating emotion found for ~A"
;                             top-level-goal))
                   (cond
                    ((= (strength top-level-goal) highest-strength)
                     (setq candidates (cons top-level-goal candidates)))
                    ((> (strength top-level-goal) highest-strength)
                     (setq highest-strength (strength top-level-goal))
                     (setq candidates (list top-level-goal)))))))
         (yresult candidates)))

;
; Debugging functions
;

(defun print-tasks ()
  (yloop
   (yfor c in *top-level-goals*)
   (ydo (ndbg-roman-nl *gate-dbg* task "~A concern ~A motiv ~A status ~A"
                       (if (eq? 'imaginary (ob$get c 'planning-type))
                           "Daydreaming goal"
                           "Personal goal")
                       (tlg->string c)
                       (strength c)
                       (ob$get c 'status)))))

;
; Control algorithm for a particular top-level-goal:
; 1) Run one step of the planner on the next context.
; 2) If the top-level goal succeeded, terminate planning for this
;    top-level goal.
; 3) Otherwise if running the planner produced no sprouts, then
;    attempt to backtrack.
; 4) Otherwise set the next context to run to be one of the sprouts,
;    selected at random.
;
(setq *top-level-goal* nil)

(defun daydreamer-control1 (top-level-goal)
  (let ((next-context (get-next-context top-level-goal))
        (sprouts nil)
        (tlg-switch? (neq? top-level-goal *top-level-goal*))
        (succeeded-goal nil))
       (if tlg-switch? (gen-new-paragraph))
       (setq *top-level-goal* top-level-goal)
       (ndbg-roman-nl *gate-dbg* rule-long
                      "Running control algorithm for ~A in ~A"
                      top-level-goal next-context)
       (if (and (number? (ob$get next-context 'timeout))
                (<= (ob$get next-context 'timeout) 0))
           (progn
            (ndbg-roman *gate-dbg* rule "Timeout")
            (ndbg-roman *gate-dbg* rule " on ~A" next-context)
            (ndbg-newline *gate-dbg* rule)
            (backtrack-top-level-goal top-level-goal next-context))
           (progn
            (run-rules top-level-goal next-context tlg-switch?)
            (if (setq succeeded-goal (find-top-level-goal-outcome?
                                      top-level-goal next-context
                                      *succeeded-goal-ob*))
                ; Todo: eventually we may wish to run scenario generator
                ; past top-level goal success.
                (terminate-top-level-goal top-level-goal succeeded-goal
                                          next-context)
           (progn
            (if (or (eq? 'runable (ob$get top-level-goal 'status))
                    (eq? 'fired-halt (ob$get top-level-goal 'status)))
                (progn
                 (if (eq? 'fired-halt (ob$get top-level-goal 'status))
                     (change-tlg-status top-level-goal 'halted))
                 (setq sprouts (prune-possibilities (cx$children next-context)))
                 (if (null? sprouts)
                     ; should include case of top-level goal failure
                     (backtrack-top-level-goal top-level-goal next-context)
                     (set-next-context top-level-goal
                                       (car sprouts)))))))))))

(defun change-tlg-status (tlg status)
  (ndbg-roman *gate-dbg* rule
              "Change status of ~A to ~A"
              (tlg->string tlg)
; Can't print below because it might be FIRED-HALT, a weird state.
;              (ob$get tlg 'status)
              status)
  (ob$set tlg 'status status))

(defun get-next-context (top-level-goal)
  (if (eq? 'imaginary (ob$get top-level-goal 'planning-type))
      (ob$get top-level-goal 'next-context)
      *reality-lookahead*))

(defun set-next-context (top-level-goal next-context)
  (if (eq? 'imaginary (ob$get top-level-goal 'planning-type))
      (ob$set top-level-goal 'next-context next-context)
      (setq *reality-lookahead* next-context)))

(defun get-backtrack-wall (top-level-goal)
  (if (eq? 'imaginary (ob$get top-level-goal 'planning-type))
      (ob$get top-level-goal 'backtrack-wall)
      *reality*))

(defun find-top-level-goal-outcome? (top-level-goal context goal-type)
  (yloop (initial (found? nil))
         (yfor elem in (cx$get-all-ty context goal-type))
         (yuntil found?)
         (ydo (if (eq? top-level-goal elem) ; was (ob$get elem 'active-goal)
                  (setq found? elem)))
         (yresult found?)))

(defun terminate-top-level-goal (top-level-goal resolved-goal
                                 resolution-context)
  (reality-stabilize) ; necessary?
  (ndbg-roman *gate-dbg* rule "Terminating planning for top-level goal")
  (ndbg-roman *gate-dbg* rule " ~A" top-level-goal)
  (ndbg-newline *gate-dbg* rule)
  (task-print-plans top-level-goal)
  (ob$set resolved-goal 'termination-context resolution-context)
  ; Stop any future planning on this top-level goal
  (setq *top-level-goals* (delq! top-level-goal *top-level-goals*))
  ; Update to trace the list of tasks
  (print-tasks)
  ; Remove emotional motivators for active goal (which got carried
  ; over to resolved goal)
  (remove-motivating-emotions *reality* resolved-goal)
  ; Invoke general emotional response
  (if (or (not (dd-goal? resolved-goal))
          (ty$instance? (ob$get resolved-goal 'obj) 'revenge))
      (progn
       (emotional-response resolved-goal nil
                           (strength (ob$get resolved-goal 'obj))
                           resolution-context)
;       (gen-overall-emot-state) Now done in dd_gen
))
  ; Store the successful planning episode in episodic memory
  (if (ty$instance? resolved-goal 'succeeded-goal)
      (episode-store-top-goal resolved-goal resolution-context))
  ; If top-level goal was imaginary planning, assert final goal status
  ; (and objective if goal success) into reality context.
  (if (eq? 'imaginary (ob$get top-level-goal 'planning-type))
      (no-gen
       (cx$assert *reality* resolved-goal)
       (if (ty$instance? resolved-goal 'succeeded-goal)
           (cx$assert *reality* (ob$get resolved-goal 'obj)))))
  ; If top-level goal was real planning and goal failure, we better assert
  ; the failure into the reality context in case it isn't already there.
  (if (and (eq? 'real (ob$get top-level-goal 'planning-type))
           (ty$instance? resolved-goal 'failed-goal))
      (no-gen (cx$assert *reality* resolved-goal)))
  (ndbg-roman-nl *gate-dbg* rule-xtra "About to sprout ~A" *reality*)
  (setq *reality* (cx$sprout *reality*))
  (ndbg-roman-nl *gate-dbg* rule-xtra "Back from sprouting")
  (setq *reality-lookahead* *reality*)
  (clear-subgoals resolved-goal *reality* *me-belief-path*)
; (gc-plans1 *reality* (list top-level-goal))
; If we use gc-plans1 here, we must modify it so it does not retract
; side-effect goal outcomes.
)

(defun remove-motivating-emotions (context goal)
  (ndbg-roman-nl *gate-dbg* rule "Removing motivating emotions of ~A in ~A"
                        goal context)
  (let ((dependencies (get-dependencies goal context *me-belief-path*)))
       (yloop (yfor dependency in dependencies)
              (ydo 
               (if (ty$instance? (ob$get dependency 'linked-from) 'emotion)
                   (progn
                    (cx$retract context dependency)
                    (remove-if-free (ob$get dependency 'linked-from)
                                    context)))))))

(defun remove-if-free (emotion context)
  (let ((dependees (get-dependees emotion context *me-belief-path*))
        (dependencies (get-dependencies emotion context *me-belief-path*)))
       (if (and (null? dependees)
                (null? dependencies))
           (progn
            (cx$retract context emotion)
            (emotion-delete emotion)))))

; Assumes reality is stabilized.
(defun emotional-response (resolved-goal altern? realism context)
  (ndbg-roman-nl *gate-dbg* rule "Emotional responses for ~A in ~A"
                        resolved-goal context)
  ; Actually, as coded currently, get-other-causes returns a bd list
  ; (whose car is an other cause).
  (let* ((other-causes (get-other-causes resolved-goal context))
         (emot nil))
        (setq emot
              (if other-causes
                  (if altern?
                      (if (ty$instance? resolved-goal 'succeeded-goal)
                          ; angry at someone for not doing something
                          ; Should we substitute function 'first-non-me'
                          ; which is used in generator? No, other-causes
                          ; will not contain a me.
                          (ob$fcreate `(NEG-EMOTION to ,(ob$get
                                                         (car other-causes)
                                                         'actor)
                                                    altern? t))
                          (ob$fcreate '(POS-EMOTION altern? t)))
                      (if (ty$instance? resolved-goal 'succeeded-goal)
                          (ob$fcreate '(POS-EMOTION))
                          (ob$fcreate `(NEG-EMOTION to ,(ob$get (car
                                                           other-causes)
                                                          'actor)))))
                  (if altern?
                      (if (ty$instance? resolved-goal 'succeeded-goal)
                          (ob$fcreate '(NEG-EMOTION altern? t))
                          (ob$fcreate '(POS-EMOTION altern? t)))
                      (if (ty$instance? resolved-goal 'succeeded-goal)
                          (ob$fcreate '(POS-EMOTION))
                          (ob$fcreate '(NEG-EMOTION))))))
        (add-emotion resolved-goal emot realism context)
        emot))

(setq *emotions* nil)

; Note that goal may not be in *reality*. Will this cause inconsistencies
; later?
(defun add-emotion (goal emotion realism context)
  (emotion-add emotion)
  (add-depend *reality* goal emotion realism 0.0 0.0 nil)
  (cx$assert *reality* emotion)
  ; The purpose of the below is to activate any daydreaming goals
  ; that should be activated as a result of the new emotion.
  (if (neq? *reality* context)
      (progn
       (ndbg-roman-nl *gate-dbg* rule-long "Running reality inferences")
       (run-inferences *reality* nil *me-belief-path*))))

; This is called from run-inferences.
(defun personal-goal-outcome (goal context top-level-goal)
  (ndbg-roman-nl *gate-dbg* rule "Personal goal outcome ~A in ~A" goal context)
  (reality-stabilize) ; yes?
  (ob$set goal 'termination-context context)
  (let ((emot (emotional-response goal (altern? context)
                                  (strength (ob$get goal 'obj)) context)))
       (if (ty$instance? (ob$get top-level-goal 'obj) 'rationalization)
           (divert-emot-to-tlg emot context top-level-goal))
;       (gen-overall-emot-state) Now done in dd_gen
))

; Divert strength of new emotion to main motivator of a top-level goal.
; Used for rationalization and surprise.
;  Why do we have to do it this way? Why do we have to divert to the
; main motivator? Why can't we just connect a new emotion up to the
;  goal and recalculate its value? Was this because of something that is
; no longer true?
; The reason is that 1) Rationalization-Inf1 works off of the emotion
; which originally activated rationalization; 2) we actually want
; an initial NEG-EMOTION to be nulled out of existence through
; rationalization rather than simply summing in POS-EMOTIONs to
; counteract.
(defun divert-emot-to-tlg (emot context top-level-goal)
  (let ((main-motiv (ob$get top-level-goal 'main-motiv)))
       (ndbg-roman-nl *gate-dbg* rule "Divert strength of ~A to ~A"
                      emot top-level-goal)
       (modify-strength *reality* main-motiv (strength emot)
                        (sign-correction emot main-motiv))
       (ndbg-roman-nl *gate-dbg* rule "Null out charge of ~A" emot)
       (modify-strength *reality* emot (strength emot) -1.0)
       (print-tasks)))

(defun sign-correction (emot1 emot2)
  (cond
   ((or (and (ty$instance? emot1 'neg-emotion)
             (ty$instance? emot2 'pos-emotion))
        (and (ty$instance? emot1 'pos-emotion)
             (ty$instance? emot2 'neg-emotion)))
    -1.0)
   (else 1.0)))

(defun emotion-sign (emot)
  (if (ty$instance? emot 'neg-emotion)
      -1.0
      1.0))

(defun backtrack-top-level-goal (top-level-goal next-context)
  (ndbg-roman-nl *gate-dbg* rule
                 "Attempting to backtrack for top-level goal ~A in ~A"
                 top-level-goal next-context)
  (yloop (initial (backtrack-wall (get-backtrack-wall top-level-goal))
                 (sprouts nil)
                 (done? nil))
        (yuntil done?)
        (ydo (if (eq? backtrack-wall next-context)
                (progn
                 (ndbg-roman *gate-dbg* rule "Top-level goal")
                 (ndbg-roman *gate-dbg* rule " ~A" top-level-goal)
                 (ndbg-roman-nl *gate-dbg* rule
                                " fails: all possibilities exhausted")
                 ; The below will do for now.
                 (all-possibilities-failed top-level-goal backtrack-wall)
                 ; If return value is NIL, there were no successful
                 ;  mutations and so the top-level goal is terminated.
                 ; If return value is T, there was a successful mutation
                 ;  and the next-context of this top-level-goal has
                 ;  already been set by that process.
                 ; Was (setq done? (null? returned-next-context)) because this
                 ; algorithm had to do some more backup in the old mutation alg.
                 (setq done? t))
                (progn
                 (setq sprouts
                      (prune-possibilities (cx$children
                                            (cx$parent next-context))))
                 (if sprouts
                     (progn
                      (setq next-context (car sprouts)) ; was random-element
                      ; but we want to go by ordering, so no random...
                      (set-next-context top-level-goal next-context)
                      (ndbg-roman *gate-dbg* rule "Backtracking")
                      (ndbg-roman *gate-dbg* rule
                            " to next context of ~A for ~A"
                            next-context top-level-goal)
                      (ndbg-newline *gate-dbg* rule)
                      (setq done? t))
                     (setq next-context (cx$parent next-context))))))))

;
; Currently, DAYDREAMER stops upon the first success and so the below
; function is invoked if there was not a single success.
;
; Returns leaf if should continue because more contexts have been
; sprouted, or NIL if we should stop.
;
; Todo: What about eventual mutation exhaustion?
;
; Possibly, a top-level goal failure has been asserted in some context;
; But we have to do this in the 'resolution context', which in this
; case is backtrack-wall.
;
(defun all-possibilities-failed (top-level-goal backtrack-wall)
 (ndbg-roman-nl *gate-dbg* rule "All possibilities failed for ~A in ~A"
                           top-level-goal backtrack-wall)
 (let ((result nil))
  (if (and (daydreaming-mode?)
           (eq? 'imaginary (ob$get top-level-goal 'planning-type))
           (setq result (action-mutations top-level-goal backtrack-wall)))
      result
;      (let ((failed-goal (ob$fcreate `(FAILED-GOAL obj ,(ob$get
;                                                           top-level-goal
;                                                           'obj))))))
      (progn
;       (if (interrogate "Break? (before tlg failure) ") (breakpoint))
       (terminate-top-level-goal top-level-goal
                                (make-goal-failure top-level-goal backtrack-wall
                                                   nil *me-belief-path*
                                                   top-level-goal t)
                                backtrack-wall)
       nil))))

; This is pruning possibilities after they are generated. Another way
; is to prune before they are generated.
(defun prune-possibilities (contexts)
  (ndbg-roman-nl *gate-dbg* rule "Pruning possibilities from ~A" contexts)
  (yloop
   (initial (result nil))
   (yfor context in contexts)
   (ydo
    (if (and (not (ob$get context 'rules-run?))
             (not (ob$get context 'dd-goal-sprout?)))
        ; will the above prevent backups to the first dd goal context?
        (setq result (cons context result))))
   (yresult (sort result (lambda (context1 context2)
                                (> (ordering context1)
                                   (ordering context2)))))))

; Extra slots associated with a top-level goal:
;       status: 'runable (if this goal is ready to run)
;               'halted (if this goal is halted)
;               'waiting (if this goal is waiting to be performed)
;       active-goal: used for the top-level goal upon replacement
;       planning-type: 'real or 'imaginary
;       backtrack-wall: (only if planning-type = 'imaginary) backtrack
;                       wall context
;       next-context: (only if planning-type = 'imaginary) next context
;                     to run
;       mutation-plan-contexts: slot values contain ideas for new plans
;       run-mutations?: t or nil
;       termination-context: not set until the top-level goal is terminated
;        main-motiv: main motivation emotion
;
;       For top-level goals AND subgoals:
;       activation-context: points to the context in which the goal
;              was first activated.
;       top-level-goal: points to the top-level goal for all subgoals
;               and, of course, the top-level goal itself
;
; Termination and activation contexts are more or less at this time
; for REVERSAL. (But, see also uses of 'top-context which really
; are referring to 'activation-context)
;
(setq *genable-emot-thresh* 0.1)

(defun activate-top-level-goal (goal context bd rule)
  (ndbg-roman-nl *gate-dbg* rule "******************")
  (ndbg-roman *gate-dbg* rule "Activate top-level goal")
  (ndbg-roman *gate-dbg* rule " ~A in ~A" goal context)
  (ndbg-newline *gate-dbg* rule)
  (let ((emotions (ob$gets rule 'emotion))
        (ddg? (dd-goal? goal))
        (new-context nil)
        (any-emot nil)
        (main-motiv nil))
       (setq *top-level-goals* (cons goal *top-level-goals*))
       (if ddg?
           (no-gen (cx$assert context goal))
           (cx$assert context goal))
       (ob$add goal 'top-level-goal goal)
       (if (null? (ob$get rule 'initial-status))
           (ob$add goal 'status 'runable)
           (ob$add goal 'status (ob$get rule 'initial-status)))
       (yloop (yfor emotion in emotions)
              (ydo (if (var? emotion)
                       (setq main-motiv
                        (setq emotion (ob$instantiate emotion bd)))
                       (setq any-emot
                        (setq emotion (ob$instantiate emotion bd))))
                   (add-depend context emotion goal 1.0 0.0 0.0 nil)
                   (emotion-add emotion)
                   (if (or (eq? emotion main-motiv)
                           (fl< (strength emotion) *genable-emot-thresh*))
                       (no-gen (cx$assert context emotion))
                       (cx$assert context emotion))))
       ; should really batch strength recalculations above
       (if (null? main-motiv)
           (setq main-motiv any-emot))
       (ob$add goal 'main-motiv main-motiv)
       (if ddg?
           (progn
            (ob$add goal 'planning-type 'imaginary)
            (setq new-context (cx$sprout context))
            (ob$add new-context 'dd-goal-sprout? t)
            (ob$add goal 'backtrack-wall new-context)
            (ob$add goal 'activation-context new-context)
            (ob$add goal 'next-context new-context))
           (progn
            (ob$add goal 'activation-context context)
            (ob$add goal 'planning-type 'real)))
       (print-tasks)))

; End of file.
