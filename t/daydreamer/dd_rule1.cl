;*******************************************************************************
;
; Daydreamer
; Version 3.5
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
;  6/22/85: First version with contexts written (3.0)
;  8/27/85: Changed to new rule syntax (3.1)
; 11/26/85: Modified rules to be obs (3.2)
;  1/14/86: Changed non-action goal successes to 'fact plans'
;  1/19/86: Wrote inference chain to planning trc
;  1/24/86: Finished adding new analogy code and planner mods
;  1/28/86: Added use of cx$get-all-ty
;  2/15/86: Started adding new tracing and linearization, running off nu-dd3
;  7/19/86: Added chaining mechanism
;  9/23/86: Got rid of flavors
;
;*******************************************************************************

(setq *linearized?* t)

;
; Daydreamer rules
;

(ty$create 'RULE nil '(nil (subgoal goal delete initial emotion is comment
                                    plausibility self-type initial-status
                                    halt? inf-comments plan-comments
                                    top-level-goal
                                    no-pp-all
                                    inf-no-gen plan-no-gen reality-subgoal
                                    script)
                           (self-type inf-comments plan-comments inf-no-gen
                                      top-level-goal plan-no-gen
                                      reality-subgoal)))

(setq *rules* nil)

(defun rule-fire-msg (rule kind context bd sprouted-context goal)
    (ndbg-roman-nl *gate-dbg* rule "******************")
    (format (standard-output) "~A " (ob->string rule))
    (force-output (standard-output))
    (ndbg-roman *gate-dbg* rule "~A fired as ~A " (ob->string rule) kind)
    (if goal
        (progn
         (ndbg-newline *gate-dbg* rule)
         (ndbg-roman-nl *gate-dbg* rule "for ~A" goal)))
    (ndbg-roman *gate-dbg* rule "in ~A" (ob->string context))
    (if sprouted-context
        (ndbg-roman *gate-dbg* rule " sprouting ~A"
                    (ob->string sprouted-context)))
    (ndbg-newline *gate-dbg* rule)
    (if-interested-in rule (print-comments *gate-dbg* rule kind))
;    (if-interested-in rule (ob$print rule *gate-dbg*)
;              (if (not *typeset?*)
;                  (ndbg-newline *gate-dbg* rule))
;                      (bd-print bd *gate-dbg*))
    (if-interested-in rule (if (not *typeset?*)
                               (ndbg-newline *gate-dbg* rule))
                      (bd-print bd *gate-dbg*))
  )

; Example:
; \begin{tabular}{|ll|} \hline
; IF & this is a test \\
;    & of the system \\
; THEN & this is a test \\ \hline
; \end{tabular}
;

(defun print-comments (stream rule kind)
  (let ((com (cdr (ob$get rule (if (string-equal? kind "inference")
                                    'inf-comments
                                    'plan-comments)))))
   (if com
       (if *typeset?*
           (progn
            (format stream "\\end{flushleft}~%")
;               (progn
;                (format stream "\\begin{flushleft}~%")
;                (format stream "English description for rule as ~A:~%"
;                        kind)
;                (format stream "\\end{flushleft}~%"))
            (format stream "\\begin{center}\\begin{tabular}{|ll|} \\hline~%")
            (format stream "{\\sl{}IF} & {\\rm{}")
            (setq com (cdr (print-if-then-strings com stream)))
            (format stream "}\\\\~% {\\sl{}THEN} & {\\rm{}")
            (print-if-then-strings com stream)
            (format stream "} \\\\ \\hline~%\\end{tabular}\\end{center}")
            (format stream "\\begin{flushleft}~%"))
           (progn
        (format stream
         "-------------------------------------------------------~%")
            (format stream "IF   ")
            (setq com (cdr (print-if-then-strings com stream)))
            (format stream "~%THEN ")
            (print-if-then-strings com stream)
        (format stream
         "~%-------------------------------------------------------~%"))))))

(setq *if-then-length* 40)

; Use 'et' for 'and' if you do not want it to break into separate lines.
(defun print-if-then-strings (com stream)
  (yloop
   (initial (len 0)
            (last-word "silly"))
   (yuntil (or (null? com)
              (eq? (car com) 'then)))
   (ydo
    (yloop
     (yfor word in (break-into-words (car com)))
     (ydo (if (or (> len *if-then-length*)
                 (string-equal? last-word "and"))
             (progn
              (if *typeset?*
                  (format stream "} "))
              (do-newline stream)
              (if *typeset?*
                  (format stream " & {\\rm{}")
                  (format stream "     "))
              (setq len 0)))
         (setq last-word word)
         (cond
          ((uppercase? (string-head word))
           (begin-head-font stream) (format stream "~A " word)
           (end-font stream)
           (setq len (+ 1 len (string-length word))))
          ((string-equal? word "et")
           (format stream "and ")
           (setq len (+ len 4)))
          (else
           (format stream "~A " word)
           (setq len (+ 1 len (string-length word)))))))
     (setq com (cdr com)))
   (yresult com)))

(defun break-into-words (string)
  (yloop (initial (result nil)
                 (pos nil))
        (yuntil (string-empty? string))
        (ydo (setq pos (string-posq #\SPACE string))
            (if pos
                (progn
                 (setq result
                  (append! result (list (string-slice string 0 pos))))
                 (setq string (string-nthtail string (+ 1 pos))))
                (progn
                 (setq result (append! result (list string)))
                 (setq string ""))))
        (yresult result)))

;
; Unenforced assumptions about rule base:
; - Every action must have a precondition plan, even if it is only TRUE->ACTION
; - Assume only one rule matches each action, and that each rule contains
;   all the necessary preconditions for any matchee
;
; Syntax:
;
; (define-rule name subsets spec)
; where spec =
;       (RULE
;        [comment <string> <string> ...]
;        plausibility <number>
;        subgoal <pattern>
;        goal <pattern>
;        [delete <pattern> <pattern> ...]
;        [is inference-only |
;             plan-only | plan-only-no-auto | action-plan |
;             global-inference])
;
;   Optional slots in goal slot: WEIGHT, OFFSET, DECAY
;       (otherwise plausibility is used, right?)
;
;   Inferences always sum into assertions existing in the
;   appropriate context
;
;   There is no reason maintenance, i.e., things aren't
;   removed upon removal of their justifications.
;

(defun undefine-rule (rule)
  (rule-destroy-chaining rule)
  (ob$remove-all rule)
  (ob$set rule 'is 'destroyed)
  (setq *rules* (delq! rule *rules*))
  t)

(defun rule-destroy-chaining (rule)
  (yloop (yfor r in (ob$gets rule 'forward-chain))
        (ydo (remove-chaining rule r))))

; If modified, change add-rule-print.
(defun add-rule (rule)
  (ndbg-roman-nl *gate-dbg* rule "Adding rule ~A" (ob->string rule))
  (setq *rules* (cons rule *rules*))
  (rule-create-chaining rule))

; If modified, change add-rule.
(defun add-rule-print (rule)
  (ndbg-roman *gate-dbg* rule "Adding rule ")
  (dbg-print-ob rule)
  (setq *rules* (cons rule *rules*))
  (rule-create-chaining rule))

(defun dbg-print-ob (ob)
  (ndbg-roman-nl *gate-dbg* rule "~A:" (ob->string ob))
  (ob$pr ob *gate-dbg* *ob-print-options*)
  (ndbg-newline *gate-dbg* rule))

(defun check-rule (rule name)
  (if (null? (ob$get rule 'plausibility))
      (progn
       (ndbg-roman-nl *gate-dbg* rule "Warning: ~A has no plausibility." name)
       (ob$add rule 'plausibility 1.0)))
  (if (and (plan? rule)
           (ob$get rule 'subgoal)
           (ty$instance? (ob$get rule 'subgoal) 'rand))
      (ndbg-roman-nl *gate-dbg* rule "Warning: ~A should be RSEQ?" name))
  (if (and (ob$get rule 'goal)
           (ty$instance? (ob$get rule 'goal) 'action)
           (not (action-plan? rule)))
      (ndbg-roman-nl *gate-dbg* rule "Warning: ~A should be action-plan?" name))
  (if (and (action-plan? rule)
           (ob$get rule 'goal)
           (not (ty$instance? (ob$get rule 'goal) 'action)))
      (progn
       (ndbg-roman-nl *gate-dbg* rule
             "Warning: Either ~A should not be an action-plan"
             name)
       (ndbg-roman-nl *gate-dbg* rule
             " or you forgot to declare ~A as type ACTION"
             (ob$name (ob$get (ob$get rule 'goal) 'type))))))

(defun inference? (rule)
  (memq? (ob$get rule 'is) '(() inference-only global-inference)))

(defun plan? (rule)
  (memq (ob$get rule 'is)
        '(() plan-only plan-only-no-auto action-plan)))

(defun plan-only-no-auto? (rule)
  (eq? (ob$get rule 'is) 'plan-only-no-auto))

(defun global-inference? (rule)
  (eq? (ob$get rule 'is) 'global-inference))

(defun action-plan? (rule)
  (eq? (ob$get rule 'is) 'action-plan))

;
; Rule chaining mechanism
;

; Compile a new rule into the chaining graph.
(defun rule-create-chaining (rule1)
  (yloop (yfor rule2 in *rules*)
        (ydo (rule-create-chaining1 rule1 rule2)
            (rule-create-chaining1 rule2 rule1))))

; An alternative tack would be to create two separate graphs: one
; for plans and one for inferences. But note that plans often function
; as inferences. So doing this may involve some difficult-to-predict
; consequences. I am taking the more sure-fire route of putting both
; in the same graph and testing for type of rule upon use (which is
; a fast test anyway; nothing compared to unification).
;
; delete slots are ignored for the purpose of the chaining graph.

; Todo: might implement rep invariant that for a planning rule, the
; length of the subgoal's objs must be the same as the number of
; embedded patterns.
; Note that subgoalnum is whacky for inferences.
(defun rule-embedded-subgoals (rule)
  (cond
   ((ob$get rule 'embedded-subgoals))
   (else (ob$set rule 'embedded-subgoals
                 (embedded-patterns (ob$get rule 'subgoal))))))

(defun rule-create-chaining1 (rule1 rule2)
  (let ((rule1-goals (ob$gets rule1 'goal))
        (rule2-subgoals (rule-embedded-subgoals rule2))
        (dir1 nil) (dir2 nil))
   (if (eq? rule2-subgoals 'rnot)
    (progn
     (ob$set rule2 'number-of-subgoals 0)
     (add-chaining rule1 rule2 0)
     t)
    (progn
     (ob$set rule2 'number-of-subgoals (length rule2-subgoals))
     (yloop (yfor rule1-goal in rule1-goals)
      (ydo
       (yloop
        (initial (i 0))
        (yfor rule2-subgoal in rule2-subgoals)
        (ydo
         (if (possible-unify? rule1-goal rule2-subgoal)
             (progn
              (setq dir1 (ob$unify rule1-goal rule2-subgoal *empty-bd*))
              (setq dir2 (ob$unify rule2-subgoal rule1-goal *empty-bd*)))
             (progn
              (setq dir1 nil)
              (setq dir2 nil)))
         (if (or dir1 dir2)
          (progn
           (add-chaining rule1 rule2 i)
;           (if (or (null? dir1) (null? dir2))
;               (progn
;                (ndbg-roman-nl *gate-dbg* rule
;         "warning: Asymmetrical unify in rule chaining ~A ~A ~A ~A ~A ~A"
;                 rule1 rule2 rule1-goal rule2-subgoal dir1 dir2)
;                (ndbg-roman-nl *gate-dbg* rule "Offenders:")
;                (po rule1-goal)
;                (po rule2-subgoal)))
         ))
         (setq i (+ i 1)))))
       (yresult t))))))

; Returns 'RNOT if RNOT is embedded within.
; Also, a pattern having DEPENDENCY is treated as if it were an RNOT
; (since DEPENDENCIES don't act as touched obs; will this cause a problem
; if ever a dependency but nothing else is asserted? Should never be.)
(defun embedded-patterns (ob)
  (cond
   ((or (ty$instance? ob 'rseq)
        (ty$instance? ob 'rand)
        (ty$instance? ob 'ror))
    (yloop (initial (result nil)
                   (temp nil))
          (yfor y in (ob$gets ob 'obj))
          (ydo (setq temp (embedded-patterns y))
              (if (neq? temp 'rnot)
                  (setq result (append! result temp))
                  (setq result 'rnot)))
          (ywhile (neq? result 'rnot))
          (yresult result)))
   ((ty$instance? ob 'rnot) 'rnot)
   ((ty$instance? ob 'dependency) 'rnot)
   (else (list ob))))

; Save chaining info in forms useful both for regular planning/inferencing
; and for rule intersection.
(defun add-chaining (from-rule to-rule subgoalnum)
  (if (not (memq? to-rule (ob$gets from-rule 'forward-chain)))
      (progn
       (ob$add from-rule 'forward-chain to-rule)
       (ob$add to-rule 'backward-chain from-rule)))
  (ob$add from-rule 'forward-chain-nums (list to-rule subgoalnum))
  (ob$add to-rule 'backward-chain-nums (list from-rule subgoalnum))
;  (ndbg-roman-nl *gate-dbg* chain "Chaining from ~A goal to ~A subgoal[~A]"
;                 from-rule to-rule subgoalnum)
  (ndbg-roman-nl *gate-dbg* chain "~&~A subgoal[~A] <-- ~A goal"
                 to-rule subgoalnum from-rule))

(defun remove-chaining (from-rule to-rule)
  (ob$remove from-rule 'forward-chain to-rule)
  (ob$remove to-rule 'backward-chain from-rule)
  (yloop (yfor pair in (ob$gets from-rule 'forward-chain-nums))
        (ydo (if (eq? to-rule (car pair))
                (ob$remove from-rule 'forward-chain-nums pair))))
  (yloop (yfor pair in (ob$gets to-rule 'backward-chain-nums))
        (ydo (if (eq? from-rule (car pair))
                (ob$remove to-rule 'backward-chain-nums pair)))))

;  (ob$set from-rule 'forward-chain-nums
;           (del! (lambda (x y) (eq? x (car y)))
;                 to-rule
;                 (ob$get from-rule 'forward-chain-nums)))
;  (ob$set to-rule 'backward-chain-nums
;           (del! (lambda (x y) (eq? x (car y)))
;                 from-rule
;                 (ob$get to-rule 'backward-chain-nums))))

;
; Rule processes
;

(defun run-rules (top-level-goal context tlg-switch?)
  (let ((rule-ever-fired? nil) (halted? nil) (con nil))
       (ndbg-roman-nl *gate-dbg* rule
                      "----------------------~A--------------------"
                      (ob->string context))
       (ndbg-roman-nl *gate-dbg* rule "Running rules for ~A" top-level-goal)
       (if (and (not tlg-switch?)
                (setq con (cx$last-sprout-con (cx$parent context))))
           (progn
            (generate1 con '((backtrack t) (tense gerund))
                       context *me-belief-path*)))
       (if (null? (cx$parent context))
           (error "context has no parent")
           (cx$set-last-sprout-con (cx$parent context)
                                   (ob$get context 'first-gened-concept)))
       (if *linearized?* (cx$print-sprout-trace context))
       (yloop
        (initial (bp nil) (rule-fired? nil))
        (yuntil halted?)
        (yfor other in (others context))
        (ydo
         (setq bp (->belief-path other))
         (ndbg-roman-nl *gate-dbg* rule-xtra "Belief path is ~A" bp)
         (setq rule-fired?
              (perform-goal-successes top-level-goal context bp))
         (setq rule-ever-fired?
              (or rule-ever-fired? rule-fired?))
         ; Experimentally, run inferences if above did not
         ; run and touched facts.
         ; ---> put this into perform-goal-successes instead....
         ; If it works, can get rid of temp variable above.
;         (if (and (me-belief-path? bp)
;                  (null? rule-fired?)
;                  (ob$get context 'touched-facts))
;             (run-inferences context top-level-goal bp))
         (if (neq? 'runable (ob$get top-level-goal 'status))
             (setq halted? t)
             (progn
              (if (me? other)
                  (setq rule-ever-fired?
                       (or (run-p-goals context top-level-goal)
                           rule-ever-fired?)))
              (setq rule-ever-fired?
                   (or (run-plans top-level-goal context bp)
                       rule-ever-fired?))))))
       (if (not halted?)
           (ob$set context 'rules-run? t)
           (ob$removes context 'sprout-trace)) ; Todo: actually, we
       ; should only flag it not to be reprinted. For later
       ; graphics and debugging we would like to leave it.
       rule-ever-fired?))

(defun perform-goal-successes (top-level-goal context belief-path)
  (ndbg-roman-nl *gate-dbg* rule-long "Perform goal successes in ~A" context)
  (yloop
   (initial (ever-fired? nil))
   (ywhile (and (perform-goal-successes1 top-level-goal context belief-path)
               (eq? 'runable (ob$get top-level-goal 'status))
               (progn 
                (if (me-belief-path? belief-path)
                    (entered-concept-serendipity))
                t)))
   (ydo (setq ever-fired? t))
   (yresult ever-fired?)))

(defun perform-goal-successes1 (top-level-goal context belief-path)
  (ndbg-roman-nl *gate-dbg* rule-long "Goal successes cycle")
  (let* ((fired1 (perform-non-action-goal-successes top-level-goal
                                                    context belief-path))
         (fired2 (perform-action-goal-successes top-level-goal
                                                context belief-path fired1)))
       (or fired1 fired2)))

(setq *enter-concepts?* nil)

(defun perform-action-goal-successes (top-level-goal context belief-path
                                       fired1?)
  (ndbg-roman-nl *gate-dbg* rule-long
                 "Perform action goal successes in ~A" context)
  (let ((fired? nil)
        (old-enter-concepts *enter-concepts?*)
        (concepts nil))
    (setq *enter-concepts?* nil)
    (yloop
     (yfor ob in (if (me-belief-path? belief-path)
                     (cx$get-all-ty context *active-goal-ob*)
                     (cx$get-all-ty context *believe-ob*)))
     (ywhile (eq? 'runable (ob$get top-level-goal 'status)))
     (ydo (setq fired? (or (perform-action-goal-success ob top-level-goal
                                                      context belief-path)
                         fired?))))
       ; Will get asserted active goals from previous sprouting.
    (if (ob$get context 'touched-facts) ; (or fired? (eq? fired1? 'inferences))
        (progn
         (run-inferences context top-level-goal belief-path)
         (if (and fired? (not-me-belief-path? belief-path))
             (run-inferences context top-level-goal *me-belief-path*))))
    (if *enter-concepts?*
        (progn
         (ndbg-roman-nl *gate-dbg* rule
                        "Taking optional concept input")
         (if (setq concepts (enter-concepts context belief-path))
             (progn
              (ndbg-roman-nl *gate-dbg* rule "Concepts entered")
              (run-inferences context top-level-goal belief-path)
              (if (me-belief-path? belief-path)
                  (rule-induction concepts context))))))
    (setq *enter-concepts?* old-enter-concepts)
    fired?))

(defun perform-action-goal-success (ob top-level-goal context
                                     belief-path)
  (if (if (me-belief-path? belief-path)
          (and (eq? top-level-goal (ob$get ob 'top-level-goal))
               (ty$instance? (ob$get ob 'obj) 'action)
               (subgoals-completed? ob context belief-path))
          (and (setq ob (absolute->relative ob belief-path))
               (ty$instance? ob 'active-goal)
               (eq? top-level-goal (ob$get ob 'top-level-goal))
               (ty$instance? (ob$get ob 'obj) 'action)
               (subgoals-completed? ob context belief-path)))
      (let ((rule (goal-subgoals-rule ob context belief-path))
            (actors (ob$gets (ob$get ob 'obj) 'actor)))
       (ndbg-roman-nl *gate-dbg* rule-long
                      "Perform action goal success for ~A in ~A" ob context)
       (if (performance-mode?)
           (cond
            ((vars-in? (ob$get ob 'obj))
              (ndbg-roman-nl *gate-dbg* rule
               "About to perform real action but variables in action")
              (change-tlg-status top-level-goal 'halted)
              nil)
            ((and (me-in? actors) (non-mes actors))
             (ndbg-roman-nl *gate-dbg* rule "Perform external shared action")
             (perform-action ob top-level-goal context belief-path rule t))
            ((non-mes actors)
             (perform-other-action ob top-level-goal context belief-path rule))
            (else
             (ndbg-roman-nl *gate-dbg* rule "Perform external action")
             (perform-action ob top-level-goal context belief-path rule nil)
             (if (me-belief-path? belief-path) (setq *enter-concepts?* t))
             t))
           (if (eq? 'real (ob$get top-level-goal 'planning-type))
               (progn
                (ndbg-roman-nl *gate-dbg* rule
                 "About to perform real action but not in performance mode")
                (change-tlg-status top-level-goal 'waiting)
                nil)
               (perform-action ob top-level-goal context
                               belief-path rule nil))))
      nil))

; Todo: Should also probably run inferences off of entered concepts
(defun perform-other-action (ob top-level-goal context belief-path rule)
  (ndbg-roman-nl *gate-dbg* rule "Perform other action ~A in ~A" ob context)
  (let ((found-bd nil)
        (found-concept nil)
        (concepts (enter-concepts context belief-path))) 
   (yloop
    (initial (action (ob$get ob 'obj)))
    (yfor concept in concepts)
    (yuntil found-bd)
    (ydo
     (setq found-bd (ob$unify action concept *empty-bd*))
     (if found-bd (setq found-concept concept))))
   ; was concept action (other order).
   (if found-bd
       (progn
        (ob$set found-concept 'inference-rule rule)
        (make-goal-success ob context found-concept belief-path found-bd))
       (make-goal-failure ob context (tlast concepts) belief-path
                          top-level-goal nil))
   (setq *reality* *reality-lookahead*)
   t))

(defun perform-action (ob top-level-goal context belief-path rule shared?)
  (ndbg-roman-nl *gate-dbg* rule "Perform action goal ~A in ~A" ob context)
  ; Can only perform below if ob obj is full instantiated?
  (if (or (not shared?)
          (shared-action-approved? (ob$get ob 'obj)))
      (progn
       (setq ob (make-goal-success ob context nil belief-path *empty-bd*))
       (ob$set (ob$get ob 'obj) 'inference-rule rule)
       (cx$hyper-assert-relative context (ob$get ob 'obj) belief-path))
      (make-goal-failure ob context nil belief-path top-level-goal nil))
  (if (eq? 'real (ob$get top-level-goal 'planning-type))
      (setq *reality* *reality-lookahead*))
  t)

; Todo: Maybe later I will want to input that the other party
; does not go along with a shared action.
(defun shared-action-approved? (action) t)

(defun perform-non-action-goal-successes (top-level-goal context
                            belief-path)
  (ndbg-roman-nl *gate-dbg* rule-long
   "Perform non-action goal successes in ~A" context)
  (yloop
   (initial (fired? nil))
   (yfor ob in (if (me-belief-path? belief-path)
                   (cx$get-all-ty context *active-goal-ob*)
                   (cx$get-all-ty context *believe-ob*)))
   (ydo (if (not-me-belief-path? belief-path)
           (progn
            (setq ob (absolute->relative ob belief-path))
            (if (and (ty$instance? ob 'active-goal)
                     (not (ty$instance? ob 'active-p-goal))
                     (not (ty$instance? (ob$get ob 'obj) 'action))
                     ; Below not needed anymore. Seq logic fixed.
;                     (seq-head? ob context belief-path)
                     (not (memq? context (ob$gets ob 'failed-context)))
                     (or (eq? top-level-goal (ob$get ob 'top-level-goal))
                         (not (real? top-level-goal)))
                     ) ; end and
                (setq fired? (or-inf (perform-non-action-goal-success
                                                   ob top-level-goal
                                                   context belief-path)
                                     fired?))))
           (if (and (not (ty$instance? (ob$get ob 'obj) 'action))
                    (not (ty$instance? ob 'active-p-goal))
;                    (seq-head? ob context belief-path)
                    (not (memq? context (ob$gets ob 'failed-context)))
                     (or (eq? top-level-goal (ob$get ob 'top-level-goal))
                         (not (real? top-level-goal)))
                    ) ;end and
               (setq fired? (or-inf (perform-non-action-goal-success
                                                  ob top-level-goal
                                                  context belief-path)
                                    fired?)))))
   (yresult fired?)))

; Should return sprouted contexts
(defun perform-non-action-goal-success (ob top-level-goal context belief-path)
  (ndbg-roman-nl *gate-dbg* rule-long
   "Perform non-action goal success for ~A in ~A" ob context)
  (let* ((intends-links (goal-intends-links-uo ob context belief-path))
         (rule nil)
         (no-auto? nil))
  (if intends-links
      (progn
       (setq rule (ob$get (car intends-links) 'rule))
       (setq no-auto? (plan-only-no-auto? rule))))
  (cond
   ((and intends-links
         (null? no-auto?)
         (any-subgoals-failed? ob context belief-path))
    (make-goal-failure ob context nil belief-path top-level-goal nil) t)
   (else
    (let ((bds nil)
          (fired? nil)
          (sprouted-context nil))
         (cond
          ((or (if (ty$instance? (ob$get ob 'obj) 'rtrue)
                   (setq bds (list *empty-bd*))
                   nil)
               (setq bds (cx$retrieve-relative context
                         (ob$get ob 'obj) belief-path)))
           (map 'list
            (lambda (bd)
             (if (not (empty-bd? bd))
                 ; For non-empty bd, this is done in run-plan
                 (progn
;                  (if-interested-in rule (bd-print bd *gate-dbg*))
                  (ndbg-roman-nl *gate-dbg* rule-long
                   "Non-empty bd ~A, so doing nothing" bd)
                  )
; Todo: this is inelegant; needs to be redid (redone).
;                  (if (and (neq? top-level-goal (ob$get ob 'top-level-goal))
;                           (eq? 'runable (ob$get ob 'top-level-goal)))
;                      (progn
;                      (ndbg-roman-nl *gate-dbg* rule-long
;                                      "Wake up that top-level goal though")
;                       (surprise (ob$get ob 'top-level-goal))))
                 (progn
                  (setq sprouted-context context)
                  (setq fired? t)
                  (if intends-links
                      (setq ob (make-goal-success ob sprouted-context 
                                             (if (neq? 't (car bd))
                                                 (car bd)
                                                 nil)
                                         belief-path bd))
                      (no-gen (setq ob (make-goal-success ob sprouted-context
                                             (if (neq? 't (car bd))
                                                 (car bd)
                                                 nil)
                                         belief-path bd)))))))
             bds)
           (if (and fired?
                    intends-links
                    (not (subgoals-completed? ob context belief-path)))
               (progn
                (ndbg-roman *gate-dbg* rule "Fortuitous goal success")
                (ndbg-roman *gate-dbg* rule " ~A in ~A" ob context)
                (ndbg-newline *gate-dbg* rule)
                (clear-subgoals ob context belief-path)
                (if (neq? top-level-goal (ob$get ob 'top-level-goal))
                    (surprise (ob$get ob 'top-level-goal)))
                )) ; end block end if
           fired?)
          ((and (null? no-auto?)
                (subgoals-completed? ob context belief-path))
           (set-strength (ob$get ob 'obj)
                         (calculate-supergoal-obj-strength
                          (map 'list (lambda (x) (ob$get x 'linked-to))
                               intends-links)
                          rule))
           (setq ob (make-goal-success ob context nil belief-path *empty-bd*))
           (ob$set (ob$get ob 'obj) 'inference-rule rule)
           (cx$assert-relative context (ob$get ob 'obj)
                 belief-path)
           (fake-inference-deletes
            (map 'list (lambda (x) (ob$get x 'obj))
                 (goal-subgoals ob context belief-path))
            rule context belief-path)
           'inferences)
          (else nil)))))))

; Clears subgoals. Does not include goal.
(defun clear-subgoals (goal context belief-path)
  (ndbg-roman-nl *gate-dbg* rule-xtra
                 "Clearing subgoals of ~A in ~A" goal context)
  (yloop
   (yfor subgoal in (goal-subgoals-uo goal context belief-path))
   (ydo
    (clear-subgoals subgoal context belief-path)
    (cx$retract-relative context subgoal belief-path)))
  (yloop
   (yfor intends in (goal-intends-links-uo goal context belief-path))
   (ydo
    (cx$retract-relative context intends belief-path))))

(defun fake-inference-deletes (subgoal-objs rule ctxt belief-path)
  (let ((bd *empty-bd*)
        (deletes (ob$gets rule 'delete)))
       (if deletes
           (progn
            (ndbg-roman-nl *gate-dbg* rule
                           "FAKE INFERENCE ~A ~A" rule ctxt)
            (yloop (yfor rule-subgoal-obj in (ob$gets rule 'subgoal))
                  (yfor subgoal-obj in subgoal-objs)
                  (ydo (setq bd (ob$unify rule-subgoal-obj subgoal-obj bd))))
            (if (null? bd)
                (error "null bd in fake-inference-deletes")
                (setq bd *empty-bd*))
            (yloop
             (yfor elem in deletes)
             (ydo (cx$retract-relative ctxt (ob$instantiate-o elem bd)
                                      belief-path)))))))

(defun or-inf (a b)
  (if (or a b)
      (if (or (eq? a 'inferences)
              (eq? b 'inferences))
          'inferences
          t)
      nil))

; Todo: there is a flaw in run-fact-plan. It thinks the goal succeeds even
; if there are remaining unbound variables in the instantiated result.
(defun run-fact-plan (ob top-level-goal context belief-path)
  (ndbg-roman-nl *gate-dbg* rule-long "Run fact plan for ~A in ~A" ob context)
  (let ((bds (cx$retrieve-relative context (ob$get ob 'obj) belief-path))
        (sprouted-context nil)
        (sprouted-contexts nil))
    (map 'list (lambda (bd)
      (if (not (empty-bd? bd))
          (progn
           (setq sprouted-context (cx$sprout context))
           (delay-dbgs sprouted-context
            (set-ordering sprouted-context 1.0)
            (setq sprouted-contexts (cons sprouted-context sprouted-contexts))
            (ndbg-roman-nl *gate-dbg* rule "Fact plan ~A found" (car bd))
            (if-interested-in rule
                              (ob$sprint (car bd) *gate-dbg*)
                              (ndbg-newline *gate-dbg* rule)
                              (bd-print bd *gate-dbg*))
            (make-goal-success ob sprouted-context
                               (car bd) belief-path bd)))))
         bds)
    sprouted-contexts))

; Order of subgoals does not matter.
(defun calculate-supergoal-obj-strength (subgoals rule)
  (yloop (initial (result 0.0)
                 (weight (fl/ (ob$get rule 'plausibility)
                              (fixnum->flonum (length subgoals)))))
        (yfor subgoal in subgoals)
        (ydo (setq result (fl+ result
                             (fl* weight (strength (ob$get subgoal 'obj))))))
        (yresult result)))

(defun subgoals-completed? (active-goal context belief-path)
  (let ((subgoals (goal-subgoals-uo active-goal context belief-path)))
   (if (and subgoals (every? (lambda (x) (ty$instance? x 'succeeded-goal))
                             subgoals))
       (progn (ndbg-roman-nl *gate-dbg* rule "Subgoals of ~A completed"
                             active-goal) t)
       nil)))

;
; Comments on goal completion:
; Could we do the below when inferences fire, instead of looping
; through everything? Should the below create some sort of
; 'achieves' link? (below is above now)
;
; The below isn't nice from a debugging printout pov: it asserts
; before changing to success and thuis the assert message
; may appear mislaeading.
;
; FOR ANALOGICAL Non-realistic goals, we need to assume goal success
; if all subgoals succeed.
;
; --> The below should be modified to go until it quiesces, now
; that auto subgoal -> goal success exists. Or should we
; keep this stuff in different contexts?

(defun any-subgoals-failed? (active-goal context belief-path)
  (let ((subgoals (goal-subgoals-uo active-goal context belief-path)))
   (if (and subgoals (any? (lambda (x) (ty$instance? x 'failed-goal))
                           subgoals))
       (progn (ndbg-roman-nl *gate-dbg* rule
               "Subgoal of ~A failed" active-goal)
              t)
       nil)))

(defun seq-head? (goal context belief-path)
  (not (any? (lambda (x) (cx$true-relative context x belief-path))
             (ob$gets goal 'seq-next-of))))

; Assumes goal is seq-head.
(defun next-in-seq (goal context belief-path)
  (ndbg-roman-nl *gate-dbg* rule-xtra "Go to next in RSEQ from ~A in ~A"
                 goal context)
  (yloop (yfor seq-next in (ob$gets goal 'seq-next))
        (ydo (if (cx$true-relative context seq-next belief-path)
                (progn
                 (ob$remove goal 'seq-next seq-next)
                 (if (ty$instance? seq-next 'succeeded-goal)
                     (next-in-seq seq-next context belief-path)))))))

(defun make-goal-success (active-goal context dependency belief-path bd)
 (let ((top-level-goal (ob$get active-goal 'top-level-goal)))
; Don't sprout context. It should already be sprouted if the caller wants that.
  (ndbg-roman-nl *gate-dbg* rule "******************")
  (ndbg-roman *gate-dbg* rule "Goal")
  (ndbg-roman *gate-dbg* rule " ~A" active-goal)
  (ndbg-roman *gate-dbg* rule " succeeds")
  (ndbg-roman *gate-dbg* rule " in ~A" context)
  (ndbg-newline *gate-dbg* rule)
  (if (neq? 'runable (ob$get top-level-goal 'status))
      (progn
       (ndbg-roman-nl *gate-dbg* rule
                      "Waking up halted or waiting top-level goal")
       (change-tlg-status top-level-goal 'runable)))
  (let ((new-ob (or (plan-instantiate active-goal bd context
                                      top-level-goal belief-path t)
                    active-goal)))
       (if (seq-head? new-ob context belief-path)
           (next-in-seq new-ob context belief-path))
       (if dependency
           (cx$assert-relative context
                 (ob$fcreate `(DEPENDENCY linked-from ,dependency
                                            linked-to ,(ob$get new-ob
                                                                'obj)
                                            weight 1.0
                                            offset 0.0
                                            decay 0.0))
                 belief-path))
; Who, if anyone, uses the action link created below?
; Was used for rationalization LEADTO detection.
;    (if (ty$instance? (ob$get goal 'obj) 'action)
;        (ol-set-relative new-ob
;                         *action-link-ob*
;                         (ob$get goal 'obj)
;                         context
;                         belief-path))
       ; The below generates emotional responses
       ; for subgoalized personal goals in REVERSAL, etc.
       (if (and (personal-goal? new-ob)
                (neq? top-level-goal active-goal)
; Was only for subgoals of top-level-goal
;                (eq? (goal-supergoal new-ob context) top-level-goal)
                (me-belief-path? belief-path))
           (personal-goal-outcome new-ob context top-level-goal))
    new-ob)))

;
; plan-instantiate: if bd not empty, instantiate everything in the planning
; structure with the new bindings.
;
; Does not instantiate goals if no variables in the goal (except for the
; active-goal which is always instantiated).
;
; The top-level goal is never replaced, since top-level goals are
; used as unique, global task identifiers. However, the obj slot
; may be instantiated, since the objective of a top-level goal is
; global (the same across contexts).
; (But what about on final top-level goal success?)
;
(defun plan-instantiate (active-goal bd context top-level-goal belief-path
                          goal-success?)
 (let ((new-ob nil))
      (if (not (empty-bd? bd))
          (ndbg-roman-nl *gate-dbg* rule
                         "Instantiating plan for ~A"
                         (tlg->string top-level-goal)))
      (ndbg-roman-nl *gate-dbg* rule-long
                     "Ag = ~A, cx = ~A, bd = ~A, tlg = ~A"
                     active-goal context bd top-level-goal)
 (no-gen
  (if (not (empty-bd? bd))
       (yloop
        (yfor elem in (cx$get-all context))
        (ydo
         (setq elem (absolute->relative elem belief-path))
         (if (and elem ; (neq? elem top-level-goal) 
                  (ty$instance? elem 'goal)
                  (eq? (ob$get elem 'top-level-goal) top-level-goal)
                  (neq? elem active-goal)
                  (vars-in? (ob$get elem 'obj)) ; save some object copying
                  ) ; end and
             (if (eq? elem top-level-goal)
                 (replace-obj elem bd)
                 (replace-linked-ob elem context belief-path bd))))))
  (if (neq? active-goal top-level-goal)
      (progn
            (ndbg-roman-nl *gate-dbg* rule-xtra
                           "ag = ~A, tlg = ~A"
                           active-goal top-level-goal)
            (setq new-ob (replace-linked-ob active-goal context
                                           belief-path bd)))
      (replace-obj top-level-goal bd)))
  (if goal-success?
          (let ((goal-ob (or new-ob active-goal)))
           ; We need to retract and reassert because obs in contexts with
           ; hashing are restricted not to change type.
           (cx$retract-relative context goal-ob belief-path)
           (ob$set goal-ob 'type *succeeded-goal-ob*)
           (if (and ; (eq? active-goal top-level-goal)
                    (not (dd-goal? active-goal))
                    (ga-gen-on-outcome? (ob$get goal-ob 'gen-advice)))
               (cx$assert-relative context goal-ob belief-path)
               (no-gen (cx$assert-relative context goal-ob belief-path)))))
      new-ob))

;
; In the below, we should also stop activity on other active subgoals,
; no? But watch out--not in the case where this is called from
; all-possibilities-exhausted? ... Now I don't see why this is so; I am going
; to try it anyway and see what happens. ... As of early November 86, there
; seems to be no trouble with this.
;
; Todo: Must check for "protection violations": if the objective of a
; succeeded subgoal is not true, the subgoal becomes a failed subgoal.
; Don't check goals whose objectives are actions.
; But only do this if the rule does not say not to (i.e., 'scripts'
; like M-RESTAURANT do not obey this property).
;
(defun make-goal-failure (active-goal context dependency belief-path
                           top-level-goal all-poss-failed?)
  (ndbg-roman-nl *gate-dbg* rule "******************")
  (ndbg-roman *gate-dbg* rule "Goal")
  (ndbg-roman *gate-dbg* rule " ~A" active-goal)
  (ndbg-roman *gate-dbg* rule " fails")
  (ndbg-roman *gate-dbg* rule " in ~A" context)
  (ndbg-newline *gate-dbg* rule)
  (let ((new-ob active-goal))
    (if (not (eq? active-goal top-level-goal))
        (progn
         (clear-subgoals active-goal context belief-path)
         (setq new-ob (replace-linked-ob active-goal context belief-path
                                        *empty-bd*))))
    ; We need to retract and reassert because obs in contexts with
    ; hashing are restricted not to change type.
    (if (or (not (eq? active-goal top-level-goal))
            all-poss-failed?)
        (progn
         (cx$retract-relative context new-ob belief-path)
;        (if (eq? active-goal top-level-goal)
;            (ob$set new-ob 'active-goal active-goal))
         (ob$set new-ob 'type *failed-goal-ob*)
         (if (and (not (dd-goal? new-ob))
                  (ga-gen-on-outcome? (ob$get new-ob 'gen-advice)))
             (cx$assert-relative context new-ob belief-path)
             (no-gen (cx$assert-relative context new-ob belief-path)))))
     (if (and (eq? active-goal top-level-goal)
              (null? all-poss-failed?))
         (ob$add active-goal 'failed-context context))
       ; Since any failed subgoal implies top-level goal failure (in
       ; this context at least), we can connect dependency directly
       ; to top-level goal. (was new-ob).
    (if dependency
     (cx$assert-relative context (ob$fcreate `(DEPENDENCY
                                                    linked-from ,dependency
                                                    linked-to ,top-level-goal
                                                    weight 1.0
                                                    offset 0.0
                                                    decay 0.0))
                                    belief-path))
    new-ob))

; Note: must omit because else a cx would get copied.
(defun replace-linked-ob (ob context belief-path bd)
   (no-gen (replace-linked-ob1 ob context belief-path
                        (ob$instan-omit ob bd #'varize-object?
                         nil ; cannot be 0, because we want to instantiate
                             ; the goal obj with bd.
                         *link-slots*
                         nil))))

(defun replace-linked-ob1 (ob context belief-path ob-copy)
  (let ((from-links (ob$gets ob 'linked-from-of))
        (to-links (ob$gets ob 'linked-to-of))
        (seq-nexts (ob$gets ob 'seq-next))
        (seq-next-ofs (ob$gets ob 'seq-next-of))
        (episode (ob$get ob 'episode))
        (preserve-values (map 'list (lambda (slot-name) (ob$get ob slot-name))
                              *preserve-link-slots*))
        (plan-rule (if (ob$get ob 'obj)
                       (ob$get (ob$get ob 'obj) 'plan-rule) nil))
        (plan-subgoalnum (if (ob$get ob 'obj)
                             (ob$get (ob$get ob 'obj) 'plan-subgoalnum)
                             nil))
        (obj-strength (if (ob$get ob 'obj)
                          (ob$get (ob$get ob 'obj) 'strength)
                          nil))
        (link nil))
    (cx$retract-relative context ob belief-path)
    (cx$assert-relative context ob-copy belief-path)
    (yloop (yfor from-link in from-links)
          (ydo (if (cx$true-relative context from-link belief-path)
                  (progn
                   (setq link (ob$copy-omit from-link '(top-context)))
                   (ob$set link 'linked-from ob-copy)
                   (cx$retract-relative context from-link belief-path)
                   (cx$assert-relative context link belief-path)))))
    (yloop (yfor to-link in to-links)
          (ydo (if (cx$true-relative context to-link belief-path)
                   (progn
                    (setq link (ob$copy-omit to-link '(top-context)))
                    (ob$set link 'linked-to ob-copy)
                    (cx$retract-relative context to-link belief-path)
                    (cx$assert-relative context link belief-path)))))
    (yloop (yfor seq-next in seq-nexts)
          (ydo (if (cx$true-relative context seq-next belief-path)
                  (ob$add ob-copy 'seq-next seq-next))))
    (yloop (yfor seq-next-of in seq-next-ofs)
          (ydo (if (cx$true-relative context seq-next-of belief-path)
                  (ob$add ob-copy 'seq-next-of seq-next-of))))
       (if episode
           (progn
            (ob$set ob-copy 'episode episode)
            (if (ty$instance? ob-copy 'goal)
                (ob$set episode 'goal ob-copy))))
       (if plan-rule
           (ob$set (ob$get ob-copy 'obj) 'plan-rule plan-rule))
       (if plan-subgoalnum
           (ob$set (ob$get ob-copy 'obj) 'plan-subgoalnum plan-subgoalnum))
       (if obj-strength
           (set-strength (ob$get ob-copy 'obj) obj-strength))
       (yloop
        (initial (slot-values preserve-values))
        (yfor slot-name in *preserve-link-slots*)
        (ydo
         (if (car slot-values) (ob$set ob-copy slot-name (car slot-values)))
         (setq slot-values (cdr slot-values))))
    ob-copy))

(defun replace-obj (goal bd)
 (ndbg-roman-nl *gate-dbg* rule "Replace obj of ~A with ~A" goal bd)
 (let* ((goal-obj (ob$get goal 'obj))
        (new-obj (ob$instan-omit goal-obj bd #'varize-object?
                                 nil *link-slots* nil)))
   (ob$set new-obj 'plan-rule (ob$get goal-obj 'plan-rule))
   (ob$set new-obj 'plan-subgoalnum (ob$get goal-obj 'plan-subgoalnum))
   (if (ob$get goal-obj 'strength)
       (set-strength new-obj (ob$get goal-obj 'strength)))
   (ob$set goal 'obj new-obj)))

; End of file.
