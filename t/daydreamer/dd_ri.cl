;*******************************************************************************
;
; Daydreamer
; Version 3.5
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
; This file contains:
; Serendipity mechanism including rule intersection and verification
;
; 7/19/86: First version written
; 7/22/86: Generalized code for personal and daydreaming goals
; 9/25/86: Took out flavors
;
;*******************************************************************************

;
; Run serendipities for any recent entered concepts.
;
(defun entered-concept-serendipity ()
  (ndbg-roman-nl *gate-dbg* seren-long "Running entered concept serendipity")
  (let ((result nil))
       (yloop (yfor fact in *entered-concepts*)
              (yuntil result)
              (ydo (setq result (run-fact-serendipity fact))))
       (setq *entered-concepts* nil)
       result))

;
; Run serendipities for a fact.
;
(defun run-fact-serendipity (fact)
  (ndbg-roman-nl *gate-dbg* seren-long "Running fact serendipity for ~A" fact)
  (run-serendipity (bottom-rules fact)
                   (ob$fcreate `(SUCCEEDED-GOAL obj ,fact))))

;
; Run serendipities for all recent episodes. (However, are not
; serendipities checked upon episode retrieval? Why would we need
; this batch-mode function?)
;
(defun run-serendipities ()
 (ndbg-roman-nl *gate-dbg* seren-long "Run serendipities")
 (yloop
  (initial (result nil))
  (yfor episode in *recent-episodes*)
  (ydo
   (setq result
        (t-list-append!
         result
         (run-serendipity (inaccessible-planning-rules episode) nil))))
  (yresult result)))

; Todo: add invocation of this function in activate-top-level-goal.
;
; Note that this works for personal goals but not for daydreaming goals
; since a subgoal has not yet been activated.
(defun upon-activate-top-level-goal (top-level-goal)
  (yloop
   (yfor episode in *recent-episodes*)
   (ydo 
    (run-top-level-goal-serendipity top-level-goal 
                                    (inaccessible-planning-rules episode)
                                    nil))))

;
; Run serendipities for a set of bottom rules and a given bottom goal.
; (If bottom-goal is non-NIL, then bottom-rules must correspond
;  to this goal.)
;
; The below is also called when a new episodic planning rule is induced.
;
; Note: bottom-rules is actually list of (<bottom-rule> . <subgoalnum>),
; where <subgoalnum> may be nil.
;
(defun run-serendipity (bottom-rules bottom-goal)
  (if bottom-rules
      (progn
 (ndbg-roman-nl *gate-dbg* seren-long "Run serendipity for ~A and ~A"
                bottom-rules bottom-goal)
 (yloop
  (initial (result nil))
  (yfor top-level-goal in *top-level-goals*)
  (ydo
   (setq result
        (t-list-append!
         result
         (run-top-level-goal-serendipity top-level-goal bottom-rules
                                         bottom-goal))))
  (yresult result)))
      nil))

;
; Run serendipities for a set of bottom rules, bottom goal, and a given
; top-level goal.
;
; Note: bottom-rules is actually list of (<bottom-rule> . <subgoalnum>),
; where <subgoalnum> may be nil.
;
(defun run-top-level-goal-serendipity (top-level-goal bottom-rules bottom-goal)
 (ndbg-roman-nl *gate-dbg* seren-long "Run serendipity for top-level goal ~A"
                top-level-goal)
  (let ((subgoal nil))
 (cond
  ((and (dd-goal? top-level-goal)
        (setq subgoal (dd-goal-subgoal top-level-goal)))
   (serendipity-recognize-apply top-level-goal subgoal
                                bottom-rules bottom-goal))
  ((and (personal-goal? top-level-goal)
        (vars-in? (ob$get top-level-goal 'obj))
        (eq? 'halted (ob$get top-level-goal 'status))
        ; The above is needed in particular to prevent running serendipities
        ; for "He gives me a newspaper" on tlg EMPLOYMENT.
        (null? (ob$gets top-level-goal 'analogical-episode)))
   (serendipity-recognize-apply top-level-goal nil
                                bottom-rules bottom-goal))
  (else nil))))

;
; Recognize and apply serendipities
;
; Returns NIL or (T <ep> <ep> ...) where <ep> is an episode retrieved
; in the process.
;
; If subgoal is non-NIL, then top-level-goal must be a daydreaming goal.
;
; If bottom-goal is non-NIL, then it is assumed that bottom-rules all apply
; to this goal. Otherwise, bottom-rules are unrelated to any particular
; bottom goal.
;
; Note: bottom-rules is actually list of (<bottom-rule> . <subgoalnum>),
; where <subgoalnum> may be nil.
;
(defun serendipity-recognize-apply (top-level-goal subgoal
                                     bottom-rules bottom-goal)
 (ndbg-roman-nl *gate-dbg* seren-long "Recognize and apply serendipity")
 (yloop 
  (initial (episode-spec nil) (result nil) (ri-paths nil))
  (yfor top-rule in (top-rules (ob$get (or subgoal top-level-goal) 'obj)))
  (ydo 
   ; We do a bit of batching here (so that we can use condense-paths).
   (setq ri-paths nil)
   (yloop
    (yfor bottom-rule in bottom-rules)
    (ydo
     (setq ri-paths 
          (append! ri-paths
                   (rule-intersection top-rule bottom-rule *recent-episodes*))))
    (yresult
     (if (and (setq ri-paths (condense-paths ri-paths))
              (setq episode-spec (rip-paths->episode ri-paths
                                                    (or subgoal top-level-goal)
                                                    bottom-goal)))
         (progn
          (if t ; (null? result)
              (setq result (cons 't (car episode-spec)))
              (setq result (cons 't (append! (cdr result) (car episode-spec)))))
          (if subgoal
              (new-analogical-dd-goal-plan top-level-goal subgoal
                                           (cdr episode-spec))
              (new-analogical-pers-goal-plan top-level-goal
                                             (cdr episode-spec))))))))
  (yresult result)))

(defun condense-paths (ri-paths)
  (if *action-mutations?*
      ri-paths
      (remove-sublist-duplicates ri-paths (lambda (x) (ri-pathelt-rule x)))))

(defun remove-sublist-duplicates (lsts elem-accessor)
  (cond
   ((null? lsts) lsts)
   ((null? (cdr lsts)) lsts)
   (else
    (let ((rest (remove-sublist-duplicates (cdr lsts) elem-accessor)))
         (if (any? (lambda (x) (sublist? (car lsts) x elem-accessor))
                   rest)
             rest
             (yloop (yfor relem in rest)
                   (ydo (if (sublist? relem (car lsts) elem-accessor)
                           (setq rest (delq! relem rest))))
                   (yresult (cons (car lsts) rest))))))))

(defun sublist? (lst1 lst2 elem-accessor)
  (yloop
   (initial (result t) (lst2x lst2))
   (yfor elem1 in lst1)
   (ywhile result)
    (ydo
     (if (neq? (apply elem-accessor (list elem1))
               (apply elem-accessor (list (car lst2x))))
         (setq result nil))
     (setq lst2x (cdr lst2x)))
    (yresult (and result (<= (length lst1) (length lst2))))))

;
; Create a new plan (by analogy) for a personal goal
; (Clobbers existing plans!)
;
(defun new-analogical-pers-goal-plan (top-level-pers-goal analogical-episode)
  (ndbg-roman-nl *gate-dbg* rule "Serendipity for ~A personal goal"
                 (tlg->string top-level-pers-goal))
  (ep-print analogical-episode)
  (reality-stabilize)
  ; Create new context
  (setq *reality* (cx$sprout *reality*))
  (setq *reality-lookahead* *reality*)
  ; Clobber existing planning for this top-level goal.
  (gc-plans1 *reality* (list top-level-pers-goal))
  ; Set up a new analogical plan for the top-level goal.
  (ob$set top-level-pers-goal 'analogical-episode analogical-episode)
  ; Give the top-level goal some more emotional charge.
  (surprise top-level-pers-goal))

;
; Create a new plan (by analogy) for a daydreaming goal
; (Leaves existing plans around as other possibilities.)
;
(defun new-analogical-dd-goal-plan (top-level-dd-goal subgoal
                                     analogical-episode)
  (ndbg-roman-nl *gate-dbg* rule "Serendipity for ~A daydreaming goal"
                 (tlg->string top-level-dd-goal))
  (ep-print analogical-episode)
  (let ((new-context (cx$sprout (ob$get subgoal 'activation-context))))
       (if *action-mutations?*
           (cx$assert new-context (ob$fcreate '(MUTATION))))
       ; Copy the subgoal in the new context.
       (setq subgoal
            (replace-linked-ob subgoal new-context *me-belief-path* *empty-bd*))
       ; Set up a new analogical plan for the subgoal.
       (ob$set subgoal 'analogical-episode analogical-episode)
       ; Set the next context of the top-level goal to this new context.
       (set-next-context top-level-dd-goal new-context)
       ; Give the top-level goal some more emotional charge.
       (reality-stabilize)
       (surprise top-level-dd-goal)))

(defun reality-stabilize ()
  (if (neq? *reality* *reality-lookahead*)
      (setq *reality* *reality-lookahead*)))

;
; Find the (unique) first-level subgoal of a daydreaming goal.
;
(defun dd-goal-subgoal (dd-goal)
  (let ((subgoals (goal-subgoals dd-goal
                                 (car (cx$children (ob$get dd-goal
                                                     'activation-context)))
                                 *me-belief-path*)))
       (if subgoals
           (if (cdr subgoals)
               (progn
                (ndbg-roman-nl *gate-dbg* rule-long
                               "Non-unique subgoal ~A" dd-goal)
                nil)
               (if (not (ty$instance? (ob$get (car subgoals) 'obj)
                                      'rtrue))
                   (car subgoals)
                   nil)
               ); end if
           (progn
            (ndbg-roman-nl *gate-dbg* rule-long "No subgoal found ~A" dd-goal)
            nil))))

;
; The below will later be extended to report on inaccessible inference
; rules (once path to episode is also so extended).
;
; Wouldn't we also like to use the episode in constructing the trace?
; I don't think we do currently. Have to carry along bottom-subgoals?
;
(defun inaccessible-planning-rules (episode)
  (yloop (initial (result nil)
                 (rule nil))
        (yfor ep in (ob$get episode 'descendants))
        (ydo 
         (setq rule (ob$get ep 'rule))
         (if (and (inaccessible? rule)
                  (not (memq? rule result)))
             (setq result (cons (cons rule nil) result))))
        (yresult result)))

;  (yloop (initial (result nil)
;                 (rule nil))
;        (yfor intends in (cx$get-all-ty (ob$get episode 'context) *intends-ob*))
;        (ydo 
;         (setq rule (ob$get intends 'rule))
;         (if (and (inaccessible? rule)
;                  (not (memq? rule result)))
;             (setq result (cons rule result))))
;        (yresult result)))

;
; Generate surprise emotion associated with top-level goal and unhalt the
; task if it was halted.
;
(setq *surprise-strength* 0.25)

(defun surprise (top-level-goal)
  (let ((main-motiv (ob$get top-level-goal 'main-motiv))
        (emot nil))
       (setq emot (ob$fcreate
                  (if (ty$instance? main-motiv 'pos-emotion)
                      `(POS-SURPRISE strength ,*surprise-strength*)
                      `(NEG-SURPRISE strength ,*surprise-strength*))))
       (ndbg-roman-nl *gate-dbg* rule "Generate surprise emotion")
       (cx$assert *reality* emot)
       (divert-emot-to-tlg emot *reality* top-level-goal)
       (if (neq? (ob$get top-level-goal 'status) 'runable)
           (change-tlg-status top-level-goal 'runable))))

(defun top-rules (fact)
  (let ((rules (backward-chain-rules fact)))
       (yloop
        (initial (result nil))
        (yfor rule in rules)
        (ydo (if (and (plan? rule)
                     (not (constructed-plan? rule))
                     (ob$unify (ob$get rule 'goal) fact *empty-me-bd*))
                (setq result (cons rule result))))
        (yresult result))))

; This will not include subgoals which are just variables, as this
; is plainly absurde. Or so it would seem...
(defun bottom-rules (fact)
  (let ((rules (forward-chain-rules fact)))
       (yloop
        (initial (result nil))
        (yfor rule in rules)
        (ydo
         (if (and (plan? rule)
                  (not (constructed-plan? rule)))
             (yloop
              (initial (subgoalnum 0))
              (yfor subgoal in (rule-subgoal-objs rule))
              (yuntil (prog1 (if (and (not (var? subgoal))
                                      (ob$unify subgoal fact *empty-me-bd*))
                                 (setq result (cons (cons rule subgoalnum)
                                                   result))
                                 nil)
                             (setq subgoalnum (+ 1 subgoalnum)))))))
        (yresult result))))

; Path lengths will be at most twice the below depth.
; 5 3 3 works for lovers1 without lovers
(setq *max-ri-depth* 5) ; Try 4
(setq *max-paths-found* 3) ; may need to be 6 for some applications
(setq *max-rip-paths* 3) ; Try 5 5
(setq *paths-found* 0)
; The above two aren't the same because after ri is performed,
; the result is sorted (currently by path length).

;
; Rule intersection search.
;
; Returns a list of ri-paths, where an ri-path is a list of ri-pathelts.
;
; Note: bottom-rule is actually (<bottom-rule> . <subgoalnum>),
; where <subgoalnum> may be nil.
;
(defun rule-intersection (top-rule bottom-rule episodes)
  (ndbg-roman-nl *gate-dbg* seren-long "Rule intersection from ~A to ~A"
                 top-rule bottom-rule)
  (setq *paths-found* 0)
  (let ((result (rule-intersection1 top-rule (list (list top-rule nil))
                                    (car bottom-rule)
                                    (list (list (car bottom-rule)
                                                (cdr bottom-rule)))
                                    episodes 1)))
    (ndbg-roman-nl *gate-dbg* seren-long "Rule intersection returns ~A"
                   result)
       result))

; Todo: This algorithm could be made very efficient by compiling
; routings in advance! Perhaps, but I think dealing with cycles
; may be difficult.
(defun rule-intersection1 (top-rule backward-path bottom-rule
                            forward-path episodes depth)
 (ndbg-roman-nl *gate-dbg* ri "Ri1 ~A ~A ~A ~A ~A ~A"
                top-rule backward-path bottom-rule forward-path
                episodes depth)
 (if (> depth *max-ri-depth*)
     (progn
      (ndbg-roman-nl *gate-dbg* ri "Ri1 depth exceeded")
      nil)
  (let ((result nil) (backward-car (car backward-path))
        (forward-car (car forward-path)))
   ; I feel the following two cases are disjoint, but am not positive.
   (if (eq? top-rule bottom-rule)
       (progn
        (setq result (list (append (reverse (cdr backward-path)) forward-path)))
        (setq *paths-found* (+ *paths-found* 1))))
   (if (and (eq? (ri-pathelt-rule backward-car)
                 (ri-pathelt-rule (cadr forward-path)))
            (eq? (ri-pathelt-rule forward-car)
                 (ri-pathelt-rule (cadr backward-path)))
            (eq? (ri-pathelt-subgoalnum forward-car)
                 (ri-pathelt-subgoalnum (cadr backward-path))))
       (progn
        (setq result (cons (append (reverse (cdr backward-path))
                                  (cdr forward-path)) result))
        (setq *paths-found* (+ *paths-found* 1))))
   (if (< *paths-found* *max-paths-found*)
   (yloop
    (initial (f-ep nil) (b-ep nil) (new-backward-path nil)
             (new-forward-path nil))
    (yfor f in (ob$gets bottom-rule 'forward-chain-nums))
    (ydo
     (yloop
      (yfor b in (ob$gets top-rule 'backward-chain-nums))
       (ydo
        (if
         (and (plan? (chain-rule f)) (plan? (chain-rule b))
              (setq f-ep (ri-useable-rule? (chain-rule f) episodes))
              (setq b-ep (ri-useable-rule? (chain-rule b) episodes))
              (not (in-path-twice? (chain-rule f) backward-path))
              (not (in-path-twice? (chain-rule f) forward-path))
              (not (and (in-path? (chain-rule f) backward-path)
                        (in-path? (chain-rule f) forward-path)))
              (not (in-path-twice? (chain-rule b) backward-path))
              (not (in-path-twice? (chain-rule b) forward-path))
              (not (and (in-path? (chain-rule b) backward-path)
                        (in-path? (chain-rule b) forward-path))))
         (progn
          (setq new-backward-path
               (cons (ri-pathelt-make (chain-rule b) nil (if (ob? b-ep)
                                                             (list b-ep)
                                                             nil))
                     (cons (ri-pathelt-make (ri-pathelt-rule backward-car)
                                            (chain-num b)
                                            (ri-pathelt-episodes backward-car))
                           (cdr backward-path))))
          (setq new-forward-path
               (cons (ri-pathelt-make (chain-rule f) (chain-num f)
                                      (if (ob? f-ep) (list f-ep) nil))
                     forward-path))
          (setq result
           (append! result (rule-intersection1 (chain-rule b) new-backward-path
                                               (chain-rule f) new-forward-path
                                               episodes (+ 1 depth)))))))))
     (yresult
      (progn
        (ndbg-roman-nl *gate-dbg* ri "Ri1 returns ~A" result)
       result)))
       (progn
        (ndbg-roman-nl *gate-dbg* ri "Ri1 returns ~A (maxed out)" result)
       result)))))

(defun in-path-twice? (rule path)
  (let ((once-in? (in-path? rule path)))
       (if once-in?
           (in-path? rule (cdr once-in?))
           nil)))

(defun in-path? (rule path)
  (mem (lambda (o e) (eq? o (ri-pathelt-rule e)))
       rule
       path))

;
; Assumes rule is a planning rule.
; Returns NIL, T, or episode.
;
; Should this function return episodes for plain planning rules?
; Should we then incorporate metrics here?
;
(defun ri-useable-rule? (rule episodes)
  (cond
   ((not (constructed-plan? rule)) t)
   ((rule-in-any-episode? rule episodes))
   (else nil)))

;
; Returns NIL or episode.
;
; Todo: This should be optimized. For example, you can keep a list
; of rules associated with each episode.
;
(defun rule-in-any-episode? (rule episodes)
  (any (lambda (episode) (rule-in-episode? rule episode))
       episodes))

; This old code does not take into account enclosing episodes
;  (let ((eps (ob$gets rule 'indexes)) ; was (episode-retrieve rule)
;        (result nil))
;       (any? (lambda (ep) (if (memq? ep episodes) (setq result ep) nil))
;             eps)
;       result))

; Returns not episode, necessarily, but the actual sub-episode in
; which this rule is used.
(defun rule-in-episode? (rule episode)
  (any (lambda (ep) (if (eq? rule (ob$get ep 'rule))
                        ep
                        nil))
       (ob$get episode 'descendants)))

;  (any (lambda (intends) (if (eq? rule (ob$get intends 'rule))
;                             (ob$get (ob$get intends 'linked-from) 'episode)
;                             nil))
;       (cx$get-all-ty (ob$get episode 'context) *intends-ob*)))

;
; Verification of rule intersection search.
;
; From ri-paths, create first episode(s) suitable for use in analogical
; planning. Returns NIL (if unsuccessful) or an episode-spec
; (<accessed-eps> . <ep>)
;
; Perhaps we could add some heuristics to select certain successful
; episodes over others.
;
; This function favors SHORTER paths. Should this be?
; Note that before this is called, substrings are pruned.
;
; Todo: Destroy and otherwise GC contexts etc. that are thrown
; away when an rip-path verification fails.
(defun rip-paths->episode (ri-paths top-level-goal bottom-goal)
 (ndbg-roman-nl *gate-dbg* seren-long "Rule intersection paths to episode")
 (no-gen
   (yloop (initial (result nil)
                  (i 0))
         (yfor ri-path in (sort-ri-paths ri-paths))
         (ywhile (< i *max-rip-paths*))
         (yuntil (and (> (length ri-path) 1)
                     (or (null? *action-mutations?*)
                         (< (length ri-path) 5))
                     (setq i (+ 1 i))
                     (setq result (ri-path->episode ri-path top-level-goal
                                                   bottom-goal))))
         (yresult
          (progn
           (ndbg-roman-nl *gate-dbg* seren-long "Result ~A, i = ~A" result i)
           result)))
)) ; end no-gen and define

; For action mutations, favor shorter paths; otherwise favor longer paths.
; (ad hoc?)
(defun sort-ri-paths (ri-paths)
  (if *action-mutations?*
      (sort ri-paths
            (lambda (path1 path2) (< (length path1) (length path2))))
      (sort ri-paths
            (lambda (path1 path2) (> (length path1) (length path2))))))

;
; From an ri-path, create episode(s) suitable for use in analogical planning.
; Returns NIL (if unsuccessful) or an episode-spec (<accessed-eps> . <ep>)
;
; An assumption is that the top-rule unifies with the top-level-goal.
;
(defun ri-path->episode (ri-path top-level-goal bottom-goal)
  (let ((result (if nil ; (interrogate "Debugs? ")
                    (ri-path->episode0 ri-path top-level-goal bottom-goal)
                    (with-no-dbg (ri-path->episode0 ri-path top-level-goal
                                                    bottom-goal)))))
       (if (null? result)
           (ndbg-roman-nl *gate-dbg* rule
                          "Rule intersection for ~A of length ~A not verified"
                          (tlg->string top-level-goal) (length ri-path))
           (ndbg-roman-nl *gate-dbg* rule
                          "Rule intersection for ~A of length ~A verified"
                          (tlg->string top-level-goal) (length ri-path)))
       result))

(defun ri-path->episode0 (ri-path top-level-goal bottom-goal)
  (let* ((context (cx$create))
         (top-rule (ri-pathelt-rule (car ri-path)))
         (bd (ob$unify (ob$get top-rule 'goal) (ob$get top-level-goal 'obj)
                       *empty-me-bd*)))
       (setq top-level-goal (ob$fcreate `(ACTIVE-GOAL
                                          obj ,(ob$get top-level-goal 'obj)
                                          activation-context ,context)))
        (ob$set top-level-goal 'top-level-goal top-level-goal)
        (cx$assert context top-level-goal)
        (ri-path->episode1 ri-path top-level-goal context bd nil
                           top-level-goal bottom-goal)))

;
; This function must work for inferences as well as plans? When?
;
(defun ri-path->episode1 (ri-path goal context bd ep-goal top-level-goal
                          bottom-goal)
 (ndbg-roman-nl *gate-dbg* seren-long "ri-path->episode1 ~A ~A ~A ~A ~A ~A ~A"
                ri-path goal context bd ep-goal top-level-goal
                bottom-goal)
 (let* ((pathelt (car ri-path)) (rule (ri-pathelt-rule pathelt))
        (subgoal-objs (rule-subgoal-objs rule))
        (subgoalnum (ri-pathelt-subgoalnum pathelt))
        (episode (car (ri-pathelt-episodes pathelt)))
        (ep-subgoals nil)
        (subgoals nil)
        (result-ep nil)
        (temp nil)
        (nth-subgoal nil))
  (if episode (setq ep-goal (ob$get episode 'goal)))
  (if ep-goal
   (progn
    (ndbg-roman-nl *gate-dbg* seren-long "ep-goal")
    ; Since a SUCCEEDED-GOAL is reasserted upon success,
    ; taking the top-context of that goal will get us a good
    ; context for finding subgoals. I hope. Otherwise,
    ; we are going to have to pass in the last episode.
    (setq ep-subgoals (goal-subgoals ep-goal (ob$get ep-goal 'top-context)
                                    *me-belief-path*))
    (if (and (null? (cdr ri-path)) bottom-goal)
        (progn
         (if (null? subgoalnum)
             (progn
              (error "subgoalnum is nil")
              (setq subgoalnum 0)))
         (setf (nth-elem ep-subgoals subgoalnum) bottom-goal)
;         (ndbg-roman-nl *gate-dbg* seren-long "ep-sgls = ~A, bd before = ~A"
;                        ep-subgoals bd)
;         (interest 'unify 'all)
         (setq bd (episodic-unify (ob$get rule 'goal) subgoal-objs
                                 (ob$get ep-goal 'obj) ep-subgoals goal
                                 context bd *me-belief-path* nil
                                 top-level-goal nil))
;         (disinterest 'unify 'all)
;        (ndbg-roman-nl *gate-dbg* seren-long "bd after = ~A" bd)
         ) ; end block
        (setq bd (episodic-unify (ob$get rule 'goal) subgoal-objs
                                (ob$get ep-goal 'obj) ep-subgoals goal
                                context bd *me-belief-path* t
                                top-level-goal nil)))
    ) ; end first clause of if
      (progn
       (ndbg-roman-nl *gate-dbg* seren-long "no ep-goal")
;      (ndbg-roman-nl *gate-dbg* seren-long "pre bd ~A ob1 ~A ob2 ~A"
;                   bd (ob$get rule 'goal) (ob$get goal 'obj))
; The below was redundant. Moved down.
; The below line 'verifies' this goal.
;       (setq bd (ob$unify (ob$get rule 'goal) (ob$get goal 'obj) bd))
; The below modification enables variable instantiation
; from the goal unification. Iynwim.
; Similar to a swatch of code in dd_rule (run-generic-plan).
;       (if (vars-in? (ob$get goal 'obj))
;           (setq goal (or (plan-instantiate goal (bd-no-var-bds bd)
;                                           context top-level-goal
;                                           *me-belief-path* nil)
;                         goal)))
; End above
       (ndbg-roman-nl *gate-dbg* seren-long "post bd ~A" bd)
       (if (and bd
                (null? (cdr ri-path))
                bottom-goal)
           ; The below is a mini episodic-unify.
           (let ((source-bd (ob$unify (nth-elem subgoal-objs subgoalnum)
                                      (ob$get bottom-goal 'obj)
                                      bd)))
                ; ran into problems with type compatibility when using
                ; *empty-me-bd* instead of bd above.
             (ndbg-roman-nl *gate-dbg* seren-long "source bd = ~A" source-bd)
             (if source-bd
                 (setq bd (bd-special-append bd source-bd goal
                                            context *me-belief-path*
                                            top-level-goal nil ;was t
                                            (lambda (x) t)))
;                 (progn
;                  (setq bd source-bd)
;                  (setq goal (or (plan-instantiate goal bd
;                                                  context top-level-goal
;                                                  *me-belief-path* nil)
;                                goal)))
                 (setq bd nil)
                 ) ; end if
                )) ; end let, end if
       ) ; end block
      ) ; end if
    (if (and bd (ob? (car bd)))
        (progn
         (ndbg-roman-nl *gate-dbg* seren-long "Resetting goal from ~A to ~A"
                        goal (car bd))
         (setq goal (car bd))))
 (ndbg-roman-nl *gate-dbg* seren-long "bd = ~A" bd)
  (if (null? bd)
      nil
      (progn
       (setq subgoals
            (instan-and-activate-subgoals goal subgoal-objs bd rule context
                                          (ty$instance? (ob$get rule 'subgoal)
                                                        'rseq) nil nil
                                          top-level-goal *me-belief-path*))
       (setq result-ep (make-episode rule goal context nil nil))
       (ob$set result-ep 'seren-ep t)
       (ndbg-roman-nl *gate-dbg* seren-long "Ep ~A for goal ~A" result-ep goal)
       (if (cdr ri-path)
           (progn
            (setq nth-subgoal (nth-elem subgoals subgoalnum))
            (ndbg-roman-nl *gate-dbg* seren-long "Unify ~A and ~A"
                           (ob$get (ri-pathelt-rule (cadr ri-path)) 'goal)
                           (ob$get nth-subgoal 'obj))
            (setq bd (ob$unify (ob$get (ri-pathelt-rule (cadr ri-path)) 'goal)
                               (ob$get nth-subgoal 'obj)
                               bd)) ; was *empty-me-bd* <------------- !
            (ndbg-roman-nl *gate-dbg* seren-long "bd now = ~A" bd)
            (if (null? bd)
                nil
                (progn
                 ; Below is new. (dle)
                 (if (vars-in? (ob$get nth-subgoal 'obj))
                     (setq nth-subgoal (or (plan-instantiate nth-subgoal
                                                         (bd-no-var-bds bd)
                                                         context top-level-goal
                                                         *me-belief-path* nil)
                                       nth-subgoal)))
                 ; Todo: reality and desirability are ignored here
                 (if (setq temp (ri-path->episode1 (cdr ri-path) nth-subgoal
                                                 context bd
                                                 (if ep-subgoals
                                                   (nth-elem ep-subgoals subgoalnum)
                                                   nil)
                                                 top-level-goal bottom-goal))
                     (if episode
                         (cons (cons episode (car temp)) result-ep)
                         (cons (car temp) result-ep))
                     nil))))
           (cons nil result-ep))))))

;
; Rule induction
;

(defun rule-induction (concepts context)
  (if (cdr concepts)
      (let ((pers-attr
             (any (lambda (con) (if (ty$instance? con 'personal-attribute)
                                    con
                                    nil))
                  concepts))
            (state
             (any (lambda (con)
                          (if (and (ty$instance? con 'state)
                                   (not (ty$instance? con 'personal-attribute)))
                              con
                              nil))
                  concepts)))
           (if (and pers-attr state)
               (induce-rule state (list pers-attr) context)
               nil))
      nil))

(setq *next-irule-number* 1)

(defun *induced-rule-name-genproc* ()
  (string->symbol
   (string-append "INDUCED-RULE."
                  (prog1 (fixnum->string *next-irule-number*)
                          (increment-me *next-irule-number*)))))

; No real need to assert an inference (or plan) in the context, since
; serendipity will use the rule directly in this case. (?)
;
; Note: an inaccessible rule is created here? Should it be generic?
; The fact of the matter is, if it isn't used in the serendipity, it
; will never be used in the future.
(defun induce-rule (goal subgoals context)
  (let ((rule (plan->rule goal subgoals 1.0 *induced-rule-name-genproc*)))
       (ndbg-roman-nl *gate-dbg* rule "Induced rule ~A" (ob->string rule))
       (run-serendipity (list (cons rule nil)) nil)
       rule))

; End of file.
