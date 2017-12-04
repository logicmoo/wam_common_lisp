;*******************************************************************************
;
; Daydreamer
; Version 3.5
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
; 11/19/86: Broke up into two files
;
;*******************************************************************************

; Todo: Is it redundant also to run fact plans in run-plan? (no 's') Will they
; all already have been tried before?
(defun run-plans (top-level-goal context belief-path)
 (ndbg-roman-nl *gate-dbg* rule "Running plans in ~A for ~A bp ~A"
       context top-level-goal belief-path)
 ; (New) p-goals override any other goals. If there is a p-goal, then
 ; that's all we do this time. Plus we only do one p-goal at
 ; a time (thus the until; in the case of regular planning,
 ; seq next will ensure that only one subgoal is planned for at
 ; a time; if there is no seq next, then the user desired for
 ; more than one to be planned at a time...)
 (let ((rule-fired? nil)
       (elems (cx$get-all context)))
  (yloop
   (yfor elem in elems)
   (yuntil rule-fired?)
   (ydo
    (setq elem (absolute->relative elem belief-path))
    (if (and elem
             (ty$instance? elem 'active-goal)
             (not (ty$instance? elem 'p-goal))
             (eq? (ob$get elem 'top-level-goal) top-level-goal)
             (not (has-link-relative? elem 'linked-from-of *intends-ob*
                                      context belief-path))
             (preservation-goal-subgoal? elem)
             ; Added the below
             (seq-head? elem context belief-path)
             ) ; end and
        (setq rule-fired? (or (run-plan elem top-level-goal context
                                       belief-path) rule-fired?)))))
  (if (not rule-fired?)
   (yloop 
    (yfor elem in elems)
    (ydo (setq elem (absolute->relative elem belief-path))
        (cond
         ((and elem
               (ty$instance? elem 'active-goal)
               (not (ty$instance? elem 'p-goal))
               (eq? (ob$get elem 'top-level-goal) top-level-goal)
               (not (has-link-relative? elem 'linked-from-of *intends-ob*
                                        context belief-path))
               (not (preservation-goal-subgoal? elem))
               (seq-head? elem context belief-path)
               ) ; end and
          (setq rule-fired? (or (run-plan elem top-level-goal context
                                         belief-path) rule-fired?)))
         ; This case would allow fact plans to fire in non-seq order.
         ; This does not work for EXPERIENCE1 in the beginning when
         ; it needs to keep ?Person2 as a variable so that serendipity
         ; can occur later.
         ; Fact plans are also done by the above, so the below isn't
         ; strictly necessary.
         ((and nil
               elem
               (ty$instance? elem 'active-goal)
               (not (ty$instance? elem 'p-goal))
               (eq? (ob$get elem 'top-level-goal)
                    top-level-goal))
          ;           (not (ob$get elem 'seq-next-of)) or like above
          (setq rule-fired? (or (run-fact-plan elem top-level-goal
                                              context belief-path)
                               rule-fired?)))))
    (yresult rule-fired?))
  rule-fired?)))

(defun preservation-goal-subgoal? (goal)
  (or (ty$instance? (ob$get goal 'obj) 'preservation)
      (ob$get goal 'preservation-subgoal?)))
 
; (Active-Goal Me ?x)
; (Believe Debra (Active-Goal Debra ?x)
; (Believe Debra (Believe Me (Active-Goal Me ?x)))

; Below to be determined empirically
(setq *perf-sim-thresh* 0.0)
(setq *perf-reality-thresh* 0.6)
(setq *perf-desir-thresh* 0.0) ; who knows?
(setq *goal-relaxation-realism* 0.2)

(setq *fanciful-sim-thresh* 0.0)
(setq *realistic-sim-thresh* 0.0)

;
; Order (and prune) episodes within candidates:
; - Real top-level-goal: Order according to reality*similarity*desirability
;                        with threshold cutoffs for each
;   Note: We want to keep around some alternatives for reversal; that's
;         why why don't just prune all but the highest.
; - Imaginary-realistic top-level-goal: Order according to
;       reality*similarity*desirability with threshold only for similarity
; - Imaginary-fanciful top-level goal: Order according to similarity with
;       threshold.
;
(defun order-candidates (goal-obj candidates top-level-goal)
  (ndbg-roman-nl *gate-dbg* rule "Order candidates ~A" candidates)
  (yloop (initial (sim nil)
                 (ordering nil)
                 (any-episode? nil))
        (yfor candidate in candidates)
        (ydo (yloop (yfor episode in (candidate-episodes candidate))
                  (ydo (setq sim (ob$similarity goal-obj
                                               (ob$pget episode '(goal obj))))
                      (cond
                       ((real? top-level-goal)
                        (setq ordering (fl* (thresh (ob$get episode 'realism)
                                                   *perf-reality-thresh*)
                                           (fl* (thresh sim *perf-sim-thresh*)
                                                (thresh (ob$get episode
                                                                 'desirability)
                                                    *perf-desir-thresh*)))))
                       ((imaginary-realistic? top-level-goal)
                        (setq ordering (fl* (ob$get episode 'realism)
                                           (fl* (thresh sim
                                                 *realistic-sim-thresh*)
                                           (ob$get episode 'desirability)))))
                       ((imaginary-fanciful? top-level-goal)
                        (setq ordering (thresh sim *fanciful-sim-thresh*))))
                      (ob$set episode 'ordering ordering)
                      ; 'ordering and 'bd are used purely as temporary
                      ; variables which need dure only until all plans
                      ; are sprouted, which will not be interrupted
                      ; by other top-level goals.
                      (if (fl> ordering 0.0)
                          (setq any-episode? t)))))
    (yresult any-episode?)))

(defun candidates->episodes (candidates)
  (ndbg-roman-nl *gate-dbg* rule "Candidates to episodes ~A" candidates)
  (yloop (initial (result nil))
        (yfor candidate in candidates)
        (ydo (yloop (yfor episode in (candidate-episodes candidate))
                  (ydo (if (fl> (ob$get episode 'ordering) 0.0)
                          (progn
                           (ob$set episode 'bd
                                             (candidate-bd candidate))
                           (setq result (cons episode result)))))))
        (yresult
         (ndbg-roman-nl *gate-dbg* rule "Result = ~A" result)
         result)))

(defun find-candidate-rules (goal-obj believe-other? belief-path context)
  (ndbg-roman-nl *gate-dbg* rule "Find candidate rules for obj ~A in ~A"
                        goal-obj context)
  (yloop
   (initial (candidates nil))
   (yfor rule in (collect-planning-rules goal-obj))
   (ydo
    (if (plan? rule)
        (yloop (yfor bd in (rule-applications goal-obj context rule
                                            belief-path believe-other?))
              (ydo (setq candidates
                       (cons (candidate-create rule bd (episode-retrieve rule))
                             candidates))))))
   (yresult candidates)))

(defun collect-planning-rules (goal-obj)
   (ndbg-roman-nl *gate-dbg* rule-long "Collecting planning rules for ~A"
                  goal-obj)
   (let ((rules (backward-chain-rules goal-obj)))
        (if (eq? rules *rules*)
            (ndbg-roman-nl *gate-dbg* rule-long "Using full rule set")
            (ndbg-roman-nl *gate-dbg* rule-long "Collected rules = ~A" rules))
        rules))


; End of file.
