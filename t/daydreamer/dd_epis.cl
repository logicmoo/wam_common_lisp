;*******************************************************************************
;
; Daydreamer
; Version 3.5
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
;  7/22/86: Added object episodic memory
;   8/5/86: Wrote new storage/retrieval functions
;  9/25/86: Took out flavors
; 10/12/86: Added episode descendants
;
;*******************************************************************************

;
; Episode creation
;

(ty$create 'EPISODE nil '(nil (rule goal context realism desirability
                                    ordering) (ordering)))

(setq *infinite-thresh* 100)

(defun make-and-store-episode (rule goal context realism desirability
                                hidden? children)
  (ndbg-roman-nl *gate-dbg* ep-store "Make episode for goal ~A" goal)
  (let ((ep (make-episode rule goal context realism desirability)))
       (if children
           (progn
            (ob$set ep 'children children)
            (ob$set ep 'descendants  ; includes self
                    (yloop
                     (initial (result (list ep)))
                     (yfor child in children)
                     (ydo
                      (ob$set child 'parent ep)
                      (setq result (append! result
                                            (copy-list
                                             (ob$get child 'descendants)))))
                     (yresult result))))
           (ob$set ep 'descendants (list ep)))
       (if hidden?
           (progn
            (ob$set ep 'plan-threshold *infinite-thresh*)
            (ob$set ep 'reminding-threshold *infinite-thresh*)
            (epmem-store ep rule nil nil))
           (progn
            (ob$set rule 'accessible? t)
            (epmem-store ep rule t t)))
       ep))

(defun accessible? (rule)
  (ob$get rule 'accessible?))

(defun inaccessible? (rule)
  (null? (ob$get rule 'accessible?)))

(setq *next-ep-number* 1)

; The below is for debugging purposes only.
(setq *episodes* nil)

; We want two level eps to be stored. E.g., Harrison goes to Cairo.
(defun make-episode (rule goal context realism desirability)
  (let ((ep (ob$fcreate `(EPISODE rule ,rule goal ,goal
                                  context ,context
                                  realism ,(or realism
                                               (strength (ob$get goal 'obj)))
                                  desirability ,(or desirability 1.0)))))
                                  ; Todo: I guess 1.0 is default?
    (ob$set goal 'episode ep)
    (ob$add-unique-name ep (string->symbol
     (string-append "EPISODE." (fixnum->string *next-ep-number*))))
    (increment-me *next-ep-number*)
    (setq *episodes* (cons ep *episodes*))
    ep))

;
; Episode storage and retrieval
;

(setq *episodic-memory* nil)

(defun epmem-init ()
  (setq *episodic-memory* (cx$create))
  (ob$add-name *episodic-memory* 'episodic-memory)
  *episodic-memory*)

(defun epmem-initialize ()
  (setq *recent-episodes* nil)
  (setq *recent-indices* nil))

(defun epmem-store (episode index needed-for-plan? needed-for-reminding?)
  (ndbg-roman-nl *gate-dbg* ep-store "Storing ~A under ~A" episode index)
  (setq index (index-intern index 'new-ok))
  (ob$add index 'indexes episode)
  (ob$add episode 'indexed-under index)
  (if needed-for-plan?
      (ob$set episode 'plan-threshold
              (+ 1 (or (ob$get episode 'plan-threshold) 0))))
  (if needed-for-reminding?
      (ob$set episode 'reminding-threshold
              (+ 1 (or (ob$get episode 'reminding-threshold) 0)))))

(setq *epmem-marks* nil)

(defun mark-init ()
 (mark-unmark-all))
; currently as a safety precaution
;(setq *epmem-marks* nil)

(defun mark-unmark-all ()
  (yloop (yfor mark in *epmem-marks*)
         (ydo (ob$removes mark 'marks)))
; later change to replace slot value to 0 for
; less garbage.
  (setq *epmem-marks* nil))

(defun mark-mark (ob)
  (let ((marks (+ 1 (or (ob$get ob 'marks) 0))))
       (ob$set ob 'marks marks)
       (if (eq? marks 1)
           (setq *epmem-marks* (cons ob *epmem-marks*)))
       marks))

; This function is used to retrieve episodes in planning.
; epmem-reminding is later called on an episode returned by this
; procedure if in fact other heuristics decide to use that episode.
; (Thus only "appropriate" episodes are actually recalled in this case.)
(defun episode-retrieve (rule)
  (ndbg-roman-nl *gate-dbg* rule-xtra "Find potential episodes for ~A" rule)
  (let ((new (epmem-retrieve1 (list rule) nil 'plan-threshold))
        (result nil))
       (yloop (yfor ep in (ob$gets rule 'indexes))
              (ydo (if (or (memq? ep new)
                           (recent-episode? ep))
                       (setq result (cons ep result)))))
       (if result (ndbg-roman-nl *gate-dbg* rule-xtra
                                 "Potential episodes = ~A" result))
       result))

(defun epmem-retrieve (indices serendipity? threshold-type)
  (let ((eps (epmem-retrieve1 indices serendipity? threshold-type)))
       (yloop
        (yfor ep in eps)
        (ydo (epmem-reminding ep nil nil)))
       eps))

; If serendipity? is T, then one less the normal threshold (kind specified
; by threshold-type) will result in retrieval (since serendipity can be
; thought of as providing an extra index).
; This does NOT retrieve episodes that are already recent.
(defun epmem-retrieve1 (indices serendipity? threshold-type)
  (mark-init)
  (yloop
   (initial (result nil)
            (marks nil)
            (threshx nil))
   (yfor index in indices)
   (ydo
    (if (setq index (index-intern index 'old))
        (yloop
         (yfor episode in (ob$gets index 'indexes))
         (ydo (if (not (recent-episode? episode))
                 (progn
                  (setq marks (mark-mark episode))
                  (ndbg-roman-nl *gate-dbg* remind "~A marks on ~A"
                                 marks episode)
                  (setq threshx (if serendipity?
                                  (- (ob$get episode threshold-type) 1)
                                  (ob$get episode threshold-type)))
                  (ndbg-roman-nl *gate-dbg* remind "Net thresh for ~A is ~A"
                                 episode threshx)
                  (cond
                   ((= marks threshx)
                    (setq result (cons episode result)))
                   ((> marks threshx)
                    (if (memq? episode result)
                        (ndbg-roman-nl *gate-dbg* remind
                                       "Overdetermined epmem-retrieve ~A"
                                       episode)
                        (setq result (cons episode result)))))))))))
    (yresult (progn
            (mark-unmark-all)
            (if result (ndbg-roman-nl *gate-dbg* remind
                                      "epmem-retrieve1 returns ~A"
                                      result))
            result))))

; Assumes index is already copied if this is necessary.
; new? = 'new-ok if it is OK to create a new index, else 'old
; Note that rule indices don't get asserted in *episodic-memory*.
(defun index-intern (index new?)
  (if (ty$instance? index 'rule)
      index
      (let ((found (cx$retrieve *episodic-memory* index)))
           ; Retrieve returns a list of bindings, where the car of each
           ; is not T but rather the retrieved ob.
           (if found
               (caar found)
               (if (eq? new? 'new-ok)
                   (progn
                    (cx$assert *episodic-memory* index)
                    index)
                   nil)))))

;
; Reminding mechanism
;

(setq *recent-indices* nil)

; Todo: get rid of the superfluous consing.
(defun remindings ()
  (epmem-retrieve (append *recent-indices* 
                          (get-emotion-indices))
                  nil 'reminding-threshold))

(setq *pos-emot-ptn* (ob$fcreate '(POS-EMOTION)))
(setq *neg-emot-ptn* (ob$fcreate '(NEG-EMOTION)))
(setq *pos-neg-list* (list *pos-emot-ptn* *neg-emot-ptn*))
(setq *pos-list* (list *pos-emot-ptn*))
(setq *neg-list* (list *neg-emot-ptn*))

; Todo: in the future, we would like to index on the "quality" of the
; emotion (e.g., embarrassment) in addition to the sign.
(defun get-emotion-indices ()
  (cond
   ((fl> *overall-emotional-state* 1.0) *pos-list*)
   ((fl< *overall-emotional-state* -1.0) *neg-list*)
   (else nil)))

(setq *recent-index-max-length* 6)

(defun add-recent-index (index)
  (if (not (memq? index *recent-indices*))
      (progn
       (ndbg-roman-nl *gate-dbg* remind "Activate index ~A" index)
       (if (>= (length *recent-indices*) *recent-index-max-length*)
           (progn
            (ndbg-roman-nl *gate-dbg* remind "Index ~A fades"
                           (car *recent-indices*))
            (setq *recent-indices* (cdr *recent-indices*))))
       (setq *recent-indices* (append! *recent-indices* (list index))))
      (ndbg-roman-nl *gate-dbg* remind "Index ~A already active"
                     index)))

;
; Environmental object input
;

(defun environmental-object-input ()
  (ndbg-roman-nl *gate-dbg* rule
                 "Taking optional object or concept input")
  (let ((concepts (enter-concepts *reality* *me-belief-path*)))
       (if (null? concepts)
           nil
           (let ((result1 (run-object-serendipities concepts))
                 (result2 (entered-concept-serendipity)))
                (or result1 result2)))))

(defun run-object-serendipities (concepts)
  (let ((episodes (epmem-retrieve1 (append! concepts *recent-indices*)
                                   t 'reminding-threshold))
        (old-recent-episodes *recent-episodes*)
        (temp nil))
       (setq *recent-episodes* (append! *recent-episodes* episodes))
       (setq temp (run-serendipities))
       (setq *recent-episodes* old-recent-episodes)
       (if temp (yloop (yfor episode in episodes) ; was (cdr temp)
                       (ydo
                        (if (any? (lambda (d) (memq? d (cdr temp)))
                                  (ob$get episode 'descendants))
                            (epmem-reminding episode t nil)))))
       temp))

;
; Episode recency mechanism
;

(setq *recent-episodes* nil)

(setq *recent-ep-max-length* 4)

(defun add-recent (episode)
  (yloop (yfor ep in *recent-episodes*)
         (ydo (if (memq? ep (ob$get episode 'descendants))
                  (setq *recent-episodes* (delq! ep *recent-episodes*)))))
  (if (>= (length *recent-episodes*) *recent-ep-max-length*)
      (progn
       (ndbg-roman-nl *gate-dbg* rule "Episode ~A fades"
                      (car *recent-episodes*))
       (setq *recent-episodes* (cdr *recent-episodes*))))
  (setq *recent-episodes* (append! *recent-episodes* (list episode))))

(defun recent-episode? (episode)
  (any? (lambda (ep) (memq? episode (ob$get ep 'descendants)))
        *recent-episodes*))
; was (memq? episode *recent-episodes*)

; Todo: if an episode is defined after the system is already going,
; this should be called.
(defun epmem-reminding (episode no-serendipities? new-stored-ep?)
  (if (not (recent-episode? episode))
      (progn
  ;
  ; Print out stuff
  ;
  (if (not new-stored-ep?)
      (progn
       (ndbg-roman *gate-dbg* rule "Episodic reminding")
       (ndbg-roman *gate-dbg* rule " of ~A" episode)
       (ndbg-newline *gate-dbg* rule)
       (generate-episode episode)))
  ;
  ; Add to recent episodes.
  ;
  (add-recent episode)
  ;
  ; Add other indices of episode to recent indices.
  ; Note: this effectively results in a `reminding' link (subject
  ;   to threshold requirements) from any two episodes having
  ;   the same index.
  ;
  (yloop (yfor index in (ob$gets episode 'indexed-under))
         (ydo 
          (if (ty$instance? index 'emotion)
              (if (not new-stored-ep?)
                  (progn
                   (setq *reality* *reality-lookahead*)
                   (ndbg-roman-nl *gate-dbg* rule "Reactivate emotion")
                   (add-emotion (ob$get episode 'goal)
                                index (ob$get episode 'realism) *reality*)))
              (add-recent-index index))))
  ;
  ; Run serendipities unless told not to.
  ; Note: inacc. planning rules can be plans OR inferences!
  ;
  (if (null? no-serendipities?)
      (run-serendipity (inaccessible-planning-rules episode) nil))
  ;
  ; Get any new remindings from indices now active.
  ;
  (remindings))))

(setq *auto-rule-plausibility* 0.7)

(defun episode-defn-goal (defn)
  (car defn))

(defun episode-defn-subgoals? (defn)
  (cdr defn))

(defun episode-defn-rule (defn)
  (cadr defn))

(defun episode-defn-subgoals (defn)
  (cdddr defn))

(defun episode-defn-plan-no-gen (defn)
  (caddr defn))

; If T, non top-level goals of a hand-coded episode are not accessible
; directly for planning.
(setq *hidden-ep-subgoals?* t)

(defun episode-defn->stored-episode (episode-defn context hidden?)
  (let ((goal (ob$create `(SUCCEEDED-GOAL
               obj ,(episode-defn-goal episode-defn))))
        (rule nil) (intend nil) (intends nil) (subgoal nil) (subgoals nil)
        (realism 1.0) (num-subgoals nil) (weight nil)
        (plan-no-gen (episode-defn-plan-no-gen episode-defn)))
       (if (not (episode-defn-subgoals? episode-defn))
           (progn
            (no-gen (cx$assert context goal))
            goal)
           (progn
            (setq rule (episode-defn-rule episode-defn))
            (if (eq? rule 'induce-rule)
                (setq rule nil)
                (progn
                 (setq rule (ob$name->ob rule))
                 (if (null? rule)
                     (error "Rule ~A not defined yet; (ret) to induce"
                            (episode-defn-rule episode-defn)))))
            (setq realism 0.0)
            (setq num-subgoals
                 (fixnum->flonum (length (episode-defn-subgoals episode-defn))))
            (setq weight
                 (if rule
                     (fl/ (ob$get rule 'plausibility) num-subgoals)
                     (fl/ *auto-rule-plausibility* num-subgoals)))
       (yloop (initial (subgoalnum 0))
             (yfor subgoal-spec in (episode-defn-subgoals episode-defn))
             (ydo 
              (setq subgoal (episode-defn->stored-episode
                            subgoal-spec context
                            (or *hidden-ep-subgoals?* hidden?)))
              (ob$set (ob$get subgoal 'obj) 'plan-subgoalnum subgoalnum)
              (setq realism (fl+ realism
                                (fl* weight (strength (ob$get subgoal 'obj)))))
              (setq subgoals (append subgoals (list subgoal)))
              (if rule
                  (setq intend (ob$fcreate `(INTENDS linked-from ,goal
                                                     linked-to ,subgoal
                                                     rule ,rule
                                                     seq? 't)))
                  (setq intend (ob$fcreate `(INTENDS linked-from ,goal
                                                     linked-to ,subgoal
                                                     seq? 't))))
              (setq intends (cons intend intends))
              (no-gen (cx$assert context intend))
              (setq subgoalnum (+ 1 subgoalnum))))
       (no-gen (cx$assert context goal))
       (if (null? rule)
           (progn
            (ndbg-roman-nl *gate-dbg* rule "Generating rule automatically.")
            (setq rule (plan->rule (ob$get goal 'obj)
                                  (subgoal-objs subgoals)
                                  *auto-rule-plausibility*
                                  #'*episodic-rule-name-genproc*))
            (if (not (nil? plan-no-gen))
                (ob$set rule 'plan-no-gen plan-no-gen))
            (yloop (yfor i in intends)
                   (ydo (ob$add i 'rule rule))))
;           (check-episodic-plan rule (ob$get goal 'obj) subgoals)
       )
       (ob$set goal '(obj strength) realism)
       (make-and-store-episode rule goal context realism nil hidden?
                               (subgoals->eps subgoals))
       goal))))

(defun subgoals->eps (subgoals)
  (yloop (initial (result nil))
         (yfor subgoal in subgoals)
         (ydo (if (ob$get subgoal 'episode)
                  (setq result (append result (list (ob$get subgoal
                                                     'episode))))))
         (yresult result)))

(defun constructed-plan? (rule)
  (ob$get rule 'constructed?))

(setq *next-erule-number* 1)

(defun *episodic-rule-name-genproc* ()
  (string->symbol
   (string-append "EPISODIC-RULE."
                  (prog1 (fixnum->string *next-erule-number*)
                          (increment-me *next-erule-number*)))))

(defun plan->rule (goal subgoals plausibility name-gen-proc)
  (let ((concrete-rule nil)
        (abstract-rule nil))
       (if (cdr subgoals)
           (setq concrete-rule
                (ob$create
                 `(RULE subgoal (RSEQ ,@subgoals)
                        goal ,goal
                        plausibility ,plausibility
                        is 'plan-only)))
           (setq concrete-rule
                (ob$create
                 `(RULE subgoal ,(car subgoals)
                        goal ,goal
                        plausibility ,plausibility
                        is 'plan-only))))
       (setq abstract-rule
        (ob$variabilize concrete-rule #'varize-object? nil *link-slots* nil))
       (ob$add-unique-name abstract-rule (funcall name-gen-proc))
       (ob$set abstract-rule 'constructed? t)
       (add-rule-print abstract-rule)
       abstract-rule))

(defun varize-object? (x)
  (and (ob? x)
       (not (vars-in? x))
       (or (and (ty$instance? x 'object)
                (not (ty$instance? x 'no-varize-obj)))
           (ty$instance? x 'city)
           (ty$instance? x 'location))))

(defun subgoal-objs (subgoals)
  (map 'list (lambda (x) (ob$get x 'obj)) subgoals))

; We don't count goals that terminated before the beginning of this
; top-level goal's 'scenario'.
(defun scenario-desirability (context top-level-goal)
  (ndbg-roman-nl *gate-dbg* desire "Assess scenario desirability in ~A" context)
  (yloop (initial (result 0.0)
                 (pre-scenario-contexts
                  (cx$ancestors (ob$get top-level-goal 'activation-context))))
        (yfor ob in (append (cx$get-all-ty context *failed-goal-ob*)
                            (cx$get-all-ty context *succeeded-goal-ob*)))
        (ydo (if (and (personal-goal? ob)
                     (not (memq? (ob$get ob 'termination-context)
                                 pre-scenario-contexts)))
                (progn
                 (ndbg-roman-nl *gate-dbg* desire "~A (~A)"
                                ob (strength ob))
                (cond
                 ((ty$instance? ob 'succeeded-goal)
                  (setq result (fl+ result (strength ob))))
                 ((ty$instance? ob 'active-goal)
                  (setq result (fl- result (strength ob))))
                 ((ty$instance? ob 'p-goal)
                  (setq result (fl- result (strength ob))))
                 ((ty$instance? ob 'failed-goal)
                  (setq result (fl- result (strength ob))))))))
        (yresult
     (progn
      (ndbg-roman-nl *gate-dbg* rule "Scenario desirability = ~A" result)
      result))))

;
; This is called upon top-level goal failure or success.
;
; Todo: don't store episode if derived from another episode by
; analogy without repairs.
;
; Todo: use situation assumptions as indices?
;
(defun episode-store-top-goal (top-level-goal context)
  (ndbg-roman *gate-dbg* rule "Store episode")
  (ndbg-roman *gate-dbg* rule " ~A in ~A" top-level-goal context)
  (ndbg-newline *gate-dbg* rule)
  (no-gen
  (let ((desirability (scenario-desirability context top-level-goal))
        (ep nil)
        (result-emot (goal->result-emotion top-level-goal *reality*)))
       (if (ty$instance? (ob$get top-level-goal 'obj) 'skipindex)
           (setq ep (episode-store1 (car (goal-subgoals top-level-goal context
                                                       *me-belief-path*))
                                   context desirability))
           (setq ep (episode-store1 top-level-goal context desirability)))
       (if ep
           (progn
            (if result-emot (epmem-store ep result-emot nil nil))
            ; Todo: index under causing state (personal goal)
            ;       or causing emotion (daydreaming goal).
            ; In below, indices are needed neither for plan nor for
            ; reminding. But wouldn't we like to make it require, say,
            ; half the misc indices for a reminding?
            (yloop (yfor index in (find-misc-indices context top-level-goal))
                   (ydo (epmem-store ep index nil nil)))
            ; Do housekeeping for a recent episode (but without the
            ; reminding hoopla; also no serendipities since those
            ; are run whenever a new rule is induced).
            (epmem-reminding ep t t)
            ep)
           nil))))

(defun goal->result-emotion (goal context)
  (let ((result 
         (prune (ol-get goal *dependency-ob* 'forward context)
                (lambda (x) (ty$instance? x 'emotion)))))
       (if result
           (progn
            ; Checks to be removed once they seem to hold.
            (if (cdr result)
                (error "More than one result emotion for ~A?" goal))
;            (if (any? (lambda (x) (not (ty$instance? x 'emotion)))
;                      result)
;                (error "Not all of ~A are emotions!!" result))
            (car result))
           nil)))

(defun goal->motiv-emotion (goal context)
  (ob$get goal 'main-motiv))

; This is expensive and should only be called upon indexing of
; a final episode.
(defun find-misc-indices (context top-level-goal)
  (yloop
   (initial (result nil)
            (ancestors
             (cx$ancestors 
              (ob$get top-level-goal 'activation-context))))
   (yfor ob in (cx$get-all context))
   (ydo
    ; First condition checks that ob was asserted in activation context
    ; or later.
    (if (and (not (any? (lambda (x) (memq? x ancestors))
                        (ob$gets ob 'top-context)))
             (or (ty$instance? ob 'goal)
                 (ty$instance? ob 'state)))
    ; Todo: should not include other common indices.
        (setq result
             (prune (union result (objects-in ob))
                    (lambda (elem) (not (me? elem)))))))
   (yresult result)))

(defun episode-store1 (goal context desirability)
  (ndbg-roman-nl *gate-dbg* ep-store "Store goal of episode ~A, realism ~A"
                 goal (strength (ob$get goal 'obj)))
  (let* ((rule nil)
         (subgoals (goal-subgoals goal context *me-belief-path*))
         (ep nil))
        (if subgoals
            (progn
             (setq rule (goal-subgoals-rule goal context *me-belief-path*))
             (yloop (yfor subgoal in subgoals)
                    (ydo (episode-store1 subgoal context nil)))
             (setq ep (make-and-store-episode rule goal context
                                             (strength (ob$get goal 'obj))
                                             desirability nil
                                             (subgoals->eps subgoals)))))
        ep))

;
; Similarity metric for episodic retrieval
;

(defun ob$similarity (ob1 ob2)
  (ndbg-roman-nl *gate-dbg* simil
   "Assess similarity between ~A and ~A" ob1 ob2)
  (let ((result (ob$similarity1 ob1 ob2)))
    (ndbg-roman-nl *gate-dbg* simil
     "Similarity between ~A and ~A = ~A" ob1 ob2
          result)
    result))

(defun distance->similarity (x)
  (fl- 1.0 (fl* .25 (fixnum->flonum x))))

(defun ty-distance (type1 type2)
  (if (and (ty? type1) (ty? type2))
      (ty$distance type1 type2)
      0))

;
; Todo: We have to take 1/type-distance, no? What was original alg in notebook?
;
(defun ob$similarity1 (ob1 ob2)
  (cond
   ((eq? ob1 ob2) 1.0)
   ((or (pair? ob1) (symbol? ob1) (string? ob1)
        (pair? ob2) (symbol? ob2) (string? ob2)) 0.0) ; should do for now
   ((and (var? ob1) (var? ob2))
    (distance->similarity (ty-distance (variable-type ob1)
                                   (variable-type ob2))))
   ((var? ob1)
    (distance->similarity (ty-distance (variable-type ob1)
                                   (ob$ty ob2))))
   ((var? ob2)
    (distance->similarity (ty-distance (ob$ty ob1)
                                   (variable-type ob2))))
   ((and (ty$instance? ob1 'object)
         (ty$instance? ob2 'object))
    (distance->similarity (ty-distance (ob$ty ob1)
                                   (ob$ty ob2))))
   ((and (ob? ob1) (ob? ob2))
    (yloop (initial (val2 nil)
                   (temp nil)
                   (result (distance->similarity (ty-distance (ob$ty ob1)
                                                          (ob$ty ob2)))))
          (yuntil (= result *min-flonum*))
          ; Todo: Could keep track of used slots as in unify?
          (yfor sv in (ob$pairs ob1))
          ; below code really assumes no multiple slot values
          (ydo (if (and (neq? (slots-name sv) 'type)
                       (neq? (slots-name sv) 'strength)
                       (not (memq? (slots-name sv)
                                   *permanent-ignore-slots*)))
                  (progn
                   (setq val2 (ob$gets ob2 (slots-name sv)))
                   (if (null? val2)
                       (setq result *min-flonum*) ; non-homomorphic obs
                       (progn
                        (setq temp (apply 'max
                                         (map 'list (lambda (x) (ob$similarity1
                                                           (slots-value sv) x))
                                              val2)))
                        (if (not (= temp *min-flonum*))
                            (setq result (fl+ result (fl* 0.5 temp)))
                            (setq result *min-flonum*)))))))
           (yresult result)))
   (else (error "Bug: ob$similarity got unknown stuff: ~A ~A" ob1 ob2))))

(setq *min-flonum* -10000.0) ; T has such a constant?

(defun try-analogical-plan (goal goal-obj context analogical-episode
                                           belief-path top-level-goal)
  (ndbg-roman-nl *gate-dbg* rule-long "Try analogical plan for ~A in ~A ep ~A"
        goal context analogical-episode)
  (yloop
   (initial (sprouted-contexts nil))
   (yfor bd in (rule-applications goal-obj context
                                 (ob$get analogical-episode 'rule)
                                 belief-path nil))
   (ydo (setq sprouted-contexts
            (append! (run-analogical-plan goal goal-obj context bd
                                          (ob$get analogical-episode 'goal)
                                          (ob$get analogical-episode 'context)
                                          (ob$get analogical-episode 'rule)
                                          1.0 belief-path top-level-goal
                                          analogical-episode nil)
                     sprouted-contexts)))
   (yresult sprouted-contexts)))

(setq *relaxed-analogy-realism* 0.5)

; new vsn -- but doesn't do verification; that is left to
; try-analogical-plan since when analogical plan is first invoked,
; it is automatically verified.
;
; Todo: Need to deal with initial slot in here? (Note, however, possible
; conflictions with mutation4, etc.)
(defun run-analogical-plan (target-goal target-goal-obj target-context
                             target-bd source-goal source-context rule
                             ordering belief-path top-level-goal
                             analogical-episode reminding?)
  (ndbg-roman-nl *gate-dbg* rule "Run analogical plan for ~A in ~A" target-goal
        target-context)
  (let* ((source-subgoals (goal-subgoals source-goal source-context
                                         *me-belief-path*))
         (source-goal-obj (ob$get source-goal 'obj))
         (r-subgoal-objs (rule-subgoal-objs rule))
         (bd nil)
         (sprouted-context nil)
         (seq? (goal-subgoals-seq? source-goal source-context)))
    (cond
     ((null? source-subgoals)
      (error "I thought bottoming out was detected in activate-subgoal")
      (ndbg-roman-nl *gate-dbg* rule "Analogical plan for ~A in ~A bottoms out"
            target-goal target-context)
      nil)
     (else
      (setq sprouted-context (cx$sprout target-context))
      (delay-dbgs sprouted-context
       (set-ordering sprouted-context ordering)
       (setq bd (episodic-unify (ob$get rule 'goal) r-subgoal-objs
                               source-goal-obj source-subgoals target-goal
                               sprouted-context target-bd belief-path t
                               top-level-goal ; was target context
                               analogical-episode))
       (if (ob? (car bd))
           (progn
            (ndbg-roman-nl *gate-dbg* rule-xtra
                           "Resetting target goal from ~A to ~A"
             target-goal (car bd))
            (setq target-goal (car bd))))
       (if reminding?
           (progn
            (epmem-reminding analogical-episode nil nil)
            (ndbg-roman-nl *gate-dbg* rule "Apply episode ~A"
                           (ob->string analogical-episode))
            (rule-fire-msg rule "analogical plan" target-context bd
                           sprouted-context target-goal))
           (progn
            (ndbg-roman-nl *gate-dbg* rule "Apply suggested episode ~A"
                           (ob->string analogical-episode))
            (rule-fire-msg rule "analogical plan" target-context
                           bd sprouted-context target-goal)
            (ndbg-newline *gate-dbg* rule)))
       (instan-and-activate-subgoals target-goal r-subgoal-objs bd rule
                                     sprouted-context seq? source-subgoals
                                     nil top-level-goal belief-path))
      (list sprouted-context)))))

(defun episodic-unify (rule-goal-obj r-subgoal-objs source-goal-obj
                        source-subgoals target-goal target-context
                        target-bd belief-path disallow-failures?
                        top-level-goal episode)
 (let ((bd (ob$unify rule-goal-obj source-goal-obj (planner-empty-bd
                                                    belief-path)))
       (seren-ep (if episode (ob$get episode 'seren-ep) nil)))
  (cond
   ((null? bd)
    (if disallow-failures?
        (progn
         (ndbg-roman-nl *gate-dbg* rule "~A does not correspond with ~A"
                        rule-goal-obj source-goal-obj)
         (error "bd nil in analogical plan!!!")
         nil)
        nil))
   (else
    ; In following, order has to be not reversed.
    (yloop
     (initial (source-subgoalsx source-subgoals))
     (yfor rule-subgoal-obj in r-subgoal-objs)
     (yuntil (null? bd))
     (ydo
      (setq bd (ob$unify rule-subgoal-obj
                         (ob$get (car source-subgoalsx) 'obj) bd))
      (if (and (null? bd) disallow-failures?)
          (progn
           (ndbg-roman-nl *gate-dbg* rule "~A does not correspond with ~A"
            rule-subgoal-obj (ob$get (car source-subgoalsx) 'obj))
           (error "bd nil in analogical plan!!")))
      (setq source-subgoalsx (cdr source-subgoalsx))))
    (if (null? bd)
        nil
        (progn
         (ndbg-roman-nl *gate-dbg* analogy "Target-bd:")
         (if-interested-in analogy (bd-print target-bd *gate-dbg*))
         (ndbg-roman-nl *gate-dbg* analogy "Bd:")
         (if-interested-in analogy (bd-print bd *gate-dbg*))
         (bd-special-append target-bd bd target-goal
                            target-context belief-path top-level-goal nil
                            (lambda (x)
                             (analogy-instantiatible1? x seren-ep)))))))))

; Verification function for reading in episodes.
(defun check-episodic-plan (rule source-goal-obj source-subgoals)
  (let* ((rule-goal-obj (ob$get rule 'goal))
         (r-subgoal-objs (rule-subgoal-objs rule))
         (bd (ob$unify rule-goal-obj source-goal-obj (planner-empty-bd
                                                      *me-belief-path*))))
        (if (null? bd)
            (ndbg-roman-nl *gate-dbg* rule
                           "Warning: ~A does not correspond with ~A, rule ~A"
                           rule-goal-obj source-goal-obj rule)
            (yloop
             (initial (source-subgoalsx source-subgoals))
             (yfor rule-subgoal-obj in r-subgoal-objs)
             (yuntil (null? bd))
             (ydo
              (setq bd (ob$unify rule-subgoal-obj
                                (ob$get (car source-subgoalsx) 'obj) bd))
              (if (null? bd)
               (ndbg-roman-nl *gate-dbg* rule
                              "Warning: ~A does not correspond with ~A, rule ~A"
                              rule-subgoal-obj (ob$get (car source-subgoalsx)
                                                       'obj) rule))
              (setq source-subgoalsx (cdr source-subgoalsx)))))))

(defun generate-episode (episode)
  (generate1 episode *global-switches* (ob$get episode 'context)
             *me-belief-path*))

(defun task-print-plans (tlg)
  (yloop (initial (continue? t))
        (yfor leaf in (cx$leaf-descendants (get-backtrack-wall tlg)))
        (ywhile continue?)
        (ydo (format *gate-dbg* "Leaf context ~A~%" leaf)
            (plan-print tlg leaf)
;            (setq continue? (interrogate "More? "))
            (setq continue? t))
        (yresult nil)))

(defun ep-print (episode)
  (plan-print1 (ob$get episode 'goal) (ob$get episode 'context) *gate-dbg* 0))

(defun plan-print (goal context)
  (plan-print1 goal context *gate-dbg* 0))

; There is probably no such thing as always-prop
(setq *ep-print-options* '(parens always-prop no-newline))

(defun plan-print1 (goal context stream level)
  (print-spaces stream (* level 2))
  (cond
   ((ty$instance? goal 'active-goal)
    (if *typeset?*
        (format stream "[~A: (\\typepp{AG}. " (ob->string goal))
        (format stream "[~A: (AG. " (ob->string goal))))
   ((ty$instance? goal 'failed-goal)
    (if *typeset?*
        (format stream "[~A: (\\typepp{FG}. " (ob->string goal))
        (format stream "[~A: (FG. " (ob->string goal))))
   ((ty$instance? goal 'succeeded-goal)
    (if *typeset?*
        (format stream "[~A: (\\typepp{SG}. " (ob->string goal))
        (format stream "[~A: (SG. " (ob->string goal))))
   (else
    (format stream "[~A: (?? " (ob->string goal))))
  (if (ob$get goal 'obj)
      (ob$pr (ob$get goal 'obj) stream *ep-print-options*)
      (format stream "--"))
  (if (ob$get goal 'episode)
      (format stream ") ~A]" (ob->string (ob$get goal 'episode)))
      (format stream ")]"))
  (do-newline stream)
  (yloop (yfor subgoal in (goal-subgoals goal context *me-belief-path*))
         (ydo (plan-print1 subgoal context stream (+ 1 level)))))

; End of file.
