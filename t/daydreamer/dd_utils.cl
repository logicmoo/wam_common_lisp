;*******************************************************************************
;
; Daydreamer
; Version 3.5
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
; 7/19/86: Added rule chaining macros
; 9/24/86: Removed flavors
;
;*******************************************************************************

(defun htlg ()
  (ob$set (car *top-level-goals*) 'status 'halted))

(defun hlt (gl)
  (ob$set gl 'status 'halted))

;
; This isn't right for non-fully-loaded obs
;
(setq *default-strength* 1.0)

;
; Inverse slot declarations
;
(ob$decl-inverses 'causes 'caused-by)
(ob$decl-inverses 'seq-next 'seq-next-of)
(ob$decl-inverses 'g-situ 'g-situ-of)

;
; Type definitions
;
(ty$create 'ORDERING nil '(prop (strength) ()))
(ty$create 'ALTERN nil nil)
(ty$create 'MUTATION nil nil)
(ty$create 'OTHER nil '(prop (actor) ()))

(defun cx$print-sprout-trace (self)
  (if (ob$get self 'sprout-trace)
      (progn
       (ndbg-roman-nl *gate-dbg* rule "Broadcasting delayed debugs.")
       (format *gate-dbg* "~A" (car (ob$get self 'sprout-trace)))
       (ndbg-roman-nl *gate-dbg* rule "End of delayed broadcast."))))

(setq *global-switches* nil)

;
; Utility ob link functions
;

(defun has-link-relative? (ob direction type context belief-path)
  (let ((links (ob$gets ob direction)))
    (any? (lambda (x) (and (cx$true-relative context x belief-path)
                           (ty$instance-of? x type))) links)))

(defun get-links-relative (ob link-type context belief-path)
  (let ((links (ob$gets ob 'linked-from-of)))
    (yloop (initial (result nil))
          (yfor link in links)
          (ydo (if (and (cx$true-relative context link belief-path)
                       (ty$instance-of? link link-type))
                  (setq result (append! result (list link)))))
          (yresult result))))

(defun get-links-from-relative (ob link-type context belief-path)
  (let ((links (ob$gets ob 'linked-to-of)))
    (yloop (initial (result nil))
          (yfor link in links)
          (ydo (if (and (cx$true-relative context link belief-path)
                       (ty$instance-of? link link-type))
                  (setq result (append! result (list link)))))
          (yresult result))))

(defun ol-get-relative (ob link-type dir context belief-path)
  (let ((links (ob$gets ob (if (eq? dir 'backward)
                                'linked-to-of
                                'linked-from-of)))
        (other-dir (if (eq? dir 'backward) 'linked-from
                                           'linked-to)))
    (yloop (initial (result nil))
           (yfor link in links)
           (ydo
            (if (and (ty$instance-of? link link-type)
                     (cx$true-relative context link belief-path))
                (setq result (append (ob$gets link other-dir) result))))
           (yresult result))))

(defun ol-get-relative-rule (ob link-type dir context belief-path rule)
  (let ((links (ob$gets ob (if (eq? dir 'backward)
                                'linked-to-of
                                'linked-from-of)))
        (other-dir (if (eq? dir 'backward) 'linked-from
                                           'linked-to)))
    (yloop (initial (result nil))
           (yfor link in links)
           (ydo
            (if (and (ty$instance-of? link link-type)
                     (eq? rule (ob$get link 'rule))
                     (cx$true-relative context link belief-path))
                (setq result (append (ob$gets link other-dir) result))))
           (yresult result))))

(defun ol-set-relative (from-ob link-type to-ob context belief-path)
  (let ((link (ob$fcreate `(NOTYPE linked-from ,from-ob linked-to ,to-ob))))
       (ob$set link 'type link-type)
       (cx$assert-relative context link belief-path)))

; see replace-linked-ob
; It is not clear to me why we have to not copy and then later restore
; these slots (except for those that are permanent ignore). Can't
; we just not omit them in the first place?
(setq *preserve-link-slots* '(analogical-episode activation-context
                                                 inference-rule strength
                                                 termination-context
                                                 top-level-goal
                                                 preservation-subgoal?))

; plan-rule inference-rule
; These are contained in obj slot anyway?
; Also plan-subgoalnum

(setq *link-slots*
      (append '(linked-from-of linked-to-of seq-next seq-next-of
                               active-goal top-context episode)
              *preserve-link-slots*))

;(defun ob-copy-basic (ob)
;  (copy-ob-omit ob *link-slots*))

; Weight, offset, decay: offset optional (default 0.0),
; weight optional (default = percentage of number
; of numeric dependencies).
;
;;; DEPENDENCY ROUTINES
;
; froms: ((from-ob from-ptn-ob) ...)
;

; is bd right?
(defun make-dependency (froms to rule context belief-path plausibility bd)
  (let* ((num (length froms))
         (weight (fl/ plausibility (fixnum->flonum num))))
        ; weight assumes either all or none of the ptns
        ; have all or none of the params
    (yloop
     (yfor from in froms)
     (ydo
; Old code
;      (if (and (ty$instance? (car from) 'not)
;               (not (cx$true-relative context (car from) belief-path)))
;          (cx$assert-relative context (car from) belief-path))
      (if (ty$instance? (car from) 'rnot)
          (progn
           (setf (car from) (ob$instantiate (cadr from) bd))
           (ob$set (car from) 'type *not-ob*)
           (no-gen (cx$assert-relative context (car from) belief-path))))
      (cx$assert-relative context
            (ob$fcreate `(DEPENDENCY
                            linked-from ,(car from)
                            linked-to ,to
                            weight ,(with-default
                                     (if (ob? (cadr from))
                                         (ob$get (cadr from) 'weight)
                                         nil)
                                      weight)
                            offset ,(with-default
                                     (if (ob? (cadr from))
                                         (ob$get (cadr from) 'offset)
                                         nil)
                                      0.0)
                            decay ,(with-default
                                     (if (ob? (cadr from))
                                         (ob$get (cadr from) 'decay)
                                         nil)
                                     0.0)
                            rule ,rule))
            belief-path)))
    (recalculate-strength to context belief-path)))

(defun add-depend (context from to weight offset decay rule)
  (ndbg-roman-nl *gate-dbg* rule "Add dependency from ~A to ~A in ~A"
                 from to context)
  (let ((dep
         (if rule (ob$fcreate `(DEPENDENCY
                                  linked-from ,from linked-to ,to
                                  weight ,weight offset ,offset
                                  decay ,decay rule ,rule))
                  (ob$fcreate `(DEPENDENCY
                                  linked-from ,from linked-to ,to
                                  weight ,weight offset ,offset
                                  decay ,decay)))))
       (cx$assert context dep)
       (recalculate-strength to context (->belief-path *me-ob*))))

; Sets the strength of an ob, maintaining dependency consistency.
(defun hyper-set-strength (ob value context)
  (let ((old-strength (strength ob)))
       (modify-strength context ob (fl- value old-strength) 1.0)))

; Works on dangling or dependent objects.
(defun modify-strength (context ob delt weight)
  (ndbg-roman-nl *gate-dbg* depend "Modify strength for ~A in ~A by ~A ~A"
                 ob context delt weight)
  (if (get-dependencies ob context *me-belief-path*)
      (let ((delta (ob$fcreate `(NOTYPE strength ,delt))))
           (add-depend context delta ob weight 0.0 0.0 nil))
      (progn
       (ndbg-roman-nl *gate-dbg* depend "Dangling.")
       (set-strength ob (fl+ (fl* weight delt) (strength ob)))
       (recalculate-strength ob context *me-belief-path*))))

; Todo: the below could probably be made faster by coding it
; analogously to ol-get-relative. Depends on whether typical number of
; dependencies in a context is greater than typical number of
; 'linked-to-of connections.
(defun get-dependencies (ob context belief-path)
  (let ((ptn (ob$fcreate `(DEPENDENCY linked-to ,ob))))
   (retrieve-bd->ob (cx$retrieve-relative context ptn belief-path))))

(defun get-dependees (ob context belief-path)
  (ol-get-relative ob *dependency-ob* 'forward context belief-path))

(defun divert-strength (from-ob to-ob factor)
  (let ((diversion (fl* factor (strength from-ob))))
    (set-strength from-ob (fl- (strength from-ob) diversion))
    (set-strength to-ob (fl+ (strength to-ob) diversion))
    diversion))
    
(defun recalculate-strength (ob context belief-path)
  (ndbg-roman-nl *gate-dbg* depend "Recalculate strength for ~A in ~A"
                 ob context)
  (let ((deps (get-dependencies ob context belief-path)))
  (if deps
      (progn
  (ndbg-roman-nl *gate-dbg* depend "Initial strength = ~A" (strength ob))
  (yloop
   (initial (new-strength 0.0) (offset nil) (weight nil))
   (yfor dependency in deps)
   (ydo
    (setq offset (ob$get dependency 'offset))
    (setq weight (ob$get dependency 'weight))
    (ndbg-roman-nl *gate-dbg* depend "Dependency ~A with offset ~A weight ~A"
                   dependency offset weight)
    (if (or (null? offset)
            (null? weight))
        (progn
         (error
"weight = ~A offset = ~A ob = ~A for ~A!!!~%(ret) to proceed"
          weight offset ob dependency)
         (setq offset 0.0)
         (setq weight 1.0)))
    (setq new-strength
         (fl+ (fl+ new-strength offset)
              (fl* (strength (ob$get dependency 'linked-from))
                   weight)))
    (ndbg-roman-nl *gate-dbg* depend "Sum so far is ~A"
                   new-strength))
   (yresult (set-strength ob new-strength)
           (ndbg-roman-nl *gate-dbg* depend "New strength = ~A" new-strength)
           (if (ty$instance? ob 'emotion)
               (normalize-emotion ob context)))))))
  (yloop (yfor dependee in (get-dependees ob context belief-path))
        (ydo (recalculate-strength dependee context belief-path))))

(defun normalize-emotion (emot context)
  (if (fl< (strength emot) 0.0)
      (progn
       (set-strength emot (- (strength emot)))
       (if (ty$instance? emot 'neg-emotion)
           (no-gen (ob$set-type emot *pos-emotion-ob* context))
           (no-gen (ob$set-type emot *neg-emotion-ob* context))))))

; We need to retract and reassert because obs in contexts with
; hashing are restricted not to change type.
; (see above comments in dd_rule for similar operations as below--
;  which do not yet use below... For one, they need to use belief
;  paths...)
; Todo: Also, what if more than one context are involved?!!!
(defun ob$set-type (ob new-type context)
  (cx$retract context ob)
  (ob$set ob 'type new-type)
  (cx$assert context ob))

(defun get-other-causes (fact context)
  (ol-path fact nil *dependency-ob* 'backward
           context (lambda (dummy ob)
                           (if (and (ty$instance? ob 'action)
                                    (not (memq? *me-ob* (ob$gets ob 'actor))))
                               *empty-bd*
                               nil))
                   *empty-bd*))

(defun tlg->string (tlg)
  (string-append
   (ob->string tlg)
   ": "
  (cond
   ((dd-goal? tlg) (type->string (ob$ty (ob$get tlg 'obj))))
   ((not-hurt-goal? tlg)
    (if *typeset?* "{\\bf{}NOT-HURT}" "NOT-HURT"))
   ((social-esteem-goal? tlg)
    (if *typeset?* "{\\bf{}SOCIAL-ESTEEM}" "SOCIAL-ESTEEM"))
   ((self-esteem-goal? tlg)
    (if *typeset?* "{\\bf{}SELF-ESTEEM}" "SELF-ESTEEM"))
   (else (type->string (ob$ty (ob$get tlg 'obj)))))))

(defun dd-goal? (goal)
  (and (ob? (ob$get goal 'obj))
       (ty$instance? (ob$get goal 'obj) 'dd-goal-obj)))

(defun social-esteem-goal? (goal)
  (and (ob? goal)
       (ob? (ob$get goal 'obj))
       (ty$instance? (ob$get goal 'obj) 'BELIEVE)
       (neq? *me-ob* (ob$get goal '(obj actor)))
       (ob? (ob$pget goal '(obj obj)))
       (ty$instance? (ob$pget goal '(obj obj)) 'POS-ATTITUDE)
       (eq? *me-ob* (ob$pget goal '(obj obj obj)))))

(defun self-esteem-goal? (goal)
  (and (ob? goal)
       (ob? (ob$get goal 'obj))
       (ty$instance? (ob$get goal 'obj) 'POS-ATTITUDE)
       (eq? *me-ob* (ob$pget goal '(obj obj)))))

(defun personal-goal? (goal)
  (let ((obj (ob$get goal 'obj)))
       (and (ob? obj)
    (cond
     ((memq? obj *new-personal-goals*)
      t)
     ((or (ty$instance? obj 'employment)
          (ty$instance? obj 'lovers))
      (memq? *me-ob* (ob$gets obj 'actor)))
     (else
      (or (social-esteem-goal? goal)
          (not-hurt-goal? goal)
          (ty$instance? obj 'personal-goal-obj)))))))

(defun not-hurt-goal? (goal)
  (and (ty$instance? (ob$get goal 'obj) 'not)
       (ty$instance? (ob$pget goal '(obj obj)) 'hurt)))

;
; Enter concepts from the external world for performance mode and
; environmental object input.
;
; Assert and add to *entered-concepts* (if not object)
;
(defun enter-concepts (context belief-path)
  (ndbg-roman-nl *gate-dbg* rule "Enter concepts in ~A" context)
  (yloop
   (initial (line nil) (concept nil) (concepts nil) (done nil))
   (yuntil done)
   (ydo
    (ndbg-roman-nl *gate-dbg* rule "Parser> ")
    (setq line (read-input))
    (if (string-empty? line)
        (setq done t)
        (progn
         (setq concept (parse line))
         (if concept
             (progn
              (ndbg-roman-nl *gate-dbg* rule "Input received")
;              (generate1 concept *global-switches* context belief-path)
              (setq concepts (append concept concepts))
              (yloop
               (yfor con in concept)
               (ydo 
                (if (not (ty$instance? con 'object))
                    (progn
                     (ob$set con 'input-state? t)
                     (cx$hyper-assert-relative context con belief-path)
                     (setq *entered-concepts*
                      (append! *entered-concepts* (list con))))))))))))
   (yresult concepts)))

(setq *empty-string* "")

(defun read-input ()
  (let ((line (read-line *gate-input* nil)))
    (cond
     ((null line)
      (ndbg-roman-nl *gate-dbg* rule "[stdin] ")
      (close *gate-input*)
      (setq *gate-input* (standard-input))
      (read-input))
     ((string-empty? line)
      (read-input))
     ((string-equal? line "quit")
      (ndbg-roman-nl *gate-dbg* rule "Breaking DAYDREAMER...")
      (breakpoint)
      (read-input))
     ((not (string-equal? line "end"))
      (ndbg-roman-nl *gate-dbg* rule "Input: ~A" line)
      line)
     (else
      (ndbg-roman-nl *gate-dbg* rule "End of parser input")
      *empty-string*))))

(setq *phrases* nil)

(defun parse (str)
  (let ((found (assoc (string-downcase str)
                      *phrases* :test 'string-equal)))
    (if found
        (map 'list (lambda (x) (ob$copy x))
             (cdr found))
        (progn
         (ndbg-roman-nl *gate-dbg* rule "Cannot parse input (ignored)")
         (ndbg-roman-nl *gate-dbg* rule "Input = '~A'" str)
         nil))))

(defun bd-print (bd stream)
  (if bd
      (yloop (initial (temp nil)
                     (already nil))
            (yfor bdp in (cdr bd))
            (ydo (if (not (memq? (car bdp) already))
                    (progn
                     (setq already (cons (car bdp) already))
                     (setq temp (symbol->string (car bdp)))
                     (string-downcase! (chdr temp))
                     (begin-slanted-font stream)
                     (format stream "?~A" temp)
                     (end-font stream)
                     (begin-regular-font stream)
                     (format stream " = ~A" (cadr bdp))
                     (end-font stream)
                     (do-newline stream)))))))

(setq *ordering-ptn* (ob$fcreate '(ORDERING)))

(defun ordering (context)
  (let ((found (cx$retrieve context *ordering-ptn*)))
    (if found
        (ob$get (car (car found)) 'value)
        (error "Ordering not found in ~A" context))))

(setq *altern-ptn* (ob$fcreate '(ALTERN)))

(defun altern? (context)
  (cx$retrieve context *altern-ptn*))

(defun set-altern (context)
  (cx$assert context *altern-ptn*))

(defun set-ordering (context value)
  (let ((found (cx$retrieve context *ordering-ptn*)))
    (if found
        (cx$retract context (car (car found))))
    (cx$assert context (ob$fcreate `(ORDERING value ,value)))))

(setq *other-ptn* (ob$fcreate '(OTHER)))

; Also want to allow nested planning stuff: the below only

(defun others (context)
 (append (map 'list (lambda (x) (ob$get (car x) 'actor))
            (cx$retrieve context *other-ptn*))
         (list *me-ob*)))

(defun ->belief-path (x)
  (if (eq? x *me-ob*)
      (list *me-ob*)
      (list x *me-ob*)))

;
; belief path       absolute                          relative
; -----------       --------                          --------
; (*me-ob*)             ?x                                ?x
; (*debra-ob* *me-ob*)      (Believe Debra ?x)                ?x
; (*me-ob* *debra-ob* *me-ob*)  (Believe Debra (Believe Me ?x))   ?x
;
; invariant: (tlast belief-path) = *me-ob*
;
; All things are asserted and retrieved in the database in absolute form.
; Planning and inferencing is done relatively.
;
(defun relative->absolute (con belief-path)
  (if (null? belief-path) (error "Bogus belief path ~A" belief-path))
  (if (null? (cdr belief-path))
      con
      (ob$fcreate `(BELIEVE
                      actor ,(car belief-path)
                      obj ,(relative->absolute con (cdr belief-path))))))

; Returns nil if no relative equivalent.
(defun absolute->relative (con belief-path)
  (if (null? (cdr belief-path))
      con
      (if (and (ty$instance? con 'believe)
               (eq? (car belief-path) (ob$get con 'actor)))
          (absolute->relative (ob$get con 'obj) (cdr belief-path))
          nil)))

(defun cx$retrieve-relative (self ob belief-path)
  (absolute-bd-list->relative
   (cx$retrieve self (relative->absolute ob belief-path))
   belief-path))

(defun cx$retrieve-bd-relative-ignore (self ob bd belief-path ignore)
  (absolute-bd-list->relative
   (cx$retrieve-bd-ignore self (relative->absolute ob belief-path) bd
                          ignore)
   belief-path))

(defun cx$retrieve-bd-ignore (self pattern bd ignore)
  (yloop (initial (result nil)
                 (bindings nil))
        (yfor ob in (if *hashing?*
                        (cx$retrieve-hash self pattern)
                        (ob$get self 'all-obs)))
        (ydo (setq bindings (ob$unify-cx1 pattern ob bd
                                        ignore self))
            (if bindings
                (setq result (cons (cons ob (cdr bindings)) result))))
        (yresult result)))

(defun cx$retrieve-bd-relative (self ob bd belief-path)
  (absolute-bd-list->relative
   (cx$retrieve-bd self (relative->absolute ob belief-path) bd)
   belief-path))

(defun cx$true-relative (self ob belief-path)
  (if (me-belief-path? belief-path)
      (cx$true? self ob)
      (absolute-bd-list->relative
       (cx$retrieve self (relative->absolute ob belief-path))
       belief-path)))

; Or keep mapping so we don't have to do a retrieve each time.
; Mapping is set up upon assert and destroyed upon retract.
; (But this doesn't work in the face of multiple contexts. Thus
; gc/maintenance problems make this infeasible.)
; Cannot use (cx$retract self) here as an optimization for
; me-belief-path case, because we want to delete things that
; unify with the ob.
(defun cx$retract-relative (self ob belief-path)
  (let ((found (cx$intern-relative self ob belief-path)))
       (ndbg-roman-nl *gate-dbg* rule-xtra "Interned ~A as ~A"
                      ob found)
     (if (null? found)
         (ndbg-roman-nl *gate-dbg* context
          "Retract-relative: Cannot intern ~A in ~A"
          ob self)
         (cx$retract self found))))

(defun cx$intern-relative (self ob belief-path)
  (cond
   ((me-belief-path? belief-path) ob)
   (else
    (any (lambda (belief-bd)
                 (if (eq? (absolute->relative (car belief-bd) belief-path) ob)
                     (car belief-bd)
                     nil))
         (cx$retrieve self (relative->absolute *any-pattern* belief-path))))))

(defun cx$hyper-retract-relative (self ob belief-path)
  (walkcdr (lambda (bp)
            (cx$retract self (relative->absolute ob bp)))
           belief-path))

(defun absolute-bd-list->relative (bd-list belief-path)
  (yloop (initial (relative nil)
                 (result nil))
        (yfor bd in bd-list)
        (ydo (setq relative (absolute->relative (car bd) belief-path))
            (if relative
                (setq result (cons (cons relative (cdr bd)) result))))
          ; actually, relative should always be true?
        (yresult result)))

(defun cx$assert-relative (self ob belief-path)
  (cx$assert self (relative->absolute ob belief-path)))

(defun cx$hyper-assert-relative (self ob belief-path)
  (walkcdr (lambda (bp)
            (cx$assert self (relative->absolute ob bp)))
           belief-path))

(defun imaginary? (top-level-goal)
  (eq? 'imaginary (ob$get top-level-goal 'planning-type)))

(defun real? (top-level-goal)
  (eq? 'real (ob$get top-level-goal 'planning-type)))

(defun imaginary-realistic? (top-level-goal)
  (and (eq? 'imaginary (ob$get top-level-goal 'planning-type))
       (ty$instance? (ob$get top-level-goal 'obj)
                     'realistic-goal-obj)))

(defun imaginary-fanciful? (top-level-goal)
  (and (eq? 'imaginary (ob$get top-level-goal 'planning-type))
       (ty$instance? (ob$get top-level-goal 'obj)
                     'fanciful-goal-obj)))

(defun realistic? (top-level-goal)
  (or (real? top-level-goal)
      (imaginary-realistic? top-level-goal)))

(defun top-goal? (goal context belief-path)
  (null? (get-links-from-relative goal *intends-ob* context belief-path)))

(defun goal-supergoal (goal context)
  (car (ol-get goal *intends-ob* 'backward context)))

(defun goal-supergoals (goal context belief-path)
  (if (null? goal)
      nil
      (cons goal
            (goal-supergoals (car (ol-get-relative
                              goal *intends-ob* 'backward context belief-path))
                              context belief-path))))

; Returns subgoals in order asserted. (That is, in regular forward order.)
(defun goal-subgoals (goal context belief-path)
  (sort (ol-get-relative goal *intends-ob* 'forward context belief-path)
        (lambda (subgoal1 subgoal2)
                (< (ob$pget subgoal1 '(obj plan-subgoalnum))
                   (ob$pget subgoal2 '(obj plan-subgoalnum))))))

(defun goal-subgoals-uo (goal context belief-path)
  (ol-get-relative goal *intends-ob* 'forward context belief-path))

(defun goal-subgoals-rule (goal context belief-path)
  (ob$get (car (get-links-relative goal *intends-ob* context belief-path))
          'rule))

(defun goal-subgoals-seq? (goal context)
  (ob$get (car (get-links goal *intends-ob* context))
          'seq?))

(defun goal-intends-links-uo (goal context belief-path)
  (get-links-relative goal *intends-ob* context belief-path))

;(defun goal-subgoals (goal context belief-path)
;  (map 'list (lambda (x) (ob$get x 'linked-to))
;       (get-links-relative goal *intends-ob* context belief-path)))

(defun rule-subgoal-objs (rule)
  (let ((rule-subgoal (ob$get rule 'subgoal)))
       (cond
        ((or (ty$instance? rule-subgoal 'rand)
             (ty$instance? rule-subgoal 'rseq))
         (ob$gets rule-subgoal 'obj))
        ((ty$instance? rule-subgoal 'ror)
         (error "rule-subgoal-objs: ROR not allowed")
         (list rule-subgoal))
        (else (list rule-subgoal)))))

; Set the below always to return T if you always want analogical
; filling in of details.
(defun analogy-instantiatible? (sval)
  (or (ty$instance? sval 'building)
      (ty$instance? sval 'location)
      (ty$instance? sval 'organization)))

(defun analogy-instantiatible1? (sval seren-ep)
  (or (ty$instance? sval 'building)
      (ty$instance? sval 'location)
      (ty$instance? sval 'organization)
      (and (or (ty$instance? sval 'person)
               (ty$instance? sval 'phys-obj))
           seren-ep)))

; Note this alg isn't exactly the one given in notes p. 43/7.
; plan-instantiate should be optimized not to perform an instantiation
; and replace if there are no variables inside; even better--if there are
; not any variables given values in bd inside. The former was done, but
; not the latter.
;
; - Modified the below to batch to a single plan-instantiate.
; - vars-ok? is currently used from rip.
;
(defun bd-special-append (target-bd source-bd target-goal target-context
                           belief-path top-level-goal vars-ok? ok-proc)
  (let ((val nil)
        (bd2 (bd-append target-bd source-bd))
        (accum-bds *empty-bd*)
        (sval nil))
       (bd-walk
        (lambda (var)
         (setq val (bd-hyper-lookup var bd2))
         (if (var? val)
             (progn
              (setq sval (bd-lookup (variable-name val) source-bd))
              (if (and sval
                       (apply ok-proc (list sval))
                       (or vars-ok? (not (var? sval))))
                  (progn
                   ; Todo: don't add var if it came from the rule only.
                   (setq accum-bds (bd-bind var sval accum-bds))
; was                   (setq target-bd (bd-unbind var target-bd))
                   (setq target-bd (bd-bind (variable-name val) sval target-bd))
                   )))))
        target-bd)
       (if (neq? accum-bds *empty-bd*)
           (progn
            (setq target-goal (plan-instantiate target-goal accum-bds
                                               target-context top-level-goal
                                               belief-path nil))
            (if (ob? target-goal)
                (cons target-goal (cdr (bd-append-ai target-bd source-bd)))
                (bd-append-ai target-bd source-bd)))
           (bd-append-ai target-bd source-bd))))

(defun bd-walk (proc bd)
  (map 'list (lambda (pair) (funcall proc (car pair)))
             (cdr bd)))

(defun bd-unbind (var bd)
  (cons (car bd)
        (del (lambda (x y) (eq? (car y) x))
             var (cdr bd))))

(defun make-seq (subgoals)
  (yloop
   (ywhile (cdr subgoals))
   (ydo (ob$add (car subgoals) 'seq-next (cadr subgoals))
       (setq subgoals (cdr subgoals)))))

(defun clear-seq (subgoals)
  (yloop
   (yfor subgoal in subgoals)
   (ydo (ob$removes subgoal 'seq-next))))

(defun var-value (var)
  (bd-lookup (variable-name var) *ob-bindings*))

(setq *ob-bindings* nil)

;
; Result of show:
; ((bd (from-ob from-ptn-ob)
;      (from-ob from-ptn-ob) ...)
;  ...)
;

;
; Doesn't do multi-level dependencies, but basically we never need them:
; (RAND (RAND a b) c) can be unfolded to (RAND a b c);
; (RAND (ROR a b) c) actually isn't a multi-level dependency
; (RAND (RNOT (RAND a b)) c) isn't so well-defined
; etc..
;
(defun show (ob context bd belief-path)
  (ndbg-roman-nl *gate-dbg* show "Show ~A in ~A" ob context)
  (let ((result (show1 ob context bd belief-path)))
    (if result
        (ndbg-roman-nl *gate-dbg* show "~A shown in ~A" ob context))
    result))

(ty$create 'NOT '() '(prop (obj) ()))
(setq *temp-not-ob* (ob$fcreate '(NOT)))

; So far, only used in minimization.
(defun negation-of (fact)
  (if (ty$instance? fact 'not)
      (ob$get fact 'obj)
      (ob$fcreate `(NOT obj ,fact))))

(setq *realistic-show-strength-thresh* 0.4)

(defun show1 (ob context bd belief-path)
  (cond
   ((ty$instance? ob 'rfalse) nil)
   ((ty$instance? ob 'rtrue)
    (list (list bd (list ob ob))))
   ((or (ty$instance? ob 'rand)
        (ty$instance? ob 'rseq))
    (show-all (ob$gets ob 'obj)
              context bd belief-path))
   ((ty$instance? ob 'rnot)
    (ob$set *temp-not-ob* 'obj (ob$get ob 'obj))
    ; First look for asserted NOTs.
    ; Todo: do we get into recursion problems here?
    (or (show1 *temp-not-ob* context bd belief-path)
        ; Then look for absense of assertion.
        (if (not (show (ob$get ob 'obj) context bd belief-path))
            (list (list bd (list ob ob)))
            nil)))
; In the latter case, make-dependency will instantiate and assert it
;        (let ((instan (ob$instantiate ob bd)))
;             (ob$set instan 'type *not-ob*)
;             ; make-dependency will assert it.
;             (list (list bd (list instan ob))))
   ((ty$instance? ob 'ror)
    (yloop (initial (shown? nil)
                   (result nil))
          (yfor elem in (ob$gets ob 'obj))
          (yuntil shown?)
          (ydo
           (setq shown? (show elem context bd belief-path))
           (if shown? (setq result (append! result shown?))))
          (yresult result)))
   (else
     (yloop
      (initial (result nil))
      (yfor retrieved in (cx$retrieve-bd-relative-ignore context
                                                        ob bd belief-path
                                                        '(linked-from-of
                                                          linked-to-of)))
      (ydo
       (if (or (null? *top-level-goal*)
               (not (realistic? *top-level-goal*))
               (ty$instance? (car retrieved) 'need)
               (ty$instance? (car retrieved) 'emotion)
               ; Why was it exactly that the below was needed?
               ; For what example?
               (fl> (strength (car retrieved))
                    *realistic-show-strength-thresh*))
           (setq result (cons (list retrieved (list (car retrieved) ob))
                             result))))
      (yresult result)))))

; ((bd (from-ob weight offset decay) (from-ob weight offset decay) ...) ...)

(defun show-all (obs context bd belief-path)
  (ndbg-roman-nl *gate-dbg* show "~&Show all ~A" obs)
  (let ((show-list (show (car obs) context bd belief-path)))
    (if (null? (cdr obs))
        show-list
        (yloop (yfor show1 in show-list)
              (initial (result nil))
              (ydo (yloop
                   (yfor show2 in (show-all (cdr obs) context
                                           (car show1) belief-path))
                   (ydo (setq result (append! result
                                            (list
                                             (cons (car show2)
                                                   (append (cdr show1)
                                                           (cdr show2)))))))))
              (yresult result)))))

; Todo: should most likely be relative to belief path, but this is gnarly.

(defun top-level-goal? (x)
  (and (ty$instance? x 'goal)
       (null? (ol-get x *intends-ob* 'backward *unify-context*)))) 

(defun tlg? (x cx)
  (and (ty$instance? x 'goal)
       (null? (ol-get x *intends-ob* 'backward cx))))

(defun prune (lst predicate)
  (yloop (initial (result nil))
        (yfor i in lst)
        (ydo (if (funcall predicate i)
                (setq result (cons i result))))
        (yresult result)))

(defun get-leaf-causes (fact context)
  (get-leafs fact *dependency-ob* 'backward context))

; Altern impl:
; BUT MUST RETURN *empty-bd* AS BELOW.
;(defun get-leaf-causes (fact context)
;  (ol-path fact nil *dependency-ob* 'backward
;           context (lambda (dummy ob) (leaf-fact? ob context)) nil))
;
;(defun leaf-fact? (ob context)
;  (null? (ol-get ob *dependency-ob* 'backward context)))

(defun get-result-emotions (fact context)
  (ol-path fact nil *dependency-ob* 'forward
           context (lambda (dummy ob)
                           (if (ty$instance? ob 'emotion)
                               *empty-bd*
                               nil))
           nil))

(defun get-leafs (ob link-type dir context)
  (let ((obs (ol-get ob link-type dir context)))
       (if (null? obs)
           (list ob)
           (yloop (initial (result nil))
                 (yfor w in obs)
                 (ydo (setq result (append (get-leafs w link-type dir context)
                                         result)))
                 (yresult result)))))

(setq *objects-in-omits* (cons 'plan-rule *link-slots*))

(defun objects-in (ob)
  (items-in ob 'varize-object? *objects-in-omits*))

(setq *visited-item-obs* nil)

(defun items-in (ob pred omit-slots)
  (setq *visited-item-obs* nil)
  (items-in1 ob pred omit-slots))

(defun items-in1 (ob pred omit-slots)
  (if (memq? ob *visited-item-obs*)
      nil
      (progn
       (setq *visited-item-obs* (cons ob *visited-item-obs*))
       (cond
        ((and (ob? ob) (ob$literal? ob)) nil)
        ((ob? ob)
         (yloop
          (initial (result nil))
          (yfor sv in (ob$pairs ob))
          (ydo
           (if (and (not (memq? (slots-name sv) omit-slots))
                    (not (cx? (slots-value sv))))
               (if (and (apply pred (list (slots-value sv)))
                        (not (memq? (slots-value sv) result)))
                   (setq result (cons (slots-value sv) result))
                   (setq result (union result
                                      (items-in1 (slots-value sv)
                                                 pred
                                                 omit-slots))))))
             (yresult result)))
        (else nil)))))

(defun subset? (set1 set2)
  (every? (lambda (x) (memq? x set2))
          set1))

(defun map-append (fn lst arg2)
  (yloop (initial (result nil))
        (yfor item in lst)
        (ydo (setq result (append (funcall fn item arg2) result)))
        (yresult result)))

(defun map-app (fn lst)
  (yloop (initial (result nil))
        (yfor item in lst)
        (ydo (setq result (append (funcall fn item) result)))
        (yresult result)))

(defun t-list-append! (old new)
  (if old
      (if new
          (cons 't (append! (cdr old) (cdr new)))
          old)
      new))

(defun me-in? (lst)
  (memq? *me-ob* lst))

(defun non-mes (lst)
  (yloop (initial (result nil))
        (ywhile lst)
        (ydo (if (neq? *me-ob* (car lst))
                (setq result (cons (car lst) result)))
            (setq lst (cdr lst)))
        (yresult result)))

(defun non-persons (lst person)
  (yloop (initial (result nil))
        (ywhile lst)
        (ydo (if (neq? person (car lst))
                (setq result (cons (car lst) result)))
            (setq lst (cdr lst)))
        (yresult result)))

(defun persons-in (lst)
  (yloop (initial (result nil))
        (yfor elem in lst)
        (ydo (if (and (ob? elem) (ty$instance? elem 'person))
                (setq result (cons elem result))))
        (yresult result)))

(defun non-persons-in (lst)
  (yloop (initial (result nil))
        (yfor elem in lst)
        (ydo (if (and (ob? elem)
                      (not (ty$instance? elem 'person)))
                (setq result (cons elem result))))
        (yresult result)))

(defun do-interest (proc)
  (funcall proc 'rule 'all)
;  (funcall proc 'rule-long 'all) ; interleaved but not activated processes.
;  (funcall proc 'rule-xtra 'all)
  (funcall proc 'task 'all)
  (funcall proc 'ep-store 'all) (funcall proc 'remind 'all)
  (funcall proc 'desire 'all) (funcall proc 'simil 'all)
  (funcall proc 'analogy 'all)
  (funcall proc 'night 'all)
;  (funcall proc 'seren-long 'all)
;  (funcall proc 'ri 'all)
;  (funcall proc 'chain 'all)
;  (funcall proc 'inference 'all)
;  (funcall proc 'depend 'all)
;  (funcall proc 'show 'all)
)

; End of file.
