;*******************************************************************************
;
; Daydreamer
; Version 3.5
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
;*******************************************************************************

(defmacro define-initial-fact (subsets spec)
  `(if (loadable-subsets? ',subsets)
       (let ((temp (ob$create ',spec)))
            (setq *initial-facts* (cons temp *initial-facts*))
            temp)
       nil))

;
; Episode definition
;
; (define-episode <subsets> <indices: ([ob-spec | obname] ...)> <plan-thresh>
;                 <reminding-thresh> (<goal> [<rulename> | nil] <recurse> ...))
;
; Todo: A define-episode pretty-printer (full obs are too cumbersome)
;
; Plan-thresh and reminding-thresh must include the rule index.
;
(defmacro define-episode (subsets indices
                               plan-thresh reminding-thresh
                               episode-defn)
  `(if (loadable-subsets? ',subsets)
       (let ((ep-context (cx$create))
             (ep nil)
             ; If indices are present, only the top-level goal of episode
             ; is retrievable.
             (hidden? (not (nil? ',indices)))
             (temp nil))
            (ndbg-roman-nl *gate-dbg* rule "Defining episode...")
            (setq ep (ob$get (episode-defn->stored-episode ',episode-defn
                                                           ep-context
                                                           hidden?)
                             'episode))
            (if hidden?
                (progn
                 (ob$set ep 'plan-threshold *infinite-thresh*)
                 (ob$set ep 'reminding-threshold 1)
                 (yloop (yfor index in ',indices)
                        (ydo (if (symbol? index)
                                 (setq temp (ob$name->ob index))
                                 (setq temp (ob$fcreate index)))
                             (if (null? temp)
                                 (progn
                                  (error "Trouble with defining ~A" index)
                                  (ndbg-roman-nl *gate-dbg* rule "Ignored."))
                                 (progn
                                  (epmem-store ep temp t t)))))))
            (if ,plan-thresh (ob$set ep 'plan-threshold ,plan-thresh))
            (if ,reminding-thresh
                (ob$set ep 'reminding-threshold ,reminding-thresh))
            ep)
       nil))

(defmacro with-no-dbg (&rest rest)
  `(unwind-protect
    (progn
     (do-interest #'disinterest)
     ,@rest)
    (do-interest #'interest)))

(defmacro self-type-ok? (rule self)
  `(or (null? (ob$gets ,rule 'self-type))
       (any? (lambda (x) (ty$instance-of? ,self x))
             (ob$gets ,rule 'self-type))))

(defmacro define-rule (name subsets spec)
  (if (loadable-subsets? subsets)
      (let ((rule (ob$name->ob name))
            (ruleob (ob$create spec)))
       (if rule
           (progn
;           (if (not (memq? rule *rules*))
;               (add-rule rule))
            (rule-destroy-chaining rule)
            (ob$remove-all rule)
            (ob$concatenate! rule ruleob)
            (rule-create-chaining rule)
            (ndbg-roman *gate-dbg* rule "~A redefined " name))
           (progn
            (if (nil? name)
                (progn
                 (setq rule (ob$create-empty))
                 (ob$concatenate! rule ruleob))
                (progn
                 (setq rule (ob$create-empty))
                 (ob$add-name rule name)
                 (ob$concatenate! rule ruleob)))
              (ob$set rule 'accessible? t)
            (add-rule rule)))
       (check-rule rule (ob$name rule))
       (list 'quote (ob$name rule)))
     (list 'quote 'rule-not-loaded)))

(defmacro possible-unify? (ob1 ob2)
  `(or (special? ,ob1)
       (special? ,ob2)
       (var? ,ob1)
       (var? ,ob2)
       (eq? (ob$ty ,ob1) (ob$ty ,ob2))))

(defmacro ri-pathelt-rule (x)
  `(car ,x))

(defmacro ri-pathelt-subgoalnum (x)
  `(cadr ,x))

(defmacro ri-pathelt-episodes (x)
  `(cddr ,x))

(defmacro ri-pathelt-make (rule subgoalnum episodes)
  `(cons ,rule (cons ,subgoalnum ,episodes)))

(defmacro chain-rule (x)
  `(car ,x))

(defmacro chain-num (x)
  `(cadr ,x))

(defmacro old-backward-chain-rules (goal-obj)
  `(if (ob$get ,goal-obj 'plan-rule)
       (ob$gets (ob$get ,goal-obj 'plan-rule) 'backward-chain)
       *rules*))

(defmacro backward-chain-rules (goal-obj)
  `(if (ob$get ,goal-obj 'plan-rule)
       (yloop (initial (result nil)
                       (subgoalnum (ob$get ,goal-obj 'plan-subgoalnum)))
              (yfor chain-num in (ob$gets (ob$get ,goal-obj 'plan-rule)
                                          'backward-chain-nums))
              (ydo (if (eq? subgoalnum (cadr chain-num))
                       (setq result (cons (car chain-num) result))))
              (yresult result))
       *rules*))

(defmacro forward-chain-rules (goal-obj)
  `(if (ob$get ,goal-obj 'inference-rule)
       (ob$gets (ob$get ,goal-obj 'inference-rule) 'forward-chain)
       *rules*))

(defmacro define-gen (type args . body)
  `(let ((ty (ob$name->ob ',type)))
     (if (null? ty)
         (format t "define-gen: unknown type: ~A~%" ',type)
         (ob$set ty
            'gen
            ,`(lambda (con stream switches context bp) ,@body)))))

(defmacro define-no-gen (type)
  `(ob$set (ob$name->ob ',type)
            'gen
            'no-gen))

(defmacro strength (ob)
  `(let ((found (ob$get ,ob 'strength)))
    (if (flonum? found)
        found
        *default-strength*)))

(defmacro set-strength (ob str)
  `(ob$set ,ob 'strength ,str))

; Doesn't affect linkages, so how does this offset really work
; in the long run!?
(defmacro offset-strength (ob offset)
  `(set-strength ,ob (fl+ ,offset (strength ,ob))))

(defmacro delay-dbgs (context . body)
  `(let ((string1 nil) (xxcontext ,context) (temp nil))
    (if *linearized?*
        (ndbg-roman-nl *gate-dbg* rule
         "Debugging being delayed for broadcast at a later time."))
    (setq string1
     (with-output-to-string (stream1)
      (let ((old-gate-dbg *gate-dbg*)
            (old-gen-stream *gen-stream*))
           (unwind-protect
            (progn
             (setq *gate-dbg* stream1)
             (setq *gen-stream* (make-gen-stream *gate-dbg*))
             (setq temp (progn ,@body)))
            (setq *gate-dbg* old-gate-dbg)
            (setq *gen-stream* old-gen-stream)))))
    (ob$set xxcontext 'sprout-trace (list string1))
    (if *linearized?* (ndbg-roman-nl *gate-dbg* rule "Debugging resumed."))
    (if (not *linearized?*) (cx$print-sprout-trace xxcontext))
    temp))

(defmacro no-gen (&rest body)
  `(let ((old *global-switches*)
         (temp nil))
     (setq *global-switches* (cons '(no-gen t) *global-switches*))
     (setq temp (progn ,@body))
     (setq *global-switches* old)
     temp))

(defmacro gen-future-assumption (&rest body)
  `(let ((old *global-switches*)
         (temp nil))
     (setq *global-switches*
          (cons '(tense past-subjunctive)
                (cons '(what-if t) *global-switches*)))
     (setq temp (progn ,@body))
     (setq *global-switches* old)
     temp))

(defmacro gen-past-assumption (&rest body)
  `(let ((old *global-switches*)
         (temp nil))
     (setq *global-switches*
          (cons '(tense past-perfect)
                (cons '(what-if t) *global-switches*)))
     (setq temp (progn ,@body))
     (setq *global-switches* old)
     temp))

(defmacro gen-relaxation (context . body)
  `(let ((old *global-switches*)
         (temp nil))
     (setq *global-switches*
          (cons (if (altern? ,context)
                    '(tense past-subjunctive)
                    '(tense present))
                (cons '(relaxation t) *global-switches*)))
     (setq temp (progn ,@body))
     (setq *global-switches* old)
     temp))

(defmacro me-belief-path? (x)
  `(null? (cdr ,x)))

(defmacro not-me-belief-path? (x)
  `(cdr ,x))

(defmacro define-phrase (pattern . concepts)
  `(progn (setq *phrases*
           (cons (cons ,pattern
            ',(map 'list (lambda (x) (ob$create x))
                    concepts))
            *phrases*))
           nil))

(defmacro candidate-create (rule bd episodes)
  `(cons ,rule (cons ,bd ,episodes)))

(defmacro candidate-rule (candidate)
  `(car ,candidate))

(defmacro candidate-bd (candidate)
  `(cadr ,candidate))

(defmacro candidate-episodes (candidate)
  `(cddr ,candidate))

(defmacro thresh (value threshold)
  `(if (fl< ,value ,threshold)
       0.0
       ,value))

(defmacro bd-append (bd1 bd2)
  `(cons 't (append (cdr ,bd1) (cdr ,bd2))))

(defmacro bd-append-ai (bd1 bd2)
  `(yloop (initial (result (cons 't (copy-list (cdr ,bd1)))))
          (yfor elem in (cdr ,bd2))
          (ydo (if (or (var? (cadr elem))
                       (analogy-instantiatible? (cadr elem)))
                   (setq result (append! result (list elem)))))
          (yresult result)))

; End of file.
