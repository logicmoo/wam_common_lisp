;*******************************************************************************
;
; Daydreamer
; Version 3.5
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
; 10/9/84:  Original generator written
;  2/8/86:  Started adding new generation code
; 9/24/86:  Took out flavor calls
; 11/7/86:  Started adding some new entries
;
;*******************************************************************************

(setq *references* nil)

(defun gn (con) (generate1 con *global-switches* *reality* *me-belief-path*))

(defun gns (con sw) (generate1 con sw *reality* *me-belief-path*))

; This function should be called only from assert.
(defun generate (con context switches)
 (if (and (null? (switches-lookup 'no-gen switches))
          (null? (no-top-gen? con context)))
     (progn
;      (setq *references* nil)
      (gs-reset-sentence *gen-stream*) ; just for good measure.
      (gs-string-write *gen-stream*
       "==================================================")
      (gs-newline *gen-stream*)
      (generate1 con switches context *me-belief-path*)
      (gs-string-write *gen-stream*
       "==================================================")
      (gs-newline *gen-stream*))))

(setq *no-top-gen-lst* (list ^rprox ^causal-link ^other ^ordering
                             ^altern ^at))

(defun no-top-gen? (con context)
  (and (ob? con)
       (not (ob$get con 'input-state?))
  (or (instance-of-any? con *no-top-gen-lst*)
      (action-goal-success? con)
      (believe-action? con)
      (believe-introduction? con)
      (believe-rprox? con)
      (believe-link? con)
      (know-location? con)
      (very-small-emotion? con)
      (rtrue-subgoal? con))))

(defun very-small-emotion? (ob)
  (and (ty$instance? ob 'emotion)
       (fl< (strength ob) 0.1)))

(defun instance-of-any? (ob lst)
  (any? (lambda (x) (ty$instance-of? ob x)) lst))

(defun believe-action? (con)
  (and (ty$instance? con 'BELIEVE)
       (ob? (ob$get con 'obj))
       (ty$instance? (ob$get con 'obj) 'ACTION)))

(defun action-goal-success? (con)
  (and (ty$instance? con 'SUCCEEDED-GOAL)
       (ob? (ob$get con 'obj))
       (ty$instance? (ob$get con 'obj) 'ACTION)))

(defun believe-introduction? (con)
  (and (ty$instance? con 'BELIEVE)
       (ob? (ob$get con 'obj))
       (ty$instance? (ob$get con 'obj) 'INTRODUCTION)))

(defun believe-link? (con)
  (and (ty$instance? con 'BELIEVE)
       (ob? (ob$get con 'obj))
       (ty$instance? (ob$get con 'obj) 'CAUSAL-LINK)))

(defun believe-rprox? (con)
  (and (ty$instance? con 'BELIEVE)
       (ob? (ob$get con 'obj))
       (ty$instance? (ob$get con 'obj) 'RPROX)))

(defun know-location? (con)
  (and (ty$instance? con 'KNOW)
       (ob? (ob$get con 'obj))
       (ty$instance? (ob$get con 'obj) 'LOCATION)))

(defun rtrue-subgoal? (con)
  (or (and (ty$instance? con 'ACTIVE-GOAL)
       (ob? (ob$get con 'obj))
           (ty$instance? (ob$get con 'obj) 'RTRUE))
      (and (ty$instance? con 'BELIEVE)
       (ob? (ob$get con 'obj))
           (ty$instance? (ob$get con 'obj) 'ACTIVE-GOAL)
       (ob? (ob$pget con '(obj obj)))
           (ty$instance? (ob$pget con '(obj obj)) 'RTRUE))))

(defun discourse-reset ()
 (setq *references* nil))

;
; Imagined future assumption === past-subjunctive
; "What if I went to the store?"
; Imagined future === conditional
; "I would go to the store"
; Imagined past assumption === past-perfect
; "What if I had gone to the store?"
; Imagined past === conditional-present-perfect
; "I would have gone to the store"
;

; The below doesn't work (?) because obs can change (or are they copied
; for context's sake?)
;
;(setq *gen-history* nil)
;
;(defun gen-history ()
;  (setq *references* nil)
;  (yloop (yfor item in (reverse *gen-history*))
;        (ydo (generate1 (car item)
;                       (cadr item)
;                       (caddr item)
;                       (cadddr item)))))

; Will add paragraph to English-only trace.
(defun gen-new-paragraph ()
  (setq *references* nil)
  nil)

(setq *possibly-realism* 0.3)

;   Perf   Dd
;   ------------------+
;   Maybe    Say      | Subgoal relaxation
;   Possibly Possibly | Plausible planning

(defun generate1 (con switches context bp)
  (let ((gened? nil))
;   (setq *gen-history* (cons (list con switches context bp) *gen-history*))
   (if *typeset?*
       (format *gate-dbg* "\\vspace{2 mm}~%"))
   (ndbg-large-bold-font *gate-dbg* rule)
   (gs-reset-sentence *gen-stream*)
   (gs-reset-line *gen-stream*)
   (if (and (< (strength con) *possibly-realism*)
            (not (ty$instance? con 'surprise))
            (not (ty$instance? con 'overall-emotion))
            (not (switches-lookup 'tongue-in-cheek switches)))
       (gs-string-write *gen-stream* " possibly"))
   (cond
    ((switches-lookup 'what-if switches)
     (gs-string-write *gen-stream* " what")
     (gs-string-write *gen-stream* " if")
     (if (setq gened? (gen con *gen-stream* switches context bp))
         (progn
          (justify-if-desired con *gen-stream* switches context bp)
          (gs-end-question *gen-stream*))))
    ((switches-lookup 'tongue-in-cheek switches)
     (gs-string-write *gen-stream* " anyway,")
     (if (setq gened? (gen con *gen-stream* switches context bp))
         (progn
          (justify-if-desired con *gen-stream* switches context bp)
          (gs-end-sentence *gen-stream*))))
    ((switches-lookup 'relaxation switches)
     (if (performance-mode?)
         (gs-string-write *gen-stream* " maybe")
         (gs-string-write *gen-stream* " say"))
     (if (setq gened? (gen con *gen-stream* switches context bp))
         (progn
          (justify-if-desired con *gen-stream* switches context bp)
          (gs-end-sentence *gen-stream*))))
    ((switches-lookup 'backtrack switches)
     (gs-string-write *gen-stream* " no")
     (setq gened? t)
;     (gs-end-sentence *gen-stream*)
;     (gs-string-write *gen-stream* " instead of")
;     (setq gened? (gen con *gen-stream* switches context bp))
;     (gs-string-write *gen-stream* ", how about this")
;     ; A justification will never be used here.
     (gs-end-sentence *gen-stream*))
    (else
     (if (setq gened? (gen con *gen-stream* switches context bp))
         (progn
          (justify-if-desired con *gen-stream* switches context bp)
          (if (ty$instance? con 'surprise)
              (gs-end-exclam *gen-stream*)
              (gs-end-sentence *gen-stream*))))))
    (ndbg-end-font *gate-dbg* rule)
    (if gened?
        (progn
         (if *typeset?*
             (format *gate-dbg* "\\vspace{2 mm}~%"))
         (if (null? (ob$get context 'first-gened-concept))
             (ob$set context 'first-gened-concept con))))
    (if (and nil ; (ty$instance? con 'emotion) No more overall gen
             (not (ty$instance? con 'surprise))
             (not (ty$instance? con 'overall-emotion)))
        (gen-overall-emot-state))))

(defun justify-if-desired (con stream switches context bp)
  (if (switches-lookup 'justify? switches)
      (justify con stream switches context bp)))

(defun justify (con stream switches context bp)
  (let ((causes 
         (prune
          (get-leaf-causes con context)
          (lambda (x) (ob$ty x)))))
       (if (and causes (neq? (car causes) con))
           (progn
            (gs-string-write stream " because")
            (generate-list causes stream switches context bp)))))

(defun generate-list (lst stream switches context bp)
  (yloop
   (ywhile lst)
   (ydo (gen (car lst) stream switches context bp)
       (setq lst (cdr lst))
       (if lst (gs-string-write stream " and")))))

;(use-font *dd-output* (load-font *dd-output* "/sys/dm/fonts/std.19l"))

(defun name-is? (ob name)
  (and (ob? ob) (eq? name (ob$name ob))))

(defun get-gen-proc (type)
  (or (ob$get type 'gen)
      (and (ob$gets type 'isa)
           (get-gen-proc (ob$get type 'isa)))))

(defun gen (con stream switches context bp)
 (prog1
  (cond
   ((var? con)
    (gen-variable con stream switches context bp))
   ((ob? con)
    (let ((gen-proc (get-gen-proc (ob$ty con))))
      (cond
       ((eq? gen-proc 'no-gen) nil) ; here we wanted to flush up to top level
       (gen-proc
         (funcall gen-proc con stream switches context bp))
       (else (gen-unknown con stream)))))
   ((and (pair? con) (null? (cdr con)))
    (gen (car con) stream switches context bp))
   ((pair? con)
    (if (memq? *me-ob* con)
        (progn
         (yloop (initial (others (non-mes con)))
               (ywhile others)
               (ydo (gen (car others) stream switches context bp)
                   (setq others (cdr others))))
         (gs-string-write stream " and")
         (gen *me-ob* stream switches context bp))
        (yloop (ywhile con)
              (ydo (gen (car con) stream switches context bp)
                  (setq con (cdr con))
                  (if (and con
                           (null? (cdr con)))
                      (gs-string-write stream " and")))))
     con)
   (else (gen-unknown con stream)))
  (if (and (ob? con)
           (ty$instance? con 'object)
           ; For now, the below prevents "I want to be
           ; going out with this person." Originally,
           ; this was used for action mutation generation.
           (not (exemplar? con))
           (neq? con *me-ob*)
           (not (memq? con *references*)))
          (setq *references*
           (cons con (del-old-ref *references*
                                  con))))))

(defun del-type (lst typ)
  (yloop (initial (result nil))
        (yfor item in lst)
        (ydo (if (neq? (ob$ty item) typ)
                (setq result (cons item result))))
        (yresult result)))

(defun del-old-ref (lst con)
  (let ((typ (cond
              ((ty$instance? con 'male-person)
               *male-person-ob*)
              ((ty$instance? con 'female-person)
               *female-person-ob*)
              (else nil))))
     (if typ
         (yloop (initial (result nil))
               (yfor item in lst)
               (ydo (if (not (ty$instance-of? item typ))
                       (setq result (cons item result))))
               (yresult result))
         (del-type lst (ob$ty con)))))

(defun gen-unknown (con stream)
  (gs-string-write stream " something")
  (if (not *typeset?*)
      (format stream " (~A)" (if (ob? con)
                                 (ob$name con)
                                 con)))
  t)

(defun gen-variable (var stream switches context bp)
  (let ((typ (variable-type var)))
       (if (ob? typ)
           (gen (ob$get typ 'exemplar) stream switches context bp)
           (gen-unknown var stream))
       t))

(defun exemplar? (ob)
  (and (ob$get ob 'type)
       (eq? ob (ob$get (ob$get ob 'type) 'exemplar))))

; Todo:
; - Allow flagging of subgoals not to be generated.
; - Add lookup for locations
; - Return subject in gen to enable NP-deletion below in 'by' phrases.
; - Diff btwn gening, say RPROX infs and RPROXs otherwise, say nested
;   in ep.
(define-gen EPISODE nil
  (let ((goal (ob$get con 'goal)))
     (gen-subgoals goal stream switches context bp t)
     *me-ob*))

(defun genable-subgoals (goal context belief-path)
  (let ((links (get-links-relative goal *intends-ob*
                                   context belief-path))
        (rule nil)
        (plan-no-gen nil))
    (if links
        (progn
         (setq rule (ob$get (car links) 'rule))
         (setq plan-no-gen (ob$get rule 'plan-no-gen))
         (yloop
          (initial (result nil)
                   (gen? t))
          (yfor link in links)
          (ydo
           (if plan-no-gen
               (progn
                (setq gen? (not (car plan-no-gen)))
                (setq plan-no-gen (cdr plan-no-gen))))
           (if gen?
               (setq result
                    (append! result
                             (list (ob$get link 'linked-to))))))
          (yresult result)))
        nil)))

(defun gen-subgoals (con stream switches context bp top?)
  (let ((subgoals (genable-subgoals con context bp))
        (subj nil))
   (if subgoals
       (progn
        (if (null? top?)
            (gs-end-sentence stream)
            (progn
             (gs-string-write stream " I")
             (gs-string-write stream " remember the time")))
        (setq subj
             (gen (ob$get con 'obj)
                  stream
                  (cons '(tense past) switches) context bp))
        (gs-string-write stream " by")
        (yloop
         (initial (rest subgoals))
         (ywhile rest)
         (ydo (gen (ob$get (car rest) 'obj)
               stream
               (if subj
                   (cons (list 's-bar subj)
                         (cons '(tense gerund) switches))
                   (cons '(tense gerund) switches))
               context bp)
          (setq rest (cdr rest))
          (if rest
              (if (null? (cdr rest))
                  (gs-string-write stream ", and by")
                  (gs-string-write stream ", by")))))
                (yloop (yfor subgoal in subgoals)
                      (ydo (gen-subgoals subgoal stream switches context
                                        bp nil)))))))

(define-gen RPROX nil
  (let ((subject (people-or-orgs (ob$gets con 'actor))))
     (gen-subject subject stream switches context bp)
     (gen-verb 'be subject stream switches (neg? con))
     (gs-string-write stream " in")
     (gen (ob$gets con 'location)
          stream switches context bp)
     subject))

(defun polities (lst)
  (yloop (initial (result nil))
        (ywhile lst)
        (ydo (if (ty$instance? (car lst) 'polity)
                (setq result (cons (car lst) result)))
            (setq lst (cdr lst)))
        (yresult result)))

(setq *neg-ob* (ob$add-name (ob$create-empty) 'neg))

(defun neg? (con)
  (eq? *neg-ob* (ob$get con 'mode)))

(defun make-negative (con)
  (let ((newcon (ob$copy con)))
    (ob$add newcon 'mode *neg-ob*)
    newcon))

(define-gen NOT (con stream switches)
  (let ((newcon (make-negative (ob$get con 'obj)))
        (result nil))
       (setq result (gen newcon stream switches context bp))
       result))

(defun lovers-goal? (goal)
  (ty$instance? (ob$get goal 'obj) 'LOVERS))

(defun friends-goal? (goal)
  (ty$instance? (ob$get goal 'obj) 'FRIENDS))

(defun goal-car (x)
  (if (pair? x)
      (if (ty$instance? (car x) 'goal)
          (car x)
          nil)
      nil))

(defun gen-gratitude-anger (con stream switches context bp subject
                             scale toward)
  (if subject
      (progn
       (gen-subject subject stream switches context bp)
       (gen-verb 'feel subject stream switches (neg? con))))
  (gen-scale scale stream)
  (if (ty$instance? con 'NEG-EMOTION)
      (gs-string-write stream " angry at")
      (gs-string-write stream " grateful to"))
  (gen toward stream switches context bp))

(defun gen-pos-neg-emot-string (con stream switches context bp subject scale
                                 pos-string neg-string)
  (if subject
      (progn
       (gen-subject subject stream switches context bp)
       (gen-verb 'feel subject stream switches (neg? con))))
  (gen-scale scale stream)
  (if (ty$instance? con 'NEG-EMOTION)
      (gs-string-write stream neg-string)
      (gs-string-write stream pos-string)))

(defun gen-emot-string (con stream switches context bp subject
                         scale emot-string )
  (if subject
      (progn
       (gen-subject subject stream switches context bp)
       (gen-verb 'feel subject stream switches (neg? con))))
  (gen-scale scale stream)
  (gs-string-write stream emot-string))

(setq *overall-emotional-state* 0.0)
(setq *as-well-as?* nil)


(define-gen EMOTION nil
  (let ((subject (generate-emotion con stream switches context bp t t))
        (neg-total 0.0) (pos-total 0.0))
       (yloop (yfor emot in *emotions*)
             (ydo (if (ty$instance? emot 'neg-emotion)
                     (setq neg-total (fl+ neg-total (strength emot)))
                     (setq pos-total (fl+ pos-total (strength emot))))))
       (setq *overall-emotional-state* (fl- pos-total neg-total))
       (if *as-well-as?*
           (yloop
            (initial (emot nil) (first? t)
                     (elems 
                      (prune *emotions*
                       (lambda (x) (and (neq? x con)
                                        (fl> (strength x)
                                             *genable-emot-thresh*))))))
            (ywhile elems)
            (ydo
             (setq emot (car elems))
             (setq elems (cdr elems))
             (if first?
                 (progn
                  (setq first? nil) (gs-string-write stream " as well as")))
             (generate-emotion emot stream switches *reality* bp nil t)
             (if elems (gs-string-write stream ","))
             (if (and elems (null? (cdr elems)))
                 (gs-string-write stream " and")))))
       subject))

(setq *overall-con* (ob$fcreate '(OVERALL-EMOTION)))

(defun gen-overall-emot-state ()
  (ob$set *overall-con* 'scale *overall-emotional-state*)
  (generate1 *overall-con* *global-switches* *reality* *me-belief-path*))

(define-gen OVERALL-EMOTION nil
  (gs-string-write stream " overall,")
  (let ((scale (ob$get con 'scale)))
       (gs-string-write stream " I feel")
       (cond
        ((fl> scale 2.0)
         (gs-string-write stream " ecstatic"))
        ((fl> scale 1.0)
         (gs-string-write stream " happy"))
        ((fl> scale 0.25)
         (gs-string-write stream " fine"))
        ((fl> scale -1.0)
         (gs-string-write stream " ill at ease"))
        ((fl> scale -2.0)
         (gs-string-write stream " upset"))
        (else
         (gs-string-write stream " miserable")))
       *me-ob*))

(defun generate-emotion (con stream switches context bp with-subject? verbose?)
  (let ((scale (ob$get con 'strength))
        (subject (if with-subject? (car bp) nil))
        (toward (ob$get con 'to))
        (to-goal (goal-car (ol-get con *dependency-ob* 'forward context)))
        (from-goal (goal-car (ol-get con *dependency-ob* 'backward context))))
   (cond
    ((ty$instance? con 'surprise)
     (gs-string-write stream " What do you know")
;     (gen-emot-string con stream switches context bp subject
;                      (fl+ *less-than-norm* .1)
;                      " surprised")
    )
    ((ob$get con 'altern?)
     (gen-pos-neg-emot-string con stream switches context bp subject scale
                       " relieved" " regretful")
     (gen-emot-verbosity from-goal stream switches context bp subject " about"
                         verbose?))
    ((and to-goal (null? from-goal))
     (gen-emot-string con stream switches context bp subject scale
                      " interested")
     (gen-emot-verbosity to-goal stream switches context bp subject " in"
                         verbose?))
    ((and to-goal from-goal (imagined-outcome? from-goal))
     (gen-pos-neg-emot-string con stream switches context bp subject scale
                       " hopeful" " worried")
     (gen-emot-verbosity from-goal stream switches context bp subject " about"
                         verbose?))
    ((and toward from-goal (social-esteem-goal? from-goal))
     (gen-pos-neg-emot-string con stream switches context bp subject scale
                       " proud" " humiliated"))
    (toward
     (gen-gratitude-anger con stream switches context bp subject scale toward))
    ((and from-goal (social-esteem-goal? from-goal))
     (gen-pos-neg-emot-string con stream switches context bp subject scale
                       " poised" " embarrassed"))
    ((and from-goal (self-esteem-goal? from-goal))
     (gen-pos-neg-emot-string con stream switches context bp subject scale
                       " proud" " ashamed"))
    ((and from-goal (ty$instance? con 'neg-emotion) (lovers-goal? from-goal)
          (existing-relationship1? (ob$get from-goal 'obj)
                                   (or (ob$get from-goal 'activation-context)
                                       (cx$parent context))))
     (gen-emot-string con stream switches context bp subject scale
                      " broken hearted"))
    ((and from-goal (ty$instance? con 'neg-emotion)
          (or (lovers-goal? from-goal) (friends-goal? from-goal)))
     (gen-emot-string con stream switches context bp subject scale " rejected"))
    ((and from-goal
          (ty$instance? (ob$get from-goal 'obj) 'entertainment)
          (ty$instance? con 'pos-emotion))
     (gen-emot-string con stream switches context bp subject scale " amused"))
    ((and from-goal (ty$instance? (ob$get from-goal 'obj) 'food))
     (gen-pos-neg-emot-string con stream switches context bp subject scale
                              " satiated" " starved"))
    (else  
     (gen-pos-neg-emot-string con stream switches context bp subject scale
                       " pleased" " displeased")
     (gen-emot-verbosity from-goal stream switches context bp subject " about"
                         verbose?)
     ))
   subject))

(defun imagined-outcome? (goal)
  (dd-goal? (ob$get goal 'top-level-goal)))

(defun gen-emot-verbosity (goal stream switches context bp subject prep
                            verbose?)
  (if (and verbose? goal)
      (progn
       (gs-string-write stream prep)
       (gen (if (or (ty$instance? goal 'succeeded-goal)
                    (ty$instance? goal 'active-goal))
                (ob$get goal 'obj)
                goal)
            stream
            (cons (list 's-bar subject)
                  (cons '(tense gerund) switches))
            context bp))))

(setq *less-than-norm* .3)

(setq *greater-than-norm* .7)

(defun gen-scale (scale stream)
  (if scale
  (cond
   ((< scale *less-than-norm*)
    (gs-string-write stream " a bit")) ; also slightly
   ((> scale *greater-than-norm*)
    (gs-string-write stream " really"))) ; also very
      nil))

(define-gen WAIT nil
  (let ((subject (car bp)))
       (gen-subject subject stream switches context bp)
       (gen-verb 'wait subject stream switches (neg? con))
       subject))

(define-gen POS-ATTITUDE nil
  (let ((subject (car bp))
        (obj (ob$get con 'obj)))
    (if nil ; (ty$instance? obj 'person)
        (progn
         ; 964.4 (Roget's Fourth Edition)
         (gen-subject subject stream switches context bp)
         (gen-verb 'think subject stream switches (neg? con))
         ; This is too strong. Maybe " appreciates"
         (gs-string-write stream " highly of")
         (gen obj stream switches context bp)
         subject)
        (progn
         (gen-subject subject stream switches context bp)
         (gen-verb 'like subject stream switches (neg? con))
         (gen obj stream switches context bp)
         subject))))

(define-gen NEG-ATTITUDE nil
  (let ((subject (car bp))
        (obj (ob$get con 'obj)))
    (if (ty$instance? obj 'person)
        (progn
         (gen-subject subject stream switches context bp)
         (gen-verb 'think subject stream switches (not (neg? con)))
         (gs-string-write stream " much of")
         (gen obj stream switches context bp)
         subject)
        (progn
         (gen-subject subject stream switches context bp)
         (gen-verb 'dislike subject stream switches (neg? con))
         (gen obj stream switches context bp)
         subject))))

(define-gen ROMANTIC-INTEREST nil
  (let ((subject (car bp)))
    (gen-subject subject stream switches context bp)
    (gen-verb 'be subject stream switches (neg? con))
    (gs-string-write stream " interested in")
    (gen (ob$get con 'obj) stream switches context bp)
    subject))

(defun gen-regular (con stream switches context bp verb str)
  (let ((subject (ob$gets con 'actor)))
    (gen-subject subject stream switches context bp)
    (gen-verb verb subject stream switches (neg? con))
    (if str (gs-string-write stream str))
    subject))

(define-gen RICH nil
  (gen-regular con stream switches context bp 'be " rich"))

(defun gen-regular-plus (con stream switches context bp verb str plus)
  (let ((subject (ob$gets con 'actor)))
    (gen-subject subject stream switches context bp)
    (gen-verb verb subject stream switches (neg? con))
    (if plus
        (if (and str
                 (is-particle? str)
                 (null? (cdr subject))
                 (memq? (car subject) *references*))
            (progn
             (gen plus stream switches context bp)
             (gs-string-write stream str))
            (progn
             (if str (gs-string-write stream str))
             (gen plus stream switches context bp))))
    subject))

(defun is-particle? (str)
  (or (string-equal? str " up")
      (string-equal? str " on")))

(define-gen POSS nil
  (if (switches-lookup 'active-goal? switches)
      (gen-regular-plus con stream switches context bp 'get nil
                        (ob$get con 'obj))
      (gen-regular-plus con stream switches context bp 'have nil
                        (ob$get con 'obj))))

(define-gen WEARING nil
  (gen-regular-plus con stream switches context bp 'wear nil
                    (ob$get con 'obj)))

(define-gen STAR nil
  (if (and (ob$get con 'level)
           (ty$instance? (ob$get con 'level) 'greater-than))
      ; Todo: Look in context for who it is that this level is greater
      ; than.
      (gen-regular con stream switches context bp 'be
                   " a star even more famous than he is")
      (gen-regular con stream switches context bp 'be " a movie star")))

; 231.45 and 644.13 (Roget's Fourth Edition)
(define-gen WELL-DRESSED nil
  (gen-regular con stream switches context bp 'be " dressed to kill"))

(define-gen EXECUTIVE nil
  (gen-regular con stream switches context bp 'be " a powerful executive"))

(define-gen UNDER-DOORWAY nil
  (gen-regular con stream switches context bp 'be " under a doorway"))

(define-gen HURT nil
  (gen-regular con stream switches context bp 'be " injured"))

(define-gen INSURED nil
  (gen-regular con stream switches context bp 'be " insured"))

(define-gen FASHIONABLE-CLOTHES nil
 (gs-string-write stream " my cute outfit")
 t)

(define-gen FRIDAY-NIGHT nil
 (gs-string-write stream " it is friday night")
 t)

(define-gen TIME-OF-DAY nil
 (gs-string-write stream " the time")
 t)

(define-gen ATTRACTIVE nil
  (gen-regular con stream switches context bp 'be " cute"))

(define-gen M-WALK-ON-PATROL nil
  (gen-regular con stream switches context bp 'walk " on patrol"))

(define-gen M-RETURN-TO-STATION nil
  (gen-regular con stream switches context bp 'return " to the station"))

(define-gen ACADEMIC-SUCCESS nil
  (gen-regular con stream switches context bp 'be " an academic success"))

(define-gen M-COURSE nil
  (gen-regular con stream switches context bp 'take " the course"))

(define-gen M-RANGER nil
  (gen-regular con stream switches context bp 'be " a ranger"))

(define-gen ENTERTAINMENT nil
  (gen-need-obj con stream switches context bp 'be " entertained"))

(define-gen MONEY nil
  (gen-need-obj con stream switches context bp 'have " enough money"))

(defun gen-need-obj (con stream switches context bp verb str)
  (let ((subject (car bp)))
    (gen-subject subject stream switches context bp)
    (gen-verb verb subject stream switches (neg? con))
    (gs-string-write stream str)
    subject))

(define-gen POSSESSIONS nil
  (let ((subject (car bp)))
    (gen-subject subject stream switches context bp)
    (gen-verb 'keep subject stream switches (neg? con))
    (gen subject stream (cons '(case possessive) switches) context bp)
    (gs-string-write stream " belongings")
    subject))

(setq *gen-thats* nil)

(define-gen BELIEVE nil
  (let ((obj (ob$get con 'obj))
        (subject (ob$get con 'actor)))
    (if (or (and (ty$instance? obj 'MENTAL-STATE)
                 (not (ty$instance? obj 'KNOW))
                 (not (ty$instance? obj 'BELIEVE)))
            (ty$instance? obj 'NOT)) ; this is not currently used.
        (progn
         
         (if (neg? con) ; Must propagate negation.
             (gen (make-negative obj)
                  stream switches context (cons subject bp))
             (gen obj stream switches context (cons subject bp)))
         (if (ty$instance? obj 'attitude)
             (justify con stream switches context bp)))
        (progn
         (gen-subject subject stream switches context bp)
         (if (ty$instance? (ob$get con 'obj) 'attractive)
             (gen-verb 'think subject stream switches (neg? con))
             (gen-verb 'believe subject stream switches (neg? con)))
         (if *gen-thats* (gs-string-write stream " that"))
         (gen (ob$get con 'obj)
              stream (simplify-tense switches) context (cdr bp))))
    subject))

(define-gen MOVIE nil
  (gs-string-write stream " a movie")
  t)

(define-gen LOVERS nil
  (gen-split-subject con stream (progressivize switches)
                     context bp 'go " out with"))

(define-gen ACQUAINTED nil
  (gen-regular con stream switches context bp 'be " acquainted"))

(define-gen M-PHONE nil
  (gen-regular-plus con stream switches context bp 'call nil
                    (ob$get con 'to)))

(define-gen M-PUTON nil
  (gen-regular-plus con stream switches context bp 'put " on"
                    (ob$get con 'obj)))

(define-gen M-LOGIN nil
  (gen-regular-plus con stream switches context bp 'log " into"
                    (ob$get con 'obj)))

(define-gen M-MOVIE nil
  (if (switches-lookup 'active-goal? switches)
      (gen-regular con stream switches context bp 'go " see a movie")
      (gen-regular con stream (cons '(tense present-perfect) switches)
                   context bp 'go " to see a movie")))

(define-gen MTRANS-ACCEPTABLE nil
  (if (switches-lookup 'active-goal? switches)
      (gen-split-subject con stream switches context bp 'break " the ice with")
      (gen-split-subject con stream (cons '(tense present-perfect) switches)
                         context bp 'break " the ice with")))

(define-gen M-AGREE nil
  (let ((subject (ob$gets con 'actor)))
    (gen-subject subject stream switches context bp)
    (gen-verb 'agree subject stream switches (neg? con))
    (gen (ob$get con 'obj)
         stream
         (cons (list 's-bar 'no-subject) (cons '(tense infinitive) switches))
         context bp)
    subject))

(define-gen M-RESTAURANT nil
  (let ((subject (gen-split-subject con stream switches context bp
                                    'have " dinner with")))
       (gs-string-write stream " at a restaurant")
       subject))

(define-gen M-CONVERSATION nil
  (gen-split-subject con stream switches context bp
                     'have " a conversation with"))

(define-gen M-DATE nil
  (gen-regular con stream switches context bp 'go " out on a date"))

(define-gen M-STUDY nil
  (if (ty$instance? (ob$get con 'obj) 'actor)
      (gen-regular con stream switches context bp 'study " to be an actor")
      (gen-regular-plus con stream switches context bp 'study " to be"
                        (ob$get con 'obj))))

(define-gen M-BEAT-UP nil
  (gen-regular-plus con stream switches context bp 'beat " up"
                    (ob$get con 'obj)))

(define-gen SELLS nil
  (gen-regular-plus con stream switches context bp 'sell nil
                    (ob$get con 'obj)))

; 554.6,8 (Roget's Fourth Edition)
; 13.4 (Roget's Fourth Edition) "each other"
(define-gen ENABLE-FUTURE-VPROX nil
  (gen-regular con stream switches context bp 'be
               " able to contact each other")) ; also one another

(define-gen M-KISS nil
  (gen-regular con stream switches context bp 'kiss nil))

(define-gen M-BREAK-UP nil
  (let ((subject
         (gen-regular con stream switches context bp 'break " up with")))
       (if (ty$instance? (car subject) 'male-person)
           (gs-string-write stream " his girlfriend")
           (gs-string-write stream " her boyfriend"))
       subject))

(define-gen DATING-SERVICE-MEMBER nil
  (gen-regular con stream switches context bp 'be
               " a member of the dating service"))

(define-gen M-WORK nil
  (gen-regular con stream switches context bp 'work nil))

; Maybe add past subjunctive and conditional here.
(define-gen LEADTO nil
  (let ((ante (ob$get con 'ante))
        (conseq (ob$get con 'conseq))
        (subject nil))
    (setq subject (gen ante stream
                  (cons '(possessive-subj t)
                         (cons '(tense gerund) switches))
                  context bp))
    (gen-verb 'lead ante stream switches (neg? con))
    (gs-string-write stream " to")
    (gen conseq stream
         (cons '(tense gerund) switches)
         context bp)
    subject))

;(defun gen-pu-mixed-blessing (con stream switches)
;  (gen (ob$get con 'plus)
;       stream
;       (cons (list 's-bar (ob$gets con 'char1))
;             (cons '(tense gerund) switches)))
;  (gs-string-write stream " leads to")
;  (gen (ob$get con 'minus) stream
;       (cons (list 's-bar (ob$gets con 'char1))
;             (cons '(tense gerund) switches))))

;(defun gen-pu-denied-request (con stream switches)
;  (gen (ob$get con 'action) stream switches))

;(defun gen-pu-retaliation (con stream switches)
;  (gen-subject (ob$gets con 'char1) stream switches)
;  (gen-verb 'get (list (ob$gets con 'char1))
;                 stream switches (neg? con))
;  (gs-string-write stream " back at")
;  (gen (ob$get con 'char2) stream switches)
;  (gs-string-write stream " by")
;  (gen (ob$get con 'action) stream
;       (cons (list 's-bar (ob$gets con 'char1))
;             (cons '(tense gerund) switches))))

(define-gen RATIONALIZATION nil
  (let ((subject (list (car bp))))
   (gen-subject subject stream switches context bp)
   (gen-verb 'rationalize subject stream switches (neg? con))
   (gen (ob$get con 'obj)
        stream
        (cons (list 's-bar subject) (cons '(tense gerund) switches))
        context bp)
   (if (ob$get con 'reason)
       (progn
        (gs-string-write stream " by the fact that")
        (gen (ob$get con 'reason) stream switches context bp)))
  subject))

(define-gen RECOVERY nil
  (let ((subject (list (car bp))))
   (gen-subject subject stream switches context bp)
   (gen-verb 'recover subject stream switches (neg? con))
   (gs-string-write stream " from")
   (gen (ob$get con 'obj)
        stream
        (cons (list 's-bar subject) (cons '(tense gerund) switches))
        context bp)
   subject))

(define-gen REPERCUSSIONS nil
  (let ((subject (list (car bp))))
   (gen-subject subject stream switches context bp)
   (gen-verb 'consider subject stream switches (neg? con))
   (gen (ob$get con 'obj)
        stream
        (cons (list 's-bar subject) (cons '(tense gerund) switches))
        context bp)
   subject))

(define-gen HYPOTHESIZE nil
  (let ((subject (list (car bp))))
   (gen-subject subject stream switches context bp)
   (gen-verb 'imagine subject stream switches (neg? con))
   (gen (ob$get con 'state)
        stream
        (cons (list 's-bar subject) (cons '(tense gerund) switches))
        context bp)
   subject))

(define-gen LIST nil
  (gen (ob$get con 'first) stream switches context bp)
  (if (not (ty$instance? (ob$get con 'rest) 'list))
      (gs-string-write stream ", and")
      (gs-string-write stream ","))
  (gen (ob$get con 'rest) stream switches context bp)
  t)

(define-gen REVERSAL nil
  (let ((subject (list (car bp))))
   (gen-subject subject stream switches context bp)
   (gen-verb 'reverse subject stream switches (neg? con))
   (gen (ob$get con 'obj)
        stream
        (cons (list 's-bar subject) (cons '(tense gerund) switches))
        context bp)
   subject))

(define-gen UNDO-CAUSES nil
  (let ((subject (list (car bp))))
   (gen-subject subject stream switches context bp)
   (gen-verb 'undo subject stream switches (neg? con))
   (gen (ob$get con 'obj)
        stream
        (cons (list 's-bar subject) (cons '(tense gerund) switches))
        context bp)
   subject))

(define-gen ROVING nil
  (let ((subject (list (car bp))))
   (gen-subject subject stream switches context bp)
   (gen-verb 'think subject stream switches (neg? con))
   (gs-string-write stream " about something else")
   subject))

(define-gen REVENGE nil
  (let ((subject (list (car bp))))
   ; 956.4 (Roget's Fourth Edition)
   (gen-subject subject stream switches context bp)
   (gen-verb 'get subject stream switches (neg? con))
   (gs-string-write stream " even with")
   (if (ob$gets con 'to)
       (gen (ob$gets con 'to) stream switches context bp))
;   Could use get-action-causes to generate the below.
;   (gs-string-write stream " for")
;   (gen (ob$get con 'obj)
;        stream
;        (cons (list 's-bar subject) (cons '(tense gerund) switches))
;        context bp)
   subject))

(define-gen ADVANTAGE nil
  (let ((subject (list (car bp))))
   ; 956.4 (Roget's Fourth Edition)
   (gen-subject subject stream switches context bp)
   (gen-verb 'be subject stream switches (neg? con))
   (gs-string-write stream " in a position of advantage over")
   (if (ob$gets con 'obj)
       (gen (ob$gets con 'obj) stream switches context bp))
   subject))

(define-gen GROUP nil
  (let ((subject (list (car bp))))
   (gen-subject subject stream switches context bp)
   (if (ob$gets con 'nonmembers)
       (progn
        (gen-verb 'exclude subject stream switches (neg? con))
        (gen (ob$gets con 'nonmembers) stream switches context bp)
        (gs-string-write stream " from the group"))
       (progn
        (gen-verb 'be subject stream switches (neg? con))
        (gs-string-write stream " in a group")))
   subject))

(define-gen WELL-PREPARED nil
  (let ((subject (list (car bp))))
   ; 956.4 (Roget's Fourth Edition)
   (gen-subject subject stream switches context bp)
   (gen-verb 'be subject stream switches (neg? con))
   (gs-string-write stream " well prepared")
   (if (and (ob$gets con 'to)
            (not (var? (ob$get con 'to))))
       (progn
        (gs-string-write stream " in front of")
        (gen (ob$gets con 'to) stream switches context bp))
       (progn
        (gs-string-write stream " for")
        (gen (ob$get con 'obj)
             stream
             (cons (list 's-bar subject) (cons '(tense gerund) switches))
             context bp)))
       subject))

(define-gen WELL-PREPARED-FOR nil
  (let ((subject (list (car bp))))
   ; 956.4 (Roget's Fourth Edition)
   (gen-subject subject stream switches context bp)
   (gen-verb 'be subject stream switches (neg? con))
   (gs-string-write stream " well prepared")
   (if (and (ob$gets con 'to)
            (not (var? (ob$get con 'to))))
       (progn
        (gs-string-write stream " in front of")
        (gen (ob$gets con 'to) stream switches context bp))
       (progn
        (gs-string-write stream " for")
        (gen (ob$get con 'obj)
             stream
             (cons (list 's-bar subject) (cons '(tense gerund) switches))
             context bp)))
       subject))

(define-gen M-GET-READY nil
  (let ((subject (list (car bp))))
   ; 956.4 (Roget's Fourth Edition)
   (gen-subject subject stream switches context bp)
   (gen-verb 'get subject stream switches (neg? con))
   (gs-string-write stream " ready for")
   (gen (ob$get con 'obj)
        stream
        (cons (list 's-bar subject) (cons '(tense gerund) switches))
        context bp)
   subject))

(define-gen PERSON nil
  (cond
   ((eq? con *me-ob*)
    (case (switches-lookup 'case switches)
     ((nominative) (gs-string-write stream " I"))
     ((possessive) (gs-string-write stream " my"))
     ((reflexive) (gs-string-write stream " myself"))
     (otherwise (gs-string-write stream " me"))))
   ((memq? con *references*)
    (if (exemplar? con)
        (gs-string-write stream " this person")
        (progn
         (case (switches-lookup 'case switches)
               ((nominative)
                (if (ty$instance? con 'male-person)
                    (gs-string-write stream " he") (gs-string-write stream " she")))
               ((possessive)
                (if (ty$instance? con 'male-person)
                    (gs-string-write stream " his") (gs-string-write stream " her")))
               ((reflexive)
                (if (ty$instance? con 'male-person)
                    (gs-string-write stream " himself")
                    (gs-string-write stream " herself")))
               (otherwise
                (if (ty$instance? con 'male-person)
                    (gs-string-write stream " him")
                    (gs-string-write stream " her")))))))
   (else 
      (let ((name (or (and (ob$get con 'first-name)
                           (ob$get con 'last-name)
; Use first names for people with whom you have a positive ipt
; (real or imagined) in the current context.
                           (null? (pos-ipt-with? con context))
                           (string-append (ob$get con 'first-name)
                                      " "
                                      (ob$get con 'last-name)))
                      (ob$get con 'first-name)
                      (ob$get con 'last-name)
                      "someone")))
       (case (switches-lookup 'case switches)
        ((possessive) (gs-string-write stream (string-append " " name "'s")))
        (otherwise (gs-string-write stream (string-append " " name)))))))
   t)

(defun pos-ipt-with? (pers context)
  (cx$retrieve context (ob$fcreate `(UAND obj ?Pos-Relationship
                                          obj (NOTYPE actor Me
                                                      actor ,pers)))))

;(define-gen POLITY nil
;  (gs-string-write stream
;   (string-append " "
;                  (ob$get con 'name))))

(define-gen ORGANIZATION nil
  (let ((name (ob$get con 'name)))
    (if name
        (gs-string-write stream
                      (string-append " " name))
        (gs-string-write stream " some company"))
    t))

(define-gen CITY nil
  (let ((name (ob$get con 'name)))
    (if name
        (gs-string-write stream
                      (string-append " " name))
        (gs-string-write stream " some city"))
    t))

; Todo: We might add pronouns for objects of various types too.
(define-gen OBJECT nil
  (let ((name (ob$get con 'name)))
    (if name
        (gs-string-write stream
                      (string-append " " name))
        (progn
         (gs-string-write stream " the ")
         (gs-string-write stream (string-downcase! (symbol->string
                                                 (ob$name (ob$ty con)))))))
    t))

; Default generator for an action; tries to come up with something reasonable.
(define-gen ACTION nil
  (let ((subject (ob$gets con 'actor)))
       (gen-subject subject stream switches context bp)
       (gen-verb (ob$name (ob$ty con))
                 subject stream switches (neg? con))
       (if (ob$gets con 'obj)
           (gen (ob$gets con 'obj) stream switches context bp))
       (if (ob$get con 'from)
           (progn
            (gs-string-write stream " from")
            (gen (ob$get con 'from) stream switches context bp)))
       (if (ob$get con 'to)
           (progn
            (gs-string-write stream " to")
            (gen (ob$get con 'to) stream switches context bp)))
       subject))

; Default generator for a state; tries to come up with something reasonable.
(define-gen STATE nil
  (let ((subject (ob$gets con 'actor))
        (object (ob$gets con 'obj)))
       (cond
        ((and subject object)
         (gs-string-write stream " the ")
         (gs-string-write stream (string-downcase! (symbol->string
                                                 (ob$name (ob$ty con)))))
         (gs-string-write stream " of")
         (gen-subject subject stream switches context bp)
         (gen-verb 'be subject stream switches (neg? con))
         (gen object stream switches context bp)
         subject)
        (subject
         (gen-subject subject stream switches context bp)
         (gen-verb 'be subject stream switches (neg? con))
         (gs-string-write stream " ")
         (gs-string-write stream (string-downcase! (symbol->string
                                                 (ob$name (ob$ty con)))))
         subject)
        (else
         (gs-string-write stream " ")
         (gs-string-write stream (string-downcase! (symbol->string
                                                 (ob$name (ob$ty con)))))
         t))))

;(defun plural-form (str)
;  (string-append str (if (vowel? (last-char str)) "es" "s")))

(defun gen-subject (con stream switches context bp)
  (let ((previous-subject (switches-lookup 's-bar switches)))
    (if previous-subject
        (progn
         (if (and (neq? previous-subject 'no-subject)
                  (or (memq? (switches-lookup 'tense switches)
                             '(present past future))
                      ; Only do NP-deletion for infinitive/gerund tenses...
                      ; not quite complete, but it doesn't matter because the
                      ; verbs for which this matters, i.e., know that, etc.,
                      ; only call this with tense present..
                      (null? (same-subject? con previous-subject))))
             (gen-objective-subject con stream switches context bp)))
        (progn
         (cond
          ((eq? (switches-lookup 'case switches) 'reflexive)
           (gen con stream switches context bp))
          ((or (infinitive-tense? switches)
               (gerund-tense? switches))
           (gen-objective-subject con stream switches context bp))
          (else
           (gen con stream (cons '(case nominative) switches) context bp)))))))

(defun gen-objective-subject (con stream switches context bp)
  (if (switches-lookup 'possessive-subj switches)
      (gen con stream (cons '(case possessive) switches) context bp)
      (gen con stream (cons '(case objective) switches) context bp)))

(defun same-subject? (x y)
  (cond
   ((and (pair? x) (pair? y))
    (alikeq? x y))
   ((pair? x)
    (alikeq? x (list y)))
   ((pair? y)
    (alikeq? y (list x)))
   (else (eq? x y))))

(define-gen FAILED-GOAL nil
  (let ((subject (car bp))
        (obj (ob$get con 'obj)))
    (cond
     ((null? obj)
      (gs-string-write stream " a failure")
      t)
     ((and (ty$instance? obj 'lovers)
           (existing-relationship1? obj (or (ob$get con 'activation-context)
                                            (cx$parent context))))
      (gen-subject subject stream switches context bp)
      (gen-verb 'lose subject stream switches (neg? con))
      (gen (non-persons (ob$gets obj 'actor)
                        subject) stream switches context bp)
      subject)
     ((and (ty$instance? obj 'employment)
           (existing-relationship1? obj (or (ob$get con 'activation-context)
                                            (cx$parent context))))
      (gen-subject subject stream switches context bp)
      (gen-verb 'lose subject stream switches (neg? con))
      (gen subject stream (cons '(case possessive) switches) context bp)
      (gs-string-write stream " job")
      subject)
     (else
      (gen-subject subject stream switches context bp)
      (gen-verb 'fail subject stream switches (neg? con))
      (gs-string-write stream " at")
      (gen obj
           stream
            (cons (list 's-bar subject) (cons '(tense gerund) switches))
           context bp)
      subject))))

; Todo: Change to "I successfully went to the store"
(define-gen SUCCEEDED-GOAL nil
  (let ((subject (car bp))
        (obj (ob$get con 'obj)))
    (if obj
        (progn
         (gen-subject subject stream switches context bp)
         (gen-verb 'succeed subject stream switches (neg? con))
         (gs-string-write stream " at")
         (gen obj
              stream
              (cons (list 's-bar subject) (cons '(tense gerund) switches))
              context bp)
         subject)
        (progn
         (gs-string-write stream " a success")
         t))))

(define-gen PRESERVATION nil
  (gen (ob$get con 'obj)
       stream
       (cons '(continue? t)
             (cons (list 's-bar (car bp))
                   (cons '(tense infinitive) switches)))
       context bp))

(define-gen ACTIVE-GOAL nil
  (let ((subject (car bp))
        (obj (ob$get con 'obj))
        (top? (top-goal? con context bp)))
    (cond
     ((and (ty$instance? obj 'employment)
           (ty$instance? subject 'actor))
      (gen-subject subject stream switches context bp)
      (gen-verb 'need subject stream switches (neg? con))
      (gs-string-write stream " work"))
     ((ty$instance? obj 'employment)
      (let ((pers (persons-in (ob$gets obj 'actor)))
            (org (non-persons-in (ob$gets obj 'actor))))
        (if (null? org)
            (progn
             (gen-subject pers stream switches context bp)
             (if top?
                 (gen-desire-verb subject stream switches con)
                 (gen-verb 'need subject stream switches (neg? con)))
             (gs-string-write stream " a job"))
            (progn
             (gen-subject pers stream switches context bp)
             (if top?
                 (gen-desire-verb subject stream switches con)
                 (gen-verb 'need subject stream switches (neg? con)))
             (gs-string-write stream " to get a job with")
             (gen org stream switches context bp)))))
     (top?
      (gen-subject subject stream switches context bp)
      (gen-desire-verb subject stream switches con)
      (gen (ob$get con 'obj)
           stream
           (cons (list 'continue? (ty$instance? con 'p-goal))
                 (cons '(active-goal? t)
                       (cons (list 's-bar subject)
                             (cons '(tense infinitive) switches))))
           context bp))
      (else ;subgoal
       (gen (ob$get con 'obj)
            stream
            (cons '(active-goal? t) (have-to-ize switches))
            context bp)))
     subject))

(defun gen-desire-verb (subject stream switches con)
  (if (switches-lookup 'mtrans switches)
      (progn
       (gen-verb 'would subject stream switches (neg? con))
       (gs-string-write stream " like"))
      (gen-verb 'want subject stream switches (neg? con))))

(define-gen ACTIVE-P-GOAL nil
  (let ((subject (car bp))
        (obj (ob$get con 'obj)))
    (cond
     ((and (ty$instance? obj 'lovers)
           (memq? *me-ob* (ob$gets obj 'actor)))
      (gs-string-write stream " our relationship")
      (gen-verb 'be nil stream switches (neg? con))
      (gs-string-write stream " in trouble"))
     (else nil))
    subject))

(define-gen KNOW nil
  (let ((subject (ob$gets con 'actor))
        (obj (ob$get con 'obj)))
       (if (any? (lambda (x) (and (not (var? x))
                                  (not (ty$instance? x 'person))))
                 subject)
           (progn
            (gen-subject obj stream switches context bp)
            (gen-verb 'be subject stream switches (neg? con))
            (gs-string-write stream " in")
            (gen subject stream switches context bp)
            obj)
           (progn
            (gen-subject subject stream switches context bp)
            (gen-verb 'know subject stream switches (neg? con))
            (if obj
                (cond
                 ((or (ty$instance? obj 'OBJECT) (ty$instance? obj 'LOCATION)
                      (ty$instance? obj 'TIME-OF-DAY)
                      (ty$instance? obj 'VPROX) (ty$instance? obj 'TELNO))
                  (gen obj stream (make-indicative switches) context bp))
                 (else
                  (if *gen-thats* (gs-string-write stream " that"))
                  (gen obj stream (make-indicative switches) context bp))))
            subject))))

(defun make-indicative (switches)
  (append '((tense present) (s-bar ())) switches))

(define-gen TELNO nil
  (let ((subject (ob$gets con 'actor))
        (number (ob$gets con 'obj)))
    (if (string? number)
        (progn
         (gen subject stream (cons '(case possessive) switches) context bp)
         (gs-string-write stream " telephone number is")
         (gs-string-write stream (string-append " " number)))
        (progn
         (gen subject stream (cons '(case possessive) switches) context bp)
         (gs-string-write stream " telephone number")))
    t))

(define-gen ADDRESS nil
  (let ((subject (ob$gets con 'actor))
        (number (ob$gets con 'obj)))
    (if (string? number)
        (progn
         (gen subject stream (cons '(case possessive) switches) context bp)
         (gs-string-write stream " address is")
         (gs-string-write stream (string-append " " number)))
        (progn
         (gen subject stream (cons '(case possessive) switches) context bp)
         (gs-string-write stream " address")))
    t))

(defun non-var-ob? (x)
  (and (ob? x)
       (not (var? x))))

(define-gen ALUMNI-DIR nil
  (let ((college (ob$pget con '(obj obj)))
        (person (ob$pget con '(obj actor))))
       (cond
        ((and (non-var-ob? college) (non-var-ob? person))
         (gs-string-write stream " the Alumni Directory for")
         (gen person stream (cons '(case possessive) switches) context bp)
         (gs-string-write stream " college")
         (gen college stream (cons '(case possessive) switches) context bp))
        ((non-var-ob? college)
         (gs-string-write stream " the")
         (gen college stream (cons '(case possessive) switches) context bp)
         (gs-string-write stream " Alumni Directory"))
        ((non-var-ob? person)
         (gs-string-write stream " the Alumni Directory for")
         (gen person stream (cons '(case possessive) switches) context bp)
         (gs-string-write stream " college"))
        (else
         (gs-string-write stream " an Alumni Directory")))
       t))

(define-gen COLLEGE nil
  (let ((college (ob$pget con 'obj))
        (subject (ob$pget con 'actor)))
       (cond
        ((non-var-ob? college)
         (gen-regular-plus con stream switches context bp 'go " to school at"
                           college))
        ((non-var-ob? person)
         (gen person stream (cons '(case possessive) switches) context bp)
         (gs-string-write stream " college")
         t)
        (else
         (gs-string-write stream " someone's college")
         t))))

(define-gen CLOTHES nil
  (let ((obj (ob$get con 'obj)))
   (gen obj stream (cons '(case possessive) switches) context bp)
   (gs-string-write stream " clothes")
   t))

(define-gen MOVIES nil
  (let ((obj (ob$get con 'obj)))
   (gen obj stream (cons '(case possessive) switches) context bp)
   (gs-string-write stream " movies")
   t))

(define-gen PTRANS nil
  (let ((subject (ob$gets con 'actor)))
     (gen-subject subject stream switches context bp)
     (gen-verb 'go subject stream switches (neg? con))
     (if (ob$get con 'to)
         (progn
          (if (not (to-less? (ob$get con 'to)))
              (gs-string-write stream " to"))
          (gen (ob$get con 'to) stream switches context bp)))
     subject))

(define-gen ATRANS nil
  (let ((subject (ob$gets con 'actor)))
       (if (and (ob$get con 'obj)
                (ty$instance? (ob$get con 'obj) 'cash))
           (progn
            (gen-subject subject stream switches context bp)
            (gen-verb 'pay subject stream switches (neg? con))
            (gen (ob$get con 'to) stream switches context bp))
           (progn
            (gen-subject subject stream switches context bp)
            (gen-verb 'give subject stream switches (neg? con))
            (gen (ob$get con 'obj) stream switches context bp)
            (gs-string-write stream " to")
            (gen (ob$get con 'to) stream switches context bp)))
     subject))

(define-gen EARTHQUAKE nil
  (let ((subject (ob$gets con 'obj)))
     (gen-subject subject stream switches context bp)
     (gen-verb 'has subject stream switches (neg? con))
     (gs-string-write stream " an earthquake")
     subject))

(define-gen EARTHQUAKE-ONSET nil
  (let ((subject (ob$gets con 'obj)))
     (gs-string-write stream " an earthquake")
     (gen-verb 'begin subject stream switches (neg? con))
     (gs-string-write stream " in")
     (gen subject stream switches context bp)
     subject))

(define-gen M-PURCHASE nil
  (let ((subject (ob$gets con 'actor)))
     (gen-subject subject stream switches context bp)
     (gen-verb 'buy subject stream switches (neg? con))
     (gen (ob$get con 'obj) stream switches context bp)
     (gs-string-write stream " from")
     (gen (ob$get con 'from) stream switches context bp)
     subject))

(defun to-less? (to)
  (or (eq? to (ob$name->ob 'Outside))
      (eq? to (ob$name->ob 'Home))))

(define-gen PTRANS1 nil
  (gen-regular-plus con stream switches context bp 'go " to"
                    (ob$get con 'to)))

(define-gen EMPLOYMENT nil
  (let ((subject (gen-split-subject con stream switches context bp 'have
                                    " a job with")))
       (gs-string-write stream " at")
       (gen (ob$get con 'organization) stream switches context bp)
       subject))

(defun people (lst)
  (yloop (initial (result nil))
        (ywhile lst)
        (ydo (if (ty$instance? (car lst) 'person)
                (setq result (cons (car lst) result)))
            (setq lst (cdr lst)))
        (yresult result)))

(defun people-or-orgs (lst)
  (yloop (initial (result nil))
        (ywhile lst)
        (ydo (if (or (ty$instance? (car lst) 'person)
                    (ty$instance? (car lst) 'organization))
                (setq result (cons (car lst) result)))
            (setq lst (cdr lst)))
        (yresult result)))

(define-gen AT nil
  (gen-regular-plus con stream switches context bp 'be " at"
                    (ob$get con 'obj)))

(define-gen LOCATION nil
  (let ((name (ob$get con 'name)))
    (if name
        (gs-string-write stream
                      (string-append " " name))
        (gs-string-write stream " someplace"))
    t))

(define-gen RANGER-STATION nil
  (let ((name (ob$get con 'name)))
    (if name
        (gs-string-write stream (string-append " " name))
        (gs-string-write stream " the ranger station"))
    t))

;(define-gen PROX nil
;  (let ((subject (ob$gets con 'actor)))
;    (if (> (length subject) 1)
;        (progn
;         (gen-subject subject stream switches context bp)
;         (gen-verb 'be subject stream switches (neg? con))
;         (gs-string-write stream " near each other"))
;        (progn
;         (gen subject stream (cons '(case possessive) switches context bp))
;         (gs-string-write stream " location")))))

(define-gen VPROX nil
  (let ((subject (ob$gets con 'actor)))
       (cond
        ((any? (lambda (x) (not (ty$instance? x 'person)))
               subject)
         (let ((subj nil)
               (virtual-me (car bp)))
              (if (memq? virtual-me subject)
                  (progn
                   (setq subj (list virtual-me))
                   (gen-subject subj stream switches context bp)
                   (gen-verb 'be subj stream switches (neg? con))
                   (gs-string-write stream " near")
                   (gen (non-persons subject virtual-me) stream switches
                        context bp))
                  (progn
                   (setq subj subject)
                   (gen-subject subject stream switches context bp)
                   (gen-verb 'be subject stream switches (neg? con))
                   (gs-string-write stream " near")
                   (if (= (length subject) 1)
                       (if (neg? con)
                           (gs-string-write stream " anything")
                           (gs-string-write stream " something")))))
              subj))
        ((> (length subject) 1)
         (if (switches-lookup 'active-goal? switches)
             (gen-split-subject con stream switches context bp 'get
                                " in touch with")
             (gen-split-subject con stream switches context bp 'be
                                " in touch with")))
        (else
         (gen subject stream (cons '(case possessive) switches context bp))
         (gs-string-write stream " location")
         t))))

(defun gen-split-subject (con stream switches context bp verb prep-phrase)
  (let ((subject (ob$gets con 'actor))
        (subj nil)
        (virtual-me (car bp))
        (non-virtual-mes nil))
    (if (memq? virtual-me subject)
        (progn
         (setq subj (list virtual-me))
         (gen-subject subj stream switches context bp)
         (gen-verb verb subj stream switches (neg? con))
         (gs-string-write stream prep-phrase)
         (setq non-virtual-mes (non-persons subject virtual-me))
         (if non-virtual-mes
             (gen non-virtual-mes stream switches context bp)
             (if (neg? con)
                 (gs-string-write stream " anyone")
                 (gs-string-write stream " someone"))))
        (progn
         (setq subj subject)
         (gen-subject subject stream switches context bp)
         (gen-verb verb subj stream switches (neg? con))
         (gs-string-write stream prep-phrase)
         (if (= (length subject) 1)
             (if (neg? con)
                 (gs-string-write stream " anyone")
                 (gs-string-write stream " someone")))))
   subj))

(define-gen MTRANS nil
  (let ((subject (ob$get con 'actor))
        (from (ob$get con 'from))
        (to (ob$get con 'to))
        (obj (ob$get con 'obj))
        (bd (ob$unify *pos-rel-mtrans* con *empty-bd*))
        (rel nil))
   (cond
    (bd
     (setq rel (bd-lookup 'pos-relationship bd))
     (cond
      ((ty$instance? rel 'lovers)
       (gen-subject (bd-lookup 'person1 bd) stream switches context bp)
       (gen-verb 'ask subject stream switches (neg? con))
       (gen (bd-lookup 'person2 bd) stream switches context bp)
       (gs-string-write stream " out"))
      ((ty$instance? rel 'employment)
       (gen-subject (bd-lookup 'person1 bd) stream switches context bp)
       (gen-verb 'offer subject stream switches (neg? con))
       (gen (bd-lookup 'person2 bd) stream switches context bp)
       (gs-string-write stream " a job"))))
    ((setq bd (ob$unify *neg-rel-mtrans* con *empty-bd*))
     (setq rel (bd-lookup 'pos-relationship bd))
     (cond
      ((null? rel)
       (gen-subject (bd-lookup 'person1 bd) stream switches context bp)
       (gen-verb 'decline subject stream switches (neg? con)))
      ((ty$instance? rel 'lovers)
       (if (existing-relationship? rel con context)
           (progn
            (gen-subject (bd-lookup 'person1 bd) stream switches context bp)
            (gen-verb 'dump subject stream switches (neg? con))
            (gen (bd-lookup 'person2 bd) stream switches context bp))
           (progn
            (gen-subject (bd-lookup 'person1 bd) stream switches context bp)
            (gen-verb 'turn subject stream switches (neg? con))
            (gen (bd-lookup 'person2 bd) stream switches context bp)
            (gs-string-write stream " down"))))
      ((ty$instance? rel 'employment)
       (if (existing-relationship? rel con context)
           (progn
            (gen-subject (bd-lookup 'person1 bd) stream switches context bp)
            (gen-verb 'fire subject stream switches (neg? con))
            (gen (bd-lookup 'person2 bd) stream switches context bp))
           (progn
            (gen-subject (bd-lookup 'person1 bd) stream switches context bp)
            (gen-verb 'pass subject stream switches (neg? con))
            (gen (bd-lookup 'person2 bd) stream switches context bp)
            (gs-string-write stream " up"))))))
    ((ty$instance? obj 'introduction)
     (gen-subject subject stream switches context bp)
     (gen-verb 'introduce subject stream switches (neg? con))
     (gen subject stream (cons '(case reflexive) switches) context bp)
     (gs-string-write stream " to")
     (gen to stream switches context bp))
    ((ty$instance? obj 'movie)
     (gen-subject subject stream switches context bp)
     (gen-verb 'watch subject stream switches (neg? con))
     (gen obj stream switches context bp)
     (if (and from (not (var? from)))
         (progn
          (gs-string-write stream " at")
          (gen from stream switches context bp))))
    ((or (eq? subject from)
         (and (var? subject)
              (var? from)
              (eq? (variable-name subject)
                   (variable-name from))))
     (gen-subject subject stream switches context bp)
     (gen-verb 'tell subject stream switches (neg? con))
     (gen to stream switches context bp)
     (gen-that obj stream (cons '(mtrans t) switches) context bp))
    ((and (eq? subject to)
          (not (ty$instance? from 'person))
          (ty$instance? from 'object))
     (gen-subject subject stream switches context bp)
     (if (ty$instance? obj 'telno) ; should be knowable? or personal-attribute?
         (progn
          (gen-verb 'look subject stream switches (neg? con))
          (gs-string-write stream " up"))
         (gen-verb 'read subject stream switches (neg? con)))
     (gen-that obj stream switches context bp)
     (gs-string-write stream " in")
     (gen from stream switches context bp))
    ((and (neq? subject to)
          (not (ty$instance? from 'person))
          (ty$instance? from 'object))
     (gen-subject subject stream switches context bp)
     (if (ty$instance? obj 'telno)
         (progn
          (gen-verb 'look subject stream switches (neg? con))
          (gs-string-write stream " up"))
         (gen-verb 'read subject stream switches (neg? con)))
     (gen-that obj stream switches context bp)
     (gs-string-write stream " in")
     (gen from stream switches context bp)
     (gs-string-write stream " and")
     (gen-verb 'tell subject stream (cons '(to-less-infinitive t) switches)
               (neg? con))
     (gs-string-write stream " it to")
     (gen to stream switches context bp))
    (else
     (gen-subject subject stream switches context bp)
     (gen-verb 'say subject stream switches (neg? con))
     (gs-string-write stream " something")))
   subject))

; Did relationship exist at time of MTRANS?
(defun existing-relationship? (rel mtrans-con context)
  (if (null context)
      nil
      (or (cx$retrieve rel context) ; Would this ever be?
          (let ((mtrans-ctxt (ob$get mtrans-con 'top-context)))
               (if mtrans-ctxt
                   (existing-relationship1? rel (cx$parent mtrans-ctxt))
                   nil)))))

(defun existing-relationship1? (rel context)
  (if (null context)
      nil
      (cx$retrieve-bd-ignore context rel *empty-bd*
       '(linked-from-of linked-to-of))))

(defun gen-that (obj stream switches context bp)
 (if (and *gen-thats* (not (ty$instance? obj 'telno)))
     (gs-string-write stream " that"))
 (gen obj stream (make-indicative switches) context bp))

(defun gen-verb (verb subject stream switches negative?)
  (cond
   ((switches-lookup 'continue? switches)
    (gen-verb1 'continue subject stream switches negative?)
    (gen-verb1 verb subject stream (infinitivize switches) nil))
   (else (gen-verb1 verb subject stream switches negative?))))

(defun gen-verb1 (verb subject stream switches negative?)
  (gs-string-write stream
                (string-append " "
                               (get-verb-form
                                verb
                                (or (switches-lookup 'number switches)
                                    (cadr (get-number subject)))
                                (switches-lookup 'tense switches)
                                negative? switches))))

(defun switches-lookup (item switches)
  (cadr (assq item switches)))

(defun get-number (con)
  (if (pair? con)
      (if (= (length con) 1)
          (get-number (car con))
         '(number plural))
      (if (eq? con *me-ob*)
          '(number first-person-singular)
          '(number third-person-singular))))

;*******************************************************************************
;                          VERB MORPHOLOGY
;*******************************************************************************

(defun get-verb-form (verb number tense negative? switches)
  (if negative?
      (get-negative-verb-form verb number tense switches)
      (get-positive-verb-form verb number tense switches)))

(defun progressivize (switches)
  (let ((old (switches-lookup 'tense switches)))
    (if old
        (cond
         ((eq? old 'gerund)
          (cons '(tense gerund) switches))
; could go to gerund-progressive, but this is too much!!
         ((eq? old 'infinitive)
          (cons '(tense infinitive-progressive) switches))
         ((eq? old 'past)
          (cons '(tense past-progressive) switches))
         ((eq? old 'past-subjunctive)
          (cons '(tense past-subjunctive-progressive) switches))
         ((eq? old 'future)
          (cons '(tense future-progressive) switches))
         ((eq? old 'present-have-to)
          (cons '(tense have-to-progressive) switches))
         ((eq? old 'conditional-have-to-present-perfect)
          (cons '(tense conditional-have-to-present-perfect)
                 switches))
         ((eq? old 'have-to-gerund)
          (cons '(tense have-to-gerund) switches))
         ((eq? old 'conditional-have-to)
          (cons '(tense conditional-have-to) switches))
         (else (cons '(tense present-progressive) switches)))
        (cons '(tense present-progressive) switches))))

(defun have-to-ize (switches)
  (let ((old (switches-lookup 'tense switches)))
    (if old
        (cond
         ((eq? old 'conditional-present-perfect)
          (cons '(tense conditional-have-to-present-perfect) switches))
         ((eq? old 'gerund)
          (cons '(tense have-to-gerund) switches))
         ((eq? old 'conditional)
          (cons '(tense conditional-have-to) switches))
         (else (cons '(tense present-have-to) switches)))
        (cons '(tense present-have-to) switches))))

(defun simplify-tense (switches)
 (cons (list 's-bar nil)
  (let ((old (switches-lookup 'tense switches)))
    (if old
        (cons
         (list 'tense
          (cond
           ((eq? old 'future-progressive) 'future)
           ((eq? old 'present-progressive) 'present)
           ((eq? old 'past-progressive) 'past)
           ((eq? old 'past-subjunctive-progressive) 'past)
           ((eq? old 'present-perfect) 'past)
           ((eq? old 'conditional-present-perfect) 'past)
           ((eq? old 'conditional-have-to-present-perfect) 'past)
           ((eq? old 'past-perfect) 'past)
           ((eq? old 'future) 'future)
           ((eq? old 'conditional) 'present)
           ((eq? old 'conditional-have-to) 'present)
           ((eq? old 'past) 'past)
           ((eq? old 'past-subjunctive) 'past)
           ((eq? old 'gerund) 'present)
           ((eq? old 'gerund-progressive) 'present)
           ((eq? old 'have-to-gerund) 'present)
           ((eq? old 'infinitive-progressive) 'present)
           ((eq? old 'infinitive) 'present)
           ((eq? old 'past-gerund) 'past)
           ((eq? old 'past-infinitive) 'past)
           ((eq? old 'present-have-to) 'present)
           (else 'present)))
         switches)
        switches))))

(defun tense-time (switches)
  (let ((old (switches-lookup 'tense switches)))
    (if old
        (cond
         ((eq? old 'past) 'past) ((eq? old 'future-progressive) 'future)
         ((eq? old 'present-progressive) 'present)
         ((eq? old 'past-progressive) 'past)
         ((eq? old 'past-subjunctive-progressive) 'past)
         ((eq? old 'present-perfect) 'past)
         ((eq? old 'conditional-present-perfect) 'past)
         ((eq? old 'conditional-have-to-present-perfect) 'past)
         ((eq? old 'past-perfect) 'past) ((eq? old 'future) 'future)
         ((eq? old 'conditional) 'present)
         ((eq? old 'conditional-have-to) 'present)
         ((eq? old 'past-subjunctive) 'past) ((eq? old 'gerund) 'present)
         ((eq? old 'gerund-progressive) 'present)
         ((eq? old 'have-to-gerund) 'present)
         ((eq? old 'infinitive-progressive) 'present)
         ((eq? old 'infinitive) 'present) ((eq? old 'past-gerund) 'past)
         ((eq? old 'past-infinitive) 'past)
         ((eq? old 'present-have-to) 'present)
         (else 'present))
        'present)))

(defun infinitivize (switches)
  (let ((old (switches-lookup 'tense switches)))
    (if old
        (cond
         ((eq? old 'past)
          (cons '(tense past-infinitive) switches))
         (else (cons '(tense infinitive) switches)))
        (cons '(tense infinitive) switches))))

(defun gerundize (switches)
  (let ((old (switches-lookup 'tense switches)))
    (if old
        (cond
         ((eq? old 'past)
          (cons '(tense past-gerund) switches))
         (else (cons '(tense gerund) switches)))
        (cons '(tense gerund) switches))))

;          (cons '(tense conditional-have-to-present-perfect) switches))
;          (cons '(tense conditional-have-to) switches))
;        (cons '(tense present-have-to) switches))))

(defun infinitive-tense? (switches)
  (let ((found (switches-lookup 'tense switches)))
    (if found
        (or (eq? found 'infinitive)
            (eq? found 'past-infinitive)
            (eq? found 'infinitive-progressive))
        nil)))

(defun gerund-tense? (switches)
  (let ((found (switches-lookup 'tense switches)))
    (if found
        (or (eq? found 'gerund)
            (eq? found 'have-to-gerund)
            (eq? found 'gerund-progressive)
            (eq? found 'past-gerund))
        nil)))

(defun inf-to (switches)
  (if (switches-lookup 'to-less-infinitive switches)
      ""
      "to "))

(defun get-positive-verb-form (verb number tense switches)
  (cond
   ((eq? tense 'future-progressive)
    (string-append "will " (present-form 'be number) " " (ing-form verb)))
   ((eq? tense 'present-progressive)
    (string-append (present-form 'be number) " " (ing-form verb)))
   ((eq? tense 'past-progressive)
    (string-append (past-form 'be number) " " (ing-form verb)))
   ((eq? tense 'past-subjunctive-progressive)
    (string-append (past-subjunctive-form 'be number) " " (ing-form verb)))
   ((eq? tense 'present-perfect)
    (string-append (present-form 'have number) " " (en-form verb)))
   ((eq? tense 'conditional-present-perfect)
    (string-append "would " (infinitive-form 'have) " " (en-form verb)))
   ((eq? tense 'conditional-have-to-present-perfect)
    (string-append "would have to " (infinitive-form 'have)
                   " " (en-form verb)))
   ((eq? tense 'past-perfect)
    (string-append "had " (en-form verb)))
   ((eq? tense 'future)
    (string-append "will " (infinitive-form verb)))
   ((eq? tense 'conditional)
    (string-append "would " (infinitive-form verb)))
   ((eq? tense 'conditional-have-to)
    (string-append "would have to " (infinitive-form verb)))
   ((eq? tense 'past)
    (past-form verb number))
   ((eq? tense 'past-subjunctive)
    (past-subjunctive-form verb number))
   ((eq? tense 'gerund)
    (ing-form verb))
   ((eq? tense 'gerund-progressive)
    (string-append (ing-form 'be) " " (ing-form verb)))
   ((eq? tense 'have-to-gerund)
    (string-append (ing-form 'have) " to " (infinitive-form verb)))
   ((eq? tense 'have-to-progressive)
    (string-append (present-form 'have number) " to be " (ing-form verb)))
   ((eq? tense 'infinitive-progressive)
    (string-append (inf-to switches) "be " (ing-form verb)))
   ((eq? tense 'infinitive)
    (string-append (inf-to switches) (infinitive-form verb)))
   ((eq? tense 'past-gerund)
    (string-append (ing-form 'have) " " (en-form verb)))
   ((eq? tense 'past-infinitive)
    (string-append (inf-to switches) (infinitive-form 'have) (en-form verb)))
   ((eq? tense 'present-have-to)
    (string-append (present-form 'have number) " to " (infinitive-form verb)))
  (else ; default is present
     (present-form verb number))))

(defun modal-aux? (verb)
  (eq? verb 'would))

(defun get-negative-verb-form (verb number tense switches)
  (cond
   ((modal-aux? verb)
    (string-append (present-form verb number) " not"))
   ((eq? tense 'future-progressive)
    (string-append "will not " (present-form 'be number) " " (ing-form verb)))
   ((eq? tense 'present-progressive)
    (string-append (present-form 'be number) " not " (ing-form verb)))
   ((eq? tense 'past-progressive)
    (string-append (past-form 'be number) " not " (ing-form verb)))
   ((eq? tense 'past-subjunctive-progressive)
    (string-append (past-subjunctive-form 'be number) " not " (ing-form verb)))
   ((eq? tense 'present-perfect)
    (string-append (present-form 'have number) " not " (en-form verb)))
   ((eq? tense 'conditional-present-perfect)
    (string-append "would not " (infinitive-form 'have) " " (en-form verb)))
   ((eq? tense 'conditional-have-to-present-perfect)
    (string-append "would not have to " (infinitive-form 'have)
                   " " (en-form verb)))
   ((eq? tense 'past-perfect)
    (string-append "had not " (en-form verb)))
   ((eq? tense 'future)
    (string-append "will not " (infinitive-form verb)))
   ((eq? tense 'conditional)
    (string-append "would not " (infinitive-form verb)))
   ((eq? tense 'conditional-have-to)
    (string-append "would not have to " (infinitive-form verb)))
   ((eq? tense 'past)
    (if (eq? verb 'be)
        (string-append (past-form verb number) " not")
        (string-append (past-form 'do number)
                       " not "
                       (infinitive-form verb))))
   ((eq? tense 'past-subjunctive)
    (if (eq? verb 'be)
        (string-append (past-subjunctive-form verb number) " not")
        (string-append (past-subjunctive-form 'do number)
                       " not "
                       (infinitive-form verb))))
   ((eq? tense 'gerund)
    (string-append "not " (ing-form verb)))
   ((eq? tense 'gerund-progressive)
    (string-append "not " (ing-form 'be) " " (ing-form verb)))
   ((eq? tense 'have-to-gerund)
    (string-append "not " (ing-form 'have) " to " (infinitive-form verb)))
   ((eq? tense 'have-to-progressive)
    (string-append "cannot be "
                   (ing-form verb)))
; Old version of above.
;   ((eq? tense 'have-to-progressive)
;    (string-append (present-form 'do number) " not have to be "
;                   (ing-form verb)))
   ((eq? tense 'infinitive-progressive)
    (string-append "not " (inf-to switches) "be " (ing-form verb)))
   ((eq? tense 'infinitive)
    (string-append "not " (inf-to switches) (infinitive-form verb)))
   ((eq? tense 'past-gerund)
    (string-append "not " (ing-form 'have) " " (en-form verb)))
   ((eq? tense 'past-infinitive)
    (string-append "not " (inf-to switches) (infinitive-form 'have) " " (en-form verb)))
   ((eq? tense 'present-have-to)
    (string-append (present-form 'do number) "not have to "
     (infinitive-form verb)))
   (else ; default is present
    (if (eq? verb 'be)
        (string-append (present-form verb number) " not")
        (string-append (present-form 'do number)
                       " not "
                       (infinitive-form verb))))))

(defun infinitive-form (verb)
  (string-downcase! (symbol->string verb)))

(progn
 (setq *present-forms* '((be ((first-person-singular "am")
                              (third-person-singular "is")
                              (plural "are")))
                         (do ((first-person-singular "do")
                              (third-person-singular "does")
                              (plural "do")))
                         (go ((first-person-singular "go")
                              (third-person-singular "goes")
                              (plural "go")))
                         (have ((first-person-singular "have")
                                (third-person-singular "has")
                                (plural "have")))
                         (would ((first-person-singular "would")
                                 (third-person-singular "would")
                                 (plural "would")))
)) nil)

(defun present-form (verb number)
 (let ((found1 (assq verb *present-forms*)))
  (if found1
      (let ((found2 (assq number (cadr found1))))
        (if found2
            (cadr found2)
            (derive-present-form verb number)))
      (derive-present-form verb number))))

(defun derive-present-form (verb number)
  (let ((str (infinitive-form verb)))
    (if (eq? number 'third-person-singular)
;        (string-append str (if (vowel? (last-char str)) "es" "s"))
        (string-append str (if (string-equal? (last-char str) "s") "es" "s"))
        str)))

(progn
 (setq *past-forms* '((be ((first-person-singular "was")
                           (third-person-singular "was")
                           (plural "were")))
                      (have ((first-person-singular "had")
                             (third-person-singular "had")
                             (plural "had")))
                      (go ((first-person-singular "went")
                           (third-person-singular "went")
                           (plural "went")))
                      (know ((first-person-singular "knew")
                             (third-person-singular "knew")
                             (plural "knew")))
                      (read ((first-person-singular "read")
                             (third-person-singular "read")
                             (plural "read")))
                      (tell ((first-person-singular "told")
                             (third-person-singular "told")
                             (plural "told")))
                      (feel ((first-person-singular "felt")
                             (third-person-singular "felt")
                             (plural "felt")))
                      (think ((first-person-singular "thought")
                             (third-person-singular "thought")
                             (plural "thought")))
                      (lead ((first-person-singular "led")
                             (third-person-singular "led")
                             (plural "led")))
                      (would ((first-person-singular "would")
                              (third-person-singular "would")
                              (plural "would")))
                      (ydo ((first-person-singular "did")
                           (third-person-singular "did")
                           (plural "did")))))
 nil)

(defun past-form (verb number)
 (let ((found1 (assq verb *past-forms*)))
  (if found1
      (let ((found2 (assq number (cadr found1))))
        (if found2
            (cadr found2)
            (derive-past-form verb number)))
      (derive-past-form verb number))))

(defun past-subjunctive-form (verb number)
  (past-form verb 'plural))

(defun derive-past-form (verb number)
  (let ((str (infinitive-form verb)))
    (string-append str (if (string-equal? (last-char str) "e") "d" "ed"))))

(progn
 (setq *en-forms* '((be "been") (have "had") (go "gone")
                    (tell "told") (know "known") (think "thought")
                    (agree "agreed") (watch "watched")
                    (introduce "introduced")
                    (put "put") (continue "continued")
                    (succeed "succeeded") (fail "failed")
                    (break "broken") (reverse "reversed")
                    (want "wanted")))
 nil)

(defun en-form (verb)
 (let ((found (assq verb *en-forms*)))
  (if found
      (cadr found)
      (let ((str (infinitive-form verb)))
      (string-append str (if (string-equal? (last-char str) "e") "n" "en"))))))

(progn
 (setq *ing-forms* '((agree "agreeing")
                     (get "getting")
                     ))
 nil)

(defun ing-form (verb)
 (let ((found (assq verb *ing-forms*)))
  (if found
      (cadr found)
      (let ((str (infinitive-form verb)))
       (if (and (string-equal? (last-char str) "e")
                (not (string-equal? str "be")))
           (string-append (substring str 0 (-1+ (string-length str))) "ing")
           (string-append str "ing"))))))

(defun vowel? (str)
  (or (string-equal? str "a")
      (string-equal? str "e")
      (string-equal? str "i")
      (string-equal? str "o")
      (string-equal? str "u")))

(defun last-char (string)
   (nthchdr string (- (string-length string) 1)))

(defun first-char (string)
   (string-slice string 0 1))

;*******************************************************************************

(defclass gen-stream ()
  ((hpos-value :initform 0 :accessor hpos-value)
   (line-length-value :initform (if *typeset?* 100 50)
                      :accessor line-length-value)
   (start-sentence? :initform t :accessor start-sentence?)
   (stream :initarg :stream :accessor gen-stream-stream)))

(defun make-gen-stream (stream)
  (make-instance 'gen-stream :stream stream))

(defmethod gs-newline ((self gen-stream))
 (newline (gen-stream-stream self))
 (setf (hpos-value self) 0))

(defmethod gs-reset-line ((self gen-stream))
 (setf (hpos-value self) 0))

(defmethod gs-reset-sentence ((self gen-stream))
 (setf (start-sentence? self) t))

(defmethod gs-string-write ((self gen-stream) text)
  (if (start-sentence? self)
      (progn
       (setq text (concatenate 'string text))
       (if (capitalize-first! text)
           (setf (start-sentence? self) nil))))
  (if (> (+ (hpos-value self) (string-length text))
         (line-length-value self))
      (gs-newline self))
  (string-write (gen-stream-stream self) text)
  (setf (hpos-value self) (+ (hpos-value self) (string-length text))))

(defmethod gs-end-sentence ((self gen-stream))
 (gs-string-write self ".")
 (setf (start-sentence? self) t))

(defmethod gs-end-question ((self gen-stream))
 (gs-string-write self "?")
 (setf (start-sentence? self) t))

(defmethod gs-end-exclam ((self gen-stream))
 (gs-string-write self "!")
 (setf (start-sentence? self) t))

(defun capitalize-first! (str)
  (yloop (initial (i 0)
                  (result nil)
                  (len (string-length str)))
         (yuntil result)
         (ywhile (< i len))
         (ydo (if (char/= (char str i) #\space)
                  (progn
                   (setf (char str i) (char-upcase (char str i)))
                   (setq result t))
                  (increment-me i)))
         (yresult result)))

; End of file.
