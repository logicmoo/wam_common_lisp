;*******************************************************************************
;
; Daydreamer
; Version 3.5
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
; This file contains:
; DAYDREAMER*: A computer model of night dreaming and overdetermined daydreaming
;
;   9/9/88: First version written
; 12/17/88: Debugged, modified, and wrote English generation routines.
;
;*******************************************************************************

;*******************************************************************************
; Some utilities
;*******************************************************************************

; Turn off ob warnings
(disinterest 'ob-warn 'all)

(setq *reality* (cx$create))

(defun pod (ob)
  (if (ob? ob)
      (progn
       (ob$pr ob *gate-dbg* *ob-print-options*)
       (newline *gate-dbg*))
      (format *gate-dbg* "~A~%" ob))
  *repl-wont-print*)

;*******************************************************************************
; Top-level functions for DAYDREAMER*
;*******************************************************************************

(defun daydreamer-star ()
  (run-night-sample))

(defun run-night-sample ()
  (run-night (list *test-reversal-goal* *test-revenge-goal*)))

(defun run-night (dd-goals)
  (let
   ((qplans-es nil)
    (all-rules nil)
    (metaphors nil)
    (metaphor-goal nil)
    (metaphor-qplans))
   (ndbg-roman-nl *gate-dbg* night
"----------------------------------------------------------")
   (ndbg-roman-nl *gate-dbg* night "DAYDREAMER* version of 19990506")
   (ndbg-roman-nl *gate-dbg* night "Generating plans for daydreaming goals...")
   (setq qplans-es
        (map 'list (lambda (goal) (qplan-generate goal))
             dd-goals))
   (setq all-rules
        (map-app (lambda (qplans) (map-app (lambda (qplan) (car qplan)) qplans))
                 qplans-es))
   (ndbg-roman-nl *gate-dbg* night "All rules = ~A" all-rules)
   (ndbg-roman-nl *gate-dbg* night "Selecting metaphor script...")
   (setq metaphors (select-metaphor all-rules))
   (if (not (null? metaphors))
       (progn
        (ndbg-roman-nl *gate-dbg* night
                       "Selecting first metaphor from among: ~A" metaphors)
        (setq metaphor-goal
             (ob$fcreate
              `(ACTIVE-GOAL
                obj ,(ob$instantiate-o (ob$get (caar metaphors) 'goal)
                                       *empty-me-bd*))))
        (ndbg-roman-nl *gate-dbg* night "Generating plans for metaphor...")
        (setq metaphor-qplans (qplan-generate metaphor-goal))
        (setq *metaphor-qplans* metaphor-qplans))
       (ndbg-roman-nl *gate-dbg* night "No metaphors found"))
   nil))

; Old test routines
(defun night-test ()
  (setq *reversal-qplans* (qplan-generate *test-reversal-goal*))
  (setq *revenge-qplans* (qplan-generate *test-revenge-goal*))
  (setq *ranger-qplans* (qplan-generate *test-ranger-goal*))
  (night-pp)
)


(defun pretty-print (obj st) (pprint obj st))

(defun night-pp ()
  (ndbg-roman-nl *gate-dbg* night "")
  (pretty-print *reversal-qplans* *gate-dbg*)
  (ndbg-roman-nl *gate-dbg* night "")
  (pretty-print *revenge-qplans* *gate-dbg*)
  (ndbg-roman-nl *gate-dbg* night "")
  (pretty-print *ranger-qplans* *gate-dbg*)
  (ndbg-roman-nl *gate-dbg* night "")
)

;*******************************************************************************
; Quick planning
;*******************************************************************************

;
; Possible mods to qplan:
;
; Optional unification
; Optional instantiation
;

;
; Returns:
; (qplan1 qplan2 qplan3 ...)
; where a qplan =
; ((rule1 rule2 rule3 ...) -- list of all rules used in qplan
;  qqplan)
; where a qqplan =
; (rule qqplan1 qqplan2 ... qqplann)
; where rule has n subgoals
; A qqplan can be NIL if there are no plans
;
(defun qplan-generate (goal)
  (ndbg-roman-nl *gate-dbg* night "Running quickplanner on:")
  (pod goal)
  (gn goal)
  (yloop (initial (result nil)
                  (plans nil))
        (yfor top-rule in (top-rules (ob$get goal 'obj)))
        (ydo (if (setq plans (qplan-generate1 top-rule (list top-rule)))
                 (setq result (append! result plans))))
        (yresult 
         (progn
          (ndbg-roman-nl *gate-dbg* night "Generated plans =")
          (qplans-print result)
          (qplans-gen result (ob$get goal 'obj) nil)
          (qplans-gen result (ob$get goal 'obj) t)
          result))))

;
; Returns:
; NIL (if rule is NIL) -or-
; (qplan1 qplan2 qplan3 ...)
;
(defun qplan-generate1 (rule rules-in-path)
  ; (ndbg-roman-nl *gate-dbg* night "qplan-generate1 for ~A" rule)
  (cond
   ((nil? rule) nil)
   (else
    (yloop
     (initial (result nil) (qplans-es nil))
     (yfor rules in (subrules rule rules-in-path))
     (ydo
      ; rules = (subrule1 subrule2 ... subrulen)
      ; Generate qplans for each subrule
      ; (ndbg-roman-nl *gate-dbg* night "rules = ")
      ; (pretty-print rules *gate-dbg*)
      ; (ndbg-roman-nl *gate-dbg* night "")
      (setq qplans-es
            (map 'list (lambda (r) (qplan-generate1 r (cons r rules-in-path)))
             rules))
      ; (ndbg-roman-nl *gate-dbg* night "qplans-es =")
      ; (pretty-print qplans-es *gate-dbg*)
      ; (ndbg-roman-nl *gate-dbg* night "")
      ; qplans-es = ((qplan1.1 qplan1.2) (qplan2.1))
      ; -or- qplans-es = (nil (qplan2.1))
      ; Take the cross product
      (if (and (null? (cdr qplans-es))
               (null? (car qplans-es)))
          nil
          (setq qplans-es (cross-product (embed-nils qplans-es))))
      ; qplans-es = ((qplan1.1 qplan2.1) (qplan1.2 qplan2.1))
      ; -or- qplans-es = ((nil qplan2.1))
      ; (ndbg-roman-nl *gate-dbg* night "(crossed) qplans-es =")
      ; (pretty-print qplans-es *gate-dbg*)
      ; (ndbg-roman-nl *gate-dbg* night "")
      ; qplans-es = ((qplan1.1 qplan2.1) (qplan1.1 qplan2.1))
      (yloop
       (initial (rules-used nil) (qqplan nil))
       (yfor qplans in qplans-es)
       (ydo
        ; qplans = (qplan1.1 qplan2.1)
        ; Convert the qplans for subrules into a single qplan for rule
        ; (ndbg-roman-nl *gate-dbg* night "qplans =")
        ; (pretty-print qplans *gate-dbg*)
        ; (ndbg-roman-nl *gate-dbg* night "")
        (setq rules-used
             (cons rule (map-app (lambda (qplan)
                                         (if (nil? qplan)
                                             nil
                                             (qplan-get-rules-used qplan)))
                                 qplans)))
        (setq qqplan (cons rule (map 'list (lambda (qplan)
                                            (if (nil? qplan)
                                                nil
                                                (qplan-get-qqplan qplan)))
                                    qplans)))
        ; (ndbg-roman-nl *gate-dbg* night "rules-used = ~A" rules-used)
        ; (ndbg-roman-nl *gate-dbg* night "qqplan =")
        ; (pretty-print qqplan *gate-dbg*)
        ; (ndbg-roman-nl *gate-dbg* night "")
        (setq result (cons (list rules-used qqplan) result))
        ; (ndbg-roman-nl *gate-dbg* night "result =")
        ; (pretty-print result *gate-dbg*)
        ; (ndbg-roman-nl *gate-dbg* night "")
        )))
     (yresult result)))))

(defun embed-nils (lst)
  (yloop
   (initial (result nil))
   (yfor elem in lst)
   (ydo 
        (if (not (nil? elem))
            (setq result (append result (list elem)))
            (setq result (append result '((nil))))))
   (yresult result)))

(defun qplan-get-rules-used (qplan)
  (if qplan
      (car qplan)
      nil))

(defun qplan-get-qqplan (qplan)
  (if qplan
      (cadr qplan)
      nil))

;
; Returns:
; ((subrule1 subrule2 ... subrulen)
;  (subrule1 subrule2 ... subrulen)
;  ...)
; if rule has n subgoals
;
(defun subrules (rule omit-rules)
  ; (ndbg-roman-nl *gate-dbg* night "Getting subrules for ~A" rule)
  (yloop
   (initial 
    (subrule nil) (subgoalnum nil)
    (result (list-of-n nil (ob$get rule 'number-of-subgoals))))
   (yfor chain-num in (ob$gets rule 'backward-chain-nums))
   (ydo 
    (setq subrule (car chain-num))
    (setq subgoalnum (cadr chain-num))
    (if (and (plan? subrule)
             (not (memq? subrule omit-rules)))
        (setf (nth-elem result subgoalnum)
              (cons subrule (nth-elem result subgoalnum)))))
   (yresult
    ; For each subgoal having no rules, insert a single nil
    (yloop
     (initial (i 0))
     (ywhile (< i (ob$get rule 'number-of-subgoals)))
     (ydo (if (nil? (nth-elem result i))
              (setf (nth-elem result i) '(nil))
              nil)
         (setq i (+ 1 i))))
    ; Return the cross product
    (setq result (cross-product result))
    ; (ndbg-roman-nl *gate-dbg* night "Returning ~A" result)
    result)))

(defun list-of-n (elem n)
  (yloop
   (initial (result nil) (i 0))
   (ywhile (< i n))
   (ydo (setq result (cons elem result))
       (setq i (+ 1 i)))
   (yresult result)))

;
; (cross-product '((a b) (c d))) => ((a c) (a d) (b c) (b d))
; (cross-product '((a b) ()) => ()
; (cross-product '((a b) (nil)) => ((a nil) (b nil))
; (cross-product '((nil) (nil)) => ((nil nil))
; Preserves order of elements, but not order of lists
;
(defun cross-product (lst)
  (cond
   ((nil? lst) nil)
   ((nil? (cdr lst))
    (map 'list (lambda (elem) (list elem))
         (car lst)))
   (else
    (yloop
     (initial (result nil))
     (yfor rest-cross in (cross-product (cdr lst)))
     (ydo
      (yloop
       (yfor elem in (car lst))
       (ydo (setq result (cons (cons elem rest-cross) result)))))
     (yresult result)))))

;*******************************************************************************
; Qplan printing
;*******************************************************************************

(defun qplans-es-print (qplans-es)
  (yloop
   (yfor qplans in qplans-es)
   (ydo (qplans-print qplans))))

(defun qplans-print (qplans)
  (yloop
   (yfor qplan in qplans)
   (ydo (qplan-print qplan))))

(defun qplan-print (qplan)
  (let ((rules (car qplan))
        (qqplan (cadr qplan)))
       (qqplan-print qqplan 0)))

(defun qqplan-print (qqplan indent)
  (cond
   ((nil? qqplan)
    (print-spaces *gate-dbg* indent)
    (ndbg-roman-nl *gate-dbg* night "~A" 'LEAF))
   (else
    (print-spaces *gate-dbg* indent)
    (ndbg-roman-nl *gate-dbg* night "~A" (car qqplan))
    (yloop
     (yfor subqqplan in (cdr qqplan))
     (ydo (qqplan-print subqqplan (+ 1 indent)))))))

;*******************************************************************************
; Qplan generation:
;        Generate the plan in English.
;       Also, instantiate it.
; Todo: Later on, we would use the full-blown DAYDREAMER planner to instantiate
;       a plan.
;*******************************************************************************

(defun qplans-es-gen (qplans-es topgoal english)
  (yloop
   (yfor qplans in qplans-es)
   (ydo (qplans-gen qplans topgoal english))))

(defun qplans-gen (qplans topgoal english)
  (yloop
   (yfor qplan in qplans)
   (ydo (qplan-gen qplan topgoal english))))

(defun qplan-gen (qplan topgoal english)
  (let ((rules (car qplan))
        (qqplan (cadr qplan)))
       (ndbg-roman-nl *gate-dbg* night "----")
       (qqplan-gen qqplan 0 topgoal english)
       (ndbg-roman-nl *gate-dbg* night "----")))

(defun qqplan-gen (qqplan indent goal english)
  (cond
   ((nil? qqplan)
    ;(print-spaces *gate-dbg* indent)
    ;(ndbg-roman-nl *gate-dbg* night "~A" 'LEAF)
    )
   (else
    (let ((subgoals (rule-embedded-subgoals (car qqplan)))
          (bd nil))
         (if goal
             (setq bd (ob$unify (ob$get (car qqplan) 'goal) goal *empty-bd*))
             (setq bd *empty-bd*))
         (if (nil? bd)
             nil
             ;(ndbg-roman-nl *gate-dbg* night "(Does not unify.)")
             (progn
              (setq qqplan (cdr qqplan))
              (yloop
               (initial (subgoal nil) (subqqplan nil))
               (ywhile (or (not (null? subgoals))
                          (not (null? qqplan))))
               (ydo (setq subgoal nil)
                   (if (not (null? subgoals))
                       (progn (setq subgoal (car subgoals))
                              (setq subgoal (ob$instantiate-o subgoal bd))
                              (qplan-gen-subgoal subgoal indent english)
                              (setq subgoals (cdr subgoals))))
                   (if (not (null? qqplan))
                       (progn 
                        (setq subqqplan (car qqplan))
                        (qqplan-gen subqqplan (+ 1 indent) subgoal english)
                        (setq qqplan (cdr qqplan))))))))))))

(defun qplan-gen-subgoal (subgoal indent english)
    (print-spaces *gate-dbg* indent)
    (if english
        (gn subgoal)
        (pod subgoal)))

;*******************************************************************************
; Metaphor selection
;*******************************************************************************

;
; Returns ((rule1 occurrences1) (rule2 occurrences2) ...)
; where (eq? (ob$get rulei 'script) t)
; Order of list is rules with greater occurrences to less
; occurrences.
; If occurrences = (length rules) then we have found a
; bona fide intersection.
;
(defun select-metaphor (rules)
  (uniquify-count-occurrences-and-sort
   (map-app (lambda (rule) (find-scripts-above rule)) rules)))

; (A A B B B C) =>
; ((B 3) (A 2) (C 1))
(defun uniquify-count-occurrences-and-sort (lst)
  (yloop
   (initial (result nil)
            (entry nil))
   (yfor elem in lst)
   (ydo (if (setq entry (mem (lambda (x y) (eq? x (car y))) elem result))
           (progn
            (setq entry (car entry))
            (setf (cadr entry) (+ 1 (cadr entry))))
           (setq result (cons (list elem 1) result))))
   (yresult (sort result (lambda (a b) (> (cadr a) (cadr b)))))))

(defun find-scripts-above (rule)
  (find-scripts-above1 rule (list rule)))

(defun find-scripts-above1 (rule rules-in-path)
  (yloop
   (initial (result nil))
   (yfor superrule in (ob$gets rule 'forward-chain))
   (ydo
    (if (and (plan? superrule)
             (not (memq? superrule rules-in-path)))
        (if (ob$get superrule 'script)
            (setq result (cons superrule result))
            (setq result
                 (append result
                         (find-scripts-above1 superrule
                                              (cons superrule
                                                    rules-in-path)))))))
   (yresult result)))

; End of file.
