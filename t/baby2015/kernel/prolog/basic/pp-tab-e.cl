;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base:10 -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHOR:   Eckehard Gross


;;; generate hash-table ;;;;;;;;;;;;;;;;

(defbabylon-table prolog-io-table english :size 100)



;;;;;;;; entries for prolog-mixin ;;;;;;


(defbabylon-entry type-end-to-continue-str prolog-io-table english
  "[Type ~:C to continue] ")

(defbabylon-entry rel-name-fstr prolog-io-table english "~S-CLAUSES")

(defbabylon-entry  sys-trace-fstr prolog-io-table english
  " META -> PROLOG ~S  ~S")

(defbabylon-entry no-current-kb-fstr prolog-io-table english
  "~%No current knowledge base")

(defbabylon-entry no-prolog-fstr prolog-io-table english
  "Current knowledge base without prolog")

(defbabylon-entry no-develop-entry prolog-io-table english
  '(" No develop environment available " :no-select t))


;;;;;;;; entries for ax-basic ;;;;;;;;;;;

(defbabylon-entry clause-syntax-error-fstr prolog-io-table english
  "~S wrong clause syntax.")

(defbabylon-entry unknown-axset-fstr prolog-io-table english
  "~%Unknown axiom set: ~S")

(defbabylon-entry  clauses-header-fstr prolog-io-table english
  "~% --- Clauses for predicate ~S of ~S --- ~% ")

(defbabylon-entry no-ax-fstr prolog-io-table english "~%No axioms.")

(defbabylon-entry list-ax-fstr prolog-io-table english "~%Axioms: ~{~S   ~}")

(defbabylon-entry unknown-mode-fstr prolog-io-table english "~%unknown mode ~S")

(defbabylon-entry relations-fstr prolog-io-table english
  "~2%;; ********* C L A U S E S ************~%")

(defbabylon-entry number-of-relations-fstr prolog-io-table english
  "~%- Number of CLAUSES: ~38T~D")

;;;;;;;; ENTRIES FOR AX-SC ;;;;;;;;;;;;;;;;;

(defbabylon-entry a-list-str prolog-io-table english "a list")

(defbabylon-entry goals-prompt-fstr prolog-io-table english "~&~%Goals = ")

(defbabylon-entry explain-goal-format-fstr prolog-io-table english
  "~%Format: <goal> or (<goal1> ... <goalN>) ~@
  ~8@Twith goal = (<pred> <arg1> ... <argN>) or <variable>")

(defbabylon-entry result-fstr prolog-io-table english "~%Result: ~S")

(defbabylon-entry no-fstr prolog-io-table english "~%NO")

(defbabylon-entry status-fstr prolog-io-table english "~%Status: ~S")

(defbabylon-entry yes-fstr prolog-io-table english "~%YES")

(defbabylon-entry status-str prolog-io-table english "Status")

(defbabylon-entry wrong-format-fstr prolog-io-table english
  "~S: wrong format.")

;;;;;;;; entries for syspreds ;;;;;;;;;

(defbabylon-entry abort-fstr prolog-io-table english
  "~%Syntax error at goal: ~S")

(defbabylon-entry not-instant-fstr prolog-io-table english
  "~%Goal: ~S not instantiated")

(defbabylon-entry illegal-pred-fstr prolog-io-table english
  "~%Predicate of goal: ~S illegal")

(defbabylon-entry not-eval-fstr prolog-io-table english
  "~%form: ~S not evaluable")

(defbabylon-entry wrong-argument-fstr prolog-io-table english
  "~%~S wrong argument for ~S.")

(defbabylon-entry illegal-clause-fstr prolog-io-table english
  "~%~S illegal argument for clause.")


;;;;;;;;;; entries from syspreds-trace ;;;;;;;;

(defbabylon-entry first-proof-fstr prolog-io-table english
  "~A. . . . . first proof of ~S . . . . .")

(defbabylon-entry next-proof-fstr prolog-io-table english
  "~A. . . . . next proof of ~S . . . . .")

(defbabylon-entry top-cut-fail-fstr prolog-io-table english
  "~A- - - - top level cut-fail - - - ")

(defbabylon-entry normal-try-fstr prolog-io-table english
  "~A=> try: ~S")

(defbabylon-entry normal-retry-fstr prolog-io-table english
  "~A=> retry: ~S")

(defbabylon-entry normal-succ-fstr prolog-io-table english
  "~A<= succ: ~S")

(defbabylon-entry normal-fail-fstr prolog-io-table english
  "~A<= fail: ~S")

(defbabylon-entry cut-fail-fstr prolog-io-table english
  "~A<= cut-fail: ~S")

(defbabylon-entry top-cut-fstr prolog-io-table english
  "~A.......top level cut ........")

(defbabylon-entry cut-fstr prolog-io-table english
  "~A..........cut............")

(defbabylon-entry forced-fail-fstr prolog-io-table english
  "~A- - - - - forced fail - - - - - - ")

(defbabylon-entry succ-lisp-fstr prolog-io-table english
  "~A+ + succeeded lisp-call: ~S")

(defbabylon-entry fail-lisp-fstr prolog-io-table english
  "~A- - failed lisp-call: ~S")

(defbabylon-entry succ-is-fstr prolog-io-table english
  "~A+ + succeeded is: ~S == ~S = ~S")

(defbabylon-entry fail-is-fstr prolog-io-table english
  "~A- - failed is: ~S =//= ~S = ~S")

(defbabylon-entry repeat-fstr prolog-io-table english
  "~A- - - - ~S repetition - - - - - - ")

(defbabylon-entry succ-equal-fstr prolog-io-table english
  "~A+ + succeeded = : ~S and ~S unifiable")

(defbabylon-entry fail-equal-fstr prolog-io-table english
  "~A- - failed = : ~S and ~S not unifiable")

(defbabylon-entry succ-noequal-fstr prolog-io-table english
  "~A+ + succeeded \= : ~S and ~S not unifiable")

(defbabylon-entry fail-noequal-fstr prolog-io-table english
  "~A- - failed \= : ~S and ~S unifiable")

(defbabylon-entry succ-read-fstr prolog-io-table english
  "~A+ + succeeded read: ~S == ~S")

(defbabylon-entry fail-read-fstr prolog-io-table english
  "~A- - failed read: ~S =//= ~S")

(defbabylon-entry write-fstr prolog-io-table english
  "~A+ + written: ~S")

(defbabylon-entry format-fstr prolog-io-table english
  "~A+ + written using format ~S : ~{~S ~}")

(defbabylon-entry succ-type-fstr prolog-io-table english
  "~A+ + succeeded type-check: ~S is of type ~S")

(defbabylon-entry fail-type-fstr prolog-io-table english
  "~A- - failed type-check: ~S is not of type ~S")

(defbabylon-entry assert-fstr prolog-io-table english
  "~A+ + added to ~S: ~S")

(defbabylon-entry remove-fstr prolog-io-table english
  "~A+ + removed from ~S: ~S")

(defbabylon-entry pred-remove-fstr prolog-io-table english
  "~A+ + predicate ~S removed from ~S")

(defbabylon-entry pred-not-def-fstr prolog-io-table english
  "~A+ + predicate ~S was not defined")

;;;;;;;;;;;; entries for prolog-interpreter ;;;;

(defbabylon-entry cr-wrong-status-fstr prolog-io-table english
  "~%~S wrong status for ~S")

(defbabylon-entry wrong-status prolog-io-table english
  "~S wrong status for ~S")

(defbabylon-entry wrong-mode prolog-io-table english
  "~S: wrong mode for :PROVE.")

(defbabylon-entry next-solution-str prolog-io-table english
  " next solution ")


;;;;;;;;;;  entries for trace-mixin ;;;;;;;;;;;;;;;

;;; ersetzt fuer 2.1

;;;;;;;;;;;; entries for axdevelop-mixin ;;;;;;


(defbabylon-entry no-axioms-item prolog-io-table english
  '(no-axioms  " No Axioms at all" (t)))

(defbabylon-entry choose-axioms-str prolog-io-table english
  " Choose the relevant axioms: ")

(defbabylon-entry clause-prompt-fstr prolog-io-table english
  "CLAUSE (or ~:C) = ")

(defbabylon-entry clause-help-fstr prolog-io-table english
  "~&Input choice 1. = (<pred> <arg1> ... <argN>) ~@
   ~13@T2. = ((<pred> <arg1> ... <argN>) <- (<pred1> <arg1> ... <argN>) ...)~%")

(defbabylon-entry incorrect-syntax-fstr prolog-io-table english
  "~&==>> Incorrect syntax: ~S")

(defbabylon-entry answer-prompt-fstr prolog-io-table english
  "~S Answer (or RETURN to stop) = ")

(defbabylon-entry axset-prompt-fstr prolog-io-table english
  "~%Name of the axioms-base = ")

(defbabylon-entry overwrite?-fstr prolog-io-table english
  "~%~S already exists. Do you want to overwrite it? ~
                  (type Y, N or ~:C to stop): ")

(defbabylon-entry nr-clause-prompt-fstr prolog-io-table english
  "~S CLAUSE (or ~:C) = ")

(defbabylon-entry suspend-item prolog-io-table english
  '("- SUSPEND MENU -" :value suspend
    #+:lispm :font #+:lispm fonts:cptfontb
    ))

(defbabylon-entry exit-listing-item prolog-io-table english
  '("- EXIT LISTING -" :value exit
    #+:lispm :font #+:lispm fonts:cptfontb
    ))

(defbabylon-entry select-axset-str prolog-io-table english
  " Select Axiom Set ")

(defbabylon-entry all-item prolog-io-table english
  '("--- ALL ---" :value all
    #+:lispm :font #+:lispm fonts:cptfontb
    ))

(defbabylon-entry exit-menu-item prolog-io-table english
  '("-- EXIT MENUE --" :value exit
    #+:lispm :font #+:lispm fonts:cptfontb
    ))

(defbabylon-entry which-pred-fstr prolog-io-table english
  " List Which Predicate Of ~S ?")

(defbabylon-entry is-metavar-fstr prolog-io-table english
  " ~S is a metavariable ")

(defbabylon-entry is-varpred-fstr prolog-io-table english
  " ~S has variable predicate ")

(defbabylon-entry is-system-pred-fstr prolog-io-table english
  " ~S is a built-in-predicate ")

(defbabylon-entry syntax-error-fstr prolog-io-table english
  " syntax error ")

(defbabylon-entry no-clauses-for-pred-fstr prolog-io-table english
  "No clauses for predicate ~S")

(defbabylon-entry back-to-fstr prolog-io-table english
  " BACK to ~S ")

(defbabylon-entry exit-inspect-item prolog-io-table english
  '("-- EXIT INSPECT --" :value exit
    #+:lispm :font #+:lispm fonts:cptfontb
    ))

(defbabylon-entry exit-list-item prolog-io-table english
  '("-- EXIT LIST --" :value exit
    #+:lispm :font #+:lispm fonts:cptfontb
    ))

(defbabylon-entry edit-clauses-str prolog-io-table english
  " Edit Clauses From Axiom Set ?")

(defbabylon-entry edit-which-axset-str prolog-io-table english
  " Edit Which Axioms ?")

(defbabylon-entry file-prompt-fstr prolog-io-table english
  "~%Please give the name of the file ~@
   where to save the selected axiom sets ~@
   (default: ~A): ")

(defbabylon-entry nothing-saved-fstr prolog-io-table english
  "Nothing saved~%")

(defbabylon-entry save-confirm-fstr prolog-io-table english
  "~&Going to save axiom sets: ~{~S ~} on File: ~A ~@
                            Please confirm (Yes or No): ")

(defbabylon-entry done-fstr prolog-io-table english
  "Done~%")

(defbabylon-entry true?-fstr prolog-io-table english
  "~%Is this true: ~S ? ")

(defbabylon-entry ax-manipulation-str prolog-io-table english
  " Axioms Manipulation: ")

;;;;;;;;; entries for explain-mixin ;;;;;;;;

(defbabylon-entry subgoals-fstr prolog-io-table english
  "~%~%~3TSubgoal: ~S")

(defbabylon-entry topgoal-reached-fstr prolog-io-table english
  "~%~3TTopgoal reached")

(defbabylon-entry why-item-list prolog-io-table english
  '(Why-Goal Why-Path Exit))

(defbabylon-entry further-explanation-str prolog-io-table english
  " Further Explanation: ")

(defbabylon-entry needed-to-prove-fstr prolog-io-table english
  "~3Tneeded to prove: ~S")

(defbabylon-entry by-clause-fstr prolog-io-table english
  "~3Tby clause: ~S ~@[~S~] ~{~%~16T~S~}")

(defbabylon-entry clauses-for-pred prolog-io-table english
  "Clauses for predicate ~S")

(defbabylon-entry fictive-top-goal-str prolog-io-table english
  " fictive top goal ")

(defbabylon-entry built-in-goal-str prolog-io-table english
  "is built-in-goal")

(defbabylon-entry goal-fstr prolog-io-table english
  "  Goal: ~S ")

(defbabylon-entry clause-used-for-proof-fstr prolog-io-table english
  "Clause used to prove goal:  ~S ")

(defbabylon-entry no-clauses-fstr prolog-io-table english
  "  No clauses for goal: ~S ")

(defbabylon-entry prolog-ted-str prolog-io-table english
  "Prolog TED")

(defbabylon-entry clauses-str prolog-io-table english
  "Clauses")

(defbabylon-entry clauses-doc-str prolog-io-table english
  "Show all clauses.")

(defbabylon-entry clause-used-str prolog-io-table english
  "Clause Used")

(defbabylon-entry clause-used-doc-str prolog-io-table english
  "Show clause used.")

(defbabylon-entry prolog-prooftree-fstr prolog-io-table english
  "PROLOG-PROOFTREE~D")

(defbabylon-entry explain-mixin-item-list prolog-io-table english
  '((" Explain " :funcall  display-prooftree
     :documentation "Show prooftree")))

;;;;;; entries for prolog-processor ;;;;;;;

(defbabylon-entry choose-format-str prolog-io-table english
  " Choose Display Format Of Result: ")

(defbabylon-entry format-item-list prolog-io-table english
  '(("Goals With Substitutions" :value form)
    ("All Variables" :value vars)
    ("All Bound Variables" :value bound)
    ("No Output" :value no)))

(defbabylon-entry continue-str prolog-io-table english
  " Continue: ")

(defbabylon-entry return-to-prolog-fstr prolog-io-table english
  "Return to PROLOG Top Level")


;;;--------------------------------------------------------------------------------
;;; new entries for 2.1
;;;--------------------------------------------------------------------------------


(defbabylon-entry if-toggled-fst prolog-io-table english
  "~%If Prolog Trace toggled:")


(defbabylon-entry trace-for-preds-fstr prolog-io-table english
  "~%~S Trace for predicates:")

(defbabylon-entry none-fstr prolog-io-table english
  "  NONE")

(defbabylon-entry all-fstr prolog-io-table english
  "  ALL")


(defbabylon-entry trace-menu-title prolog-io-table english
  "Trace predicates from axiom set ")

(defbabylon-entry trace-menu-items prolog-io-table english
  '(("Trace all"    :value trace-all)
    ("Trace none"   :value trace-none)
    ("Toggle trace" :value toggle
     :documentation "Select predicates whose tracing should be altered")
    ("Exit" :value exit)))

(defbabylon-entry toggle-trace-menu-title prolog-io-table english
   "toggle tracing for predicates from ")


(defbabylon-entry toggle-trace-menu-item prolog-io-table english
   '(nil "Predicates marked by # will occur in trace" nil))


(defbabylon-entry option-menu-title prolog-io-table english
   "Select Prolog Trace Options")

(defbabylon-entry option-menu-items prolog-io-table english 
  '(("- Exit -" :value exit)
    ("Full Mode" :value (:set-prolog-trace-mode full)
     :documentation "show all clauses tried to match")
    ("Normal Mode" :value (:set-prolog-trace-mode normal)
     :documentation "show no clauses")
    (" " :no-select t)
    ("Trace All" :value (:trace-preds all)
     :documentation "trace all goals")
    ("Select Predicates" :value (:select-for-trace t)
     :documentation "select predicates to be traced.")
    ("Modify Selection" :value :select-for-trace
     :documentation "modify selection of predicates to be traced.")
    (" " :no-select t)
    ("Show Status" :value :show-trace-status
     :documentation "show options selected.")))

;;; eof

