;;;  -*-  Mode: Lisp; Syntax: Common-Lisp; Base: 10. ;Package: BABYLON -*-

(in-package "BABYLON")

;;------------------------------------------------------------------------
;;          englische kommandos fuer basic-rule-mixin
;;------------------------------------------------------------------------



(defbabylon-entry rule cmd-table english 
  '(" Rule Operations " :value (:open-menu :rule)
    :documentation "A menu of rule operations."))

(defbabylon-entry rule2 cmd-table english 
  '(" Rule Operations " :value (:toggle-menu :rule :top)
    :documentation "A menu of rule operations."))


(defbabylon-entry rule-commands cmd-table english
  `(("List Rules" :value :list-rules
     :documentation "List selected rules.")))


;;------------------------------------------------------------------------
;;          englische kommandos fuer mini-rule-mixin
;;------------------------------------------------------------------------

;;; mini-rule-mixin kommandos ersetzt fuer 2.1

(defbabylon-entry rule-trace-maincommands cmd-table english 
  '(("Toggle Rule Trace"
     :value :toggle-rule-trace
     :documentation "Start or stop tracing.")
    ("Display Rule Trace"
     :value :call-rule-trace-displayer
     :documentation "Display stored rule trace according to the display options set.")))

(defbabylon-entry rule-trace cmd-table english
    '("Rule Trace Options"
     :value (:open-menu :rule-trace)))

(defbabylon-entry rule-trace-commands cmd-table english
  '(("Immediate Trace"
     :value (:set-rule-trace-mode direct)
     :documentation "Show trace immediately.")
    ("Background Trace"
     :value (:set-rule-trace-mode back)
     :documentation "Store trace without displaying.")
    ("Combined Trace"
     :value (:set-rule-trace-mode comb)
     :documentation "Store trace and show immediately.")
    ("  " :no-select t)
    ("Mark all Rules" :value (:mark-all show)
     :documentation "All rules from all rule sets occur in trace.")
     ("Select Rules" :value (:select-rules-for-tracing t)
     :documentation "Select rules to occur in trace.")
    ("Modify Selection" :value :select-rules-for-tracing
     :documentation "Modify Selection of rules to occur in trace.")
    ("  " :no-select t)
    ("Complete Trace"
     :value (:set-rule-trace-displayer :display-rule-trace)
     :documentation "Show complete trace on calling display rule trace.")
    ("Rules Used"
     :value (:set-rule-trace-displayer :display-rules-used) 
     :documentation "Show where rules are used on calling display rule trace.")
    ("Rules Tried"
     :value (:set-rule-trace-displayer :display-rules-tried)
     :documentation "Show where rules are tried on calling display rule trace.")
    ("-----------------------" :no-select t)
    ("Display Rule Trace" :value :call-rule-trace-displayer
     :documentation "Display stored rule trace according to the display options set.")))


;;------------------------------------------------------------------------
;;          englische kommandos fuer normal-rule-mixin
;;------------------------------------------------------------------------



(defbabylon-entry rule-develop-commands cmd-table english 
  '(("  " :no-select t)
    ("Explain Results" :value :explain-results
      :documentation "Explain results")
    ("Display Rule" :value :print-rule
     :documentation "Display a rule.")
    ("Inspect Rule Terms" :value :inspect-terms
     :documentation
     "Inspect conditions and actions of rules.")))


;; eof
