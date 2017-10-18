;;;  -*-  Mode: Lisp; Syntax: Common-Lisp; Base: 10. ;Package: BABYLON -*-

(in-package "BABYLON")

;;------------------------------------------------------------------------
;;          englische kommandos fuer basic-prolog-mixin
;;------------------------------------------------------------------------



(defbabylon-entry prolog cmd-table english 
  '(" Clause Operations " :value (:open-menu :prolog)
    :documentation "A menu of clause operations."))

(defbabylon-entry prolog-commands cmd-table english
  `((" Start Prove " :value :prove-display
     :documentation "Set topgoals and find first solution")
    (" Next Solution " :value  (:prove-display *)
     :documentation "Find next solution of topgoals")
    (" Set Format " :value  :select-format
     :documentation "Set display format for results")
    (" Redisplay " :value  (:display-result nil redisplay)
     :documentation "Redisplay last result")
    (" " :no-select t)
    (" Choose Axioms " :value  :select-load-axioms
     :documentation "Select available axiom sets")	  
    (" List Axioms " :value  :list-axioms
     :documentation "List clauses from known axiom sets")
    (" Show Status " :value  :show-status
     :documentation "Displays informations on selected options.")
    ))


;;------------------------------------------------------------------------
;;          englische kommandos fuer mini-prolog-mixin
;;------------------------------------------------------------------------


(defbabylon-entry prolog-toggle-command cmd-table english 
  '((" " :no-select t)
    (" Toggle Prolog Trace"
     :value :toggle-prolog-trace
     :documentation "Start or stop tracing.")))


(defbabylon-entry prolog-trace cmd-table english
    '(" Prolog Trace Options "
     :value (:open-menu :prolog-trace)))


(defbabylon-entry prolog-trace-commands cmd-table english 
  '((" Full Mode " :value (:set-prolog-trace-mode full)
     :documentation "show all clauses tried to match")
    (" Normal Mode " :value (:set-prolog-trace-mode normal)
     :documentation "show no clauses")
    (" " :no-select t)
    (" Trace All " :value (:trace-preds all)
     :documentation "trace all predicates")   
    (" Select Predicates " :value (:select-for-trace t)
     :documentation "select predicates to be traced.")
    (" Modify Selection " :value :select-for-trace
     :documentation "modify selection of predicates to be traced.")
    (" " :no-select t)
    (" Show Status " :value :show-trace-status
     :documentation "show options selected")))


;; eof
