;;; -*- Mode: Lisp; Base:10; Syntax: Common-Lisp; Package: BABYLON -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHOR:   J. W A L T H E R


;;This is the English version of all the strings and menu-item-lists of 
;;the rule processor. 


(defbabylon-table rule-io-table english :size 200)


;;; **************** syntax ****************


(defbabylon-entry rule-set-name-error-fstr rule-io-table english
  "~S: wrong name for a rule set of knowledge base ~S.~@
   The name must be a symbol.")

(defbabylon-entry rule-set-name-error-spot-fstr rule-io-table english
  "in rule set ~S of knowledge base ~S")

(defbabylon-entry rule-lhs-example-str rule-io-table english
  "(<JUNCTOR> <premise-1> ... <premise-N>)")

(defbabylon-entry rule-rhs-example-str rule-io-table english
  "(<ACTION-TYPE> <action-1> ... <action-N>)")

(defbabylon-entry rule-example-fstr rule-io-table english
  "~%(<RULE-NAME>~@
   ~4@T~A~@
   ~4@T~A")

(defbabylon-entry rule-syntax-error-fstr rule-io-table english
  "wrong syntax in rule ~S~@
   ~A.~@
   The right rule format is:~@
   ~A")

(defbabylon-entry rule-error-description-fstr rule-io-table english
  "~%>>Wrong syntax in rule ~S ~A.")

(defbabylon-entry rule-correct-description-fstr rule-io-table english
  "The right rule format is:~@
   ~A")

(defbabylon-entry rule-variables-error-fstr rule-io-table english
  "~S: ~@
   Wrong variable specification ~A. ~@
   The variable specification must be a list of underscore symbols.")


;;; **************** rule statistics ****************


(defbabylon-entry rule-statistics-header-str rule-io-table english
  "~2%;; ************** R U L E S ************~2%")

(defbabylon-entry rule-sets-header-fstr rule-io-table english
  "~%- Number of RULE SETS: ~38T~D")

(defbabylon-entry rule-set-header-fstr rule-io-table english
  "~%  - Number of RULES in rule set~%    ~S ~38T~D")

(defbabylon-entry rule-statistics-trailer-str rule-io-table english
  "~%- Total number of RULES: ~38T~D")


;;; **************** tracing ****************

(defbabylon-entry active-str rule-io-table english
  "Active")

(defbabylon-entry inactive-str rule-io-table english
  "Inactive")

(defbabylon-entry is-needed-to-show-fstr rule-io-table english
  " ~A is needed to ~S: ")

(defbabylon-entry is-hypothesis-fstr rule-io-table english
  " ~A is hypothesis ")

(defbabylon-entry attempt-to-verify-str rule-io-table english
  " Attempt to verify ")

(defbabylon-entry click-why-str rule-io-table english
  "Click here to know why.")

(defbabylon-entry rule-str rule-io-table english
  " -> Rule ")

(defbabylon-entry click-show-str rule-io-table english
  "Click here to show this rule.")

(defbabylon-entry conclude-str rule-io-table english
  " concludes ")

(defbabylon-entry execute-str rule-io-table english
  " executes ")

(defbabylon-entry sharp-sign-str rule-io-table english
  " ## ")

(defbabylon-entry try-rule-str rule-io-table english
  " -> Trying Rule ")

(defbabylon-entry test-hypothesis-fstr rule-io-table english
  " ->> Hypothesis = ~S.")


;;; **************** term matching ****************

(defbabylon-entry a-rule-set-fstr rule-io-table english
  " ~S set")

(defbabylon-entry which-rule-set-to-inspect-str rule-io-table english
  " Which rule set to inspect ?")

(defbabylon-entry match-choose-item-list rule-io-table english
  '(("                        " :no-select t)
    (" Find All Terms         " :value equal
     :documentation "Inspect all rule terms")
    (" Match First Element    " :value filter-first
     :documentation "Inspect first element of rule terms")
    (" Match Second Element   " :value filter-second
     :documentation "Inspect second element of rule terms")
    (" Match First and Second " :value filter-first-and-second
     :documentation "Inspect first and second element of rule terms")
    ("                        " :no-select t)
;    (" CHOOSE RULE SET        " :value choose-rule-set
;     :documentation "Choose a rule set to inspect")
    ("                        " :no-select t)
    (" E X I T                " :value do-nothing
     :documentation "Exit Inpection Menu")))

(defbabylon-entry match-choose-menu-str rule-io-table english
  " What match type do you want to select the terms ?")

(defbabylon-entry match-choose-element-first-str rule-io-table english
  " Choose a first element for match: ")

(defbabylon-entry match-choose-element-second-str rule-io-table english
  " Choose a second element for match: ")

(defbabylon-entry match-choose-slots-str rule-io-table english
  " Choose one of the slots: ")

(defbabylon-entry match-choose-term-str rule-io-table english
  " There are ~S terms in the ~S rules.     
 ~% Which ones do you want to inspect ?~%     ")

(defbabylon-entry used-as-condition-str  rule-io-table english
  "------ Used as condition in: ------")

(defbabylon-entry used-as-action-str rule-io-table english
  "------ Used as action in:    ------")

(defbabylon-entry list-rules-for-term-fstr rule-io-table english
  " Below You find the rules which use the term~@
   ~@T~S.~@
   ~@TChoose the rules to display:                     ")

;;; **************** add rule ****************


(defbabylon-entry add-to-which-rule-set-str rule-io-table english
  " Select the rule set to add the rule to ! ")

(defbabylon-entry add-rule-prompt-fstr rule-io-table english
  " Shall I now add the rule ~S to rule set ~S ? ")

;;; **************** delete rule ****************


(defbabylon-entry in-which-rule-set-to-delete-str rule-io-table english
  " Select the rule set to delete the rule from ! ")

(defbabylon-entry which-rule-to-delete-fstr rule-io-table english
  " Delete which Rule from ~S ? ")

(defbabylon-entry rule-fstr rule-io-table english
  " RULE ~S")

(defbabylon-entry in-rhs-str rule-io-table english
  " => in RHS of:")

(defbabylon-entry in-lhs-str rule-io-table english
  " => in LHS of:")

(defbabylon-entry prev-rule-fstr rule-io-table english
  " PREVIOUS RULE: ~S")

(defbabylon-entry wrong-display-choice-fstr rule-io-table english
  "~%Wrong choice in :DISPLAY-RULE ~A")

;;; **************** print rule ****************

(defbabylon-entry in-which-rule-set-to-print-str rule-io-table english
  " In which rule set is the rule to print ?")

(defbabylon-entry which-rule-to-print-fstr rule-io-table english
  " Print which ~S Rule ? ")

;;; **************** edit rule ****************

(defbabylon-entry in-which-rule-set-to-edit-str rule-io-table english
  " In which rule set is the rule to edit ?")

(defbabylon-entry which-rule-to-edit-fstr rule-io-table english
  " Edit which ~S Rule ? ")


(defbabylon-entry which-rule-set-to-edit-str rule-io-table english
  " Edit which rule set ? ")

;;; **************** prolog stuff ****************

(defbabylon-entry prolog-goal-spec-str rule-io-table english
  "<PROLOG GOAL>")

(defbabylon-entry input-spec-fstr rule-io-table english
  "<INPUT ~S>")

(defbabylon-entry input-spec-str rule-io-table english
  "<INPUT USER>")

;;; **************** display rule tree ****************


(defbabylon-entry which-rule-set-to-display-str rule-io-table english
  " Select the rule set to display: ")

(defbabylon-entry match-choose-header-str rule-io-table english
  " What match type do you want to select the terms ?")

(defbabylon-entry match-choose-first-str rule-io-table english
  " Choose a first element for match: ")

(defbabylon-entry match-choose-second-str rule-io-table english
  " Choose a second element for match: ")

(defbabylon-entry choose-one-slot-str rule-io-table english
  " Choose one of the slots: ")

(defbabylon-entry choose-terms-fstr rule-io-table english
  " There are ~S terms in the ~S rules.     
 ~% Which ones do you want to display?~%     ")


;;; **************** general ****************

(defbabylon-entry exit-label-str rule-io-table english
  " E X I T ")

(defbabylon-entry confirm-str rule-io-table english
  " Confirm: ")

(defbabylon-entry do-nothing-str rule-io-table english
  "Do nothing")

(defbabylon-entry yes-str rule-io-table english
  " Yes ")

(defbabylon-entry no-str rule-io-table english
  " No  ")

(defbabylon-entry rule-sort-function-arg-error-fstr rule-io-table english
  "==>> ~S : wrong number of arguments in RULE-SORT-FUNCTION ~S~@
   ~A")

(defbabylon-entry rule-sort-function-undef-error-fstr rule-io-table english
  "==>> ~S : undefined RULE-SORT-FUNCTION~@
   ~A")

(defbabylon-entry rule-sort-function-error-fstr rule-io-table english
  "==>> ~S : wrong RULE-SORT-FUNCTION~@
   ~A")

(defbabylon-entry hypotheses-spec-number-error-fstr rule-io-table english
  "==>> ~S : wrong specification for the number of hypotheses to verify~@
   ~9@Tin :TEST-HYPOTHESES ~S for knowledge base ~S.")

(defbabylon-entry hypotheses-spec-list-error-fstr rule-io-table english
  "==>> ~S: wrong specification for the list of hypotheses to verify~@
   ~9@Tin :TEST-HYPOTHESES for knowledge base ~S.")

(defbabylon-entry hypotheses-spec-number-error-obtain-fstr rule-io-table english
  "==>> ~S : wrong specification for the number of hypotheses to verify~@
   ~9@Tin :OBTAIN ~S for knowledge base ~S.")

(defbabylon-entry method-property-error-fstr rule-io-table english
  "~S has no rule processor method property on ~S.")

(defbabylon-entry bindings-spec-error-fstr rule-io-table english
  "~S~@
   Wrong bindings specification for~@
   ~S of rule set ~S.")

(defbabylon-entry rule-set-not-found-error-fstr rule-io-table english
  "~%==>> There is no rule set ~S in the rule base.")

(defbabylon-entry rule-does-not-exist-error-fstr rule-io-table english
  "~S ~S not yet defined.")

(defbabylon-entry ask-user-wrong-answer-error-str rule-io-table english
  "Wrong answer to rule-interpreter :ask-user.")

(defbabylon-entry justifications-type-error-str rule-io-table english
  "~%unknown type of justificans ~S")

(defbabylon-entry fact-type-error-fstr rule-io-table english
  "~%~S: wrong fact type.")

(defbabylon-entry justifications-missing-error-fstr rule-io-table english
  "There is no justification for ~S")

(defbabylon-entry said-to-be-true-fstr rule-io-table english
  "You said that ~S is true.")

(defbabylon-entry said-to-be-false-fstr rule-io-table english
  "You said that ~S is false.")

(defbabylon-entry said-to-be-unknown-fstr rule-io-table english
  "You said that ~S is unknown.")

(defbabylon-entry how-description-fstr rule-io-table english
  "~S was ~Sd by ~S ~S.")

(defbabylon-entry not-provable-fstr rule-io-table english
  "~S could not be proved in rule set ~S.")

(defbabylon-entry evaluation-msg-fstr rule-io-table english
  " Now I am evaluating rule ~S ~S.")

(defbabylon-entry already-established-msg-str rule-io-table english
  " It is already established that:")

(defbabylon-entry false-true-msg-fstr rule-io-table english
  " If ~S is ~:[false,~;true,~]")

(defbabylon-entry is-false-msg-fstr rule-io-table english
  " ~S ~S is false")

(defbabylon-entry is-true-msg-fstr rule-io-table english
  " ~S ~S is true")

(defbabylon-entry then-msg-fstr rule-io-table english
  " then I can ~S ")

(defbabylon-entry since-msg-fstr rule-io-table english
  " Since ~S")

(defbabylon-entry i-have-to-msg-fstr rule-io-table english
  " I have to ~S ")

(defbabylon-entry results-header-msg-str rule-io-table english
  " The results are :")

(defbabylon-entry no-positive-results-msg-str rule-io-table english
  " There are no positive results !")

(defbabylon-entry rule-set-fstr rule-io-table english
  " ~S set")

(defbabylon-entry to-display-question-str rule-io-table english
  " Do you want to display the rule just created ? ")

(defbabylon-entry delete-item-fstr rule-io-table english
  " Delete ~S")

(defbabylon-entry display-item-fstr rule-io-table english
  " Display ~S")

(defbabylon-entry edit-item-fstr rule-io-table english
  " Edit ~S")

(defbabylon-entry type-fstr rule-io-table english
  " ~S type")

(defbabylon-entry no-point-str  rule-io-table english
  " No. ")

(defbabylon-entry how-str  rule-io-table english
  " How ? ")

(defbabylon-entry how-str-mouse-doc  rule-io-table english
  "Give a one-step explanation of one from all proved facts")

(defbabylon-entry how-all-str rule-io-table english
  " How all ? ")

(defbabylon-entry how-all-str-mouse-doc  rule-io-table english
  "Give a one-step explanation of one from all evaluated facts")

(defbabylon-entry how-ultimately-str   rule-io-table english
  " How ultimately ? ")

(defbabylon-entry how-ultimately-str-mouse-doc rule-io-table english
  "Give the evaluation tree of one from all proved facts")

(defbabylon-entry why-not-str rule-io-table english
  " Why not ? ")

(defbabylon-entry why-not-str-mouse-doc rule-io-table english
  "Give the evaluation tree of one of the could not be proved facts")

(defbabylon-entry print-rule-item-str  rule-io-table english
  " Print Rule ")

(defbabylon-entry lisp-item-str  rule-io-table english
  " LISP ")

(defbabylon-entry other-questions-str  rule-io-table english
  "  Other questions ? ")

(defbabylon-entry no-display-available-str rule-io-table english
  " Display method not available ")

(defbabylon-entry print-rule-term-header-str rule-io-table english
  " Print rules for which term ? ")

(defbabylon-entry rule-ted-str rule-io-table english
  "Rule TED")

(defbabylon-entry how-which-fact-str rule-io-table english
  "How which fact ? ")

(defbabylon-entry meta-rule-reference-trace-fstr rule-io-table english
  " META -> RULE-PROCESSOR ~S  ~S")

;;;;;;; added e.gross 28.11.86

(defbabylon-entry hypotheses-verified-fstr rule-io-table english
 "~&~%hypotheses verified: ~{~%   ~S~} ~%")

(defbabylon-entry no-hypothesis-verified-fstr rule-io-table english
 "~&~%no hypothesis could be verified. ~%")

(defbabylon-entry  true-facts-fstr rule-io-table english
  "~&~%following assertions are true: ~{~%   ~S~} ~%")

(defbabylon-entry  no-true-facts-fstr rule-io-table english
  "~&~%there are no true assertions. ~%")


(defbabylon-entry suspend-item rule-io-table  english
  '("- SUSPEND MENU -" :value suspend
    #+:lispm :font #+:lispm fonts:cptfontb
    ))


(defbabylon-entry exit-menu-item rule-io-table  english
  '("-- EXIT MENUE --" :value exit
    #+:lispm :font #+:lispm fonts:cptfontb
    ))


(defbabylon-entry type-end-to-continue-str rule-io-table  english
  "[Type ~:C to continue] ")


(defbabylon-entry select-rule-set-str rule-io-table  english
  " Select rule set ")


(defbabylon-entry which-rule-fstr rule-io-table english
  " List Which Rule Of ~S ?")

;;;----------------------------------------------------------------
;;; added for 2.1
;;;----------------------------------------------------------------

(defbabylon-entry mark-menu-title rule-io-table english
  " Mark rules from rule set ")

(defbabylon-entry mark-menu-items rule-io-table english
  '(("Show all" :value show-all)
    ("Hide all" :value hide-all)
    ("Toggle mark" :value toggle
     :documentation "Select rules whose occurrence in trace should be altered")
    ("Exit" :value exit)))

(defbabylon-entry toggle-mark-menu-title rule-io-table english
   " toggle mark for rules from ")		  

(defbabylon-entry toggle-mark-menu-item rule-io-table english
   '(nil "Rules marked by # will occur in trace" nil))

(defbabylon-entry ask-user-str rule-io-table english
  "asks user for")

(defbabylon-entry no-entry-str rule-io-table english
  "no entry qualified")


(defbabylon-entry option-menu-title rule-io-table english
 "Select Rule Trace Options")

(defbabylon-entry option-menu-items rule-io-table english
  '(("- Exit -" :value exit)
    ("Immediate Trace"
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
     :documentation "All rules from all rule sets occur in trace")
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

;;; eof

