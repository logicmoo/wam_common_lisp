;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHOR:   F. di P R I M I O, J. W A L T H E R


;; ************** RULE-PROCESSOR *****************

(def$flavor normal-rule-processor
	   ()
	   (rule-explain-mixin			; for explanation
	    rule-develop-mixin			; for development
	    mini-rule-processor)
  (:documentation "The rule-processor flavor.
This flavor combines the rule-interpreter with all those optional
mixins, the user is wanting to be available. The available mixins include
the rule-trace-mixin for tracing, rule-explain-mixin for explanations
and rule-develop-mixin for development support."))


#-:FMCS(compile-$flavor-$methods normal-rule-processor)


;;; eof

