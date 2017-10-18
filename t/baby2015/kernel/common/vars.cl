;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

(defvar *babylon-version* "2.3")

(defvar *babylon* nil)
	 
(defvar *current-knowledge-base* nil)

(defvar *language* 'english)

(defvar *known-knowledge-bases* nil)	



(defvar *axiom-sets* nil)			;from prolog-processor

(defvar	*maxvar* 0)

(defvar *prolog-syspreds* nil)

(defvar *prolog-preds-traced* nil) 


  
(defvar *font-menu* nil)			;from interface
						; muss bleiben, da auch im autonomen
                                          	; tree editor verwendet. er	 
(defvar *first-ped-interface-call* t)

(defvar *current-ted* nil			;from ted-interface
  "the unique current tree editor which can ~@
  be controlled via function calls in a lisp listener.")


;
;(defvar *crashing-item* nil)			;from frame-constraints
;
;(defvar *relations-for-create-rule* nil)	;from frame-processor



(defvar *meta-preds*
	'(not call and or))

(defvar *frame-meta-predicates*		
	'(frame frame-def super has-super instance instance-def has-slot slot
		has-property property))

(defvar *free-text-meta-predicates*	
	'(free-text))

(defvar *rule-meta-predicates*
	'(rule-set rule-set-def has-rule rule))

(defvar *prolog-junctor-for-rules*	
	'(and or))




;;----------------------------------------------------------------------------------------


(defvar *default-language* 'english)

(defvar *default-procs*
	'(mini-frame-mixin
	   mini-rule-mixin
	   mini-prolog-mixin
	   free-text-mixin
	   lisp-mixin))

(defvar *default-interface* '(mini-interface-mixin))

(defvar *default-kb-configuration* 'mini-frplx-mini) 


(defvar *default-dialog-stream* *standard-output*)

(defvar *help-key* #\?)

(defvar *c-help-key* #\$)

(defvar *end-key* #\return)

(defvar *bab-prompt* "===>")

(defvar *var-prefix* #\_)

(defvar *max-menu-entries* 20)

(defvar *item-width* 50)

;;; eof

