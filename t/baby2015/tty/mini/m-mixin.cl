;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;           Copyright   1987   BY
;;           G M D 
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;; DATE:     August 1987
;; AUTHOR:   E. Gross


;; CONTENTS: a basic interface mixin.

;;-------------------------------------------------------------------------------


(def$flavor mini-interface-mixin
	()
	(menu-loop basic-interface-mixin)
  :settable-instance-variables
  (:required-instance-variables language)
  (:documentation "contains a basic menu facility for tty-driven i/o.
                  also provides a menu loop mechanism"))



(def$method (mini-interface-mixin  :set-up-commands) ()
  "generates the command table for all possible menu entries of the configuration
and creates the list of items for the start-up menu"
  
  (let ((table (get 'cmd-table language)))
    ($send self :add-operations
	   :top (gethash 'kb-commands table))
    ($send self :add-operations
	   :top (list (gethash 'suspend-entry table)))
    ($send self :add-operations
	   :top (list (gethash 'exit-entry table)))
    ($send self :send-if-handles :set-up-frame-cmds)
    ($send self :send-if-handles :set-up-constraint-cmds)  
    ($send self :send-if-handles :set-up-rule-cmds)
    ($send self :send-if-handles :set-up-prolog-cmds)    
    ($send self :add-operations
	   :top (gethash 'submenu-entry table))
    ($send self :make-menus-current :top)))

;;; eof

