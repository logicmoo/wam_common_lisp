;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;           Copyright   1987    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;;  AUTHOR:  Eckehard Gross



(def$flavor mini-babylon
	()
	(menu-loop tty-dialog-mixin system-core))  

;;------------------------------------------------------------ 

(def$method (mini-babylon :set-up-commands) ()
  (let ((table (get 'cmd-table language)))
    ($send self :add-operations
		  :top (gethash 'system-commands table))
    ($send self :add-operations
		  :top `(,(gethash 'exit-entry table)))
    ($send self :make-menus-current :top)))

   
#+:FLAVORS(compile-$flavor-$methods mini-babylon) 


;;; eof

