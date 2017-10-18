;;;  -*-  Mode: Lisp; Syntax: Common-Lisp; Base: 10. ;Package: BABYLON -*-

(in-package "BABYLON")

;;           Copyright   1987   BY
;;           G M D 
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;; DATE:     November 1987
;; AUTHOR:   Eckehard Gross


;; Contents: a loop presenting a menu with dynamically changing  entries.
;;           

;;-------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------


(def$flavor menu-loop 
	()
	(menu-item-handler)
  :settable-instance-variables
  (:documentation "a mixin for the selection and execution of operations via menu
                   with dynamically changing entries.
                   keywords as result of a selection are interpreted as selectors for kb.
                   to return a keyword or a list with a keyword as head.
                   the keywords are interpreted as method selectors of the kb
                   the rest of the list is used as argument list for the method.
                   the arguments are not evaluated."))



(def$method (menu-loop :get-operation) ()
  "fetches operation from menu."
  
  (let ((result ($send self :choose-from-menu current-item-list)))
    (cond ((and (consp result)
		(keywordp (first result))) result)
	  ((keywordp result) (list result))
	  (t nil))))


(def$method (menu-loop :execute-operation) (operation-spec)
  "executes operation if it is supported
otherwise a notify-message is displayed to the user."
  
  (cond ((null operation-spec))
	(($send self :operation-handled-p (first operation-spec))
	 (lexpr-$send self (first operation-spec) (rest operation-spec)))
	(t ($send self :notify
		  (format nil (getentry unknown-operation-fstr babylon-io-table)
			  (first operation-spec)
			  ($send self :kb-name))))))


(def$method (menu-loop :run) ()
  "starts the get-operation/execute-operation loop
which stops when the exit-operation occurs."
  
  (do ((operation-spec ($send self :get-operation)
		       ($send self :get-operation)))	
      ((equal operation-spec '(:exit)) :exit)
    (if (eq ($send self :execute-operation operation-spec) :exit)
	(return :exit))))


(def$method (menu-loop :suspend) ()
  "waits for *end-key* to be entered."
  
  ($send self :type-end-to-continue
	 (getentry type-end-to-continue-str babylon-io-table)))

