;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-



;
;       PROLOG-SCHNITTSTELLE
;


(defaxiom-set constraint-satisfaction
  
  ((candidate _value-ass . _external-varlist) <-
   (is _choices (get-list-of-choices '_external-varlist '_value-ass))
   (choose-tuple _external-varlist _choices))
  
  ((choose-tuple (_var = _ . _external-varlist) (_valuelist . _choices)) <-
   (choose-value _var _valuelist)
   (choose-tuple _external-varlist _choices))
  ((choose-tuple nil nil))
  
  ((choose-value _var unconstrained))
  ((choose-value _var _valuelist) <- 
   (member _var _valuelist))
  
  ((member _x (_x . _tail)))
  ((member _x (_h . _tail)) <- 
   (member _x _tail)))

;;; eof

