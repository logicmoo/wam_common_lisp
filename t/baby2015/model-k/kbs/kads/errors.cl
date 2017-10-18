;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;; *** error-print-function ***

(defun print-error (fkt-name errno &rest varlist)
  (declare (special extended-lists))
  (format t "~%Error in ")
  (if (keywordp fkt-name)
    (format t "behavior ~:S : ~%" fkt-name)
    (format t "function ~:S : ~%" fkt-name))
  (case errno
    ; error-messages concerning the mixins
    ( 1 (format t "the type of slotname ~:S does not belong to ~:S!"
                (first varlist) (<- extended-lists :get 'value)))
    ( 2 (format t 
         "the current value of the slotname ~:S has no ~:S - part (there is nothing to remove)!"
                (first varlist)))

    ; error-messages concerning the domain-layer
    (10 (format t "too few arguments in prove-call!"))
    (11 (format t "~:S is not a valid slotname of the instance ~:S!"
                (first varlist) (second varlist)))
    (12 (format t "~:S is not a valid instance of frame ~:S!" 
                (first varlist) (second varlist)))
    (13 (format t "only one unbound variable for relations on concepts allowed!"))
    (16 (format t "the output-format ~:S must not be used for the argument-list ~:S!"
                (first varlist) (second varlist)))
    (17 (format t "the two lists ~:S and ~:S have not the same length!"
                (first varlist) (second varlist)))
    (18 (format t "only 2 arguments and not ~D with the :UNKNOWN option allowed!"
                (first varlist)))
    (19 (format t 
         "the truth-value ~:S can not be used here because the relation ~:S is not assumable!"
                (first varlist) (second varlist)))
    
    ; error-messages concerning the inference-layer
    (20 (format t "the value ~:S is not bound to the slot ~:S, so it can't be removed!"
                (first varlist) (second varlist)))
    
    ; error-messages concerning the task-layer
    (21 (format t "~:S is not of the expected type ~:S!"
                (first varlist) (second varlist)))
    (22 (format t "the body ~:S has not the correct format for a <control-statement>!"
                (first varlist)))
    (23 (format t "the expected body after the keyword ~:S is empty!"))
    (24 (format t "wrong keyword in ~:S (the keyword ~:S was expected)!"
                (first varlist) (second varlist)))
    (25 (format t "there is no task among the tasks '~:S' whose precondition is fullfilled!"
                (first varlist)))
    (26 (format t "the task ~:S hangs because its precondition ~:S is not T!"
                (first varlist) (second varlist)))
    (27 (format t "there is no task to select next!"))
    (30 (format t 
         "INTERNAL ERROR: the nesting-level will become < zero (its current value is ~:S)!"
                (first varlist)))

    (t (format t ". Error in errorfunction: no casestatement found for errornumber ~D." errno)))
  nil)   ; print-error returns allways nil
      
;;; eof

