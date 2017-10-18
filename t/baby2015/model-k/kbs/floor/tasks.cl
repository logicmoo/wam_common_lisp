;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")


(DEF-TASK plan-office
  WITH precondition = (true)
       goal         = initialisiere
       body         = ((
                       (CALL initialize)
                       (WHILE (> (length (<- components-to-arrange :get 'value)) 0) DO
                              ((CALL integrate-component)))
                       )))

(DEF-TASK initialize
  WITH precondition = (true)
       goal         = initialisiere
       body         = ((
                        reset-metaclasses
                        initialize-requirements
                        init
                        )))

(DEF-TASK integrate-component
  WITH precondition = true
       goal         = initialisiere
       body         = ((
                        select
                        find-conditions-for-component-to-arrange-next
                        join
                        propose-arrangements-covering
                        filter
                        update
                        )))
; Never forget:
; (initialize-task-layer)

;;; eof

