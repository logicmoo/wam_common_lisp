;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

; ========================================================
; =========== DEFINITION OF KNOWLEDGE-SOURCES ============
; ========================================================


; --------------------------------------------------------
; reset-metaclasses
; values of the metaclasses will be reset to nil.

(DEF-KNOWLEDGE-SOURCE reset-metaclasses  
  WITH input-metaclasses = -
  output-metaclasses = ((COMPONENTS-NOT-POSSIBLE-TO-INTEGRATE 
                         LEGAL-PLACES-FOR-COMPONENTS
                         VALID-ARRANGEMENTS
                         PROPOSED-VALID-ARRANGEMENTS
                         POSSIBLE-ARRANGEMENTS CONDITIONS-BETWEEN-CONCERNED-COMPONENTS
                         CONDITIONS-BETWEEN-ARRANGED-COMPONENTS
                         CONDITIONS-FOR-COMPONENT-TO-INTEGRATE-NEXT
                         COMPONENTS-TO-ARRANGE COMPONENT-TO-INTEGRATE-NEXT
                         ARRANGED-COMPONENTS 
                         LOCATION-SPECIFIC-REQUIREMENTS 
                         COMPONENT-SPECIFIC-REQUIREMENTS)) 
  formalism = message-passing
  functions-used = -)

(DEF-KNOWLEDGE-SOURCE-BODY reset-metaclasses ()
  (dolist (one-metaclass (get-all-instances 'metaclass))
    (<- one-metaclass :reset)))
       
; --------------------------------------------------------
; initialize-requirements
; both kinds of metaclasses for requirements will be initialized.

(DEF-KNOWLEDGE-SOURCE initialize-requirements  
  WITH input-metaclasses = -
       output-metaclasses = ((component-specific-requirements
                              location-specific-requirements))
       formalism = message-passing
       functions-used = -)

(DEF-KNOWLEDGE-SOURCE-BODY initialize-requirements ()
  (declare (special component-specific-requirements location-specific-requirements))
  (<- component-specific-requirements :put 'value (get-instance-list 'component-specific-requirement))
  (<- location-specific-requirements  :put 'value (get-instance-list 'location-specific-requirement)))
       

; --------------------------------------------------------
; init
; the relation initial-arrangement defines initial occupancies.
; the meaning for (initial-arrangement hall employee) is - 
; the employee has still to be placed.

(DEF-KNOWLEDGE-SOURCE init
  WITH input-metaclasses  = -
       output-metaclasses = ((arranged-components
                              legal-places-for-components
;                              valid-arrangements ;; yet not!
                              components-to-arrange))
       formalism = prolog
       functions-used = -)

(DEF-KNOWLEDGE-SOURCE-BODY init ()
  (declare (special components-to-arrange arranged-components))
  (<- components-to-arrange :reset)
  (<- arranged-components :reset)
  (send-kb :prolog-mult-prove '(init)))

(DEF-CLAUSE-SET init
  ((init)
   <-
   (initial-arrangement hall _emp )
   (LISP (<- components-to-arrange :add _emp))
   (LISP (<- _emp :put 'legal-rooms (get-instance-list 'room))))
  ((init)
   <-
   (initial-arrangement _room _emp )
   (/== _room hall)
   (LISP (<- arranged-components :add _emp))
   (LISP (<- _emp :put 'legal-rooms (list _room)))))
   
; --------------------------------------------------------
; select
; the next employee to integrate will be selected among components to arrange.

(DEF-KNOWLEDGE-SOURCE select
  WITH input-metaclasses = -
       output-metaclasses = ((components-to-arrange))
       formalism = lisp
       functions-used = -)

(DEF-KNOWLEDGE-SOURCE-BODY select ()
  (declare (special components-to-arrange component-to-integrate-next))
  (let ((selected-component (first (<- components-to-arrange :get 'value))))
    (<- component-to-integrate-next :reset)
    (<- component-to-integrate-next :add selected-component)))

; --------------------------------------------------------
; join
; the set of conditions for already arranged components will be extended
; by the conditions for component to integrate next.

(DEF-KNOWLEDGE-SOURCE join
  WITH input-metaclasses = ((conditions-for-component-to-integrate-next
                             conditions-between-arranged-components))
       output-metaclasses = ((conditions-between-concerned-components))
       formalism = message-passing
       functions-used = -)

(DEF-KNOWLEDGE-SOURCE-BODY join ()
  (declare (special conditions-between-concerned-components
                    component-to-integrate-next
                    conditions-for-component-to-integrate-next
                    conditions-between-arranged-components
                    ))
  (<- conditions-between-concerned-components :reset)
  (<- conditions-between-concerned-components :add 
      (<- conditions-for-component-to-integrate-next :get 'value))
  (<- conditions-between-concerned-components :add 
      (<- conditions-between-arranged-components :get 'value)))
       
; --------------------------------------------------------
; find-conditions-for-component-to-arrange-next
; conditions for the employee to integrate next and conditions between
; him and employees already placed will be extracted.

(DEF-KNOWLEDGE-SOURCE find-conditions-for-component-to-arrange-next
  WITH input-metaclasses = ((component-to-integrate-next
                             arranged-components))
       output-metaclasses = ((conditions-for-component-to-integrate-next))
       formalism = message-passing
       functions-used = ((find-conditions-for
                          find-conditions-between)))

(DEF-KNOWLEDGE-SOURCE-BODY find-conditions-for-component-to-arrange-next ()
  (declare (special conditions-for-component-to-integrate-next
                    arranged-components
                    component-to-integrate-next
                    ))
  (<- conditions-for-component-to-integrate-next :reset)
  (DOLIST (emp1 (<- component-to-integrate-next :get-true 'value))
    (format t "~%~%What to regard for ~A  ?" emp1)
    (find-conditions-for emp1)
    (DOLIST (emp2 (<- arranged-components :get-true 'value))
      (format t "~%~%What to regard between ~A and ~A ?" emp1 emp2)
      (find-conditions-between emp1 emp2))))
    
(defun find-conditions-for (emp1)
  (declare (special conditions-for-component-to-integrate-next 
                    component-specific-requirements
                    ))
  (dolist (req (<- component-specific-requirements :get-true 'value))
    (let ((pre-cond-relation (first (<- req :get-true 'precondition))))
;      (format t "~% Checking requirement ~A" req)
      (when (equal (prove `(,pre-cond-relation ,emp1) :PROPERTY) 'true) 
        (format t "~%  ->> ~A ~A because of the requirement ~A" emp1 (<- req :get-true 'condition) req)
        (let* ((condition (<- req :get-true 'condition))
               (condition-expr (list condition emp1)))
          (<- conditions-for-component-to-integrate-next :add (list condition-expr)))))))

(defun find-conditions-between (emp1 emp2)
  (declare (special component-specific-requirements
                    conditions-for-component-to-integrate-next
                    ))
  (DOLIST (req (<- component-specific-requirements :get-true 'value))
    (LET ((pre-cond-relation (first (<- req :get-true 'precondition))))
;      (format t "~% Checking requirement ~A" req)
      (WHEN (EQUAL (prove `(,pre-cond-relation ,emp1 ,emp2 ) :PROPERTY) 'true) 
        (format t "~%  ->> They ~A because of the requirement ~A" (<- req :get-true 'condition) req)
        (LET* ((condition (<- req :get-true 'condition))
               (condition-expr (LIST condition emp1 emp2)))
          (<- conditions-for-component-to-integrate-next :add (list condition-expr)))))))

; --------------------------------------------------------
; propose-arrangements-covering
; the constraint-net for concerned components will be satisfied.
; the conditions between them restrict the set of possible occupancies,
; which are initialy set to all legal-rooms for each employee.

(DEF-KNOWLEDGE-SOURCE propose-arrangements-covering
  WITH input-metaclasses = ((conditions-between-concerned-components
                             legal-places-for-components))
       output-metaclasses = ((possible-arrangements))
       formalism = message-passing
       functions-used = ((transfer-conditions-exprs-to-constraints-exprs
                          extract-concerned-components
                          construct-constraint-net
                          print-old-legal-places-for-components
                          satisfy-constraint-net
                          change-arrangements-format-to-alist
                          update-metaclass-possible-arrangements)))

(DEF-KNOWLEDGE-SOURCE-BODY propose-arrangements-covering ()
  (declare (special arrangements-alist 
                    consat-arrangements-result 
                    conditions-between-concerned-components
                    ))
  (let* ((constraint-exprs-list 
          (transfer-conditions-exprs-to-constraints-exprs 
           (<- conditions-between-concerned-components :get 'value)))
         (concerned-components (extract-concerned-components constraint-exprs-list)))
    (construct-constraint-net constraint-exprs-list)
    (print-old-legal-places-for-components concerned-components)
    (setf consat-arrangements-result (satisfy-constraint-net concerned-components))
    (setf arrangements-alist (change-arrangements-format-to-alist consat-arrangements-result))
    (update-metaclass-possible-arrangements arrangements-alist)))

(defun transfer-conditions-exprs-to-constraints-exprs (condition-expr-list)
  (mapcar #'(lambda (condition-expr)
              (let ((condition (first condition-expr)))
                (cons (<- condition :get 'primitive-constraint) (rest condition-expr))))
          condition-expr-list))

(defun extract-concerned-components (prim-constraint-exprs-list)
  (declare (special all-concerned-components))
  (let ((all-concerned-comps nil))
    (dolist (prim-constraint prim-constraint-exprs-list) 
      (let ((concerned-comps (rest prim-constraint)))
        (setf all-concerned-comps 
              (append all-concerned-comps concerned-comps))))
    (setf all-concerned-components (reverse (union all-concerned-comps nil)))))

(defun construct-constraint-net (constraint-exprs-list)
  (def-constraint-net 'NET17 constraint-exprs-list)
  (format t "~%~%~% ~A" "** Constructing the constraint network with the conditions **")   
  (dolist (x constraint-exprs-list) (format t "~% ~A" x))) ;printing constraint-net  

(defun print-old-legal-places-for-components (all-concerned-components)
  (format t "~%~% ~A" "** The possible rooms for the employees previously was:  **")  
  (dolist (comp all-concerned-components) ; print old values
    (format t "~%  ~A ~A" comp (<- comp :get 'legal-rooms))))

(defun satisfy-constraint-net (concerned-components)  
  (format t "~%~% ~A~%" "** Satisfying the constraint network **")
  (let ((interface-var-init-list nil))
    (dolist (comp concerned-components) ; 2. satisfying of the constraint-net
      (setf interface-var-init-list 
            (append interface-var-init-list `(,comp = (<- ,comp :get 'legal-rooms)))))  
    (eval `(satisfy net17 :GLOBALLY :WITH ,@interface-var-init-list))))

(defun change-arrangements-format-to-alist (arrangements-in-consat-format) ; this means (emp room) ... to (room emp1 emp2..)
  (mapcar #'(lambda (one-occu-in-consat-format)
              (let ((result-alist nil))
                (dolist (x one-occu-in-consat-format result-alist)
                  (let (
                        (emp (first x))
                        (room (second x)))
                    (setf result-alist (add-to-alist result-alist room emp))))))
          arrangements-in-consat-format))

(defun update-metaclass-possible-arrangements (arrangement-alist)
  (declare (special possible-arrangements))
  (<- possible-arrangements :reset)
  (<- possible-arrangements :add arrangement-alist))

; --------------------------------------------------------
; filter
; the possible arrangements will be tested for meeting the local specific
; requirements.arrangements don`t meet the requirements will be abandoned.

; --------------------------------------------------------
; filter

(DEF-KNOWLEDGE-SOURCE filter
  WITH input-metaclasses = ((possible-arrangements))
       output-metaclasses = ((proposed-valid-arrangements))
       formalism = message-passing
       functions-used = -)

(DEF-KNOWLEDGE-SOURCE-BODY filter ()
  (declare (special proposed-valid-arrangements possible-arrangements))
  (let ((occupancy-alist-format (<- possible-arrangements :get 'value)))
    (<- proposed-valid-arrangements :reset)
    (<- proposed-valid-arrangements :add 
        (remove nil (mapcar #'(lambda (one-occup-alist)
                    (if (check-validy-of-possible-arrangement one-occup-alist)
                      one-occup-alist))   
                occupancy-alist-format)))
    (print-proposed-valid-arrangements)))
    

(defun check-validy-of-possible-arrangement (one-possible-arrangement)
  (declare (special location-specific-requirements))
  (let ((result one-possible-arrangement)
        (occ-model-ok T))
    (dolist (occ-of-one-room one-possible-arrangement occ-model-ok)
      (let ((room (first occ-of-one-room))
            (emps-in-room (rest occ-of-one-room)))
        
        ;(format t "~%~%~% The new occu: ~A" one-possible-arrangement)
        (when (apply-filter-out-functions-p (<- location-specific-requirements :get 'value) room emps-in-room)
          (setf result nil)
          (return result))))
    ;(format t "~% result-for-total-occu= ~A" result)
    result))


(defun apply-filter-out-functions-p (mc-location-specific-requirements room emps-in-room-list)
  ;(format t "~% One room occu= ~A ~A"  room emps-in-room-list)
  (let ((test-result nil))
    (dolist (one-loc-req mc-location-specific-requirements test-result)
      (let ((test-function (compile nil (car (<- one-loc-req :get 'test-function)))))
        (when (funcall test-function room emps-in-room-list) 
          (setf test-result T))))
    ;(format t "~% test-result-for-room= ~A" test-result)
    test-result))

(defun print-proposed-valid-arrangements ()
  (declare (special PROPOSED-VALID-ARRANGEMENTS))
  (format t "~% ~A" "** The following valid arrangements are proposed: **")  
  (dolist (one-occup (<- proposed-valid-arrangements :get 'value))
    (print one-occup)))

  
; --------------------------------------------------------
; update
; update for the metaclasses valid-arrangements,arranged-components,
; conditions-between-arranged-components and legal-places-for-components.

(DEF-KNOWLEDGE-SOURCE update
  WITH input-metaclasses = ((proposed-valid-arrangements
                             component-to-integrate-next
                             arranged-components))
       output-metaclasses = ((valid-arrangements
                              arranged-components
                              conditions-between-arranged-components
                              legal-places-for-components))
       formalism = message-passing
       functions-used = -)

(DEF-KNOWLEDGE-SOURCE-BODY update ()
  (declare (special component-to-integrate-next 
                    conditions-between-concerned-components 
                    arranged-components 
                    proposed-valid-arrangements))
  (if (<- proposed-valid-arrangements :get 'value)
    (progn
      (update-mc-valid-arrangements 
       (<- proposed-valid-arrangements :get 'value))
      (update-mc-legal-places-for-components 
       (<- proposed-valid-arrangements :get 'value) 
       (<- component-to-integrate-next :get 'value) 
       (<- arranged-components :get 'value))
      (update-mc-conditions-between-arranged-components
       (<- conditions-between-concerned-components :get 'value))
      (update-mc-arranged-components
       (<- component-to-integrate-next :get 'value))
      (update-mc-components-to-arrange
       (<- component-to-integrate-next :get 'value)))
  (progn
    (update-mc-components-not-possible-to-integrate
     (<- component-to-integrate-next :get 'value))
    (update-mc-components-to-arrange
     (<- component-to-integrate-next :get 'value)))))
    
(defun update-mc-valid-arrangements (mc-proposed-valid-arrangements)
  (declare (special valid-arrangements))
  (<- valid-arrangements :reset)
  (<- valid-arrangements :add mc-proposed-valid-arrangements))

(defun update-mc-legal-places-for-components 
       (mc-all-valid-arrangements mc-component-to-integrate-next mc-arranged-components)        
  (let ((concerned-components (union mc-component-to-integrate-next mc-arranged-components))) 
    (dolist (comp concerned-components)              ;reset old values  
      (<- comp :put 'legal-rooms nil)))
  (dolist (one-valid-occu mc-all-valid-arrangements) ;set the new values
    (dolist (one-room-occu one-valid-occu)
      (let ((room (first one-room-occu))
            (comps-in-room (rest one-room-occu)))
        (dolist (one-comp-in-room comps-in-room)
          (<- one-comp-in-room :put 'legal-rooms 
              (union (<- one-comp-in-room :get 'legal-rooms) (list room))))))))

(defun update-mc-conditions-between-arranged-components (mc-conditions-between-concerned-components)
  (declare (special conditions-between-arranged-components))
  (<- conditions-between-arranged-components :reset)
  (<- conditions-between-arranged-components :add mc-conditions-between-concerned-components))

(defun update-mc-arranged-components (mc-component-to-integrate-next)
  (declare (special arranged-components))
  (<- arranged-components :add mc-component-to-integrate-next))

(defun update-mc-components-to-arrange (mc-component-to-integrate-next)
  (declare (special components-to-arrange))
  (<- components-to-arrange :remove  (first mc-component-to-integrate-next)))

(defun update-mc-components-not-possible-to-integrate (mc-component-to-integrate-next)
  (declare (special components-not-possible-to-integrate))
  (<- components-not-possible-to-integrate :add  (first mc-component-to-integrate-next)))


;;; eof

