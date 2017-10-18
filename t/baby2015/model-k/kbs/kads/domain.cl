;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

; =============================================
; =============== DOMAIN LAYER ================
; =============================================

(DEFFRAME domain-layer-constructs
  (SUPERS model-construct-mixin))


; ---------------- Concepts -------------------

(DEFFRAME domain-concept
  (SUPERS domain-layer-constructs))


(defmacro def-concept (&rest instance-spec)
  (let* ((name (first instance-spec))
         (supers (if (equal (first (second instance-spec)) 'SUPERS) 
                   (rest (second instance-spec))))
         (new-supers (cons 'domain-concept supers ))
         (rest-instance-spec (if (equal (first (second instance-spec)) 'SUPERS) 
                               (third instance-spec) (second instance-spec))))
    `(defframe ,name 
       (SUPERS ,@new-supers)
       ,rest-instance-spec)))


(defmacro def-concept-instance (&rest instance-spec)
  (let* ((name (first instance-spec))
         (name-spec (cons 'name (cons '= (list name))))
         (frame-name (if (equal (second instance-spec) 'OF) (third instance-spec) 
                         (first instance-spec)))
         (rest-instance-spec (if (equal (fourth instance-spec) 'WITH)
                               (nconc name-spec (nthcdr 4 instance-spec))
                               name-spec)))
    `(definstance ,name OF ,frame-name WITH ,@rest-instance-spec)))

#|
Example:
(DEF-CONCEPT person
  (SLOTS 
         (smoker - :POSSIBLE-VALUES :BOOLEAN)
         (person2 - :POSSIBLE-VALUES (:one-of-instances person))         
         (tolerating-smokers - :POSSIBLE-VALUES :BOOLEAN)
         (hobbies - :POSSIBLE-VALUES (:some-of-enumeration HOBBIES))))

(DEF-CONCEPT-INSTANCE willi OF person
  WITH 
  person2 = ralf-schukey
  hobbies = (((:true cycling squash))))

|#


(defmacro def-unnamed-concept-instance (&rest instance-spec)
  (let* ((name (gentemp (format nil "~A-" (first instance-spec))))
         (name-spec (cons 'name (cons '= (list name))))
         (frame-name (if (equal (second instance-spec) 'OF) (third instance-spec) 
                         (first instance-spec)))
         (rest-instance-spec (if (equal (fourth instance-spec) 'WITH)
                               (nconc name-spec (nthcdr 4 instance-spec))
                               name-spec)))
    `(definstance ,name OF ,frame-name WITH ,@rest-instance-spec)))

#|
(def-unnamed-concept-instance req1- OF instantiated-requirement
  WITH generic-requirement = observe-somker
       component1 = thomas-christaller
       component2 = angi-voss)
|#



; ----------------- Relations -----------------

(DEFFRAME relation
  (SUPERS domain-layer-constructs)
  (SLOTS (predicate  -)
         (domains -)
         (properties -)
         (assumable -)))


(defmacro def-relation (&rest relation-spec)
  `(definstance ,@relation-spec))

; to modify if one will omit ... of relation
;  `(create-babylon-instance relation ,relation-spec)

#|
Example:
(DEF-RELATION related-themes
  WITH domains = ((themes themes))
  properties = symmetric)
|#

#+:MCL
(defmacro def-clause-set (name &rest clauses)
  `(let ((buffer (is-known-window 
                  (first (send-kb :file-name)) 
                  #+:MCL 'kb-window
                  #-:MCL  *kb-window*)))
     (DEFAXIOM-SET ,name ,@clauses)
     (if buffer (setf (get ',name '%editor-buffer) buffer))
     (send-kb :add-axiom-set ',name)))

#-:MCL
(defmacro def-clause-set (name &rest clauses)
  `(progn (DEFAXIOM-SET ,name ,@clauses)
	  (send-kb :add-axiom-set ',name)))

; prove :


#|
                           *** calling framework for relations ***


call: ( prove ( <relation> <args> ) {<options>} )

      <relation> ::= <relation-name>   #1            -- belongs to a DEF-RELATION construct
                  |  <slot-name>       #2            -- access to a slot of a CONCEPT

      <args>     ::= (#1:)  <var_1> ... <var_n>      -- for Relations and Prolog
                     (#2:)  <instance>  <slot-value> -- for Concepts
                            (all different concepts must have different slotnames for identity reasons)

      if one of <args> should be an unbound variable it must have the form "_variable"


      <options>       ::= [<truth-value>]  [<number>]  [<mode>]  [<output-format>]  [<property>]


      <truth-value>   ::= :TRUE  |  :FALSE  |  :UNKNOWN  (only allowed if the relation is assumable)

      <number>        ::= 'ONE  |  'ALL  |  number     -- default is all

      <mode>          ::= :PROLOG  |  :CONCEPT

      <property>      ::= :PROPERTY  -- only allowed with relations
                                     -- only one property can be selected:   symetric ">" transitive

      <output-format> ::=  :COMBINATIONS  |  :VALUES

      :VALUES  if used there is only one unbound variable; prove returns a list of elements
               that can be matched with this variable;

      :COMBINATIONS  if there are more than one unbound variables (not allowed for the concept-part)
                     prove returns a list of sublists where in each sublist there are the found
                     unified values of the corresponding variables; otherwise

      nothing from above  prove returns a list of association-lists of the found value-pairs
                          which has the form ( ... ((_var`1 . value`1) ... (_var`n . value`n)) ... )
                          if there were n unbound variables _var`1 ... _var`n.

 
|#

(defun prove (relation-clause &rest options)
 ;  (print relation-clause)
  (let* (;(new-list nil)
         (user-defined-relations (get-all-instances 'relation))
         (relation-name (first relation-clause))
         (evaluated-relation-clause (cons relation-name 
                                          (eval-relation-arguments (rest relation-clause))))
         (nr-of-outvars (count-output-arguments (rest relation-clause)))   ; nr-of-out-variables
         (number 99999)          ; default-setting for number of proves
         (number-option (dolist (num options number)                       ; test for <number>
                          (cond ((equal num 'one) (setf number 1))
                                ((numberp num)    (setf number num)))))
         (property-option (if (member ':PROPERTY options)                  ; PROPERTY option set?
                            T nil)) 
         (output-option (cond ((member ':VALUES options) ':values)         ; output-format option set?
                              ((member ':COMBINATIONS options) ':combinations)
                              (T 'nil))) 
         (truth-value-option (cond ((member ':TRUE options) 'true)
                                   ((member ':FALSE options) 'false)
                                   ((member ':UNKNOWN options) 'unknown)
                                   (T 'true))) 
         (mode-option (cond ((member ':PROLOG  options) ':PROLOG)
                            ((member ':CONCEPT options) ':CONCEPT)
                            (T 'nil))))
    (cond ((and (equal output-option ':values) (/= 1 nr-of-outvars))
           (print-error 'prove 16 output-option (rest relation-clause)))
          ((and (equal output-option ':combinations) (< nr-of-outvars 1))
           (print-error 'prove 16 output-option (rest relation-clause)))
          (T (format-prove-output 
              (cond ((equal mode-option ':PROLOG)
                     (send-kb :prolog-mult-prove evaluated-relation-clause number-option))
                    ((equal mode-option ':CONCEPT)
                     (concept-prove evaluated-relation-clause              ; see case #2 above
                                    truth-value-option number-option nr-of-outvars))
                    ((member (first relation-clause) user-defined-relations)
                     (prolog-prove evaluated-relation-clause
                                   truth-value-option
                                   number-option
                                   property-option
                                   nr-of-outvars))                         ; see case #1 above
                    (T (concept-prove evaluated-relation-clause            ; see case #2 above
                                      truth-value-option number-option nr-of-outvars)))
              output-option)))))


;;; the biggest two subfunctions of function prove:

(defun concept-prove (relation-clause truth-value number nr-of-outvars)
  (if (> nr-of-outvars 1)                                   
    (print-error 'concept-prove 13)
    (let ((relation-name (first  relation-clause))
          (instance      (second relation-clause))
          (slot-value    (third  relation-clause)))
      (cond ((null instance) (print-error 'concept-prove 10))
            ((underscore-var-p instance)
; *** the instance is unbound ***
             (let* ((instance-list (get-all-instances-with-slot-x 'DOMAIN-CONCEPT relation-name))
                    (result (find-instances instance-list relation-name slot-value truth-value number)))
                     ; look for the instances that have the slot-value
               (make-a-list (list instance) result)))
            ((not (member relation-name (<- instance :slots)))
; *** the instance is bound ...
             (print-error 'concept-prove 11 relation-name instance)) ; but the slot doesn't belong to it
            ((and (not (listp slot-value))
                  (underscore-var-p slot-value))                            
; *** and the slot-value is unbound ***
             (let ((value (get-value-with-truth-value truth-value instance relation-name)))
               (if (null value) 'false                               ; do a simple slot-access
                   (make-a-list (list slot-value) (convert-to-list value)))))
            (t
; *** everything is bound ***
             (let* ((value (get-value-with-truth-value
                            truth-value instance relation-name))
                    (result (compare-values value slot-value)))      ; compare value with slot-value 
               (convert-to-extended-boolean result)))))))


(defun prolog-prove (relation-clause truth-value number property-option nr-of-out-vars)
  (declare (special property))
  (let* ((relation-name (first relation-clause))
         (assumable (<- relation-name :get 'assumable))
         (properties (convert-to-list (<- relation-name :get 'properties)))
         (symetric-property (and property-option (member 'symetric properties)))
         (transitive-property (and property-option (member 'transitive properties))))
    (if (check-par-types (<- relation-name :get 'domains) (cdr relation-clause))
      (prog2 (if symetric-property (setf relation-clause (cons 'sym relation-clause))
                 (if transitive-property (setf relation-clause (cons 'trans relation-clause))))
             (if (true-p assumable)
               ; *** the relation is assumable ***
               (cond ((or (true-p truth-value) (false-p truth-value))
                      (let* ((extended-relation-clause (append relation-clause (list truth-value)))
                             (result1 (send-kb :prolog-mult-prove extended-relation-clause number)))
                        (if (null result1)
                          ; ask the other way round that is with the opposite truth-value
                          (let ((new-clause (substitute (opposite truth-value)
                                                        truth-value extended-relation-clause)))
                            (if (null (send-kb :prolog-mult-prove new-clause number))
                              'unknown    ; neither true- nor false-clause found => unknown
                              (if (= 0 nr-of-out-vars) 
                                'false nil)))                           ; else return false or nil
                          (convert-to-extended-boolean result1))))      ; return result unchanged
                     ((unknown-p truth-value)                           ; needs a special treatement
                      (if (or symetric-property transitive-property)
                        (setf property (first relation-clause)
                              relation-clause (cdr relation-clause))
                        (setf property nil))
                      ; implicit shortens relation-clause again
                      (prove-unknown relation-clause truth-value number property-option nr-of-out-vars)))
               (if (true-p truth-value)
                 ; *** the relation is not assumable ***
                 (let ((result (send-kb :prolog-mult-prove relation-clause number)))
                   (convert-to-extended-boolean result))                ; return result unchanged
                 (print-error 'prolog-prove 19 truth-value relation-name)))))))

;;; the biggest subfunction of function prolog-prove:

(defun prove-unknown (relation-clause truth-value number property-option out-vars)
  (declare (ignore truth-value property-option))
  (if (or (= 3 (length relation-clause)) (= 0 out-vars))
    (case out-vars   ; depending on the number of unbound arguments
      (0 (let* ((extended-relation-clause (append relation-clause (list 'true)))
                (result1 (send-kb :prolog-mult-prove extended-relation-clause number)))
           (if (null result1)  ; ask the other way round that is with the opposite truth-value
             (let ((new-clause (append relation-clause (list 'false))))
               (if (null (send-kb :prolog-mult-prove new-clause number))
                 'true                                  ; neither true- nor false-clause found => true
                 'false))                               ; else return false
             'false)))                                  ; return false
      (1 (let* ((relation (first relation-clause))
                (relation-type (<- relation :get 'domains))
                (type (unbound-type (rest relation-clause) relation-type 1))
                (whole-list (get-instance-list type))
                (bound-position (count-output-arguments (list (second relation-clause))))
                (bound-var (list (if (= 0 bound-position)
                                   (second relation-clause) (third relation-clause))))
                (cross-list (if (null bound-var)
                              (mapcar #'list whole-list)
                              (if (= 0 bound-position)      ; generate all possible pairs
                                (crossproduct bound-var whole-list)
                                (crossproduct whole-list bound-var))))
                (unknown-list (remove-true-and-false cross-list relation 1)))
           (if (null unknown-list) nil                             ; nothing found
               (let ((simple-list (if (= 0 bound-position)
                                    (mapcar #'second unknown-list) (mapcar #'first unknown-list)))
                     (unbound-var (if (= 0 bound-position)
                                    (third relation-clause) (second relation-clause))))
                 (make-a-list (list unbound-var) simple-list)))))  ; return the result
      (2 (let* ((relation (first relation-clause))
                (relation-args (rest relation-clause))
                (relation-type (<- relation :get 'domains))
                (type1 (unbound-type relation-args relation-type 1))
                (type2 (unbound-type relation-args relation-type 2))
                (cross-list (crossproduct                  ; generate all possible pairs
                             (get-instance-list type1) (get-instance-list type2)))
                (unknown-list (remove-true-and-false cross-list relation 2)))
           (if (null unknown-list) nil                             ; nothing found
               (make-a-list relation-args unknown-list 2)))))
    (print-error 'prove-unknown 18 (1- (length relation-clause)))))


;;; other subfunctions:

;;; help-functions for function prove:

(defmacro eval-relation-arg (arg)
  `(if (listp ,arg) (eval ,arg)               ;arg is quoted
       (cond ((underscore-var-p ,arg) ,arg)
             ((boundp ,arg) (eval ,arg))
             (T (eval ,arg)))))

(defun eval-relation-arguments (relation-args)
; eval the arguments of the relation
  (let ((result nil))
        (dolist (arg relation-args result) 
    (setf result (append result (list (eval-relation-arg arg)))))))
    
(defun underscore-var-p (symbol-name)
; returns <> nil if the variable symbol-name begins with an underscore
  (string= "_" (char (symbol-name symbol-name) 0)))

(defmacro count-output-var (arg)
  `(if (listp ,arg) 0               ;arg is quoted
       (cond ((underscore-var-p ,arg) 1)
             ((boundp ,arg) 0)
             (T (eval 0)))))

(defun count-output-arguments (relation-args)
; count the number of the unbound variables
  (let ((result 0))
        (dolist (arg relation-args result) 
    (setf result (+ result (count-output-var arg))))))
    
(defun format-prove-output (result-of-prove output-option)
  ; format the prove-output according to the <output-format> option
  (if (extended-boolean-p result-of-prove)
    result-of-prove
    (case output-option
      (:VALUES 
       (let ((result1 nil))
         (dolist (one-prove-result result-of-prove result1)
           (setf result1 (append result1 (list (cdar one-prove-result)))))))
      (:COMBINATIONS 
       (let ((result1 nil))
         (dolist (one-prove-result result-of-prove result1)
           (let ((result2 nil))                           
             (setf result1 (append result1
                                   (list (dolist (x2 one-prove-result result2)
                                           (setf result2 
                                                 (append result2 (list (rest x2))))))))))))
      (nil result-of-prove))))
;      ('nil result-of-prove))))       ;; JW


;;; help-functions for function concept-prove:

(defun get-all-instances-with-slot-x (super-frame-name slot-name)
; returns a list of all instances of a super-frame with the slot 'slot-name'
  (let ((all-sub-frames (cons super-frame-name (GET-ALL-SUBFRAMES super-frame-name)))
        (result nil))
    (dolist (x all-sub-frames result)
      (setf result (append  (get-instance-list-with-slot-x x slot-name) result)))))
      
(defun get-instance-list-with-slot-x (frame-name slot-name)
; needed by function get-all-instances-with-slot-x
; returns a list of all instances of the frame 'frame-name' with the slot 'slot-name'
  (let ((frame-instances (GET-INSTANCE-LIST frame-name)))
    (if (and (not (null frame-instances)) 
             (member slot-name (<- (first frame-instances) :SLOTS)))
      frame-instances
      nil)))

(defun find-instances (instance-list relation-name slot-value truth-value number)
; returns a list of those instances that the 'slot-value'
; is equal or subset of the slot-value of every instance in the list
  (if (or (equal number 0) (null instance-list)) nil             ; terminate the loop
      (let* ((new-instance (car instance-list))
             ; looks if new-instance has the slot-value we are looking for
             (value (get-value-with-truth-value truth-value new-instance relation-name)))
        (if (compare-values value slot-value)
          ; call again recursively with shortened instance-list 
          ; (and decrement number if the test succeeded)
          (cons new-instance (find-instances (cdr instance-list) relation-name slot-value
                                             truth-value (1- number)))
          (find-instances (cdr instance-list) relation-name slot-value truth-value number)))))

(defun get-value-with-truth-value (truth-value instance slot-name)
; needed by function find-instances     
; depending on the truth-value one of the slot-access-methods is performed:
; :get-true, get-false or :get-unknown
  (case truth-value
    ((or nil true) (<- instance :get-true slot-name))            ; select the kind of
    (false (<- instance :get-false slot-name))                  ; the get- behavior
    (unknown (<- instance :get-unknown slot-name))))

;    ('false (<- instance :get-false slot-name))                  ; the get- behavior
;    ('unknown (<- instance :get-unknown slot-name))))   ;; JW

(defun compare-values (current-value prove-value)
; needed by function find-instances
; returns <> nil if prove-value is equal or subset of current-value
  ; (print current-value) (princ prove-value)
  (cond ((and (atom prove-value) (atom current-value))           ; current-value isn't yet defined
         (or (equal prove-value current-value)
             (and (unknown-p prove-value) (unknown-p current-value))))
        ((or (null prove-value) (null current-value)) nil)       ; values can't match
        (t (subsetp (convert-to-list prove-value) 
                    (convert-to-list current-value)))))          ; test on membership


;;; help-functions for function prolog-prove:

(defun check-par-types (type-list arg-list)
  ; checks if arguments of the relation-clause belong to the type as required
  ; in the slot type of the DEF-RELATION construct
  (do ((types type-list (cdr types))
       (args arg-list (cdr args))
       (result t))
      ((or (null types) (null args)) result)
    (let ((arg (first args))
          (type (first types)))
      (if (or (string= "_" (char (symbol-name arg) 0))
              (member type (get-instance-list 'enumeration-type))
              (member arg (get-all-instances type)))
        (setf result (and result t))                             ; check was ok.
        (print-error 'check-par-types 12 arg type)))))


;;; help-functions for function unknown-prove:

(defun unbound-type (relation-args type-list number)
  ; returns that type of the type-list that is the type of an unbound variable
  ; which type should be used is customized with number
  (if (equal (length relation-args) (length type-list))
    (do* ((args relation-args (cdr args))
          (arg (first args))
          (types type-list (cdr types))
          (type (first types)))
         ((or (null args) (= 0 number)) type)
      (if (= 1 (count-output-var arg))                           ; variable is unbound
        (setf number (1- number))))
    (print-error 'unbound-type 17 relation-args type-list)))
    
(defun remove-true-and-false (cross-list relation out-vars)
; help-function for function prove-unknown:
; it removes those pairs in cross-list that are true or false
  ; (print cross-list)
  (let ((result nil))
    (dolist (pair cross-list result)
      (if (unknown-p (prolog-prove (cons relation pair) 'true 1 ':symetric out-vars))
        (setf result (append (list pair) result))))))

(defun crossproduct (set1 set2)
; creates a crossproduct of the two lists set1 and set2: a list with sublists
  (let ((result nil))
    (dolist (x1 set1 (reverse result))
      (setf result (dolist (x2 set2 result)
                     (setf result (append (list (list x1 x2)) result)))))))

(defun make-a-list (variable result &optional (type nil))
; creates a list like BABYLON Prolog
  (let ((a-list nil))
    (if (null type)
      ; convert the 2 inputlists of the form (_var) (value`1 ... value`n)
      ; to a list of the form (((_var . value`1)) ... ((_var . value`n)))
      (dolist (item result a-list)
        (setf a-list (cons (pairlis variable (list item)) a-list)))
      ; convert (_var1 _var2) ((value`11 value`12) ... (value`n1 value`n2)) to
      ; (((_var1 . value`11) (_var . value`12)) ... ((_var1 . value`n1) (_var2 . value`n2)))
      (dolist (item result a-list)
        (setf a-list (cons (pairlis variable item) a-list))))))
      


; ------------ Generic Relations --------------

(DEFFRAME generic-relation
  (SLOTS (predicate  -)
         (domains -)
         (properties -)))


(defmacro def-generic-relation (&rest relation-spec)
  (let* ((name (first relation-spec))
         (supers (if (equal (first (second relation-spec)) 'SUPERS) 
                   (rest (second relation-spec))))
         (new-supers (cons 'relation supers))
         (rest-relation-spec (if (equal (first (second relation-spec)) 'SUPERS) 
                               (third relation-spec) (second relation-spec))))
    `(defframe ,name 
       (SUPERS ,@new-supers)
       ,rest-relation-spec)))

; (SUPERS relation) is automatically appended (without checking if there is another SUPER).


#|
Example:
(DEF-GENERIC-RELATION occupancy
  (SUPERS relations-as-facts)   ; macro expands to (SUPERS relation relations-as-facts)
  (SLOTS (domains (room employee))
         (assumable false)))


(DEFINSTANCE actual-occupancy OF occupancy
  WITH predicate = p-actual-occupancy)
|#

;;; eof

