;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

; MODEL CONSTRUCTS FOR KADS MODELS 
; based on the BABYLON system


; =======================================================================
; =======================================================================
; ==================              MIXINS                  ===============
; =======================================================================
; =======================================================================



; --------------------------------------------------------------
; ---------------- slots for documentation  --------------------
; --------------------------------------------------------------

(DEFFRAME documentation-slots-mixin 
  (SLOTS name
         documentation-string
         author
         date
         remarks))



; --------------------------------------------------------------
; -------------------- common used stuff  ----------------------
; --------------------------------------------------------------

(DEFFRAME partial-multisets-infos
  (SUPERS documentation-slots-mixin)
  (SLOTS value -))


; -----------------------------------------------------------
; --------------- Mixins for slot-values --------------------
; -----------------------------------------------------------

; -----------------------------------------------------------------------
; --------------- methods to handle possible-values ---------------------
; -----------------------------------------------------------------------

; Extionsions of the BABYLON possible-values are
; enumeration-types, instance-types, and extanded-boolean expressions
; For every possible-value there exists mixins, which are composed in the
; possible-values-mixin 

; -------- enumeration-types --------

(DEFFRAME enumeration-types-mixin)


(DEFINE-POSSIBLE-VALUES-BEHAVIOR (enumeration-types-mixin :one-of-enumeration)
  (slot-value enumeration-instance)
  (if (undefined-p slot-value) t
      (member slot-value (<- (first enumeration-instance) :get 'values))))

#|
   Syntax:  <:one-of-enumeration>  ::=  <atom>  |  unknown
|#


(DEFINE-POSSIBLE-VALUES-BEHAVIOR (enumeration-types-mixin :some-of-enumeration)
  (slot-value enumeration-instance)
  (check-notation slot-value (<- (first enumeration-instance) :get 'values)))
#|
 Syntax:   <:slot-value>  ::=  (( :EXACTLY-TRUE <atoms> ))                 [1]
                             | (( :EXACTLY-FALSE <atoms> ))
                             | ([( :TRUE <atoms> )] [( :FALSE <atoms> )])  [2]
|#

(DEFINSTANCE some-of-lists OF partial-multisets-infos
  WITH value = ((:some-of-enumeration :some-of-instances :some-of-all-instances)))

; test-function for the behaviors :some-of-enumeration,
;                                 :some-of-instances and :some-of-all-instances

(defun check-notation (slot-value list)
  (cond ((atom slot-value) nil)                                      ; slot-value must not be an atom
        ((extended-bool-member (car slot-value) t)                   ; test for alternative [2]
         (test-subsetp (cdr slot-value) list))                       ; test with subsetp
        ((and (listp (first slot-value))
              (extended-bool-member (caar slot-value))               ; test for alternative [1]
              (test-subsetp (cdar slot-value) list))                 ; test the first sublist
         (if (null (cdr slot-value))                               ; only one sublist
           t                
           (and (listp (second slot-value))
                (extended-bool-member (caadr slot-value))          ; both sublists
                (not (equal (caadr slot-value) (caar slot-value))) ; no keyword used twice
                (null (cddr slot-value))                           ; exactly two sublists
                (test-subsetp (cdadr slot-value) list))))          ; test with subsetp
        (t nil)))                                                    ; return error otherwise



; --------- instance-types ---------

(DEFFRAME instances-types-mixin)


; (DEFINE-POSSIBLE-VALUES-BEHAVIOR (instances-types-mixin :instance-of)  yet exists in BABYLON

(DEFINE-POSSIBLE-VALUES-BEHAVIOR (instances-types-mixin :instances-of) 
  (slot-value frame)
  (test-subsetp slot-value (get-instance-list (first frame))))

(DEFINE-POSSIBLE-VALUES-BEHAVIOR (instances-types-mixin :all-instance-of) 
  (slot-value super-frame)
  (member slot-value (get-all-instances (first super-frame))))

(DEFINE-POSSIBLE-VALUES-BEHAVIOR (instances-types-mixin :all-instances-of) 
  (slot-value super-frame)
  (test-subsetp slot-value (get-all-instances (first super-frame))))



(DEFINE-POSSIBLE-VALUES-BEHAVIOR (instances-types-mixin :one-of-instances) 
  (slot-value frame)
  (if (not (equal slot-value 'unknown))
    (member slot-value (get-instance-list (first frame)))
    '(t)))

(DEFINE-POSSIBLE-VALUES-BEHAVIOR (instances-types-mixin :one-of-all-instances) 
  (slot-value frame)
  (if (not (equal slot-value 'unknown))
    (member slot-value (get-all-instances (first frame)))
    '(t)))


(DEFINE-POSSIBLE-VALUES-BEHAVIOR (instances-types-mixin :some-of-instances)
  (slot-value frame)
  (check-notation slot-value (get-instance-list (first frame))))

(DEFINE-POSSIBLE-VALUES-BEHAVIOR (instances-types-mixin :some-of-all-instances)
  (slot-value frame)
  (check-notation slot-value (get-all-instances (first frame))))


#|
 Syntax:   <:slot-value>  ::=  (( :EXACTLY-TRUE <atoms> ))                 [1]
                             | (( :EXACTLY-FALSE <atoms> ))
                             | ([( :TRUE <atoms> )] [( :FALSE <atoms> )])  [2]
|#



; --------- subframes-types-mixin ---------

(DEFFRAME subframes-types-mixin)


(DEFINE-POSSIBLE-VALUES-BEHAVIOR (subframes-types-mixin :subframe-of) 
  (slot-value super-frame)
  (member slot-value (get-subframes (first super-frame))))

(DEFINE-POSSIBLE-VALUES-BEHAVIOR (subframes-types-mixin :subframes-of) 
  (slot-value super-frame)
  (test-subsetp slot-value (get-subframes (first super-frame))))

(DEFINE-POSSIBLE-VALUES-BEHAVIOR (subframes-types-mixin :all-subframe-of) 
  (slot-value super-frame)
  (member slot-value (get-all-subframes (first super-frame))))

(DEFINE-POSSIBLE-VALUES-BEHAVIOR (subframes-types-mixin :all-subframes-of) 
  (slot-value super-frame)
  (test-subsetp slot-value (get-all-subframes (first super-frame))))



; -------- extended boolean ------------

(DEFFRAME extended-boolean-mixin)

(DEFINE-POSSIBLE-VALUES-BEHAVIOR (extended-boolean-mixin :extended-boolean)
  (slot-value)
  (if (or (equal slot-value 'true) 
          (equal slot-value 'false) 
          (equal slot-value 'unknown))
    t nil))

#|
  Syntax:  <slot-value>  ::=  true  |  false  |  unknown
|#


; functions for extended-boolean values

(defun extended-boolean-p (expression)
  (member expression '(true false unknown)))

(defun true-p (expression)
  (equal 'true expression))

(defun false-p (expression)
  (equal 'false expression))

(defun unknown-p (expression)
  (member expression '(-  unknown)))

(defun opposite (ext-bool)
  (case ext-bool
    (true 'false)
    (false 'true)
    (t 'unknown)))

(defun convert-to-extended-boolean (expression)
; needed by concept-prove and prolog-prove
; converts: nil -> 'false
;           t   -> 'true
;           else unchanged
  (cond ((null expression) 'false)
        ((equal expression t) 'true)
        (T expression)))

(defun extended-bool-member (expression &optional exactly)
; test if expression is an extended-boolean keyword
  (if (null exactly)
      (member expression '(:TRUE :FALSE))
      (member expression '(:EXACTLY-TRUE :EXACTLY-FALSE))))

(defun undefined-p (expression)
; test if expression is nil, - or unknown
  (or (null expression)
      (member expression '(- unknown))))



(DEFFRAME possible-values-mixin
  (SUPERS enumeration-types-mixin instances-types-mixin
          subframes-types-mixin extended-boolean-mixin))


; -----------------------------------------------------
; ------------- Mixins for possible-values ------------
; -----------------------------------------------------

(DEFBEHAVIOR (possible-values-mixin :get-possible-values) (slot-name)
  "returns all possible values for a slot-name
slot-name can be of the type :some-of-instances :some-of-all-instances 
or :some-of-enumeration"
  (let ((possible-value-type (<- (second (<- self :get slot-name :possible-values)) :get 'values)))
    (if (is-frame possible-value-type) 
      (get-instance-list possible-value-type) ; it is :some-of-instances or :some-of-all-instances
      possible-value-type)))                  ; it is :some-of-enumeration

; -----------------------------------------------------
; -------- Mixins for the acces of slot values --------
; -----------------------------------------------------

; Slot-access functions:

; :get-true
; :get-false
; :get-unknown
; parameters: slot-name

; :put-true
; :put-false
; :add-true
; :add-false
; :remove-true
; :remove-false
; parameters: slot-name value


(DEFFRAME slot-values-access-mixin)


(DEFBEHAVIOR (slot-values-access-mixin :get-true) (slot-name)
  "returns either
1. a sublist (without the keywords) of the current value of the
slot-name that belongs to an enumeration-type-list corresponding to
that part of the value that begins with the keywords :TRUE or :EXACTLY-TRUE. 
2. the difference between the corresponding enumeration-type-list
and the value of the slot-name if and only if the keyword is :EXACTLY-FALSE 
3. the value of the slot-name if it doesn't belong to an enumeration-type-list "  
  (let ((slot-value (<- self :get slot-name)))          ; get current slot-value
    (if (<- self :some-of-listp slot-name)              ; ask for the slot-name's annotation
      (cond ((atom slot-value) nil)                     ; slot-value is an atom: return nil
            ((listp (car slot-value))                   ; see 1. above
             (if (equal (caar slot-value) ':TRUE)       ; look for sublist beginning with :TRUE
               ; return the rest of the first sublist or of the second sublist
               (cdar slot-value) (cdadr slot-value)))
            ((equal (car slot-value) ':EXACTLY-TRUE)    ; :EXACTLY-TRUE  in slot-value
             (cdr slot-value))
            ((equal (car slot-value) ':EXACTLY-FALSE)   ; :EXACTLY-FALSE in slot-value
             (set-difference (<- self :get-possible-values slot-name)
                             (rest slot-value))))       ; see 2. above
      slot-value)))                                     ; see 3. above


(DEFBEHAVIOR (slot-values-access-mixin :get-false) (slot-name)
  "returns either
1. a sublist (without the keywords) of the current value of the slot-name
that belongs to an enumeration-type-list corresponding to that part of the
value that begins with the keywords :FALSE or :EXACTLY-FALSE.
2. the difference between the corresponding enumeration-type-list and the
value of the slot-name if and only if the keyword is :EXACTLY-TRUE
3. an error if the value of the slot-name doesn't belong to an
enumeration-type-list"
  (if (<- self :some-of-listp slot-name)                ; ask for the slot-name's annotation
    (let ((slot-value (<- self :get slot-name)))        ; get current slot-value
      (cond ((atom slot-value) nil)                     ; slot-value is an atom: return nil
            ((listp (car slot-value))                   ; :TRUE and/or :FALSE in slot-value
             (if (equal (caar slot-value) ':FALSE)      ; look for sublist beginning with :FALSE
               ; return the rest of the first sublist or of the second sublist
               (cdar slot-value) (cdadr slot-value)))
            ((equal (car slot-value) ':EXACTLY-FALSE)   ; :EXACTLY-FALSE in slot-value
             (cdr slot-value))
            ((equal (car slot-value) ':EXACTLY-TRUE)    ; :EXACTLY-TRUE  in slot-value
             (set-difference (<- self :get-possible-values slot-name)
                             (rest slot-value)))))      ; 2. above
    (print-error ':get-false 1 slot-name)))             ; no enumeration-type-list


(DEFBEHAVIOR (slot-values-access-mixin :get-unknown) (slot-name)
  "returns a sublist (without the keywords) of the enumeration-type-list that 
corresponds to the type of the slot-name (i.e those elements that are not
in the :TRUE and :FALSE -part of the value of the slot-name)"
  (if (<- self :some-of-listp slot-name)                ; ask for the slot-name's annotation
    (let ((slot-value (<- self :get slot-name)))        ; get current slot-value
      (if (atom slot-value)                             ; slot-value is an atom
        (<- self :get-possible-values slot-name)        ; return whole list
        (let ((first-item (car slot-value)))
          (if (listp first-item)                        ; :TRUE and/or :FALSE in slot-value
            (set-difference (<- self :get-possible-values slot-name)
                            (collect-elements slot-value nil))
            nil))))                                     ; :EXACTLY-TRUE or :EXACTLY-FALSE
    ; slot-value is an atom :
    (print-error ':get-unknown 1 slot-name)))           ; no enumeration-type-list


(DEFBEHAVIOR (slot-values-access-mixin :put-true) (slot-name new-value)
"restores the slot-name with the new-value under the consideration of the
corresponding type (= annotation) of the slot-name (test of
enumeration-type-list)"
  (let (first-item)
    (if (<- self :some-of-listp slot-name)                ; ask for the slot-name's annotation
      (let ((slot-value (<- self :get slot-name))
            (value (convert-to-list new-value)))          ; embed new-value into a list
        (cond ((atom slot-value)                          ; slot-value is an atom
               (<- self :put slot-name (list (cons ':TRUE value))))   ; create a new list with :TRUE
              ((listp (car slot-value))                   ; :TRUE and/or :FALSE in slot-value
               (setf first-item (car slot-value))         ; slot-value is a list per definitionem
               (if (equal (car first-item) ':TRUE)
                 (<- self :put slot-name                  ; (car first-item) = :TRUE
                     (make-2-lists (cons ':TRUE value)
                                   (delete-dup (second slot-value) value))) 
                 (<- self :put slot-name                  ; (car first-item) = :FALSE
                     (make-2-lists  (cons ':TRUE value)
                                    (delete-dup first-item value)))))
              ( t                             ; :EXACTLY-TRUE or :EXACTLY-FALSE in slot-value
                (<- self :put slot-name (cons ':EXACTLY-TRUE value)))))
      (<- self :put slot-name new-value))))                ; any other type for slot-name


(DEFBEHAVIOR (slot-values-access-mixin :put-false) (slot-name new-value)
  "restores the slot-name with the new-value under the consideration of the
corresponding type (= annotation) of the slot-name (test of
enumeration-type-list)"
  (let (first-item)
    (if (<- self :some-of-listp slot-name)                ; ask for the slot-name's annotation
      (let ((slot-value (<- self :get slot-name))
            (value (convert-to-list new-value)))          ; embed new-value into a list
        (cond ((atom slot-value)                          ; slot-value is an atom
               (<- self :put slot-name (list (cons ':FALSE value))))  ; create a new list with :FALSE
              ((listp (car slot-value))                   ; :TRUE and/or :FALSE in slot-value
               (setf first-item (car slot-value))         ; slot-value is a list per definitionem
               (if (equal (car first-item) ':FALSE)
                 (<- self :put slot-name                  ; (car first-item) = :FALSE
                     (make-2-lists (cons ':FALSE value)
                                   (delete-dup (second slot-value) value))) 
                 (<- self :put slot-name                  ; (car first-item) = :TRUE
                     (make-2-lists (cons ':FALSE value) 
                                   (delete-dup first-item value)))))
              (t                                   ; :EXACTLY-TRUE or :EXACTLY-FALSE in slot-value
               (<- self :put slot-name (cons ':EXACTLY-FALSE value)))))
      (print-error ':put-false 1 slot-name))))           ; no enumeration-type-list


(DEFBEHAVIOR (slot-values-access-mixin :add-true) (slot-name new-value)
  "adds to the current slot-value of the slot-name the new-value under the
consideration of the corresponding type (= annotation) of the slot-name
(test of enumeration-type-list)"
  
  (if (<- self :some-of-listp slot-name)                   ; ask for the slot-name's annotation
    (let ((slot-value (<- self :get slot-name))
          (value (convert-to-list new-value)))             ; embed new-value into a list
      (if (atom slot-value)                                ; slot-value is an atom
        (<- self :put slot-name (list (cons ':TRUE value)))     ; create a new list with :TRUE
        (let ((first-item (first slot-value)))             ; slot-value is a not empty list
          (cond ((listp first-item)                        ; :TRUE and/or :FALSE in slot-value
                 (if (equal (car first-item) ':TRUE)
                   (<- self :put slot-name                 ; (car first-item) = :TRUE
                       (make-2-lists (cons ':TRUE (append (cdr first-item) value))
                                     (delete-dup (second slot-value) value))) 
                   (<- self :put slot-name                 ; (car first-item) = :FALSE
                       (make-2-lists (cons ':TRUE (append (cdr (second slot-value)) value)) 
                                     (delete-dup first-item value)))))
                ((equal first-item ':EXACTLY-TRUE)         ; merge the oldlist with the new-value
                 (<- self :put slot-name
                     (cons ':EXACTLY-TRUE (append (cdr slot-value) value))))
                ((equal first-item ':EXACTLY-FALSE)
                 ; remove those elements in slot-value that are also in value :
                 (let* ((falselist (delete-dup slot-value value))
                        (newlist (if (null falselist)      ; all items are true now, instead
                                   (cons ':EXACTLY-TRUE    ; of writing (:EXACTLY-FALSE nil)
                                         (<- self :get-possible-values slot-name))
                                   falselist)))
                   (<- self :put slot-name newlist)))))))
    (print-error ':add-true 1 slot-name)))                 ; no enumeration-type-list


(DEFBEHAVIOR (slot-values-access-mixin :add-false) (slot-name new-value)
  "adds to the current slot-value of the slot-name the new-value under the
consideration of the corresponding type (= annotation) of the slot-name
(test of enumeration-type-list)"
  
  (if (<- self :some-of-listp slot-name)                   ; ask for the slot-name's annotation
    (let ((slot-value (<- self :get slot-name))
          (value (convert-to-list new-value)))             ; embed new-value into a list
      (if (atom slot-value)                                ; slot-value is an atom
        (<- self :put slot-name (list (cons ':FALSE value)))    ; create a new list with :FALSE
        (let ((first-item (first slot-value)))             ; slot-value is a not empty list
          (cond ((listp first-item)                        ; :TRUE and/or :FALSE in slot-value
                 (if (equal (car first-item) ':FALSE)
                   (<- self :put slot-name                 ; (car first-item) = :FALSE
                       (make-2-lists (cons ':FALSE (append (cdr first-item) value))
                                     (delete-dup (second slot-value) value))) 
                   (<- self :put slot-name                 ; (car first-item) = :TRUE
                       (make-2-lists (cons ':FALSE (append (cdr (second slot-value)) value)) 
                                     (delete-dup first-item value)))))
                ((equal first-item ':EXACTLY-FALSE)        ; merge the oldlist with the new-value
                 (<- self :put slot-name
                     (cons ':EXACTLY-FALSE (append (cdr slot-value) value))))
                ((equal first-item ':EXACTLY-TRUE)
                 ; remove those elements in slot-value that are also in value :
                 (let* ((truelist (delete-dup slot-value value))
                        (newlist (if (null truelist)       ; all items are false, insteaad
                                   (cons ':EXACTLY-FALSE   ; of writing (:EXACTLY-TRUE nil)
                                         (<- self :get-possible-values slot-name))
                                   truelist)))
                   (<- self :put slot-name newlist)))))))
    (print-error ':add-false 1 slot-name)))                ; no enumeration-type-list


(DEFBEHAVIOR (slot-values-access-mixin :remove-true) (slot-name new-value)
  "removes the elements that are in the true-part of the current-value of
the slot-value and also belong to the element(s) of new-value; the values
of new-value become unknown"
  
  (if (<- self :some-of-listp slot-name)                   ; ask for the slot-name's annotation
    (let ((slot-value (<- self :get slot-name))
          (value (convert-to-list new-value)))             ; embed new-value into a list
      (if (atom slot-value)                                ; slot-value is an atom
        (print-error 'remove-true 2 slot-name 'TRUE)       ; slot-value is nil or unknown
        (let ((first-item (first slot-value)))             ; slot-value is a not empty list
          (cond ((listp first-item)                        ; :TRUE and/or :FALSE in slot-value
                 (if (equal (car first-item) ':TRUE)
                   (<- self :put slot-name                 ; (car first-item) = :TRUE
                       (make-2-lists (remove-dup first-item value)
                                     (second slot-value)))
                   (<- self :put slot-name                 ; (car first-item) = :FALSE
                       (make-2-lists (remove-dup (second slot-value) value)
                                     first-item))))
                ((extended-bool-member first-item t)
                 (<- self :convert-exactly-list slot-name) ; rebuild slot-value
                 (<- self :remove-true slot-name new-value)))))) ; call again recursively
    (print-error ':remove-true 1 slot-name)))              ; no enumeration-type-list


(DEFBEHAVIOR (slot-values-access-mixin :remove-false) (slot-name new-value)
  "removes the elements that are in the false-part of the current-value of
the slot-value and also belong to the element(s) of new-value"
  (if (<- self :some-of-listp slot-name)                   ; ask for the slot-name's annotation
    (let ((slot-value (<- self :get slot-name))
          (value (convert-to-list new-value)))             ; embed new-value into a list
      (if (atom slot-value)                                ; slot-value is an atom
        (print-error 'remove-false 2 slot-name 'TRUE)      ; slot-value is nil or unknown
        (let ((first-item (first slot-value)))             ; slot-value is a not empty list
          (cond ((listp first-item)                        ; :TRUE and/or :FALSE in slot-value
                 (if (equal (car first-item) ':FALSE)
                   (<- self :put slot-name                 ; (car first-item) = :TRUE
                       (make-2-lists (remove-dup first-item value)
                                     (second slot-value)))
                   (<- self :put slot-name                 ; (car first-item) = :FALSE
                       (make-2-lists (remove-dup (second slot-value) value)
                                     first-item))))
                ((extended-bool-member first-item t)
                 (<- self :convert-exactly-list slot-name) ; rebuild slot-value
                 (<- self :remove-false slot-name new-value)))))) ; call again recursively
    (print-error ':remove-false 1 slot-name)))             ; no enumeration-type-list



; test-behavior to ask for the annotation of a slot-name that must be
; either :some-of-instances, :some-of-all-instances or :some-of-enumeration
; (that's exactly the value of the partial-multisets-infos instance some-of-lists)

(DEFBEHAVIOR (slot-values-access-mixin :some-of-listp) (slot-name)
  "checks if the slot-name of the self belongs to an enumeration-type"
  (declare (special some-of-lists))
  (let ((annotation (<- self :get slot-name :possible-values)))
    (if (listp annotation)
      (member (first annotation)
              (<- some-of-lists :get 'value)))))


(DEFBEHAVIOR (slot-values-access-mixin :convert-exactly-list) (slot-name)
  "convert a list of the form (:EXACTLY-xxx ...) to ((:TRUE ... ) (:FALSE ... )) 
it is needed by the behaviors :remove-true and :remove-false"
  (let* ((slot-value (<- self :get slot-name))
         (old-list (rest slot-value))
         (whole-list (<- self :get-possible-values slot-name))
         (truth-keyword (first slot-value)))
    (<- self :put slot-name
        (if (equal truth-keyword ':EXACTLY-TRUE)
          (list (cons ':TRUE old-list)
                (cons ':FALSE (set-difference whole-list old-list)))
          (list (cons ':FALSE old-list)
                (cons ':TRUE (set-difference whole-list old-list)))))))


(DEFFRAME slot-description-mixin
  (SUPERS slot-values-access-mixin possible-values-mixin))


; ---------------------------------------------------------------------------
; ------------- model-construct-mixin for all model-constructs --------------
; ---------------------------------------------------------------------------

(DEFFRAME model-construct-mixin
  (SUPERS documentation-slots-mixin slot-description-mixin))



; =======================================================================
; =======================================================================
; ================== DEFINITION OF MODEL-CONSTRUCTS =====================
; =======================================================================
; =======================================================================

; This macro expands to a BABYLON instance and puts the name of the instance in a slot

(defmacro create-babylon-instance (model-construct-name instance-spec)
  (let ((instance-name (first instance-spec))
        (slot-spec (cddr instance-spec)))
    `(definstance ,instance-name OF ,model-construct-name
       WITH name = ,instance-name
       ,@slot-spec)))




; =============================================
; ============ TYPE SPECIFICATIONS ============
; =============================================

; ----------- Enumeration Types ---------------

(DEFFRAME enumeration-type
  (SUPERS model-construct-mixin)
  (SLOTS (values - )))

(defmacro def-enumeration (&rest enumeration-spec)
  `(create-babylon-instance enumeration-type ,enumeration-spec))

#|
Example:
(DEF-enumeration hobbies
  WITH values = ((bike radio squash)))
|#

;;; eof

