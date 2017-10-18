;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

; =============================================
; ============ INFERENCE LAYER ================
; =============================================

; ------------- Metaclasses -------------------

(DEFFRAME metaclass
  (SUPERS model-construct-mixin)
  (SLOTS 
   (value             - :POSSIBLE-VALUES :ANY)
                                        ;(:ALL-INSTANCES-OF domain-layer-constructs))
   (value-element-format - :POSSIBLE-VALUES :ANY)
   (structure         - :POSSIBLE-VALUES (:ONE-OF set ordered-list))
   (possible-concepts - :POSSIBLE-VALUES (:ALL-SUBFRAMES-OF domain-layer-constructs))))


(DEFBEHAVIOR (metaclass :reset) ()
  (<- self :put 'value nil))

(DEFBEHAVIOR (metaclass :add) (new-value) ; Standard for value structure: set
  (let ((old-value (<- self :get 'value)))
    (if new-value 
      (cond ((undefined-p old-value)
           (if (atom new-value)
             (<- self :put 'value (list new-value))
             (<- self :put 'value new-value)))
           (T
            (if (atom new-value)
              (<- self :put 'value (union (list new-value) old-value))
              (<- self :put 'value (union new-value old-value))))))))
            
(DEFBEHAVIOR (metaclass :remove) (value)
  (if (undefined-p (<- self :get 'value))
    (print-error ':remove 20 value 'value)
    (<- self :put 'value (remove value (<- self :get 'value)))))


(defmacro def-metaclass (&rest metaclass-spec)
  `(create-babylon-instance metaclass ,metaclass-spec))

#|
Example:
(DEF-METACLASS generic-requirements
  WITH value = nil
       structure = set
       possible-concepts = ((requirements)))
|#


; ------------ Knowledge Sources --------------

(DEFFRAME knowledge-source
  (SUPERS model-construct-mixin)
  (SLOTS
   (input-metaclasses  - :POSSIBLE-VALUES (:all-instances-of metaclass))
   (output-metaclasses - :POSSIBLE-VALUES (:all-instances-of metaclass))
   (functions-used -)  ; functions-used used by "ks-body"
   (formalism - :POSSIBLE-VALUES (:ONE-OF MESSAGE-PASSING PROLOG CONSTRAINTS RULES LISP))))

(DEFBEHAVIOR (knowledge-source :apply) ()
  (list
   (let ((all-input-metaclasses (<- self :get 'input-metaclasses)))
     (if (eq '- all-input-metaclasses)
       '(non)
       (mapcan #'(lambda (class) 
                   (list class (<- class :get 'value)))
               all-input-metaclasses)))
   (prog2
    (funcall (<- self :get 'name))
    (let ((all-output-metaclasses (<- self :get 'output-metaclasses)))
      (if (eq '- all-output-metaclasses)
        '(non)
        (mapcan #'(lambda (class) 
                     (list class (<- class :get 'value)))
                 all-output-metaclasses))))))


(defmacro def-knowledge-source (&rest knowledge-source-spec)
  `(create-babylon-instance knowledge-source ,knowledge-source-spec))

#|
Example:
(DEF-KNOWLEDGE-SOURCE generate-sucessor-states  
  WITH 
  input-metaclasses = ((state))
  output-metaclasses = ((successsor-states))
  function = nil
  formalism = lisp)
|#


(defmacro def-knowledge-source-body (&rest fct-definition)
  `(defun ,@fct-definition))


;;; eof

