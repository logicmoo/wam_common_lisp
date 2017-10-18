;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

 
;;; adopted to non SYMBOLICS machines for BABYLON 2.1 by Juergen Walther 10.5.89

;; __________________________________________________________________________


(def-kb-instance unit-kb k1dummyc :pkg :ks)	


;;; ************************ STUFF FOR RESET ***************************

;; This is a before-daemon which modifies
;;     1. the slot "instances-list" of flavor "frame-processor",
;;        removing all recursively-generated instances
;;     2. the slot "instances" of flavor "knowledge-base" (in the same way)
;;
;; There was a problem with the previous solution: a :reset-instances message
;; to the frame-processor makes a "(definstance ...)" on each entry
;; in the instances-list (so on all recursively-generated instances too).
;; But the top-level instances do this "(definstance ...)" in their
;; :after :initialize daemon; so their is a twofold instantiation which
;; overwrites (e.g.) the part-of initialization.


(defvar *k1-ted* nil)

(defmacro delete-kb-from-menu (kb-name)
  `(if (member ,kb-name *known-knowledge-bases*)
     (setf *known-knowledge-bases* (delete ,kb-name *known-knowledge-bases*))
     nil))					;gehoert eigentlich nicht hier hin
						;ist aber praktisch so.

(def$method (normal-frame-processor :before :reset-instances) ()
  (setf instances-list
        (mapcan #'(lambda (an-instance)
                    (if (not (<- an-instance :is-recursively-generated))
                      `(,an-instance)))
                instances-list))
  ($send *CURRENT-KNOWLEDGE-BASE* :set-instances instances-list))


(def$method (normal-frame-processor :reset-instances) (&optional (completely t))
  (let ((temporary-instances-list (copy-list instances-list)))
    ;; a copy of instances-list is necessary because mapc would
    ;; map over the newly created list of our :after :initialize daemon!
    ;; (and thus it would map over the recursively-generated instances too)
    (mapc #'(lambda (an-instance-name)
              (if completely
                (eval (get-instance-def an-instance-name))
                ;; resets only :VALUE property
                ($send (get-instance an-instance-name) :reset-slots-value)))
          temporary-instances-list)))



;;; **************** UNIT FRAME ****************

;; this is the basic frame compound units
;; are built on


(defframe unit
  ;  (supers attachable-frame)			
  ;attachable-frame allows for :put with
  ;CONSAT activation. is defined in CONSAT.
  (slots (unit-trace-on nil)))


;; ____________________________________________________________

 
(defun mark-as-recursively-generated (an-instance)
  (if an-instance
    (setf (get (%get-object-name an-instance) 'recursively-generated-flag) T)
    (error  "Unknown instance: ~S" an-instance)))

(def$method (active-value-frame-core :is-recursively-generated) ()
  (get (%get-object-name ($send self :object-name)) 'recursively-generated-flag))


;; ------------------------------------------------------------




(defbehavior (unit :before :initialize) (ignore)  
  (declare (ignore ignore))
  ; (<- self :history-off)
  t)

;; ____________________________________________________________


(defbehavior (unit :slot-internal-properties) ()
  '(:NAME :NAMES))


;; ____________________________________________________________


(defbehavior (unit :set-trace)
  (t-or-nil)
  
  ;; sets creation tracing t-or-nil
  
  (setf ($value 'unit-trace-on) t-or-nil))

;; ____________________________________________________________


(defbehavior (unit :trace-is-set)
  ()
  
  ;; t iff  creation tracing is on
  
  ($value 'unit-trace-on))

;; ____________________________________________________________

;; **************** INSTANTIATING STUFF ****************


(defbehavior (unit :after :initialize) (ignore)
  (declare (ignore ignore))
  ;; this after demon produces the necessary side effects for
  ;; instantiating all specified (sub-)components (:IS-A and :IS-A-LIST-OF properties)
  ;; as well as installing the specified relations (:RELATION property)
  
  (dolist (a-slot-name ($send self :slots))
    (let ((is-a-value (<- self :get-is-a-value a-slot-name))
          (is-a-list-of-value (<- self :get-is-a-list-of-value a-slot-name)))
      (cond ((and is-a-value is-a-list-of-value)
             (error 
              "You cannot specify for slot ~S of instance ~S of frame ~S~@
                             both part properties :IS-A and :IS-A-LIST-OF."
              a-slot-name
              ($send self :object-name)
              (type-of self)))
            (is-a-value
             (<- self :instantiate-is-a a-slot-name
                 is-a-value
                 (<- self :get-name-value a-slot-name)))
            (is-a-list-of-value
             (<- self :instantiate-is-a-list-of
                 a-slot-name is-a-list-of-value
                 (<- self :get-names-value a-slot-name))))))
  (<- self :install-relations-for-instance))


;; ____________________________________________________________

(defbehavior (unit :check-property-specification)
  (a-slot-name property-specification prop-name type-fn)
  
  ;; Checks the consistency of :IS-A and :IS-A-LIST-OF specifications
  ;; property-specification is always a list (at least the name of the frame)
  
  (let ((error nil))
    (cond ((null property-specification) (setq error t))
          ((atom property-specification) (setq error t))
          ((funcall type-fn (first property-specification))
           (cond ((null (rest property-specification)) t)
                 ((atom (rest property-specification)) (setq error t))
                 ((eq (second property-specification) 'WITH)
                  (do ((slot-spec (cddr property-specification) (cdddr slot-spec)))
                      ((null slot-spec) t)
                    (cond ((or (neq (second slot-spec) '=)
                               (null (rest (rest slot-spec))))
                           (setq error t)
                           (return nil)))))
                 (t (setq error t))))
          (t (setq error t)))
    (if error
      (error "~S:~@
                 Wrong ~S specification for slot ~S of instance ~S of frame ~S."
             property-specification
             prop-name
             a-slot-name
             ($send self :object-name)
             (type-of self)))))


;; ____________________________________________________________

(defbehavior (unit :get-is-a-value) 
  (a-slot-name)
  
  ;; gets the value of the :IS-A property of a-slot-name  
  
  (let ((is-a-value (<- self :get a-slot-name :IS-A)))
    (cond ((null is-a-value) nil)
          (t (if (and is-a-value (atom is-a-value))
               (setq is-a-value (list is-a-value))) 
             is-a-value))))

;; ____________________________________________________________


(defbehavior (unit :get-name-value) (a-slot-name)
  
  ;; liefert den wert der :NAME property 
  
  (let ((name-value (<- self :get a-slot-name :NAME)))
    (cond ((null name-value) (list (<- self :generate-part-name a-slot-name)))
          ((and name-value (symbolp name-value)) (list name-value))
          ((listp name-value)
           (<- self :check-property-specification a-slot-name name-value :NAME 'symbolp)
           (cond ((eq (first name-value) ':STANDARD)
                  `(,(<- self :generate-part-name a-slot-name) . ,(rest name-value)))
                 (t name-value)))
          (t (error "~S: wrong :NAME specification in slot ~S@
                     of instance ~S of frame ~S."
                    name-value a-slot-name ($send self :object-name) (type-of self))))))

;; ____________________________________________________________


(defbehavior (unit :generate-part-name)
  (a-slot-name)
  
  ;; liefert name der gestalt <object-name>.<a-slot-name>
  
  (intern (format nil "~S.~S" ($send self :object-name) a-slot-name)))

;; ____________________________________________________________


(defbehavior (unit :get-is-a-list-of-value)
  (a-slot-name)
  
  ;; gets the value of the :IS-A-LIST-OF property of a-slot-name  
  
  (let ((is-a-list-of-value (<- self :get a-slot-name :IS-A-LIST-OF)))
    (cond ((null is-a-list-of-value) nil)
          (t (if (atom is-a-list-of-value)
               (setq is-a-list-of-value (list is-a-list-of-value))) 
             is-a-list-of-value))))

;; ____________________________________________________________


(defbehavior (unit :get-names-value) (a-slot-name)
  
  ;; liefert den Wert der :NAMES property
  
  (let ((counter-or-list (<- self :get a-slot-name :NAMES)))
    (cond ((null counter-or-list)
           (error  "You have to specify the property :NAMES in order~@
                    to handle :IS-A-LIST-OF in slot ~S of instance ~S~@
                        of frame ~S."
                   a-slot-name
                   ($send self :object-name)
                   (type-of self)))
          ((numberp counter-or-list)
           (do ((counter counter-or-list (1- counter))
                (result nil
                        (cons (list (intern (format nil "~S.~A~S"
                                                    ($send self :object-name)
                                                    (<- self :drop-last-char a-slot-name)
                                                    counter)))
                              result)))
               ((zerop counter) (reverse result))))
          ((and (listp counter-or-list)
                (every ;counter-or-list
                 #'(lambda (name-value)
                     (or (symbolp name-value)
                         (and (listp name-value)
                              (progn
                                (<- self
                                    :check-property-specification
                                    a-slot-name name-value :NAMES 'symbolp)
                                t))))
                 counter-or-list))
           (mapcar #'(lambda (name-value) (if (atom name-value) (list name-value) name-value)) counter-or-list))
          (t (error  "~S: wrong :NAMES specification in slot ~S~@
                      of instance ~S of frame ~S."
                     counter-or-list a-slot-name ($send self :object-name) (type-of self))))))

;; ____________________________________________________________

(defbehavior (unit :drop-last-char) (a-slot-name)
  (let* ((name-string (string a-slot-name))
	 (name-length (1- (length name-string))))
    (if (char= #\S (elt name-string name-length))
	(subseq name-string 0 name-length)
	name-string)))

;; ____________________________________________________________

(defbehavior (unit :before :instantiate-is-a)
  (a-slot-name is-a-value ignore)
  (<- self :check-property-specification a-slot-name is-a-value :IS-A 'is-frame))

(defbehavior (unit :instantiate-is-a)
  (a-slot-name is-a-value name-value)
  
  ;; generates for a :IS-A specification an instance which is set
  ;; as value of a-slot-name 
  
  (let* ((part-name (first name-value))
         (unit-name (first is-a-value))
         (is-a-with-spec (rest is-a-value))
         (name-with-spec (if (null is-a-with-spec)
                           (rest name-value)
                           (rest (rest name-value)))))
    (<- self :create-part part-name unit-name
        (<- self :merge-initialisations (append is-a-with-spec name-with-spec)))
    (<- self :set a-slot-name part-name)))


;; ____________________________________________________________


(defbehavior (unit :merge-initialisations)
  (with-spec-list)
  
  ;; filters only the most specific slotvalue-initialisation
  
  (if (null with-spec-list) nil
      (prog ((result (list (pop with-spec-list))) current-init-spec)
        loop
        (cond ((null with-spec-list)(return  result))
              (t (setq current-init-spec
                       (list
                        (first with-spec-list)
                        (second with-spec-list)
                        (third with-spec-list))) ;zl:firstn 3  with-spec-list))
                 (setq with-spec-list (cdddr with-spec-list))
                 (if (<- self :is-most-specific-in-list
                         (first current-init-spec) with-spec-list)
                   (setq result (append result current-init-spec)))
                 (go loop))))))

;; ____________________________________________________________


(defbehavior (unit :is-most-specific-in-list)
  (a-slot-name a-with-spec-list)
  
  ;; tricki Function
  ;; tests if a-slot-name occurs more than once in a-with-spec-list
  
  (let ((pos (position a-slot-name a-with-spec-list))) 
    (or (null pos) (not (zerop (mod pos 3))))))

;; ____________________________________________________________

(defbehavior (unit :before :instantiate-is-a-list-of)
  (a-slot-name is-a-list-of-value ignore)
  (<- self :check-property-specification a-slot-name is-a-list-of-value :IS-A-LIST-OF 'is-frame))

(defbehavior (unit :instantiate-is-a-list-of)
  (a-slot-name is-a-list-of-value names-value)
  
  ;; generates for a :IS-A-LIST-OF specification the necessary instances
  ;; which are set as value of a-slot-name 
  
  (do ((names names-value (rest names)) 
       (unit-name (first is-a-list-of-value))
       (frame-with-spec (rest is-a-list-of-value))
       (result nil))
      ((null names)
       (<- self :set a-slot-name result))
    (let* ((part-name (first (first names)))
           (instance-with-spec (if (null frame-with-spec) (rest (first names))
                                   (rest (rest (first names)))))
           (combined-with-specification
            (<- self :merge-initialisations (append frame-with-spec instance-with-spec))))
      (<- self :create-part part-name unit-name combined-with-specification)
      (push part-name result))))


;; ____________________________________________________________


(defbehavior (unit :create-part)
  (part-name unit-name with-spec)
  
  ;; generates an instance of unit-name with the name part-name and
  ;; the initializations in with-spec
  ;; if there is already an existing (not recursively-generated) instance
  ;; it will be used.
  
  (cond ((and (is-instance part-name)
              (not (<- part-name :is-recursively-generated)))
         (make-instance-unprintable part-name))
        (t (let ((new-def `(definstance ,part-name OF ,unit-name . ,with-spec)))
             (eval new-def)
             (mark-as-recursively-generated part-name)
             (make-instance-unprintable part-name)))))

(defun make-instance-unprintable (part-name)
  (declare (ignore part-name))
  ;; ???????
  t)

;; ____________________________________________________________



(defbehavior (unit :after :create-part) (part-name unit-name with-spec)
  
  ;; tracing is done after creation
  
  (declare (ignore with-spec))
  (<- self :trace-unit-creation part-name unit-name))

;; ____________________________________________________________


(defbehavior (unit :install-relations-for-instance)
  ()
  
  ;; behavior zum installieren der auf frame-ebene
  ;; definierten verbindungen
  ;; Syntax:
  ;;
  ;;  (<PART-NAME> - :IS-A <type> :RELATION (<connection-name> . <slots>))
  ;;  oder
  ;;  (<PART-NAME> - :IS-A <type> :RELATION ((<connection-name> . <slots>)))
  ;; <connection-name> muss der name eines slots von <type> sein
  ;; <slots> muessen namen von (part-)slots dieses objekts sein
  
  (dolist (a-slot-name ($send self :slots))
    (let ((relation-value (<- self :get-relation-value a-slot-name)))
      (if relation-value
        (progn
          (if (symbolp (first relation-value))
            (setq relation-value `(,relation-value)))
          (dolist (relation-spec relation-value)
            (<- self :install-relation-for-a-part a-slot-name relation-spec)))))))


;; ____________________________________________________________


(defbehavior (unit :get-relation-value)
  (a-slot-name) 
  
  ;; liefert den Wert der :RELATION property
  ;; von a-slot-name falls angegeben.
  
  (let ((relation-value (<- self :get a-slot-name :RELATION)))
    (cond ((null relation-value) nil)
          (t (if (atom relation-value) 
               (error  "wrong :RELATION specification ~@
                          for slot ~S of instance ~S."
                       a-slot-name ($send self :object-name))
               relation-value)))))


;; ____________________________________________________________


(defbehavior (unit :install-relation-for-a-part)
  (a-slot-name relation-value)
  
  ;; hier werden die fuer einen (part-)slot spezifizierten verbindungen
  ;; bei der instanz fuer diese komponente eingetragen
  
  (if (not (listp relation-value))
    (error  "~S wrong relation specification for ~S of ~S"
            relation-value a-slot-name ($send self :object-name))
    (let ((relation-name (first relation-value))
          (second-relation-args (rest relation-value))
          (first-relation-arg (<- self :get a-slot-name))) 
      (if (not (member relation-name (<- first-relation-arg :slots)))
        (error  "~S is not a SLOT-NAME of ~S" a-slot-name first-relation-arg)
        (let ((all-slots (<- self :slots)))
          (dolist (object second-relation-args)
            (if (not (member object all-slots :test 'equal))
              (error  "~S is not a SLOT-NAME of ~S" object ($send self :object-name))
              (<- first-relation-arg :add-to-relation
                  relation-name (<- self :get object)))))))))


;; ____________________________________________________________


(defbehavior (unit :add-to-relation)
  (relation-name object)
  
  ;; fuegt object unter relation-name hinzu falls
  ;; noch nicht vorhanden
  
  (if (atom relation-name) (setq relation-name `(,relation-name :value)))
  
  (let ((old-relation-value (lexpr-$send self :get relation-name)))
    (<- self :set (first relation-name)
        (cond ((or (is-undetermined old-relation-value)
                   (is-unknown old-relation-value)
                   (null old-relation-value))
               `(,object))
              ((listp old-relation-value)
               (if (member object old-relation-value)
                 old-relation-value
                 (cons object old-relation-value)))
              (t (cons object (list old-relation-value))))
        (second relation-name))))


;; ____________________________________________________________

;; ***************** TRACING STUFF *****************


(defbehavior (unit :trace-visited-unit)
  (slot-name)
  
  ;; for tracing the searching of a relation of a part
  
  (if (<- self :trace-is-set)
    (send-kb :send-system-trace-window :format
             "~%!! UNIT ~S VISITED SEARCHING FOR RELATION ~S." ($send self :object-name) slot-name)))



(defbehavior (unit :trace-unit-creation)
  (part-name unit-name)
  
  ;; for tracing the creation of a part
  
  (if (<- self :trace-is-set)
    (send-kb :send-system-trace-window :format
             "~%!! UNIT ~S CREATED AS PART OF ~S." part-name unit-name)))


;; ____________________________________________________________

;; ***************** GENERAL BEHAVIORS ON UNITS *****************

;; ***************** BEHAVIORS FOR HANDLING RELATIONS *****************


(defbehavior (unit :get-value-of-relation)
  (relation-name)
  
  ;; gets the value of a slot which is interpreted as relation-name
  ;;  important: returns nil if value is undetermined or unknown or nil
  ;;               returns (value) if value is an atom 
  
  (if (atom relation-name) (setq relation-name `(,relation-name :value)))
  
  (let ((relation-value (lexpr-$send self :get relation-name)))
    (cond ((or (is-undetermined relation-value)
               (is-unknown relation-value)
               (null relation-value))
           nil) 
          ((symbolp relation-value) `(,relation-value))
          (t relation-value))))

;; ____________________________________________________________


(defbehavior (unit :get-related-components)
  (relation &optional (condition :all) stop-if-reached)
  
  ;; this is the basic behavior for accessing objects which are
  ;; related via relation to this object or to test if self is
  ;; connected to stop-if-reached via relation.
  ;; condition can be as follows:
  ;; a) :all
  ;;    gets all components which are related direct or indirect with self
  ;;    via relation
  ;; b) :terminal
  ;;    gets only those components which are not themselves related to other
  ;;    components (via relation)
  ;; c) nil
  ;;    is aquivalent to :all
  ;; d) in all other cases condition is interpreted as the name of a behavior
  ;;    which is sent to each related component with the predecessor as an
  ;;    additional argument. the user must take care that the behavior handles
  ;;    nil (for this object) correctly.
  (if (null (<- self :get-value-of-relation relation))
    nil
    (let ((marker (gensym "V")))
      (catch 'object-found
        (let ((result
               (<- self :get-related-components-recursive relation condition marker stop-if-reached nil nil)))	
          (dolist (node (second result))
            (remprop node marker))
          (cond (stop-if-reached nil)		; if connection should be tested and was not found then nil
                ((eq condition :all)
                 (rest (first result)))	; rest drops name of this object
                (t (first result))))))))


;; ____________________________________________________________


(defbehavior (unit :get-related-components-recursive)
  (relation condition marker stop-if-reached predecessor visited-nodes)
  
  ;; this is for getting the related components recursively or
  ;; for testing if self is connected to stop-if-reached if given
  ;;
  ;; because of the condition it is necessary also to follow the relation
  ;; behind a component which does not satisfy the condition.
  
  (if (get ($send self :object-name) marker)
    `(() ,visited-nodes)
    (progn
      (if (eq ($send self :object-name) stop-if-reached)
        (throw 'object-found
               (dolist (node visited-nodes t) (remprop node marker))))
      (setf (get ($send self :object-name) marker) t)
      (push ($send self :object-name) visited-nodes)
      (<- self :trace-visited-unit relation)
      (let ((result-for-this-node
             (if (<- self :valid-predecessor-condition relation condition predecessor)
               `(,($send self :object-name))
               nil))
            (successors (<- self :get-value-of-relation relation)))
        (prog ((result `(,result-for-this-node ,visited-nodes))
               result-for-a-successor)
          loop
          (if (null successors)
            (return result)
            (progn
              (setq result-for-a-successor
                    (<- (pop successors) :get-related-components-recursive
                        relation condition marker stop-if-reached ($send self :object-name)
                        (second result)))
              (setq result `((,@(first result) ,@(first result-for-a-successor))
                             ,(second result-for-a-successor)))))
          (go loop))))))

;; ____________________________________________________________


(defbehavior (unit :valid-predecessor-condition)
  (relation condition predecessor)
  
  ;; for testing if condition helds for this object and the predecessor
  
  (cond ((or (null condition) (eq condition ':all)) t)
        ((eq condition ':terminal)
         (null (<- self :get-value-of-relation relation)))
        (t ($send self condition predecessor) ;; here send must be used to compute the selector
           )))


;; ____________________________________________________________


(defbehavior (unit :is-connected-with)
  (a-component a-relation &optional (condition nil))
  
  ;;  ********** USER *********
  ;;  
  ;; if <condition> is nil returns t iff self is connected with
  ;; <a-component> via the edge <a-relation>. The value of the slot
  ;; with the name <a-relation> must be a list of instance-names.
  ;; if <condition> is not nil it must be the name of a behavior with
  ;; one optional argument. the function then returns t iff there is
  ;; a path from self to a-component (self i1 i2 i3 .... a-component)
  ;; such that all results of  (<- self condition nil)
  ;; (<- i1 condition self), (<- i2 condition i2) etc. returns not nil.
  ;; the user has to take care, that the behavior handles nil as argument
  ;; correctly.
  
  (<- self :get-related-components a-relation condition a-component))


;; ____________________________________________________________


(defbehavior (unit :connected-components)
  (a-relation &optional (condition :all)) 
  
  ;;; ********** USER **********
  ;;;
  ;;; liefert alle mit diesem object verbundenen komponenten
  ;;; ist condition spezifiziert, so muss es der name einer
  ;;; Nachricht sein, in diesem Fall werden nur solche komponenten
  ;;; zurueckgeliefert, die auf diese nachricht einen wert ungleich
  ;;; nil zurueckliefern. Im Unterschied zu :is-connected-with und
  ;;; :get-paths wird dieser message "kein" weiteres argument uebergeben.
  
  (<- self :get-related-components a-relation condition))

;; ____________________________________________________________



(defbehavior (unit :get-the-paths) (a-component a-relation all-or-first &optional condition)
  
  ;; liefert die pfade, die von self ueber a-relation
  ;; nach a-component fuehren. Bei all-or-first = first wird
  ;; der erste (breadth-first) pfad geliefert, bei all-or-first = all
  ;; werden alle moeglichen pfade geliefert.
  ;; condition kann optional spezifiziert werden hier gilt analoges zu
  ;; is-connected-with (s.o.) 
  
  (prog ((known-paths `((,($send self :object-name)))) 
         current-path current-node expanded-nodes new-paths all-found-paths)
    (if (and condition
             (not (eval `(<- ,($send self :object-name) ,condition nil))))
      (return nil))
    loop
    (if (and all-found-paths
             (eq all-or-first 'first))
      (return (first all-found-paths)))
    (if (null known-paths) (return all-found-paths))
    (setq current-path (pop known-paths))
    (setq current-node (first current-path))
    (setq new-paths nil)
    (setq new-paths
          (dolist (node (<- current-node :get a-relation) (nreverse new-paths)) 
            (if (and (not (eq current-node node))
                     (not (if (eq all-or-first 'first)
                            (member node expanded-nodes)
                            (>
                             (apply '+
                                    (mapcar #'(lambda (a-node)
                                                (if (eq a-node node)
                                                  1.
                                                  0.))
                                            (rest current-path)))
                             1.)))
                     (or (not condition)
                         (eval `(<- ,node ,condition ,current-node))))
              (push (cons node current-path) new-paths))))
    (push current-node expanded-nodes)
    (<- current-node :trace-visited-unit a-relation)
    (dolist (a-path new-paths)
      (if (eq (first a-path) a-component)
        (push (reverse a-path) all-found-paths)
        (setq known-paths (append known-paths (list a-path)))))
    (go loop)))

;; ____________________________________________________________


(defbehavior (unit :get-path)
  (a-component a-relation &optional condition)
  
  ;; gets the first path from self to a-component via relation
  
  (<- self :get-the-paths a-component a-relation 'first condition))


;; ____________________________________________________________


(defbehavior (unit :get-all-paths)
  (a-component a-relation &optional condition)
  
  ;; gets all paths from self to a-component via relation
  ;; cycles are handled correctly
  
  (reverse (<- self :get-the-paths a-component a-relation 'all condition)))


;; ____________________________________________________________


(defbehavior (unit :tree) (linking-slot &optional slot-names)
  
  ;; liefert die Listendarstellung eines Baumes mit linking-slot als
  ;; Kante und den Knonten als strings, ist slot-names zusaetzlich
  ;; gegeben, wobei hier nur Namen von slots angegeben sein duerfen,
  ;; die in allen instanzen des Baumes enthalten sein muessen, so wird
  ;; auf jedem level des baumes ein zusaetzlicher link auf diesen slot mit
  ;; seinem wert mitgeliefert.
  
  (if (atom linking-slot)
    (setq linking-slot `(,linking-slot :value)))
  
  (let ((children (lexpr-$send self :get linking-slot)))
    (if (null children)
      (if slot-names
        `(,(format nil "~s" ($send self :object-name))
          . ,(mapcar #'(lambda (a-slot-name)
                         `(,(format nil "==> ~s ~s"
                                    a-slot-name (<- self :get a-slot-name))))
                     slot-names))
        `(,(format nil "~s" ($send self :object-name))))
      (let ((children-tree (mapcar #'(lambda (child)
                                       (<- child :tree linking-slot slot-names))
                                   children)))
        (if slot-names
          `(,(format nil "~s" ($send self :object-name))
            ,@(mapcar #'(lambda (a-slot-name)
                          `(,(format nil "==> ~s ~s"
                                     a-slot-name (<- self :get a-slot-name))))
                      slot-names)
            . ,children-tree)
          `(,(format nil "~s" ($send self :object-name)) . ,children-tree))))))

;; ____________________________________________________________


(defbehavior (unit :describe-tree) (linking-slot &rest slot-names)
  (declare (ignore linking-slot slot-names))
  ;; displayed den durch :tree erzeugten Baum im Browser-window
  
  (progn
    (if (not (and (flavor-typep *k1-ted* 'tree-displayer)
                  *k1-ted*))
      (create-ted *k1-ted* 'tree-displayer))
    ($send *k1-ted* :display
          (cons (format nil "~S" ($send self :object-name))
                (<- self :tree linking-slot slot-names)))))

;(setf tree '("root" "root" ("a" ("aa") ("bb")) ("b")))

;; ____________________________________________________________


(defbehavior (unit :find-path) (descendant linking-slot &optional slot-names)
  
  
  (prog (the-father end result slot-name prop-name)
    (cond ((atom linking-slot)
           (setq slot-name linking-slot prop-name :value))
          (t (setq slot-name (first linking-slot) prop-name (second linking-slot))))
    loop (push (if slot-names
                 `(,descendant . ,(mapcar #'(lambda (a-slot-name)
                                              `(==> ,a-slot-name 
                                                    ,(<- descendant
                                                         :get a-slot-name)))
                                          slot-names))
                 descendant)
               result)
    (if end (return result))
    (setq the-father (first (<- descendant :get slot-name prop-name)))
    (if (null the-father)
      (return nil)
      (progn
        (if (eq the-father ($send self :object-name))
          (setq end t))
        (setq descendant the-father)
        (go loop)))))

;; ____________________________________________________________

;;; **************** COMPOUND-UNIT FRAME ****************

;; this is the basic frame functional and physical components
;; are built on. In addition to the capabilities of the UNIT frame
;; it handles two special relations (slots): PARTS and PART-OF

(defframe compound-unit
  (supers unit)
  (slots (parts nil)
         (part-of nil)))

;; ____________________________________________________________

;;; **************** PUT- AND GET-BEHAVIORS WHICH REGARD RESTRICTED-SLOTS *****************

(defbehavior (compound-unit :cput)
  (slot new-value)
  (if (eq (type-of (<- self :get-value-only slot))
          'restricted-slot)
    ($send (<- self :get-value-only slot) :put
          self slot new-value)
    (<- self :put slot new-value)))

(defbehavior (compound-unit :cget)
  (slot)
  (if (eq (type-of (<- self :get-value-only slot))
          'restricted-slot)
    ($send (<- self :get-value-only slot) :get)
    (<- self :get slot)))


(defbehavior (compound-unit :add-to-parts)
  (an-instance-or-a-list &optional (prop-name :value))
  
  prop-name ;; for the compiler
  
  ;; erweitert <parts>-slot um an-instance-or-a-list
  
  (setf ($value 'parts)
        (if (atom an-instance-or-a-list)
          `(,an-instance-or-a-list . ,($value 'parts))
          `(,@an-instance-or-a-list . ,($value 'parts)))))

(defbehavior (compound-unit :after :add-to-parts)
  (an-instance-or-a-list &optional (prop-name :value))
  
  ;; erweitert <parts>-slot um an-instance-or-a-list
  (if (or (null prop-name) (is-value prop-name))
    nil
    (let ((prop-val (<- self :get 'parts prop-name)))
      (<- self :set 'parts
          (if (atom an-instance-or-a-list)
            `(,an-instance-or-a-list . ,prop-val)
            `(,@an-instance-or-a-list . ,prop-val))
          prop-name))))

;; ____________________________________________________________

(defbehavior (compound-unit :add-to-part-of)
  (an-instance &optional (prop-name :value))
  
  ;; erweitert <part-of>-slot mit an-instance
  prop-name ;; for the compiler
  
  (setf ($value 'part-of) (cons an-instance ($value 'part-of))))

(defbehavior (compound-unit :after :add-to-part-of)
  (an-instance &optional (prop-name :value))
  
  ;; erweitert <part-of>-slot mit an-instance
  (if (or (null prop-name) (is-value prop-name))
    nil
    (let ((prop-val (<- self :get 'part-of prop-name)))
      (<- self :set 'part-of `(,an-instance . ,prop-val) prop-name))))

;; ____________________________________________________________

;;; **************** PHYSICAL-UNIT FRAME ****************

;; It handles the properties :PHYSICAL-PARTS and :PHYSICAL-PART-OF
;; of slot PARTS and PART-OF
 
(defframe physical-unit
  (supers compound-unit))

;; ____________________________________________________________


(defbehavior (physical-unit :after :initialize)
  (&rest ignore)
  ())


(defbehavior (physical-unit :add-to-physical-parts)
  (an-instance-or-a-list)
  
  ;; erweitert <physical-parts>-slot um an-instance-or-a-list
  (<- self :add-to-parts an-instance-or-a-list :physical-parts))

;; ____________________________________________________________


(defbehavior (physical-unit :add-to-physical-part-of)
  (an-instance)
  
  ;; erweitert <physical-part-of>-slot mit an-instance
  (<- self :add-to-part-of an-instance :physical-part-of))

;; ____________________________________________________________


(defbehavior (physical-unit :after :create-part) (part-name unit-name with-spec)
  (declare (ignore unit-name with-spec))

  ;; updating der <physical-part-of> und <physical-parts> slots
  ;; der entsprechenden Instanzen.
  
  (cond ((<- part-name :type 'PHYSICAL-UNIT)
         (<- self :add-to-physical-parts part-name)
         (<- part-name :add-to-physical-part-of ($send self :object-name)))))


;; ____________________________________________________________


(defbehavior (physical-unit :describe-physical-tree) (&rest slot-names)
  (lexpr-$send self :describe-tree '(PARTS :PHYSICAL-PARTS) slot-names))

(defbehavior (physical-unit :find-physical-path-down-to)
  (descendant &optional slot-names)
  (lexpr-$send self :find-path descendant '(PART-OF :PHYSICAL-PART-OF) slot-names))

(defbehavior (physical-unit :find-physical-path-up-to)
  (ancestor &optional slot-names)
  (reverse (<- ancestor :find-path ($send self :object-name) '(PART-OF :PHYSICAL-PART-OF) slot-names)))

;; ____________________________________________________________


(defbehavior (physical-unit :get-physical-subcomponents)
  (&optional (condition :TERMINAL))
  
  ;;;  ********** USER *********
  ;;;
  ;;; Behavior zur Ermittlung physikalischer Unterkomponenten.
  ;;; <condition> kann eines der Schluesselworte :ALL oder :TERMINAL
  ;;; oder der Name eine benutzerdefierten Behaviors sein, das als
  ;;; Praedikat interpretiert wird und bewirkt, dass nur solche
  ;;; physikalische Unterkomponenten ermittelt werden, die dieses
  ;;; Praedikat erfuellen.
  
  (<- self :get-related-components '(PARTS :PHYSICAL-PARTS) condition))


;; ____________________________________________________________


(defbehavior (physical-unit :get-physical-supercomponents)
  (&optional (condition :TERMINAL))
  
  ;;;  ********** USER *********
  ;;;
  ;;; Behavior zur Ermittlung physikalisch uebergeordneter Komponenten.
  ;;; <condition> kann eines der Schluesselworte :ALL oder :TERMINAL
  ;;; oder der Name eine benutzerdefierten Behaviors sein, das als
  ;;; Praedikat interpretiert wird und bewirkt, dass nur solche
  ;;; physikalisch uebergeordneter Komponenten ermittelt werden, die dieses
  ;;; Praedikat erfuellen.
  
  (<- self :get-related-components '(PART-OF :PHYSICAL-PART-OF) condition))

;; ____________________________________________________________

;;; **************** FUNCTIONAL-UNIT FRAME ****************

;; It handles the properties :FUNCTIONAL-PARTS and :FUNCTIONAL-PART-OF
;; of slot PARTS and PART-OF

(defframe functional-unit
  (supers compound-unit))

;; ____________________________________________________________


(defbehavior (functional-unit :add-to-functional-parts)
  (an-instance-or-a-list)
  (<- self :add-to-parts an-instance-or-a-list :FUNCTIONAL-PARTS))


;; ____________________________________________________________


(defbehavior (functional-unit :add-functional-part-of)
  (an-instance)
  (<- self :add-to-part-of an-instance :FUNCTIONAL-PART-OF))

;; ____________________________________________________________


(defbehavior (functional-unit :after :create-part)
  (part-name unit-name with-spec)
  unit-name ;; for the compiler
  with-spec ;; for the compiler
  (cond ((<- part-name :type 'FUNCTIONAL-UNIT)
         (<- part-name :add-functional-part-of ($send self :object-name))
         (<- self :add-to-functional-parts part-name))))

;; ____________________________________________________________


(defbehavior (functional-unit :describe-functional-tree) (&rest slot-names)
  (lexpr-$send self :describe-tree '(PARTS :FUNCTIONAL-PARTS) slot-names))

(defbehavior (functional-unit :find-functional-path-down-to)
  (descendant &optional slot-names)
  (if (<- self :type 'FUNCTIONAL-UNIT)
    (lexpr-$send self :find-path descendant '(PART-OF :FUNCTIONAL-PART-OF) slot-names)
    (error "~S is not a FUNCTIONAL-UNIT." ($send self :object-name))))

(defbehavior (functional-unit :find-functional-path-up-to)
  (ancestor &optional slot-names)
  (reverse (<- ancestor :find-path ($send self :object-name) '(PART-OF :FUNCTIONAL-PART-OF) slot-names)))

;; ____________________________________________________________


(defbehavior (functional-unit :get-functional-subcomponents)
  (&optional (condition :TERMINAL))
  
  ;;;  ********** USER *********
  ;;;
  ;;; Behavior zur Ermittlung funktionaler Unterkomponenten.
  ;;; <condition> kann eines der Schluesselworte :ALL oder :TERMINAL
  ;;; oder der Name eine benutzerdefierten Behaviors sein, das als
  ;;; Praedikat interpretiert wird und bewirkt, dass nur solche
  ;;; funktionalen Unterkomponenten ermittelt werden, die dieses
  ;;; Praedikat erfuellen.
  
  (<- self :get-related-components '(PARTS :FUNCTIONAL-PARTS) condition))

;; ____________________________________________________________


(defbehavior (functional-unit :get-functional-supercomponents)
  (&optional (condition :TERMINAL))
  
  ;;;  ********** USER *********
  ;;;
  ;;; Behavior zur Ermittlung funktional uebergeordneter Komponenten.
  ;;; <condition> kann eines der Schluesselworte :ALL oder :TERMINAL
  ;;; oder der Name eine benutzerdefierten Behaviors sein, das als
  ;;; Praedikat interpretiert wird und bewirkt, dass nur solche
  ;;; funktional uebergeordneter Komponenten ermittelt werden, die dieses
  ;;; Praedikat erfuellen.
  
  (<- self :get-related-components '(PART-OF :FUNCTIONAL-PART-OF) condition))


;; ____________________________________________________________


(if (member 'unit-kb *known-knowledge-bases*)
  ($send unit-kb :send-if-handles :export-kb))


;;; eof

