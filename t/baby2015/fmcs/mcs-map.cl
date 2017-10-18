;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: FMCS -*-

(in-package "FMCS")

;;           Copyright  1989, 1988, 1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;; Authors:  Harry Bretthauer, Eckehard Gross, Juergen Kopp, Juergen Walther

;;
;; Abbildung auf das zugrundeliegende Flavor System MCS
;;

;;;(export 'self)

(defvar flavor-class nil)

;  -------------------------------------------------------------------
;   $send self wird durch send-self substituiert
;  -------------------------------------------------------------------


(defun subst-$send-self (form)
  (cond ((atom form) form)
        ((eq (first form) '$send)
         (when (eq (second form) 'self)
           (rplaca form 'send-self)
           (rplacd form (cddr form)))
         (subst-$send-self (cdr form)))
        (t (subst-$send-self (car form))
           (subst-$send-self (cdr form))))
  form)


;  -------------------------------------------------------------------
;   Instanzvariablen werden in Methoden wie freie Variablen referiert
;  -------------------------------------------------------------------

(defun SUBLIS-SELECT (a_list tree &optional (test #'eql)
                             (filter #'(lambda (expr) 
                                         (declare (ignore expr))
                                         t)))
  (declare (list a_list))
  (cond ((atom tree) tree)
        ((funcall (the function filter) tree)
         (let ((pair (assoc (first tree) a_list :test test)))
           (cond (pair (rplaca tree (cdr pair))
                       (sublis-select a_list (cdr tree) test filter))
                 (t (sublis-select a_list (car tree) test filter)
                    (sublis-select a_list (cdr tree) test filter)))))
        (t ()))
  tree)

(defun COMPILE-SLOT-REFERENCES (slot-names lambda-body
                                &optional (slot-access-fn 'get-slot))
    ; (print "in compile-slot-ref.")
  (sublis-select  (mapcar #'(lambda (a_slot)
                              (cons a_slot (list slot-access-fn (list 'quote a_slot))))
                          slot-names)
                  lambda-body
                  #'eql
                  #'(lambda (x)
                      (and (listp x)
                           (not (eql (car x) slot-access-fn))
                           (not (eq (car x) 'quote))))))

;;; ------------------------------------------------------------------
;;;
;;; ------------------------------------------------------------------

(defun GET-ALL-REQUIRED-SLOT-NAMES (class)
  (append (mcs-slot-value class (index-of-all-slots))
          (slot-value class 'req-inst-vars)))

(defun required-instance-variables (options)
  (dolist (option options)
    (if (and (consp option)
             (equal (car option) :required-instance-variables))
      (return (cdr option)))))

(defmetaclass flavor-class (req-inst-vars) ())

(setf (mcs-slot-value flavor-class (index-of-basicnew-fn))
      #'(lambda (isit &key (name nil) (supers nil) (own-slots nil)
                      (req-inst-vars nil))
          (send-fast
           (make-mcsobject 
            :env (vector isit name supers nil nil nil own-slots
                         (make-hash-table :test #'eq)
                         nil nil nil req-inst-vars))
           :basic-init)))

(defmethod (flavor-class :basic-init) ()
   (send-self :compute-cplist)
   (send-self :inherit-slots-with-defaults)
   self)

(defmethod (flavor-class :init) (&rest inits)
  (declare (ignore inits))
  (send-self :compute-slot-accessor-fn)
  (send-self :extend-subclasses-of-supers)
  (send-self :compute-slot-access-methods)
  (send-self :compute-basicnew-fn)
  self)


;;; ------------------------------------------------------------------
;;; Definition von Flavors
;;; ------------------------------------------------------------------

;;;(export 'def$flavor)
(defmacro def$flavor (a_class a_list-of-instance-variables 
                              a_list-of-superclasses &rest options)
  `(progn
     (eval-when (compile)
       (defvar ,a_class)    ; um compiler warnings zu unterdruecken
       (setq ,a_class
             (funcall (mcs-slot-value flavor-class (index-of-basicnew-fn))
                      flavor-class
                      :name ',a_class 
                      :supers (if ',a_list-of-superclasses 
                                (list ,@a_list-of-superclasses)
                                (list standard-object))
                      :own-slots ',a_list-of-instance-variables
                      :req-inst-vars ',(required-instance-variables options))))
     ; warum das im kontext von def-kb-konfiguration in gclisp komilierbar ist
     ; und die alte version mit let nicht, the lord knows
     (eval-when (load)
       (defvar ,a_class)
       (setq ,a_class 
             (send-fast (funcall (mcs-slot-value flavor-class (index-of-basicnew-fn))
                                 flavor-class
                                 :name ',a_class 
                                 :supers (if ',a_list-of-superclasses 
                                           (list ,@a_list-of-superclasses)
                                           (list standard-object))
                                 :own-slots ',a_list-of-instance-variables
                                 :req-inst-vars ',(required-instance-variables options))
                        :init)))
     (eval-when (eval)
       (defvar ,a_class)    ; um compiler warnings zu unterdruecken
       (let ((new-class (funcall (mcs-slot-value flavor-class (index-of-basicnew-fn))
                                 flavor-class
                                 :name ',a_class 
                                 :supers (if ',a_list-of-superclasses 
                                           (list ,@a_list-of-superclasses)
                                           (list standard-object))
                                 :own-slots ',a_list-of-instance-variables
                                 :req-inst-vars ',(required-instance-variables options))))
         (if (flavorp ',a_class) 
           (redefine-class ,a_class new-class)
           (setq ,a_class (send-fast new-class :init)))))
     ))
  

;;; Waehrend der Entwicklung eines Systems will man Flavors aendern, also
;;; redefinieren. Wird ein Flavor redefiniert, so werden entsprechende
;;; Teile der Vererbungshierarchie dem neuen Stand angepasst.

;;; Das heisst:  - das Flavor muss aus den Subklassenlisten ihrer
;;;                ehemaligen Superklassen entfernt werden;
;;;              - die ehemaligen Subklassen des Flavor muessen redefiniert werden.

;;; Die Instanzen von geaenderten Flavors bleiben unveraendert, muessen also
;;; vom Programmierer selbst neu erzeugt werden, d.h. Programmteile, die Instanzen
;;; erzeugen bzw. verwenden muessen neu ausgewertet werden.
;;; Deswegen wird eine entsprechende Warnung an den Benutzer ausgegeben!

;;;(export '*redefine-warnings*)
(defvar *redefine-warnings* nil)

(defun redefine-class (old-class new-class)
  (let ((old-supers (mcs-slot-value old-class (index-of-supers)))
        (old-methods (mcs-slot-value old-class (index-of-methods)))
        (old-subclasses (mcs-slot-value old-class (index-of-subclasses)))
        (new-cplist (cons old-class (rest (slot-value new-class 'cplist)))))
    (remove-subclass old-class old-supers)
    (setf (mcs-env old-class) (mcs-env new-class))
    (setf (slot-value old-class 'cplist) new-cplist)
    (send-fast old-class :init)
    (let ((new-methods (mcs-slot-value old-class (index-of-methods))))
      (maphash #'(lambda (key value)
                 (if (not (gethash key new-methods))
                   (setf (gethash key new-methods) value)))
             old-methods))
    (if *redefine-warnings*
      (warn "~&~S has been redefined. Instances may be invalid now!" old-class))
    (redefine-subclasses old-subclasses)
    old-class))

(defun remove-subclass (class superclasses)
  (dolist (super superclasses)
    (setf (mcs-slot-value super (index-of-subclasses))
          (remove class (mcs-slot-value super (index-of-subclasses)) :test #'eq))))

(defun redefine-subclasses (list-of-classes)
  (dolist (subclass list-of-classes)
    (eval
     `(def$flavor ,(slot-value subclass 'name)
        ,(slot-value subclass 'own-slots)
        ,(mapcar #'(lambda (class)
                     (slot-value class 'name))
                 (slot-value subclass 'supers))
        (:required-instance-variables ,@(slot-value subclass 'req-inst-vars))
        ))))

;;;(export 'def$frame)
(defmacro def$frame (name instance-vars components &rest options)
  `(def$flavor ,name ,instance-vars ,components ,@options))

  

;;; ------------------------------------------------------------------
;;; Definition von Methoden
;;; ------------------------------------------------------------------

;;;(export 'def$method)
(defmacro def$method ((name . type&selector) varlist . body)
  (let ((new-body
         (compile-slot-references (get-all-required-slot-names (symbol-value name))
                                  (subst-$send-self body))))
  `(defmethod (,name ,@type&selector) ,varlist ,@new-body)))

;;;(export 'def$behavior)
(defmacro def$behavior ((name . type&selector) varlist . body)
  (let ((new-body (subst-$send-self body)))
    `(defmethod (,name ,@type&selector) ,varlist ,@new-body)))

;;;(export 'undef$method)
(defmacro undef$method ((name . type&selector)) ; for testing only
  `(undefmethod (,name . ,type&selector)))

;;;(export 'trace$method)
(defmacro trace$method ((flav-name selector))
  "traces a method on *trace-output*"
  `(mcs-trace ,flav-name ,selector))

;;;(export 'untrace$method)
(defmacro untrace$method ((flav-name selector))
  "untraces a method"
  `(mcs-untrace ,flav-name ,selector))

;;;(export 'is-traced$method)
(defmacro is-traced$method ((flav-name selector))
  "untraces a method"
  `(mcs-is-traced ,flav-name ,selector))

;;;(export 'compile-$flavor-$methods)
(defmacro compile-$flavor-$methods (&rest flavors)    
 `(eval-when (load)
    (combine-class-methods ,@flavors)))

;;; ------------------------------------------------------------------
;;; WHOPPER = :AROUND method combination
;;; ------------------------------------------------------------------

;;;(export 'defwhopper)
(defmacro defwhopper ((flavor-name operation) arglist &body body)
  `(def$method (,flavor-name :around ,operation) (,@arglist)
     ,@body))

;;;(export 'continue-whopper)
(defmacro continue-whopper (&rest changed-args)
  (if changed-args
    `(call-next-method-fn self class-env inst-env mcs%caller mcs%next-methods 
                          (list ,@changed-args))
    `(call-next-method-fn self class-env inst-env mcs%caller mcs%next-methods 
                          mcs%args)
    ))

;;; ------------------------------------------------------------------
;;; Senden von Nachrichten
;;; ------------------------------------------------------------------

; (send-message (object selector &rest message)     is provided by mcs

;;;(export '$send)
(defmacro $send (object message &rest args)
  `(send-message ,object ,message ,@args))

;;;(export 'lexpr-$send)
(defmacro lexpr-$send (object message &rest args)
  `(apply #'send-message ,object ,message ,@args))

;;; ------------------------------------------------------------------
;;; Funktionen bzw. Makros fuer Flavors und Instanzen
;;; ------------------------------------------------------------------

;;; the typep function of different lisp implementations behave differently
;;; in case of unknown type specifiers,
;;; it may warn you, give an error, or return nil
;;; what a horror
 
;;;(export 'flavorp)
(defun flavorp (object)
  (if (and (boundp object)(typep (symbol-value object) 'mcsobject))
     (send-fast (symbol-value object) :class-p)))

;;;(export 'flavor-instancep)
(defun flavor-instancep (object)
  (typep object 'mcsobject))

;;;(export 'flavor-typep)
(defun flavor-typep (object type)
  (if (typep object 'mcsobject)
    (if (and (boundp type)
             (member (symbol-value type)
                     (mcs-slot-value (mcs-slot-value object (index-of-isit))
                                     (index-of-cplist))
                     :test #'eq))
      t)
    (unless (flavorp type)
      (typep object type))))


;;;(export 'flavor-type-of)
(defun flavor-type-of (object)
  (if (typep object 'mcsobject)
    (mcs-slot-value (mcs-slot-value object (index-of-isit)) (index-of-name))
    (type-of object)))

;;; -------------------------------

;;;(export 'get-flavor-instance-slots)
(defun get-flavor-instance-slots (instance)
  (remove 'isit (mcs-slot-value (mcs-slot-value instance (index-of-isit)) 
                                (index-of-all-slots))))

;;;(export 'symbol-value-in-$instance)
(defmacro symbol-value-in-$instance (instance slot-name)
    `(slot-value ,instance ,slot-name))

;;;(export '$slot)
(defmacro $slot (slot-name)
    `(get-slot ,slot-name))

;;; ------------------------------------------------------------------
;;; Definition von Flavorinstanzen
;;; ------------------------------------------------------------------

;; (defmacro make-$instance (flavor &rest init-plist)
;;  `(send (eval ,flavor) :new ,@init-plist))

;; 3.1.89

;;;(export 'MAKE-$INSTANCE)
(defmacro MAKE-$INSTANCE (flavor &rest initializations)
  `(let ((class (symbol-value ,flavor)))
     (funcall (mcs-slot-value class (index-of-basicnew-fn))
              class ,@initializations)))

;;;(export 'make-window-or-instance)
(defmacro make-window-or-instance (flavor &rest initializations)
  `(MAKE-$INSTANCE ,flavor ,@initializations))


;;; -------------------------------------------------------------------
;;; Methoden fuer alle Instanzen
;;; -------------------------------------------------------------------

;;; sind vorhanden in mcs

;  :describe
;  :which-operations
;  :apropos
;  :operation-handled-p
;  :send-if-handles
;  :how-combined

;; eof

