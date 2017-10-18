;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: FMCS -*-

(in-package "FMCS")

;;; This is the kernel of a Meta Class System (MCS),  based on 
;;; ObjVlisp by Pierre Cointe and Micro Flavor System by Thomas Christaller.

;;; Author: Harry Bretthauer
;;;         Juergen Kopp

;;; --------------------------------------------------------------------------
;;; Global variables
;;; --------------------------------------------------------------------------

(defvar *save-combined-methods* T)

(defvar STANDARD-OBJECT nil)
(defvar STANDARD-CLASS  nil)

(defvar STANDARD-ACCESSORS nil)  ; will be set below

;;; --------------------------------------------------------------------------
;;; Data structure for objects of the Meta Class System
;;; --------------------------------------------------------------------------

;;; An object of the Meta Class System is represented as a structure.
;;; Its slots are represented as a vector called environment (env)

(defstruct (MCSOBJECT (:conc-name mcs-)
                      (:print-function print-mcs))   ; print-mcs defined below
  env)

(declaim (simple-vector mcs-env))

;;; --------------------------------------------------------------------------
;;; Slot access functions
;;; --------------------------------------------------------------------------

;;; Slots of each class object are:
;;;   isit  name  supers  cplist  all-slots  all-slot-defaults  own-slots
;;;   methods  basicnew-fn  slot-accessor-fn  subclasses

;;; Slots indices in all classes

(defmacro index-of-isit () 0)
(defmacro index-of-name () 1)
(defmacro index-of-supers () 2)
(defmacro index-of-cplist () 3)
(defmacro index-of-all-slots () 4)
(defmacro index-of-all-slot-defaults () 5)
(defmacro index-of-own-slots () 6)
(defmacro index-of-methods () 7)
(defmacro index-of-basicnew-fn () 8)
(defmacro index-of-slot-accessor-fn () 9)
(defmacro index-of-subclasses () 10)

;;; Systems internal slot access functions

(defmacro MCS-GET-SLOT (vector slot-position)
  `(svref ,vector ,slot-position))

(defmacro MCS-SLOT-VALUE (object slot-position)
  `(svref (mcs-env ,object) ,slot-position))

;;; Slot access function to use in methods

(defmacro GET-CLASS ()				;added e.gross
  `(svref inst-env (index-of-isit)))

(defmacro GET-SLOT (slot)			;changed e.gross
  `(svref inst-env
	  (funcall (svref class-env (index-of-slot-accessor-fn)) ,slot)))

;(defmacro SET-SLOT (slot new-value)
;  `(setf (svref inst-env 
;               (funcall (svref class-env (index-of-slot-accessor-fn)) ,slot)) 
;         ,new-value))

(defmacro GET-CLASS-SLOT (slot)
  (case (eval slot)
    (isit `(svref class-env (index-of-isit)))
    (name `(svref class-env (index-of-name)))
    (supers `(svref class-env (index-of-supers)))
    (cplist `(svref class-env (index-of-cplist)))
    (methods `(svref class-env (index-of-methods)))
    (basicnew-fn `(svref class-env (index-of-basicnew-fn)))
    (all-slots `(svref class-env (index-of-all-slots)))
    (t `(slot-value (mcs-get-slot inst-env (index-of-isit)) ,slot))
    ))

;(defmacro SET-CLASS-SLOT (slot new-value)
;  `(set-slot-value (mcs-get-slot inst-env (index-of-isit)) ,slot ,new-value)
;  )

;;; universal (public) slot access functions

(defun SLOT-VALUE (object slot)
  (let ((object-env (mcs-env object)))
    (svref object-env 
           (funcall (svref (mcs-env (svref object-env (index-of-isit))) 
                           (index-of-slot-accessor-fn))
                    slot))))

(defun SET-SLOT-VALUE (object slot value)
  (let ((object-env (mcs-env object)))
    (setf (svref object-env 
                 (funcall (svref (mcs-env (svref object-env (index-of-isit))) 
                                 (index-of-slot-accessor-fn))
                          slot))
          value)))

(defsetf slot-value set-slot-value)

;;; --------------------------------------------------------------------------
;;; Data structure for method entries
;;; --------------------------------------------------------------------------

(defstruct METHOD-ENTRY
  type
  methods-list
  combined-method)   

;;; methods-list = ((:before . before-fn) ... (:after . after-fn))
;;; methods lambda list: 
;;;   of around and primary methods:
;;;      (self class-env inst-env next-methods args arg1 arg2 ...)
;;;   of before and after methods:
;;;      (self class-env inst-env arg1 arg2 ...)


(defmacro GET-SELECTOR-ENTRY (a_selector)
  `(gethash ,a_selector (get-class-slot 'methods))
  )

(defmacro GET-QUALIFIED-METHOD (qualifier list-of-methods)
  `(assoc ,qualifier ,list-of-methods :test #'eq))

(defmacro QUALIFIER-OF (method)
  `(first ,method))

(defmacro LAMBDA-EXPR-OF (method)
  `(rest ,method))

(defmacro add-qualified-method (qualifier method-entry new-fn)
  `(let ((qualified-method 
          (get-qualified-method ,qualifier (method-entry-methods-list ,method-entry))))
     (if qualified-method
       (rplacd qualified-method ,new-fn)
       (setf (method-entry-methods-list ,method-entry)
             (acons ,qualifier
                    ,new-fn
                    (method-entry-methods-list ,method-entry)
                    )))
     ))

(defmacro around-of (applicable-methods)
  `(first ,applicable-methods))

(defmacro demons-of (applicable-methods)
  `(rest ,applicable-methods))

(defmacro before-of (applicable-methods)
  `(second ,applicable-methods))

(defmacro primary-of (applicable-methods)
  `(third ,applicable-methods))

(defmacro after-of (applicable-methods)
  `(fourth ,applicable-methods))


;;; --------------------------------------------------------------------------
;;; Method combination functions
;;; --------------------------------------------------------------------------

(declaim (inline DEMON-METHOD-COMBINATION))

(defun DEMON-METHOD-COMBINATION (self class-env inst-env selector applicable-methods args)
  (declare (ignore selector))
  (let ((before-methods (before-of applicable-methods))
        (primary-methods (primary-of applicable-methods))
        (after-methods (after-of applicable-methods)))
    (prog2
     (loop 
       (if (null before-methods) (return ()))
       (apply (pop before-methods) 
              self class-env inst-env args))
     (apply (first primary-methods)
            self class-env inst-env :primary-caller (rest primary-methods) 
            args args)
     (loop 
       (if (null after-methods) (return ()))
       (apply (pop after-methods) 
              self class-env inst-env args))
     )))

(declaim (inline STANDARD-METHOD-COMBINATION))

(defun STANDARD-METHOD-COMBINATION (self class-env inst-env selector applicable-methods
                                         args)
  (let ((around-methods (around-of applicable-methods)))
    (if around-methods 
      (apply (first around-methods)
             self class-env inst-env 
             :around-caller (cons (rest around-methods) 
                                  (demons-of applicable-methods)) 
             args 
             args)
      (demon-method-combination self class-env inst-env selector applicable-methods args)
      )))


(declaim (inline SIMPLE-METHOD-COMBINATION))

(defun SIMPLE-METHOD-COMBINATION (self class-env inst-env selector applicable-methods
                                       args)
  (declare (ignore selector))
  (let ((primary-methods (primary-of applicable-methods)))
    (apply (first primary-methods)
           self class-env inst-env :primary-caller (rest primary-methods)
           args args)
    ))


;;; --------------------------------------------------------------------------
;;; General message handler
;;; --------------------------------------------------------------------------

(defmacro GET-COMBINED-METHOD (a_selector)
  `(let ((method-entry (gethash ,a_selector (get-class-slot 'methods))))
     (if method-entry (method-entry-combined-method method-entry))))

(declaim (inline STANDARD-MESSAGE-HANDLER))

(defun STANDARD-MESSAGE-HANDLER (self class-env inst-env selector args)
  (let ((combined-method (get-combined-method selector)))
    (if combined-method 
      (funcall (svref combined-method 0)
               self class-env inst-env selector (svref combined-method 1) args)
      (multiple-value-bind
        (method-combination-fn applicable-methods)
        (standard-method-lookup class-env selector)
        (if applicable-methods
          (progn
            (if *save-combined-methods*
              (save-combined-method class-env selector
                                    method-combination-fn applicable-methods))
            (funcall method-combination-fn
                     self class-env inst-env selector applicable-methods args))
          (standard-message-handler self class-env inst-env 
                                    :default-handler (cons selector args)))
        ))))


;;; --------------------------------------------------------------------------
;;; Send functions and macros
;;; --------------------------------------------------------------------------


(defun SEND-MESSAGE (self selector &rest args)
  (if (typep self 'mcsobject)
    (let* ((inst-env (mcs-env self))
           (class-env (mcs-env (svref inst-env (index-of-isit)))))
      (standard-message-handler self class-env inst-env selector args))
    (format nil "ERROR in SEND: SEND can't be applied on ~S" self)))

(defun SEND-FAST (self sel &rest args)
  (let* ((inst-env (mcs-env self))
         (class-env (mcs-env (svref inst-env (index-of-isit)))))
    (standard-message-handler self class-env inst-env sel args)))

(defmacro SEND-SELF (sel &rest args)
  `(standard-message-handler self class-env inst-env ,sel (list ,@args)))


; ++++++

;;; --------------------------------------------------------------------------
;;; Compile method functions
;;; --------------------------------------------------------------------------

(defun SAVE-COMBINED-METHOD (class-env selector method-combination-fn
                                       applicable-methods)
  (let ((method-entry (gethash selector (get-class-slot 'methods))))
    (if method-entry
      (setf (method-entry-combined-method (gethash selector (get-class-slot 'methods)))
            (vector method-combination-fn applicable-methods))
      (setf (gethash selector (get-class-slot 'methods))
            (make-method-entry :type 'standard
                               :methods-list nil
                               :combined-method 
                               (vector method-combination-fn applicable-methods)))
      )))

(defun COMBINE-CLASS-METHOD (a_class a_selector)
  (let ((class-env (mcs-env a_class)))
    (multiple-value-bind
      (method-combination-fn applicable-methods)
      (standard-method-lookup class-env a_selector)
      (if applicable-methods
        (let ((method-entry (gethash a_selector (get-class-slot 'methods))))
          (if method-entry
            (setf (method-entry-combined-method 
                   (gethash a_selector (get-class-slot 'methods)))
                  (vector method-combination-fn applicable-methods))
            (setf (gethash a_selector (get-class-slot 'methods))
                  (make-method-entry 
                   :type 'standard :methods-list nil
                   :combined-method 
                   (vector method-combination-fn applicable-methods))))
          (format nil "Method ~S of class ~S has been combined" a_selector a_class))
        (format nil "No Method ~S of class ~S could been combined" a_selector a_class)
        ))))

(defmacro COMBINE-CLASS-METHODS (&rest classes)
  `(let ((list-of-classes ',classes))
     (loop
       (if (null list-of-classes) (return ()))
       (let* ((class  (eval (pop list-of-classes)))
              (all-methods-list (send-message class :get-protocol)))
         (loop
           (if (null all-methods-list) (return ()))
           (combine-class-method class (pop all-methods-list)))))
     ))


;;; --------------------------------------------------------------------------
;;; Call-next-method macro and functions
;;; --------------------------------------------------------------------------

;;; CALL-NEXT-METHOD  can be used in :around and :primary methods
;;; If (call-next-method) occurs in an :around method, the next :around method 
;;; is called, if there is one. If no, procede with :before, primary and :after
;;; methods. If (call-next-method) occurs in a :primary method the next 
;;; :primary method is called, if there is one. If no, an error message is send.

(defun CALL-NEXT-METHOD-FN (self class-env inst-env caller next-methods args)
  (if (eq caller :primary-caller)
    (let ((next-method (first next-methods)))
      (if next-method 
        (apply next-method
               self class-env inst-env 
               :primary-caller (rest next-methods) args 
               args)
        (error "Can't call next method from primary method.")))
    (let ((around-methods (around-of next-methods)))
      (if around-methods
        (apply (first around-methods)
               self class-env inst-env 
               :around-caller (cons (rest around-methods) 
                                    (demons-of next-methods)) 
               args args)
        (demon-method-combination self class-env inst-env 
                                  :dummy-selector
                                  next-methods
                                  args)))
    ))

(defmacro CALL-NEXT-METHOD (&rest changed-args)
  (if changed-args
    `(call-next-method-fn self class-env inst-env mcs%caller mcs%next-methods 
                          ',changed-args)
    `(call-next-method-fn self class-env inst-env mcs%caller mcs%next-methods mcs%args)
    ))

;;; --------------------------------------------------------------------------
;;; Method lookup functions
;;; --------------------------------------------------------------------------

(declaim (inline GET-METHOD-ENTRY))

(defun GET-METHOD-ENTRY (a_class a_selector)
  (gethash a_selector (mcs-slot-value a_class (index-of-methods))))

(defun STANDARD-METHOD-LOOKUP (class-env a_selector)
  (let ((r-class-precedence-list (reverse (get-class-slot 'cplist)))
        (around-methods nil) (before-methods nil) (primary-methods nil) 
        (after-methods nil))
    (loop
      (if (null r-class-precedence-list) 
        (return 
         (if primary-methods
           (values (if around-methods 
                     'standard-method-combination
                     (if (or before-methods after-methods) 
                       'demon-method-combination
                       'simple-method-combination))
                   (list around-methods before-methods 
                         primary-methods (reverse after-methods)))
           (if (or after-methods before-methods around-methods)
             (error "Method combination error: missing primary method for ~S."
                    a_selector)
             (values nil nil)))))
      (let ((method (get-method-entry (pop r-class-precedence-list) a_selector)))
        (if method
          (let ((own-methods-list (method-entry-methods-list method)))
            (let ((around-method (get-qualified-method :around own-methods-list))
                  (before-method (get-qualified-method :before own-methods-list))
                  (primary-method (get-qualified-method :primary own-methods-list))
                  (after-method (get-qualified-method :after own-methods-list))
                  ) 
              (if before-method 
                (setq before-methods (cons (lambda-expr-of before-method) before-methods)))
              (if after-method
                (setq after-methods (cons (lambda-expr-of after-method) after-methods)))
              (if primary-method
                (setq primary-methods (cons (lambda-expr-of primary-method) primary-methods)))
              (if around-method
                (setq around-methods (cons (lambda-expr-of around-method) around-methods)))
              )))))))


;;; --------------------------------------------------------------------------
;;; Defmethod macro and helps
;;; --------------------------------------------------------------------------

(defun modify-body (body add-parameter-list &optional result)
  (let ((f (first body))
        (r (rest body)))
    (cond ((typep f 'string)
           (modify-body r add-parameter-list (list f)))
          ((and (listp f) (eq (first f) 'declare))
           (modify-body r add-parameter-list (append result (list f))))
          (t (append result 
                     #+(or :MCL :EXCL) 
                     '((declare (ignore-if-unused self class-env inst-env 
                                                  mcs%caller mcs%next-methods mcs%args)))
                     #-(or :MCL :EXCL) 
                     add-parameter-list
                     body)))))

(defun MAKE-LAMBDA-EXPR (qualifier parameter-list body)
  (let ((add-parameter-list
         (if (member qualifier '(:around :primary) :test #'eq)
           `(self class-env inst-env mcs%caller mcs%next-methods mcs%args)
           `(self class-env inst-env))))
    `(lambda (,@add-parameter-list ,@parameter-list)
       ,@(modify-body body add-parameter-list))))


(defun REMOVE-INVALID-COMBINED-METHODS (a_class selector)
  (let ((method-entry (gethash selector
                               (mcs-slot-value a_class (index-of-methods)))))
    (if method-entry (setf (method-entry-combined-method method-entry) nil)))
  (let ((subclasses (mcs-slot-value a_class (index-of-subclasses))))
    (loop
      (if (null subclasses) (return ()))
      (remove-invalid-combined-methods (pop subclasses) selector))))

(defmacro DEFMETHOD ((a_class . qualifier-and-selector) parameter-list 
                     &rest body)
  (let ((qualifier (if (second qualifier-and-selector) 
                     (first qualifier-and-selector) 
                     :primary))
        (selector (if (second qualifier-and-selector) 
                    (second qualifier-and-selector) 
                    (first qualifier-and-selector))))
    `(let ((method-entry
            (gethash ,selector (mcs-slot-value ,a_class (index-of-methods))))
           (new-method-fn 
            (function ,(make-lambda-expr qualifier parameter-list body))))
       (if method-entry
         (add-qualified-method ,qualifier method-entry new-method-fn)
         (setf (gethash ,selector (mcs-slot-value ,a_class (index-of-methods)))
               (make-method-entry :type 'standard
                                  :methods-list
                                  (acons ,qualifier new-method-fn ())
                                  :combined-method nil) ))
       (remove-invalid-combined-methods ,a_class ,selector)
       (format nil "~:[~S~;~S ~S~] of ~S" (second ',qualifier-and-selector)
               ,@qualifier-and-selector ',a_class))))

;;;------------------------------------------------------------------------
;;;     Basic slot access methods   
;;;------------------------------------------------------------------------

(eval-when (compile eval load)
  
  (defun gen-get-slot-method (index)
    (let ((call-next-parms '(mcs%caller mcs%next-methods mcs%args)))
      `(lambda (self class-env inst-env . ,call-next-parms)
	 (declare (ignore self class-env . ,call-next-parms))
	 (svref inst-env ,index))))
  
  (defun gen-set-slot-method (index)
    (let ((call-next-parms '(mcs%caller mcs%next-methods mcs%args)))
      `(lambda (self class-env inst-env ,@call-next-parms value)
	 (declare (ignore self class-env ,@call-next-parms))
	 (setf (svref inst-env ,index) value))))
  )

(defun gen-get-slot-closure (index)
  #'(lambda (self class-env inst-env mcs%caller mcs%next-methods mcs%args)
      (declare (ignore self class-env mcs%caller mcs%next-methods mcs%args))
      (svref inst-env index)))

(defun gen-set-slot-closure (index)
  #'(lambda (self class-env inst-env mcs%caller mcs%next-methods mcs%args value)
      (declare (ignore self class-env mcs%caller mcs%next-methods mcs%args))
      (setf (svref inst-env index) value)))


(defmacro generate-standard-accessors (nr &aux result)
  `(let ((array (make-array ,nr :adjustable t)))
     (declare (vector array))
     ,@(dotimes  (i nr (nreverse result))
         (declare (fixnum i nr))
         (setf result  
               (cons `(setf (aref array ,i) 
                            (cons (function ,(gen-get-slot-method i))
                                  (function ,(gen-set-slot-method i))))
                     result)))
     array))

(defun adjust-standard-accessors (array nr)
  (declare (vector array)
           (fixnum nr))
  (let ((i (length array)))
    (declare (fixnum i))
    (multiple-value-bind (x y) (ceiling nr 16)
      (declare (ignore x))
      (setq nr (- nr y))
      ;  x nil)  ; because x should be ignored
      (adjust-array array nr)
      (loop 
        (if (>= i nr)
          (return array))
        (setf (aref array i) 
              (cons (gen-get-slot-closure i)
                    (gen-set-slot-closure i)))
        (setq i (1+ i))))))

;;; Generate 48 standard slot access methods

(setq STANDARD-ACCESSORS (generate-standard-accessors 64))

;;; ----------------------------------------------------------------
;;;                    -*- USER INTERFACE -*-
;;; ---------------------------------------------------------------- 

(defun PRINT-MCS (object stream depth)
  (declare (ignore depth))
  (let ((class-env (mcs-env (mcs-slot-value object (index-of-isit)))))
    (if (member 'supers (mcs-get-slot class-env (index-of-all-slots)))
      (format stream "#<mcs-class ~S>" 
              (mcs-slot-value object (index-of-name)))
      (format stream "#<mcs-instance of ~S>" 
	      (mcs-get-slot class-env (index-of-name))))))

(defun DESCRIBE-MCS (object &optional (stream t))
  (if (typep object 'mcsobject)
    (let* ((inst-env (mcs-env object))
           (class-env (mcs-env (mcs-get-slot inst-env (index-of-isit))))
           )
      (format stream "~&~S, an object of class ~S,~% has instance variable values:~%" 
              object (mcs-get-slot class-env (index-of-name)))
      (dolist (ivar (mcs-get-slot class-env (index-of-all-slots)))
        (format stream "~%       ~S:~27T~S" ivar (slot-value object ivar))))
    (describe object)))

;;; DEFMETHOD macro already defined

(defmacro DEFCLASS (a_class a_list-of-instance-variables a_list-of-superclasses
                            &key (metaclass 'standard-class))
  `(setq ,a_class
         (funcall (mcs-slot-value ,metaclass (index-of-basicnew-fn))
                  ,metaclass
                  :name ',a_class 
                  :supers (if ',a_list-of-superclasses 
                            (list ,@a_list-of-superclasses)
                            (list standard-object))
                  :own-slots ',a_list-of-instance-variables
                  )))

(defmacro DEFMETACLASS (a_class a_list-of-instance-variables a_list-of-superclasses
                                &key (metaclass 'standard-class))
  `(setq ,a_class
         (funcall (mcs-slot-value ,metaclass (index-of-basicnew-fn))
                  ,metaclass
                  :name ',a_class 
                  :supers (if ',a_list-of-superclasses 
                            (list ,@a_list-of-superclasses)
                            (list standard-class))
                  :own-slots ',a_list-of-instance-variables
                  )))

(defmacro MAKE-INSTANCE (a_class &rest initializations)
  `(let ((class ,(if (and (listp a_class) (eq (first a_class) 'quote))
                   (second a_class)
                   `(eval ,a_class))))
     (funcall (mcs-slot-value class (index-of-basicnew-fn))
              class ,@initializations)))


;;; eof
