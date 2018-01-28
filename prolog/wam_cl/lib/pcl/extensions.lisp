;;;-*-Mode:LISP; Package: PCL; Base:10; Syntax:Common-lisp; -*-

;;;
;;; *************************************************************************
;;;
;;;   File: extensions.lisp.
;;;
;;;     by Trent E. Lange, Effective Date 04-23-92
;;;         Modified 10-02-92 to add per-class-slot-allocation-mixin.
;;;
;;;
;;;  This file contains a small set of useful extensions to PCL. 
;;;
;;; Permission is granted to any individual or institution to use, copy,
;;; modify and distribute this document.
;;;
;;; Suggestions, bugs, criticism and questions to lange@cs.ucla.edu
;;; *************************************************************************
;;;

(in-package 'pcl)

(eval-when (compile load eval)

(defvar *extensions-exports*
        '(set-standard-instance-access
          set-funcallable-instance-access

          funcallable-instance-slot-value
          set-funcallable-instance-slot-value
          funcallable-instance-slot-boundp
          standard-instance-slot-value
          set-standard-instance-slot-value
          standard-instance-slot-boundp
          structure-instance-slot-value
          set-structure-instance-slot-value
          structure-instance-slot-boundp

          #+pcl-user-instances
          user-instance-slot-value
          #+pcl-user-instances
          set-user-instance-slot-value
          #+pcl-user-instances
          user-instance-slot-boundp

          with-optimized-slots
          with-standard-instance-slots

          method-needs-next-methods-p
          map-all-classes
          finalize-all-classes

          updater
          record-updater

          per-class-slot-allocation-mixin
          per-classable-slots-class))
)

(defclass updater ()
  ((dependent :initarg :dependent :reader dependent)))

(defun record-updater (class dependee dependent &rest initargs)
  (let ((updater
         (apply #'make-instance class :dependent dependent initargs)))
    (add-dependent dependee updater)
    updater))


(defun finalize-all-classes (&optional (root-name 't))
  "Makes sure that all classes are finalized.  If Root-Name is supplied,
   then finalizes Root-Name and all of its subclasses and their subclasses."
  (map-all-classes #'(lambda (class)
                       (unless (class-finalized-p class)
                         (finalize-inheritance class)))
                   root-name))


;;;
;;;
;;;


(defmacro slot-value-from-index (instance wrapper slot-name slots index)
  "Returns instance's slot-value given slot-name's index."
  (once-only (index)
    `(if ,index
         (let ((val (%svref ,slots ,index)))
           (if (eq val ',*slot-unbound*)
               (slot-unbound (wrapper-class ,wrapper) ,instance ,slot-name)
             val))
         (if *safe-to-use-slot-value-wrapper-optimizations-p*
             (get-class-slot-value-1 ,instance ,wrapper ,slot-name)
             (accessor-slot-value ,instance ,slot-name)))))

(defmacro set-slot-value-from-index
          (instance wrapper slot-name slots index new-value)
  "Sets instance's slot-value to new-value given slot-name's index."
  (once-only (index)
    `(if ,index
          (setf (%svref ,slots ,index) ,new-value)
          (if *safe-to-use-set-slot-value-wrapper-optimizations-p*
              (set-class-slot-value-1 ,instance ,wrapper ,slot-name ,new-value)
              (setf (accessor-slot-value ,instance ,slot-name) ,new-value)))))

(defsetf slot-value-from-index set-slot-value-from-index)

(defmacro with-slots-slot-value-from-index
          (instance wrapper slot-name slots index variable-instance)
  "Returns instance's slot-value given slot-name's index."
  (cond
   ((consp wrapper)
    `(let ((wrapper ,wrapper))
       (unless (eq (wrapper-state wrapper) 't)
         (setf wrapper (wrapper-state-trap wrapper ,instance)))
       (with-slots-slot-value-from-index
         ,instance wrapper ,slot-name ,slots ,index ,variable-instance)))
   (variable-instance
    `(let ((,instance ,variable-instance))
       (with-slots-slot-value-from-index
         ,instance ,wrapper ,slot-name ,slots ,index NIL)))
   (T `(slot-value-from-index ,instance ,wrapper ,slot-name ,slots ,index))))

(defmacro set-with-slots-slot-value-from-index
          (instance wrapper slot-name slots index variable-instance new-value)
  "Sets instance's slot-value to new-value given slot-name's index."
  (cond
   ((consp wrapper)
    `(let ((wrapper ,wrapper))
       (unless (eq (wrapper-state wrapper) 't)
         (setf wrapper (wrapper-state-trap wrapper ,instance)))
       (set-with-slots-slot-value-from-index
         ,instance wrapper ,slot-name ,slots ,index ,variable-instance
         ,new-value)))
   (variable-instance
    `(let ((,instance ,variable-instance))
       (set-with-slot-slots-value-from-index
         ,instance ,wrapper ,slot-name ,slots ,index NIL ,new-value)))
   (T
    `(setf (slot-value-from-index ,instance ,wrapper ,slot-name ,slots ,index)
           ,new-value))))

(defsetf with-slots-slot-value-from-index
         set-with-slots-slot-value-from-index)

(defmacro with-slots-slot-value-from-wrapper-and-slots
    (instance slot-name wrapper slots-layout slots variable-instance)
  (cond
   (variable-instance
    `(let ((,instance ,variable-instance))
       (with-slots-slot-value-from-wrapper-and-slots
         ,instance ,slot-name ,wrapper ,slots-layout ,slots NIL)))
   ((consp wrapper)
    `(if *safe-to-use-slot-value-wrapper-optimizations-p*
         (let ((wrapper ,wrapper))
           (unless (eq (wrapper-state wrapper) 't)
             (setf wrapper (wrapper-state-trap wrapper ,instance)))
           (slot-value-from-wrapper-and-slots ,instance ,slot-name
             wrapper ,slots-layout ,slots NIL))
         (accessor-slot-value ,instance ,slot-name)))
   (T
    `(if *safe-to-use-slot-value-wrapper-optimizations-p*
         (slot-value-from-wrapper-and-slots
           ,instance ,slot-name ,wrapper ,slots-layout ,slots NIL)
         (accessor-slot-value ,instance ,slot-name)))))

(defmacro set-with-slots-slot-value-from-wrapper-and-slots
    (instance slot-name wrapper slots-layout slots variable-instance new-value)
  (cond
   (variable-instance
    `(let ((,instance ,variable-instance))
       (set-with-slots-slot-value-from-wrapper-and-slots
         ,instance ,slot-name ,wrapper ,slots-layout ,slots NIL ,new-value)))
   ((consp wrapper)
    `(if *safe-to-use-set-slot-value-wrapper-optimizations-p*
         (let ((wrapper ,wrapper))
           (unless (eq (wrapper-state wrapper) 't)
             (setf wrapper (wrapper-state-trap wrapper ,instance)))
           (setf (slot-value-from-wrapper-and-slots ,instance ,slot-name
                    wrapper ,slots-layout ,slots NIL)
                 ,new-value))
         (setf (accessor-slot-value ,instance ,slot-name) ,new-value)))
   (T
    `(if *safe-to-use-set-slot-value-wrapper-optimizations-p*
         (setf (slot-value-from-wrapper-and-slots
                 ,instance ,slot-name ,wrapper ,slots-layout ,slots NIL)
               ,new-value)
         (setf (accessor-slot-value ,instance ,slot-name) ,new-value)))))

(defsetf with-slots-slot-value-from-wrapper-and-slots
         set-with-slots-slot-value-from-wrapper-and-slots)

(defun tree-memq-p (item form)
  (cond ((consp form)
         (or (tree-memq-p item (car form))
             (tree-memq-p item (cdr form))))
        (T (eq item form)))) 

(defmacro with-optimized-slots (slot-entries instance-form &body body)
  "Optimized version of With-Slots that is faster because it factors out
   functions common to all slot accesses on the instance.  It has two
   extensions to With-Slots: (1) the second value of slot-entries are
   evaluated as forms rather than considered to be hard slot-names, allowing
   access of variable slot-names.  (2) if a :variable-instance keyword is
   the first part of the body, then the instance-form is treated as a variable
   form, which is always expected to return an instance of the same class.
   The value of the keyword must be an instance that is the same class as
   instance-form will always return."
  ;;  E.g. (with-optimized-slots (foo-slot
  ;;                         (foo-slot-accessor     'foo-slot)
  ;;                         (variable-slot-accessor variable-slot))
  ;;                        instance
  ;;                        :instance-form (car instances-of-same-class)
  ;;         (loop for instance in objects-of-same-class
  ;;               as  variable-slot in variable-slots
  ;;               collect (list foo-slot
  ;;                             foo-slot-accessor
  ;;                             variable-slot-accessor)))
  ;;   ==> (loop for instance in objects-of-same-class
  ;;             as  variable-slot in variable-slots
  ;;             collect (list (slot-value instance 'foo-slot)
  ;;                           (slot-value instance 'foo-slot)
  ;;                           (slot-value instance variable-slot)))
  (build-with-optimized-slots-form slot-entries instance-form body))

(defmacro with-standard-instance-slots (slot-entries instance-form &body body)
  "Optimized version of With-Slots that assumes that the instance-form
   evaluates to a standard-instance.  The result is undefined if it does not.
   With-standard-instance-slots is faster than With-Slots because it factors
   out functions common to all slot accesses on the instance.  It has two
   extensions to With-Slots: (1) the second value of slot-entries are
   evaluated as forms rather than considered to be hard slot-names, allowing
   access of variable slot-names.  (2) if a :variable-instance keyword is
   the first part of the body, then the instance-form is treated as a variable
   form, which is always expected to return an instance of the same class.
   The value of the keyword must be an instance that is the same class as
   instance-form will always return."
  (build-with-optimized-slots-form slot-entries instance-form body 'std-instance))

(defun build-with-optimized-slots-form (slot-entries instance-form body
                                        &optional instance-type)
  (let* ((variable-instance
           (if (eq (car body) :variable-instance)
               (prog1
                 (cadr body)
                 (setf body (cddr body)))))
         (hard-accessors
           (let ((collect NIL))
             (dolist (slot-entry slot-entries (nreverse collect))
               (when (and (symbolp slot-entry)
                          (tree-memq-p slot-entry body))
                 (push (cons slot-entry slot-entry) collect))
               (when (and (consp slot-entry)
                          (constantp   (second slot-entry))
                          (tree-memq-p (car slot-entry) body))
                 (push (cons (car slot-entry) (second (second slot-entry)))
                       collect)))))
         (variable-accessors
           (let ((collect NIL))
             (dolist (slot-entry slot-entries (nreverse collect))
               (when (and (consp slot-entry)
                          (not (constantp (second slot-entry)))
                          (tree-memq-p (car slot-entry) body))
                 (push slot-entry collect))))))
    (if *safe-to-use-slot-wrapper-optimizations-p*
        (build-maybe-safe-w-o-s-v hard-accessors variable-accessors
                                  instance-form body variable-instance
                                  instance-type)
        (build-with-accessor-s-v  hard-accessors variable-accessors
                                  instance-form body variable-instance))))

(defun build-maybe-safe-w-o-s-v (hard-accessors variable-accessors
                                 instance-form body variable-instance
                                 instance-type)
  (let* ((instance-string
           (if (symbolp instance-form) (symbol-name instance-form) ""))
         (instance-form-var
           (if (and variable-instance (simple-eval-access-p instance-form))
               instance-form
             (gensym
               (concatenate 'simple-string instance-string "-INSTANCE-FORM"))))
         (prototype-form
           (if variable-instance
               (if (simple-eval-access-p variable-instance)
                   variable-instance
                 (gensym (concatenate 'simple-string "VARIABLE-INSTANCE"
                                                     instance-string)))
               instance-form-var))
         (wrapper-var
           (gensym (concatenate 'simple-string instance-string "-WRAPPER")))
         (slots-var
           (unless variable-instance
             (gensym (concatenate 'simple-string instance-string "-SLOTS"))))
         (type-var
           (when (and variable-instance (not instance-type))
             (gensym (concatenate 'simple-string instance-string "-TYPE"))))
         (type-var-std 1)
         (type-var-fsc 2)
         #+pcl-user-instances
         (type-var-user 3)
         (slot-index-vars
           (mapcar #'(lambda (slot-entry)
                         (list (car slot-entry)
                               (cdr slot-entry)
                               (gensym (concatenate
                                         'simple-string
                                         (if (string= instance-string "")
                                             "INSTANCE-FORM-"
                                           instance-string)
                                         (symbol-name (cdr slot-entry))
                                         "-INDEX"))))
                   (remove-duplicates hard-accessors :key #'cdr)))
         (slots-layout-var
           (gensym (concatenate 'simple-string "SLOTS-LAYOUT-" instance-string)))
         (runtime-slots-form
           (if variable-instance
               (ecase instance-type
                 (std-instance  `(std-instance-slots ,instance-form-var))
                 (fsc-instance  `(fsc-instance-slots ,instance-form-var))
                 #+pcl-user-instances
                 (user-instance `(get-user-instance-slots ,instance-form-var))
                 ((nil)
                  `(case ,type-var
                     (,type-var-std  (std-instance-slots ,instance-form-var))
                     (,type-var-fsc  (fsc-instance-slots ,instance-form-var))
                     #+pcl-user-instances
                     (,type-var-user (get-user-instance-slots ,instance-form-var)))))
                slots-var))
         (runtime-wrapper-form
           (if variable-instance
               (ecase instance-type
                 (std-instance  `(std-instance-wrapper ,instance-form-var))
                 (fsc-instance  `(fsc-instance-wrapper ,instance-form-var))
                 #+pcl-user-instances
                 (user-instance `(get-user-instance-wrapper ,instance-form-var))
                 ((nil)
                  `(case ,type-var
                     (,type-var-std  (std-instance-wrapper ,instance-form-var))
                     (,type-var-fsc  (fsc-instance-wrapper ,instance-form-var))
                     #+pcl-user-instances
                     (,type-var-user (get-user-instance-wrapper ,instance-form-var)))))
               wrapper-var)))
    (declare (type simple-string instance-string)
             (type list          slot-index-vars))
    `(let (,@(unless variable-instance
              `((,instance-form-var ,instance-form)))
           ,@(when (and variable-instance
                        (not (eq prototype-form variable-instance)))
               `((,prototype-form ,variable-instance)))
           ,wrapper-var ,slots-layout-var
           ,@(if variable-instance
                 (if type-var `((type-var 0)))
                 (list slots-var))
           ,@(mapcar #'third slot-index-vars))
       ,@(when type-var `((declare (type index ,type-var))))
       (when *safe-to-use-slot-wrapper-optimizations-p*
         ,@(ecase instance-type
             (std-instance
              `((setf ,wrapper-var (std-instance-wrapper ,prototype-form))
                ,@(unless variable-instance
                   `((setf ,slots-var (std-instance-slots ,prototype-form))))))
             (fsc-instance
              `((setf ,wrapper-var (fsc-instance-wrapper ,prototype-form))
                ,@(unless variable-instance
                   `((setf ,slots-var (fsc-instance-slots ,prototype-form))))))
             #+pcl-user-instances
             (user-instance
              `((setf ,wrapper-var (get-user-instance-wrapper ,prototype-form))
                ,@(unless variable-instance
                   `((setf ,slots-var (get-user-instance-slots ,prototype-form))))))
             ((nil)
             `((cond
                ((std-instance-p ,prototype-form)
                 (setf ,wrapper-var (std-instance-wrapper ,prototype-form))
                 ,(if variable-instance
                      `(setf ,type-var ,type-var-std)
                    `(setf ,slots-var (std-instance-slots ,prototype-form))))
                ((fsc-instance-p ,prototype-form)
                 (setf ,wrapper-var (fsc-instance-wrapper ,prototype-form))
                 ,(if variable-instance
                      `(setf ,type-var ,type-var-fsc)
                    `(setf ,slots-var (fsc-instance-slots ,prototype-form))))
                #+pcl-user-instances
                ((get-user-instance-p ,prototype-form)
                 (setf ,wrapper-var (get-user-instance-wrapper ,prototype-form))
                 ,(if variable-instance
                      `(setf ,type-var ,type-var-user)
                    `(setf ,slots-var (get-user-instance-slots ,prototype-form))))))))
         ,@(if instance-type
               (build-w-s-v-find-slot-indices wrapper-var slots-layout-var
                  prototype-form slot-index-vars)
               `((when ,wrapper-var
                  ,@(build-w-s-v-find-slot-indices wrapper-var slots-layout-var
                       prototype-form slot-index-vars)))))
       (symbol-macrolet
         (,@(mapcar
              #'(lambda (slot-cons)
                  `(,(car slot-cons)
                     (with-slots-slot-value-from-index
                        ,instance-form-var
                        ,runtime-wrapper-form
                        ',(cdr slot-cons)
                        ,runtime-slots-form
                        ,(third (assoc (car slot-cons) slot-index-vars
                                       :test #'eq))
                        ,(when (and variable-instance
                                    (not (eq variable-instance
                                             instance-form-var)))
                           variable-instance))))
              hard-accessors)
          ,@(mapcar
              #'(lambda (variable-cons)
                  `(,(car variable-cons)
                    (with-slots-slot-value-from-wrapper-and-slots
                      ,instance-form-var
                      ,(second variable-cons)
                      ,runtime-wrapper-form
                      ,slots-layout-var
                      ,runtime-slots-form
                      ,(when (and variable-instance
                                  (not (eq variable-instance
                                           instance-form-var)))
                         variable-instance))))
              variable-accessors))
         ,@body))))

(defun build-w-s-v-find-slot-indices (wrapper-var slots-layout-var
                                      prototype-form
                                      slot-index-vars)
  (declare (type list slot-index-vars))
  `((unless (eq (wrapper-state ,wrapper-var) 't)
      (setf ,wrapper-var
            (wrapper-state-trap ,wrapper-var ,prototype-form)))
    (setf ,slots-layout-var (wrapper-instance-slots-layout ,wrapper-var))
    ,@(if (<= (length slot-index-vars) 2)
          (mapcar
            #'(lambda (slot-cons)
                `(setf ,(third slot-cons)
                       (instance-slot-index-from-slots-layout
                         ,slots-layout-var ',(second slot-cons))))
            slot-index-vars)
          ;; More than two slots, so more efficient to search slots-layout-var
          ;; only once, rather than once for each with instance-slot-index.
          (labels
            ((build-comps (slot-vars index)
               (if slot-vars
                   `(if (eq slot-name ',(second (car slot-vars)))
                        (progn
                          (setf ,(third (car slot-vars)) ,index)
                          (if (= matches ,(1- (length slot-index-vars)))
                              (go end-loop)
                            (setf matches (the fixnum (1+ matches)))))
                     ,(build-comps (cdr slot-vars) index)))))
            `((block nil
                (let ((slots-left ,slots-layout-var)
                      (slot-name  NIL)
                      (index      0)
                      (matches    0))
                  (declare (type fixnum index matches))
                  (when slots-left
                    (tagbody
                      begin-instance-slots-loop
                        (setf slot-name (car slots-left))
                        ,(build-comps slot-index-vars 'index)
                        (setf index (the fixnum (1+ index)))
                        (if (null (setf slots-left (cdr slots-left)))
                            (go end-loop))
                        (go begin-instance-slots-loop)
                      end-loop)))))))))

(defun build-with-accessor-s-v (hard-accessors variable-accessors
                                instance-form body variable-instance)
  ;; Build the body for with-optimized-slot-value when it is unsafe
  ;; and accessor-slot-value must be used.
  (let ((instance-form-var
          (if variable-instance instance-form (gensym "INSTANCE-FORM"))))
  `(let (,@(unless variable-instance
            `((,instance-form-var ,instance-form))))
     (symbol-macrolet
       (,@(mapcar
            #'(lambda (slot-cons)
                `(,(car slot-cons)
                  (accessor-slot-value ,instance-form-var
                                       ',(cdr slot-cons))))
            hard-accessors)
        ,@(mapcar
            #'(lambda (variable-cons)
                `(,(car variable-cons)
                  (accessor-slot-value ,instance-form-var
                                       ,(second variable-cons))))
            variable-accessors))
       ,@body))))


;;;
;;; PER-CLASS-SLOT-ALLOCATION-MIXIN is a mixin for metaclasses that
;;;   allows its classes' instances to store slots with :allocation
;;;   :per-class.  :per-class allocated slots work like :class allocated
;;;   slots, but are specific to each individual subclass.  I.e. any
;;;   class that inherits a class with :per-class allocated slots
;;;   also inherits the slot (as with :class allocated slots), but
;;;   unlike :class allocated slots, the value stored in the slot
;;;   can be different for each subclass.
;;; PER-CLASS-EFFECTIVE-SLOT-DEFINITION is the
;;;   effective-slot-definition-class for slots that have :allocation
;;;   :per-class.
;;; PER-CLASSABLE-SLOTS-CLASS is a metaclass that includes
;;;   PER-CLASS-SLOT-ALLOCATION-MIXIN and STANDARD-CLASS.
;;;
;;; Note that there are two different "implementations" to allow
;;;   access of :per-class slots:  One is specific to July 92 PCL
;;;   and its progeny (#+july-92-pcl), that operates by tagging along
;;;   with July 92's internal implementation of :class allocated slots.
;;;   It is extremely efficient because it fools July 92's slot access
;;;   operations and optimizations into thinking :per-class slots are
;;;   like :class allocation slots.  The other implementation is the
;;;   portable implementation (#-july-92-pcl), that works by defining
;;;   the proper slot-value-using-class methods, etc.
;;;
;;;  Example usage:
;;;
;;; 
;;; (defclass a-class ()
;;;   ((class-slot1     :initform nil :allocation :class)
;;;    (per-class-slot1 :initform nil :allocation :per-class))
;;;   (:metaclass per-classable-slots-class))
;;; 
;;; (defclass b-class (a-class)
;;;   ()
;;;   (:metaclass per-classable-slots-class))
;;; 
;;; > (defvar a (make-instance 'a-class))
;;; A
;;; > (defvar b (make-instance 'b-class))
;;; B
;;; > (setf (slot-value a 'class-slot1) 'x)
;;; X
;;; > (setf (slot-value a 'per-class-slot1) 'y)
;;; Y
;;; > (describe a)
;;; 
;;; #<A-CLASS 131230736> is an instance of class #<Per-Classable-Slots-Class
;;; A-CLASS 136143546>:
;;;  The following slots have allocation as shown:
;;;  PER-CLASS-SLOT1 :PER-CLASS    Y
;;;  CLASS-SLOT1 #<Per-Classable-Slots-Class A-CLASS 136143546>    X
;;; > (describe b)
;;; 
;;; #<B-CLASS 131254216> is an instance of class #<Per-Classable-Slots-Class
;;; B-CLASS 136374666>:
;;;  The following slots have allocation as shown:
;;;  PER-CLASS-SLOT1 :PER-CLASS    NIL
;;;  CLASS-SLOT1 #<Per-Classable-Slots-Class A-CLASS 136143546>    X
;;; > (setf (slot-value b 'class-slot1) 'x2)
;;; X2
;;; > (setf (slot-value b 'per-class-slot1) 'y2)
;;; Y2
;;; > (describe a)
;;; 
;;; #<A-CLASS 131230736> is an instance of class #<Per-Classable-Slots-Class
;;; A-CLASS 136143546>:
;;;  The following slots have allocation as shown:
;;;  PER-CLASS-SLOT1 :PER-CLASS    Y
;;;  CLASS-SLOT1 #<Per-Classable-Slots-Class A-CLASS 136143546>    X2
;;; > (describe b)
;;; 
;;; #<B-CLASS 131254216> is an instance of class #<Per-Classable-Slots-Class
;;; B-CLASS 136374666>:
;;;  The following slots have allocation as shown:
;;;  PER-CLASS-SLOT1 :PER-CLASS    Y2
;;;  CLASS-SLOT1 #<Per-Classable-Slots-Class A-CLASS 136143546>    X2
;;; 

(eval-when (compile load eval)

(defclass per-class-slot-allocation-mixin
  ()
  ((per-class-slot-cells
     :initform      NIL
     :reader        class-per-class-slot-cells
     :documentation
      "Cells storing the slots with allocation :per-class."))
   (:documentation
     "Mixin for a metaclass to allow its classes' instances to store
      slots with :ALLOCATION :PER-CLASS.  :PER-CLASS allocated slots work
      like :CLASS allocated slots, but are specific to each individual
      subclass."))

(defclass per-class-effective-slot-definition
  (standard-effective-slot-definition)
  ((per-class-cell
     :initform NIL
     :type     (or cons null)
     :accessor slot-definition-per-class-cell
     :documentation
      "Cons cell storing the slot value when allocation = :per-class"))
  (:documentation
    "Effective-slot-definition-class for per-class-slot-allocation-mixin
     classes when :allocation = :PER-CLASS."))

(defclass per-classable-slots-class
  (per-class-slot-allocation-mixin
   standard-class)
  ()
  (:documentation
    "Example metaclass that's exactly like standard-class, but allows
     its slots to have :allocation :per-class."))

(defmethod validate-superclass ((class per-class-slot-allocation-mixin)
                                (new-super T))
  (or (typep new-super 'per-class-slot-allocation-mixin)
      (eq new-super (find-class 'standard-object))))

) ; eval-when


(defmethod effective-slot-definition-class
  ((class per-class-slot-allocation-mixin) initargs)
  "If the slot has :allocation :per-class, then class is
   per-class-effective-slot-definition, otherwise what it normally
   would be."
  (if (eq (cadr (memq :allocation initargs)) :per-class)
      (find-class 'per-class-effective-slot-definition)
      (call-next-method)))

(defconstant *per-class-slot-unbound* '..per-class-slot-unbound)

(defmethod compute-effective-slot-definition :around
  ((class per-class-slot-allocation-mixin)
   name
   direct-slot-definitions)
  (declare (ignore direct-slot-definitions))
  (let ((effective-slot-definition (call-next-method)))
    (when (eq (slot-definition-allocation effective-slot-definition)
              :per-class)
      (let ((per-class-cell (cons name *per-class-slot-unbound*)))
        (setf (slot-value effective-slot-definition 'per-class-cell)
              per-class-cell)
        #+july-92-pcl
        (setf (slot-definition-location effective-slot-definition)
              per-class-cell)))
    effective-slot-definition))

(defmethod compute-slots :around ((class per-class-slot-allocation-mixin))
  "Update class-per-class-slot-cells of class."
  (let ((effective-slot-definitions (call-next-method))
        (per-class-cells NIL))
    (dolist (eslotd effective-slot-definitions)
      (when (eq (slot-definition-allocation eslotd) :per-class)
        (push (slot-definition-per-class-cell eslotd) per-class-cells)))
    (setf (slot-value class 'per-class-slot-cells) per-class-cells)
    effective-slot-definitions))


#+july-92-pcl
(progn

(defmethod compute-slots :around ((class pcl::std-class))
  (let ((eslotds (call-next-method))
        (cpl (slot-value class 'pcl::class-precedence-list))
        (instance-slots  ())
        (class-slots     ())
        (per-class-slots ())
        (other-slots     ()))
    (dolist (eslotd eslotds)
      (let ((alloc (slot-definition-allocation eslotd)))
        (cond ((eq alloc :instance)  (push eslotd instance-slots))
              ((pcl::classp alloc)   (push eslotd class-slots))
              ((eq alloc :per-class) (push eslotd per-class-slots))
              (T                     (push eslotd other-slots)))))
    (let ((nlayout (pcl::compute-layout class cpl instance-slots)))
      (declare (type list nlayout))
      (dolist (eslotd instance-slots)
        (setf (slot-definition-location eslotd)
              (position (slot-definition-name eslotd) nlayout :test #'eq))))
    (dolist (eslotd class-slots)
      (setf (slot-definition-location eslotd)
            (assoc (slot-definition-name eslotd)
                   (pcl::class-slot-cells (slot-definition-allocation eslotd))
                   :test #'eq)))
    (dolist (eslotd per-class-slots)
      (setf (slot-definition-location eslotd)
            (slot-definition-per-class-cell eslotd)))
    (dolist (eslotd other-slots)
      (pcl::initialize-internal-slot-functions eslotd))
    eslotds))

(defmethod pcl::compute-storage-info ((class per-class-slot-allocation-mixin)
                                      eslotds)
  (let ((instance-slots ())
        (class-slots    ())
        (per-class-slot-cells ()))
    (dolist (eslotd eslotds)
      (let ((alloc (slot-definition-allocation eslotd)))
        (cond ((eq alloc :instance) (push eslotd instance-slots))
              ((pcl::classp alloc)  (push eslotd class-slots))
              ((eq alloc :per-class)
               (push (slot-definition-per-class-cell eslotd)
                     per-class-slot-cells)))))
    (values (pcl::compute-instance-layout class instance-slots)
            (append per-class-slot-cells
                    (pcl::compute-class-slots class class-slots)))))

) ;#+july-92-pcl

#-july-92-pcl
(progn

(defmethod slot-value-using-class
  ((class  per-class-slot-allocation-mixin)
   (object standard-object)
   (slotd  per-class-effective-slot-definition))
  (let ((per-class-cell (slot-value slotd 'per-class-cell)))
    (if (consp per-class-cell)
        (cdr per-class-cell)
        (call-next-method))))

(defmethod (setf slot-value-using-class)
  (new-value (class  per-class-slot-allocation-mixin)
             (object standard-object)
             (slotd  per-class-effective-slot-definition))
  (let ((per-class-cell (slot-value slotd 'per-class-cell)))
    (if (consp per-class-cell)
        (setf (cdr per-class-cell) new-value)
        (call-next-method))))

(defmethod slot-boundp-using-class
  ((class  per-class-slot-allocation-mixin)
   (object standard-object)
   (slotd  per-class-effective-slot-definition))
  (let ((per-class-cell (slot-value slotd 'per-class-cell)))
    (if (consp per-class-cell)
        (not (eq (cdr per-class-cell) *per-class-slot-unbound*))
        (call-next-method))))

) ;#-july-92-pcl


#-(or KCL IBCL)
(export *extensions-exports* *the-pcl-package*)

#+(or KCL IBCL)
(mapc 'export (list *extensions-exports*) (list *the-pcl-package*))

