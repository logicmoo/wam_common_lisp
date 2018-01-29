;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package 'clos)

;;; ----------------------------------------------------------------------
;;; DEFCLASS

(defmacro DEFCLASS (&rest args)
  (multiple-value-bind (name superclasses slots 
			     metaclass-name default-initargs documentation)
    (parse-defclass args)
    (unless metaclass-name
      (setq metaclass-name 'standard-class))	; the default for CLOS

    ;; process superclasses
    (dolist (superclass superclasses)
      (unless (legal-class-name-p superclass)
	(error "~A is not a legal superclass name" superclass)))

#|
;; managed by methods
    ;; default inheritance for CLOS classes that are instances
    ;; of standard-class is standard-object, else t.
    (if (eq metaclass-name 'standard-class)
	(unless (some #'(lambda (x) (subtypep x 'standard-object))
		      superclasses)
	  (push 'standard-object superclasses))
      (unless superclasses
	(push 't superclasses)))
|#

    ;; process slots
    (let* ((slots (parse-class-slots slots))
	   (all-slots (collect-all-slots (find-class metaclass-name)
					slots name superclasses))
	   (instance-slots) (local-slots) (class-slots))
      ;; update them to reflect collected information
      (update-local-slots slots all-slots)
      (dolist (slot slots)
	      (case (slotd-allocation slot)
		     (:instance (push slot local-slots))))
      (setq local-slots (nreverse local-slots))
      (dolist (slot all-slots)
	      (ecase (slotd-allocation slot)
		(:instance (push slot instance-slots))
		(:class (push slot class-slots))))
      (setq instance-slots (nreverse instance-slots))
      (setq class-slots (nreverse class-slots))
      `(eval-when (compile load eval)
	;; at compile time just create the definition
	(define-a-class (find-class ',metaclass-name)
	    ',name
	  ',superclasses
	  ',local-slots
	  ',class-slots
	  ',instance-slots
	  ',default-initargs
	  ',documentation)
	#+PDE
	(si:record-source-pathname ',name 'defclass)
	,@ (generate-methods
	    name
	    :metaclass-name metaclass-name
	    :superiors (mapcar #'find-class superclasses)
	    :slots instance-slots
	    :class-slots class-slots
	    :slots-of-the-class
	    (collect-slotds (list (find-class metaclass-name))
	     class-slots))
	(find-class ',name)))))
;;; ----------------------------------------------------------------------
;;;                                                                parsing

(defun parse-defclass (args)
  (let (name superclasses slots options
	     metaclass-name default-initargs documentation)
    (unless args
      (error "Illegal defclass form: the class name, the superclasses and the slots should always be provided"))
    (setq name (pop args))
    (unless args
      (error "Illegal defclass form: the class name, the superclasses list and the slot specifier list should always be provided"))
    (unless (listp (first args))
      (error "Illegal defclass form: the superclasses should be a list"))
    (setq superclasses (pop args))
    (unless args
      (error "Illegal defclass form: the class name, the superclasses list and the slot specifier list should always be provided"))
    (unless (listp (first args))
      (error "Illegal defclass form: the slots should be a list"))
    (setq slots (pop args))
    (setq options args)
    (unless (legal-class-name-p name)
      (error "Illegal defclass form: the class name should be a symbol"))
    ;; process options
    (dolist (option options)
      (case (first option)
	(:metaclass
	 (if metaclass-name
	     (error "Option :metaclass specified more than once for class ~A" 
		    name)
	   ;; else
	   (setq metaclass-name (second option))))
	(:default-initargs
	  (if default-initargs
	      (error "Option :default-initargs specified more than once for class ~A" name)
	      (setq default-initargs (cdr option))))
	(:documentation
	  (if documentation
	      (error "Option :documentation specified more than once for class ~A"
		     name)
	      (setq documentation (second option))))  
	(otherwise (error "~S is not a legal class-option." (first option)))))
    (values name superclasses slots 
	    metaclass-name default-initargs documentation)))


;;; ----------------------------------------------------------------------
;;;                                                               methods

(defmethod collect-all-slots ((metaclass class) slots name superclasses-names)
  (collect-slotds (mapcar #'find-class superclasses-names) slots))

(defmethod define-a-class ((metaclass class) 
			   name superclasses-names
			   local-slots class-slots all-slots
			   default-initargs documentation)
  (when class-slots
      (error "The non-standard class ~S can't have class-slots" name))
  (let ((superclasses (mapcar #'find-class superclasses-names)))
    (make-instance metaclass
		   :name name
		   :direct-superclasses superclasses
		   :slots all-slots)))


;;; Initialization is split into two parts, so that methods are
;;; generated in a way that the compiler can see them.
;;; This cant be a method because of bootstrap problem.

(defun generate-methods (class-name &rest init-options)
  (keyword-bind (metaclass-name superiors inferiors slots slots-of-the-class
				class-slots methods) init-options 
    (declare (ignore superiors inferiors methods slots-of-the-class))
    (cond
     ;; since slot-value is inherited from 'standard-object
     ((subtypep metaclass-name 'standard-class)
      (generate-optional-slot-accessors
       class-name
       slots class-slots t))
     ;; don't generate accessor for structures:
     ((subtypep metaclass-name 'structure-class) nil)
     (t
      `(,@ (generate-slot-accessors
	    class-name
	    slots
	    class-slots)
;	   ,@ (when class-slots
;		(generate-class-slot-accessors
;		 class-name slots-of-the-class class-slots))
	)))))

(defun generate-slot-accessors (name slotds class-slotds)
  (when (plusp (length slotds))
    (append
    (if  (< (+ (length slotds)
	       (length class-slotds)) 16) ; linear search is faster
	`((install-method
	    'slot-value nil '(,name nil) '(self slot) nil nil
	    #'(lambda (self slot)
		(declare (type ,name self))
		(case slot
		      ,@(do ((scan slotds (cdr scan))
			     (index 0 (1+ index))
			     (clauses))
			    ((null scan)
			     (nreverse
			       (cons
				 `(t (slot-missing (si:instance-class self)
						   self slot 'slot-value))
;				 (append (class-slot-clauses class-slotds)
					 clauses
;					 )
			       )))
			    (push `(,(slotd-name (first scan))
				    (let ((val (si:instance-ref self ,index)))
				      ,@(let ((type (slotd-type (first scan))))
					  (unless (eq 't type)
					    `((DECLARE (TYPE ,type val)))))
				      (if (si:sl-boundp val)
					  val
					  'unbound)))
				  clauses)))))

	  (install-method
	    '(setf slot-value) nil '(nil ,name nil) '(value self slot) nil nil
	    #'(lambda (value self slot)
		(declare (type ,name self))
		(case slot
		      ,@(do ((scan slotds (cdr scan))
			     (index 0 (1+ index))
			     (clauses))
			    ((null scan)
			     (nreverse
			       (cons `(t (slot-missing (si:instance-class self)
					  self slot value 'setf))
;				     (append (class-slot-setf-clauses
;					       class-slotds)
					     clauses
;					     )
			       )))
			    (push `(,(slotd-name (first scan))
				    (setf (si:instance-ref self ,index) value))
				  clauses))))))

      `((let* ((size (floor (length ',slotds) 1.5))
	       (table (make-hash-table :size (if (zerop size) 1 size))))
	  (do ((scan ',slotds (cdr scan))
	       (index 0 (1+ index)))
	      ((null scan))
	    (setf (gethash (slotd-name (first scan)) table) index))

	  (install-method 'slot-value nil '(,name nil) '(self slot) nil nil
			  #'(lambda (self slot) 
			      (declare (type ,name self))
			      (let ((index
				     (the fixnum
					  (gethash slot table 
						   most-positive-fixnum))))
				(declare (fixnum index)) 
				(if (not (= index most-positive-fixnum))
				    (let ((val (si:instance-ref self index)))
				      (if (si:sl-boundp val)
					  val
					  'unbound))
				    (slot-missing
				     (si:instance-class self) self slot 'slot-value)))))

	  (install-method '(setf slot-value) nil '(nil ,name nil)
			  '(value self slot) nil nil
			  #'(lambda (value self slot)
			      (declare (type ,name self))
			      (let ((index
				     (the fixnum
					  (gethash slot table 
						   most-positive-fixnum))))
				(declare (fixnum index))
				(if (not (= index most-positive-fixnum))
				    (si:instance-set self index value)
				    (slot-missing 
				     (si:instance-class self) self slot value 'setf)))))
	  )))

    (generate-optional-slot-accessors name slotds class-slotds))))

(defun generate-optional-slot-accessors (name slotds class-slotds
					      &optional optimized)
  (nconc
   ;; instance slots accessor methods
   (do ((scan slotds (cdr scan))
	(i 0 (1+ i))
	(slotd)
	(methods))
       ((null scan) methods)
       (declare (fixnum i))
       (setq slotd (first scan))
       (dolist (accessor (slotd-accessors slotd))
	 ;; accessors are implemented using slot-value
	 (push
	  `(install-method '(setf ,accessor) nil '(nil ,name) '(v self) nil nil
	    ,(if optimized
		 `#'(lambda (v self)
		      (declare (type ,name self))
		      (si:instance-set self ,i v))
	       `#'(lambda (v self) (setf
				    (slot-value self ',(slotd-name slotd))
				    v))))
	  methods)
	 (push
	  `(install-method ',accessor nil '(,name) '(self) nil nil
	    ,(if optimized
		 `#'(lambda (self)
		      (declare (type ,name self))
		      (let ((val (si:instance-ref self ,i)))
			,@(let ((type (slotd-type slotd)))
			    (unless (eq 't type)
			      `((DECLARE (TYPE ,type VAL)))))
			    (if (si:sl-boundp val) val
				;; else
				(slot-unbound (si:instance-class self) self
					      ',(slotd-name slotd)))))
		 `#'(lambda (self)
		      (declare (type ,name self))
		      (slot-value self ',(slotd-name slotd)))))
	  methods))
       (dolist (reader (slotd-readers slotd))
	 (push
	  `(install-method ',reader nil '(,name) '(self) nil nil
	    ,(if optimized
		 `#'(lambda (self)
		      (declare (type ,name self))
		      (let ((val (si:instance-ref self ,i)))
			,@(let ((type (slotd-type slotd)))
			    (unless (eq 't type)
			      `((DECLARE (TYPE ,type VAL)))))
			(if (si:sl-boundp val) val
			    ;; else
			    (slot-unbound (si:instance-class self) self
					  ',(slotd-name slotd)))))
		 `#'(lambda (self)
		      (declare (type ,name self))
		      (slot-value self ',(slotd-name slotd)))))
	  methods))
       (dolist (writer (slotd-writers slotd))
	 (push
	  `(install-method ',writer nil '(nil ,name) '(v self) nil nil
	    ,(if optimized
		 `#'(lambda (v self)
		      (declare (type ,name self))
		      (si:instance-set self ,i v))
	       `#'(lambda (v self)
		    (declare (type ,name self))
		    (setf (slot-value self ',(slotd-name slotd)) v))))
	  methods))
       )

   ;; class slots accessor methods
   (do ((scan class-slotds (cdr scan))
	(slotd)
	(methods))
       ((null scan) methods)
       (setq slotd (first scan))
       (dolist (accessor (slotd-accessors slotd))
	 ;; accessors are implemented using slot-value
	 (push
	  `(install-method '(setf ,accessor) nil '(nil ,name) 
	    '(v self) nil nil
	    #'(lambda (v self) 
		(declare (type ,name self))
		(setf (slot-value self ',(slotd-name slotd)) v)))
	  methods)
	 (push
	  `(install-method ',accessor nil '(,name) '(self) nil nil
	    #'(lambda (self)
		(declare (type ,name self))
		(slot-value self ',(slotd-name slotd))))
	  methods))
       (dolist (reader (slotd-readers slotd))
	 ;; readers are implemented using slot-value
	 (push
	  `(install-method ',reader nil '(,name) '(self) nil nil
	    #'(lambda (self)
		(declare (type ,name self))
		(slot-value self ',(slotd-name slotd))))
	  methods))
       (dolist (writer (slotd-writers slotd))
	 (push
	  `(install-method ',writer nil '(nil ,name) '(v self) nil nil
	    #'(lambda (v self)
		(declare (type ,name self))
		(setf (slot-value self ',(slotd-name slotd)) v)))
	  methods)))))

#|
(defun class-slot-clauses (class-slotds)
  (do ((scan class-slotds (cdr scan))
       (class-clauses))
      ((null scan)
	class-clauses)
      (let ((slotdname (slotd-name (first scan))))
	(push `(,slotdname (slot-value (si:instance-class self) ',slotdname))
	      class-clauses))))

(defun class-slot-setf-clauses (class-slotds)
  (do ((scan class-slotds (cdr scan))
       (class-clauses))
      ((null scan)
       class-clauses)
      (let ((slotdname (slotd-name (first scan))))
	(push `(,slotdname (setf (slot-value (si:instance-class self) ',slotdname)
				 value))
	      class-clauses))))


(defun metaclass-name-of (class-name)
  (intern (concatenate 'string "META-" (symbol-name class-name))))

(defun generate-class-slot-accessors (name slotds-of-the-class class-slotds)
  (append
   (if (< (length class-slotds) 16)	; linear search is faster
       `(
	 (install-method 'SLOT-VALUE NIL '(,(metaclass-name-of name) NIL) 
	  '(SELF SLOT) NIL NIL
	  #'(lambda (self slot)
	      (declare (type ,name self))
	      (case slot
		,@(do ((scan slotds-of-the-class (cdr scan)) 
		       (index 0 (1+ index))
		       (clauses))
		      ((null scan)
		       (nreverse
			(cons `(T (SLOT-MISSING (SI:INSTANCE-CLASS SELF)
						SELF SLOT 'SLOT-VALUE))
			      clauses)))
		      (push `(,(slotd-name (first scan))
			      (LET ((VAL (SI:INSTANCE-REF SELF ,index)))
				,@(let ((type (slotd-type (first scan))))
				    (unless (eq 't type)
				      `((DECLARE (TYPE ,type VAL)))))
				(IF (SI:SL-BOUNDP VAL)
				    VAL
				    (SLOT-UNBOUND
				     (SI:INSTANCE-CLASS SELF) SELF
				     ',(slotd-name (first scan)))))) 
			    clauses)))))

	 (install-method '(SETF SLOT-VALUE) NIL '(NIL ,(metaclass-name-of name) NIL)
	  '(VALUE SELF SLOT) NIL NIL
	  #'(lambda (value self slot)
	      (declare (type ,name self))
	      (case slot
		,@(do ((scan slotds-of-the-class (cdr scan)) 
		       (index 0 (1+ index))
		       (clauses))
		      ((null scan)
		       (nreverse
			(cons `(t (slot-missing (si:instance-class self)
						self slot value 'setf))
			      clauses)))
		      (push `(,(slotd-name (first scan))
			      (setf (si:instance-ref self ,index) value))
			    clauses))))))

     `((let* ((size (floor (length ',(append slotds class-slotds)) 1.5))
	      (table (make-hash-table :size (if (zerop size) 1 size))))
	 (do ((scan ',slotds-of-the-class (cdr scan)) 
	      (index 0 (1+ index))) 
	     ((null scan))
	     (setf (gethash (slotd-name (first scan)) table) index))

	 (install-method 'SLOT-VALUE NIL '(,(metaclass-name-of name) nil)
	  '(SELF SLOT) NIL NIL
	  #'(lambda (self slot) 
	      (declare (type ,name self))
	      (let ((index
		     (the fixnum (gethash slot table most-positive-fixnum))))
		(declare (fixnum index))
		(if (not (= index most-positive-fixnum))
		    (let ((val (si:instance-ref self index)))
		      ,@(let ((type (slotd-type (first scan))))
			  (unless (eq 't type)
			    `((DECLARE (TYPE ,type VAL)))))
		      (if (si:sl-boundp val)
			  val
			  (slot-unbound (si:instance-class self) self slot)))
		  (slot-missing (si:instance-class self)
				self slot 'slot-value)))))

	 (install-method '(SETF SLOT-VALUE) NIL
			 '(NIL ,(metaclass-name-of name) NIL)
			 '(VALUE SELF SLOT) NIL NIL
	  #'(lambda (value self slot)
	      (declare (type ,name self))
	      (let ((index
		     (the fixnum (gethash slot table most-positive-fixnum))))
		(declare (fixnum index))
		(if (not (= index most-positive-fixnum)) 
		    (setf (si:instance-ref self index) value)
		  (slot-missing (si:instance-class self)
				self slot value 'setf))))))))

   (do ((scan slotds-of-the-class (cdr scan)) 
	(i 0 (1+ i))
	(slotd)
	(methods))
       ((null scan) methods)
       (declare (fixnum i))
       (setq slotd (first scan))
       (dolist (accessor (slotd-accessors slotd))
	 ;; accessors are implemented using slot-value
	 (push
	  `(INSTALL-METHOD '(SETF ,accessor) nil
			   '(NIL ,(metaclass-name-of name))
	    '(V SELF) NIL NIL
	    #'(lambda (self v)
		(declare (type ,name self))
		(setf (slot-value self ',(slotd-name slotd))
				     v)))
	  methods)
	 (push
	  `(INSTALL-METHOD ',accessor NIL '(,(metaclass-name-of name))
	    '(SELF) NIL NIL
	    #'(lambda (self)
		(declare (type ,name self))
		(slot-value self ',(slotd-name slotd))))
	  methods))
       (dolist (reader (slotd-readers slotd))
	 ;; readers are implemented using slot-value
	 (push
	  `(INSTALL-METHOD ',reader NIL '(,(metaclass-name-of name))
	    '(SELF) NIL NIL
	    #'(lambda (self)
		(declare (type ,name self))
		(slot-value self ',(slotd-name slotd))))
	  methods))
       )))
|#

;;; ----------------------------------------------------------------------
;;; SLOTS

(defun collect-slotds (classes slots)
  (let ((collected-slots)
	(all-class-slots)
	(new-slot))
    (dolist (sc classes)
	    (setq all-class-slots 
		  (if (typep sc 'standard-class)
                ;; the class can have class-slots
                (append (class-slots sc) (class-class-slots sc))
	     (class-slots sc)))
	    (dolist (slot all-class-slots)
		    (if (setq new-slot
			      (find (slotd-name slot) collected-slots
				    :key #'slotd-name))
			(combine-slotds new-slot slot) ; updates the slot
		      (if (setq new-slot
				(find (slotd-name slot) slots
				      :key #'slotd-name))
			  (progn
			    (setq slots (delete new-slot slots))
			    (combine-slotds new-slot slot) ; updates the slot
			    (push new-slot collected-slots))
			(push (copy-slotd slot) collected-slots)))))
    (nconc (nreverse collected-slots) slots)))

(defun update-local-slots (local-slots all-slots)
  (do ((scan local-slots (cdr scan)))
      ((null scan) local-slots)
      (setf (car scan)
	    (find (slotd-name (car scan)) all-slots :key #'slotd-name))))

(defun combine-slotds (new-slotd old-slotd)
  (let ((new-type (slotd-type new-slotd))
	(old-type (slotd-type old-slotd)))
    (setf (slotd-initargs new-slotd)
	  (union (slotd-initargs new-slotd) (slotd-initargs old-slotd)))
    (when (eq (slotd-initform new-slotd) *initform-unsupplied*)
	    (setf (slotd-initform new-slotd)  (slotd-initform old-slotd)))
    (setf (slotd-type new-slotd)
	  ;; since (subtypep '(and t1 t2) `(and t2 t1)) returns nil
	  ;; we should be more smart then this:
	  (if (subtypep new-type old-type)
	      new-type
	    (if (subtypep old-type new-type)
		old-type
	      (if (listp old-type)
		  (if (listp new-type)
		      `(and ,@(cdr new-type) ,@(cdr old-type)) ; take out the ands
		    `(and ,new-type ,@(cdr old-type)))
		(if (listp new-type)
		    `(and ,@(cdr new-type) ,old-type)
		  `(and ,new-type ,old-type))))))
    new-slotd))

;;; ----------------------------------------------------------------------
;;; support for standard-class

(defun collect-default-initargs (classes initargs)
  (dolist (sc classes)
    (setq initargs (default-initargs sc initargs)))
  initargs)

(defun build-slot-index-table (slotds)
  (let ((table (make-hash-table :size (max 32 (* 2 (length slotds)))
				:test #'eq))
	(i 0))
    (declare (fixnum i))
    (do* ((slotdscan slotds (cdr slotdscan))
	  (slotd (car slotdscan) (car slotdscan)))
	((null slotdscan) table)
       (let ((slotd-allocation (slotd-allocation slotd)))
           (case slotd-allocation
             (:INSTANCE
	      (setf (gethash (slotd-name slotd) table) i)
	      (incf i))
	      (:CLASS
              (setf (gethash (slotd-name slotd) table) -1)))))))

(defun compute-class-precedence-list (class-name superclasses)
  ;; We start by computing two values.
  ;;   CPL
  ;;     The depth-first left-to-right up to joins walk of the supers tree.
  ;;     This is equivalent to depth-first right-to-left walk of the
  ;;     tree with all but the last occurence of a class removed from
  ;;     the resulting list.  This is in fact how the walk is implemented.
  ;;
  ;;   PRECEDENCE-ALIST
  ;;     An alist of the precedence relations. The car of each element
  ;;     of the precedence-alist is a class C, the cdr is all the classes C'
  ;;     which should precede C because either:
  ;;       C is a local super of C'
  ;;      or
  ;;       C' appears before C in some class's local-supers.
  ;;
  ;;     Thus, the precedence-alist reflects the two constraints that:
  ;;       1. A class must appear in the CPL before its local supers.
  ;;       2. Order of local supers is preserved in the CPL.
  ;;
  (labels
   ((must-move-p (element list precedence-alist &aux move)
		 (dolist (must-precede (cdr (assoc element
						   precedence-alist
						   :test #'eq)))
			 (when (setq move (member must-precede (cdr list)
						  :test #'eq))
			       (return move))))
    (find-farthest-move
     (element move precedence-alist)
     (dolist (must-precede (transitive-closure element precedence-alist))
	     (setq move (or (member must-precede move :test #'eq) move)))
     move)
    (transitive-closure
     (class precedence-alist)
     (let ((closure ()))
       (labels ((walk (element path)
		      (when (member element path :test #'eq)
			    (class-ordering-error
			     class-name element path precedence-alist))
		      (dolist (precede
			       (cdr (assoc element
					   precedence-alist :test #'eq)))
			      (unless (member precede closure :test #'eq)
				      (pushnew precede closure)
				      (walk precede (cons element path))))))
	       (walk class nil)
	       closure))))

   (multiple-value-bind
    (cpl precedence-alist)
    (walk-supers superclasses nil nil)
    (let ((tail cpl)
	  (element nil)
	  (move nil))
      ;; For each class in the cpl, make sure that there are no classes after
      ;; it which should be before it.  We do this by cdring down the list,
      ;; making sure that for each element of the list, none of its
      ;; must-precedes come after it in the list. If we find one, we use the
      ;; transitive closure of the must-precedes (call find-farthest-move) to
      ;; see where the class must really be moved. We use a hand-coded loop
      ;; so that we can splice things in and out of the CPL as we go.
      (loop (when (null tail) (return))
	    (setq element (car tail)
		  move (must-move-p element tail precedence-alist))
	    (cond (move
		   (setq move (find-farthest-move element move precedence-alist))
		   (setf (cdr move) (cons element (cdr move)))
		   (setf (car tail) (cadr tail))
		   (setf (cdr tail) (cddr tail))
		   )
		  (t
		   (setq tail (cdr tail)))))
      cpl))))

(defun walk-supers (supers cpl precedence-alist)
  (do* ((pre (reverse supers))
	(sup)
	(precedence))
       ((null pre) (values cpl precedence-alist))
    (setq sup (pop pre))
    (when pre
      (if (setq precedence (assoc sup precedence-alist :test #'eq))
	  ;; don't use rplacd here:
	  (setq precedence (cons (car precedence) (union pre precedence)))
	  (push (cons sup pre) precedence-alist)))
    (multiple-value-setq (cpl precedence-alist)
      (walk-supers (class-superiors sup) cpl precedence-alist))
    (setq cpl (adjoin sup cpl))))

(defun class-ordering-error (root element path precedence-alist)
  (setq path (cons element (reverse (member element (reverse path) :test #'eq))))
  (flet ((pretty (class) (or (class-name class) class)))
    (let ((explanations ()))
      (do ((tail path (cdr tail)))
	  ((null (cdr tail)))
	(let ((after (cadr tail))
	      (before (car tail)))
	  (if (member after (class-superiors before) :test #'eq)
	      (push (format nil
			    "~% ~A must precede ~A -- ~
                              ~A is in the local supers of ~A."
			    (pretty before) (pretty after)
			    (pretty after) (pretty before))
		    explanations)
	      (dolist (common-precede
			(intersection
			  (cdr (assoc after precedence-alist :test #'eq))
			  (cdr (assoc before precedence-alist :test #'eq))))
		(when (member after (member before
					    (class-superiors common-precede)
					    :test #'eq)
			      :test #'eq)
		  (push (format nil
				"~% ~A must precede ~A -- ~
                                  ~A has local supers ~S."
				(pretty before) (pretty after)
				(pretty common-precede)
				(mapcar #'pretty
					(class-superiors common-precede)))
			explanations))))))
      (error "While computing the class-precedence-list for the class ~A:~%~
              There is a circular constraint through the classes:~{ ~A~}.~%~
              This arises because:~{~A~}"
	     root
	     (mapcar #'pretty path)
	     (reverse explanations)))))

;;; ----------------------------------------------------------------------
