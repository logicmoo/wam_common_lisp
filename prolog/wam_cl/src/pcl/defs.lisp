;;;-*-Mode:LISP; Package:(PCL LISP 1000); Base:10; Syntax:Common-lisp -*-
;;;
;;; *************************************************************************
;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;; 
;;; Any person obtaining a copy of this software is requested to send their
;;; name and post office or electronic mail address to:
;;;   CommonLoops Coordinator
;;;   Xerox PARC
;;;   3333 Coyote Hill Rd.
;;;   Palo Alto, CA 94304
;;; (or send Arpanet mail to CommonLoops-Coordinator.pa@Xerox.arpa)
;;;
;;; Suggestions, comments and requests for improvements are also welcome.
;;; *************************************************************************
;;;

(in-package 'pcl)

(eval-when (compile load eval)
  
(defvar *defclass-times*   '(load eval))	;Probably have to change this
						;if you use defconstructor.
(defvar *defmethod-times*  '(load eval))
(defvar *defgeneric-times* '(load eval))

(defvar *boot-state* ())			;NIL
						;EARLY
						;BRAID
						;COMPLETE

)

(eval-when (load eval)
  (when (eq *boot-state* 'complete)
    (error "Trying to load (or compile) PCL in an environment in which it~%~
            has already been loaded.  This doesn't work, you will have to~%~
            get a fresh lisp (reboot) and then load PCL."))
  (when *boot-state*
    (cerror "Try loading (or compiling) PCL anyways."
	    "Trying to load (or compile) PCL in an environment in which it~%~
             has already been partially loaded.  This may not work, you may~%~
             need to get a fresh lisp (reboot) and then load PCL."))
  )



;;;
;;; This is like fdefinition on the Lispm.  If Common Lisp had something like
;;; function specs I wouldn't need this.  On the other hand, I don't like the
;;; way this really works so maybe function specs aren't really right either?
;;; 
;;; I also don't understand the real implications of a Lisp-1 on this sort of
;;; thing.  Certainly some of the lossage in all of this is because these
;;; SPECs name global definitions.
;;;
;;; Note that this implementation is set up so that an implementation which
;;; has a 'real' function spec mechanism can use that instead and in that way
;;; get rid of setf generic function names.
;;;
(defmacro parse-gspec (spec
		       (non-setf-var . non-setf-case)
		       (setf-var . setf-case))
  (declare (indentation 1 1))
  (once-only (spec)
    `(cond (#-setf (symbolp ,spec) #+setf t
	    (let ((,non-setf-var ,spec)) ,@non-setf-case))
	   ((and (listp ,spec)
		 (eq (car ,spec) 'setf)
		 (symbolp (cadr ,spec)))
	    (let ((,setf-var (cadr ,spec))) ,@setf-case))
	   (t
	    (error
	      "Can't understand ~S as a generic function specifier.~%~
               It must be either a symbol which can name a function or~%~
               a list like ~S, where the car is the symbol ~S and the cadr~%~
               is a symbol which can name a generic function."
	      ,spec '(setf <foo>) 'setf)))))

;;;
;;; If symbol names a function which is traced or advised, return the
;;; unadvised, traced etc. definition.  This lets me get at the generic
;;; function object even when it is traced.
;;;
(defun unencapsulated-fdefinition (symbol)
  #+Lispm (si:fdefinition (si:unencapsulate-function-spec symbol))
  #+Lucid (lucid::get-unadvised-procedure (symbol-function symbol))
  #+excl  (or (excl::encapsulated-basic-definition symbol)
	      (symbol-function symbol))
  #+xerox (il:virginfn symbol)
  #+CLISP (or (get symbol 'sys::traced-definition) (symbol-function symbol))
  #+setf (fdefinition symbol)
  #-(or Lispm Lucid excl Xerox CLISP setf) (symbol-function symbol))

;;;
;;; If symbol names a function which is traced or advised, redefine
;;; the `real' definition without affecting the advise.
;;;
(defun fdefine-carefully (symbol new-definition)
  #+Lispm (si:fdefine symbol new-definition t t)
  #+Lucid (let ((lucid::*redefinition-action* nil))
	    (setf (symbol-function symbol) new-definition))
  #+excl  (setf (symbol-function symbol) new-definition)
  #+xerox (let ((advisedp (member symbol il:advisedfns :test #'eq))
                (brokenp (member symbol il:brokenfns :test #'eq)))
	    ;; In XeroxLisp (late of envos) tracing is implemented
	    ;; as a special case of "breaking".  Advising, however,
	    ;; is treated specially.
            (xcl:unadvise-function symbol :no-error t)
            (xcl:unbreak-function symbol :no-error t)
            (setf (symbol-function symbol) new-definition)
            (when brokenp (xcl:rebreak-function symbol))
            (when advisedp (xcl:readvise-function symbol)))
  #+CLISP (let ((traced (get symbol 'sys::traced-definition)))
            (if traced
              (if (consp traced)
                (progn
                  (sys::untrace2 symbol)
                  (setf (symbol-function symbol) new-definition))
                (setf (get symbol 'sys::traced-definition) new-definition))
              (setf (symbol-function symbol) new-definition)))
  #+setf (setf (fdefinition symbol) new-definition)
  #-(or Lispm Lucid excl Xerox CLISP setf)
  (setf (symbol-function symbol) new-definition)
  
  new-definition)

(defun gboundp (spec)
  (parse-gspec spec
    (name (fboundp name))
    (name (fboundp (get-setf-function-name name)))))

(defun gmakunbound (spec)
  (parse-gspec spec
    (name (fmakunbound name))
    (name (fmakunbound (get-setf-function-name name)))))

(defun gdefinition (spec)
  (parse-gspec spec
    (name (or #-setf (macro-function name)		;??
	      (unencapsulated-fdefinition name)))
    (name (unencapsulated-fdefinition (get-setf-function-name name)))))

(defun #-setf SETF\ PCL\ GDEFINITION #+setf (setf gdefinition) (new-value spec)
  (parse-gspec spec
    (name (fdefine-carefully name new-value))
    (name (fdefine-carefully (get-setf-function-name name) new-value))))


(proclaim '(special *the-class-t* 
                    *the-class-vector* *the-class-symbol*
                    *the-class-string* *the-class-sequence*
                    *the-class-rational* *the-class-ratio*
                    *the-class-number* *the-class-null* *the-class-list*
                    *the-class-integer* *the-class-float* *the-class-cons*
                    *the-class-complex* *the-class-character*
                    *the-class-bit-vector* *the-class-array*

                    *the-class-slot-object*
                    *the-class-standard-object*
                    *the-class-structure-object*
                    *the-class-class*
                    *the-class-method*
                    *the-class-generic-function*
                    *the-class-built-in-class*
                    *the-class-slot-class*
                    *the-class-structure-class*
                    *the-class-standard-class*
                    *the-class-funcallable-standard-class*
                    *the-class-standard-method*
                    *the-class-standard-generic-function*
                    *the-class-standard-direct-slot-definition*
                    *the-class-standard-effective-slot-definition*))

(proclaim '(special *the-wrapper-of-t*
                    *the-wrapper-of-vector* *the-wrapper-of-symbol*
                    *the-wrapper-of-string* *the-wrapper-of-sequence*
                    *the-wrapper-of-rational* *the-wrapper-of-ratio*
                    *the-wrapper-of-number* *the-wrapper-of-null*
                    *the-wrapper-of-list* *the-wrapper-of-integer*
                    *the-wrapper-of-float* *the-wrapper-of-cons*
                    *the-wrapper-of-complex* *the-wrapper-of-character*
                    *the-wrapper-of-bit-vector* *the-wrapper-of-array*))

(defun coerce-to-class (class &optional make-forward-referenced-class-p)
  (declare (type boolean make-forward-referenced-class-p))
  (if (symbolp class)
      (or (find-class class (not make-forward-referenced-class-p))
	  (ensure-class class))
      class))

(defun specializer-from-type (type &aux args)
  (when (consp type)
    (setq args (cdr type) type (car type)))
  (cond ((symbolp type)
	 (or (and (null args) (find-class type))
	     (ecase type
	       (class    (coerce-to-class (car args)))
	       (class-eq (class-eq-specializer (coerce-to-class (car args))))
	       (eql      (intern-eql-specializer (car args))))))
	((specializerp type) type)))

(defun type-from-specializer (specl)
  (when (symbolp specl)
    (setq specl (find-class specl))) ;(or (find-class specl nil) (ensure-class specl))
  (cond ((consp specl)
         (unless (memq (car specl) '(class class-eq eql))
           (error "~S is not a legal specializer type" specl))
         specl)
        ((specializerp specl)
         (specializer-type specl))
        (t
         (error "~s is neither a type nor a specializer" specl))))

(defun type-class (type)
  (declare (special *the-class-t*))
  (setq type (type-from-specializer type))
  (if (atom type)
      *the-class-t*
      (case (car type)
        (eql (class-of (cadr type)))
        (class-eq (cadr type))
        (class (cadr type)))))

(defun class-eq-type (class)
  (specializer-type (class-eq-specializer class)))

(defun inform-type-system-about-std-class (name)
  (let ((predicate-name (make-type-predicate-name name)))
    (setf (symbol-function predicate-name) (make-type-predicate name))
    (do-satisfies-deftype name predicate-name)))

(defun make-type-predicate (name)
  (let ((cell (find-class-cell name)))
    #'(lambda (x)
	(funcall (the compiled-function (find-class-cell-predicate cell)) x))))


;This stuff isn't right.  Good thing it isn't used.
;The satisfies predicate has to be a symbol.  There is no way to
;construct such a symbol from a class object if class names change.
(defun class-predicate (class)
  (when (symbolp class) (setq class (find-class class)))
  #'(lambda (object)
      (memq class (wrapper-class-precedence-list (wrapper-of object)))))

(defun make-class-eq-predicate (class)
  (when (symbolp class) (setq class (find-class class)))
  #'(lambda (object) (eq class (class-of object))))

(defun make-eql-predicate (eql-object)
  #'(lambda (object) (eql eql-object object)))

#|| ; The argument to satisfies must be a symbol.  
(deftype class (&optional class)
  (if class
      `(satisfies ,(class-predicate class))
      `(satisfies ,(class-predicate 'class))))

(deftype class-eq (class)
  `(satisfies ,(make-class-eq-predicate class)))
||#

(deftype eql (type-object)
  `(member ,type-object))

;;;
;;; These functions are a pale imitiation of their namesake.  They accept
;;; class objects or types where they should.
;;; 
(defun *normalize-type (type)
  (cond ((consp type)
         (if (member (car type) '(not and or))
             `(,(car type) ,@(mapcar #'*normalize-type (cdr type)))
             (if (null (cdr type))
                 (*normalize-type (car type))
                 type)))
        ((symbolp type)
         (let ((class (find-class type nil)))
           (if class
               (let ((type (specializer-type class)))
		 (if (listp type) type `(,type)))
               `(,type))))
        ((specializerp type)
         (specializer-type type))
        (t
         (error "~s is not a type" type))))

(defun unparse-type-list (tlist)
  (mapcar #'unparse-type tlist))

(defun unparse-type (type)
  (if (atom type)
      (if (specializerp type)
          (unparse-type (specializer-type type))
          type)
      (case (car type)
        (eql type)
        (class-eq `(class-eq ,(class-name (cadr type))))
        (class (class-name (cadr type)))
        (t `(,(car type) ,@(unparse-type-list (cdr type)))))))

(defun convert-to-system-type (type)
  (case (car type)
    ((not and or) `(,(car type) ,@(mapcar #'convert-to-system-type (cdr type))))
    (class (class-name (cadr type))) ; it had better be a named class
    (class-eq (class-name (cadr type))) ; this one is impossible to do right
    (eql type)
    (t (if (null (cdr type))
	   (car type)
	   type))))

(declaim (ftype (function (T T) boolean) *typep))
(defun *typep (object type)
  (setq type (*normalize-type type))
  (cond ((memq (car type) '(eql wrapper-eq class-eq class))
         (specializer-applicable-using-type-p type `(eql ,object)))
        ((eq (car type) 'not)
         (not (*typep object (cadr type))))
        (t
         (typep object (convert-to-system-type type)))))

#-kcl
(declaim (ftype (function (T T) (values boolean boolean)) *subtypep))
(defun *subtypep (type1 type2)
  (setq type1 (*normalize-type type1))
  (setq type2 (*normalize-type type2))
  (if (member (car type2) '(eql wrapper-eq class-eq class))
      (multiple-value-bind (app-p maybe-app-p)
          (specializer-applicable-using-type-p type2 type1)
        (declare (type boolean app-p maybe-app-p))
        (values app-p (or app-p (not maybe-app-p))))
      (subtypep (convert-to-system-type type1)
		(convert-to-system-type type2))))

(defun do-satisfies-deftype (name predicate)
  #+(or :Genera (and :Lucid (not :Prime)) ExCL :coral CLISP)
  (let* ((specifier `(satisfies ,predicate))
	 (expand-fn #'(lambda (&rest ignore)
			(declare (ignore ignore))
			specifier)))
    ;; Specific ports can insert their own way of doing this.  Many
    ;; ports may find the expand-fn defined above useful.
    ;;
    (or #+:Genera
	(setf (get name 'deftype) expand-fn)
	#+(and :Lucid (not :Prime))
	(system::define-macro `(deftype ,name) expand-fn nil)
	#+ExCL
	(setf (get name 'excl::deftype-expander) expand-fn)
	#+:coral
	(setf (get name 'ccl::deftype-expander) expand-fn)
        #+CLISP
        (setf (get name 'sys::deftype-expander) expand-fn)))
  #-(or :Genera (and :Lucid (not :Prime)) ExCL :coral CLISP)
  ;; This is the default for ports for which we don't know any
  ;; better.  Note that for most ports, providing this definition
  ;; should just speed up class definition.  It shouldn't have an
  ;; effect on performance of most user code.
  (eval `(deftype ,name () '(satisfies ,predicate))))

(defun make-type-predicate-name (name &optional kind)
  (when (null name) (error "This shouldn't happen."))
  (if (symbol-package name)
      (intern (format nil
		      "~@[~A ~]TYPE-PREDICATE ~A ~A"
		      kind
		      (package-name (symbol-package name))
		      (symbol-name name))
	      *the-pcl-package*)
      (make-symbol (format nil
			   "~@[~A ~]TYPE-PREDICATE ~A"
			   kind
			   (symbol-name name)))))




(defvar *built-in-class-symbols* ())
(defvar *built-in-wrapper-symbols* ())

(defun get-built-in-class-symbol (class-name)
  (or (cadr (assq class-name *built-in-class-symbols*))
      (let ((symbol (intern (format nil
				    "*THE-CLASS-~A*"
				    (symbol-name class-name))
			    *the-pcl-package*)))
	(push (list class-name symbol) *built-in-class-symbols*)
	symbol)))

(defun get-built-in-wrapper-symbol (class-name)
  (or (cadr (assq class-name *built-in-wrapper-symbols*))
      (let ((symbol (intern (format nil
				    "*THE-WRAPPER-OF-~A*"
				    (symbol-name class-name))
			    *the-pcl-package*)))
	(push (list class-name symbol) *built-in-wrapper-symbols*)
	symbol)))




(pushnew 'class *variable-declarations*)
(pushnew 'variable-rebinding *variable-declarations*)

(defun variable-class (var env)
  (caddr (variable-declaration 'class var env)))




;;;
;;; This is used by combined methods to communicate the next methods to
;;; the methods they call.  This variable is captured by a lexical variable
;;; of the methods to give it the proper lexical scope.
;;; 
(defvar *next-methods* nil)

(defvar *not-an-eql-specializer* '(not-an-eql-specializer))

(defvar *umi-gfs*)
(defvar *umi-complete-classes*)
(defvar *umi-reorder*)

(defvar *invalidate-discriminating-function-force-p* ())
(defvar *invalid-dfuns-on-stack* ())


(defvar *standard-method-combination*)

(defvar *slotd-unsupplied* (list '*slotd-unsupplied*))	;***


(defmacro define-gf-predicate (predicate-name &rest classes)
  `(progn 
     (defmethod ,predicate-name ((x t)) nil)
     ,@(mapcar #'(lambda (c) `(defmethod ,predicate-name ((x ,c)) t))
	       classes)))

(defun make-class-predicate-name (name)
  (intern (format nil "~A::~A class predicate"
                  (package-name (symbol-package name))
                  name)
          *the-pcl-package*))

(defun plist-value (object name &optional default)
  (getf (object-plist object) name default))

(defun #-setf SETF\ PCL\ PLIST-VALUE #+setf (setf plist-value) (new-value object name)
  (setf (getf (object-plist object) name) new-value))



(defvar *built-in-classes*
  ;;
  ;; name       supers     subs                     cdr of cpl
  ;; prototype	predicate-name
  '(;(t         ()         (number sequence array character symbol) ())
    (number     (t)        (complex float rational) (t)
     1		numberp)
    (complex    (number)   ()                       (number t)
     #c(1 1)	complexp)
    (float      (number)   ()                       (number t)
     1.0	floatp)
    (rational   (number)   (integer ratio)          (number t)
     1		rationalp)
    (integer    (rational) ()                       (rational number t)
     1		integerp)
    (ratio      (rational) ()                       (rational number t)
     1/2)

    (sequence   (t)        (list vector)            (t)
     nil	sequencep)
    (list       (sequence) (cons null)              (sequence t)
     ()		listp)
    (cons       (list)     ()                       (list sequence t)
     (nil)	consp)
    

    (array      (t)        (vector)                 (t)
     #2A((NIL))	arrayp)
    (vector     (array
		 sequence) (string bit-vector)      (array sequence t)
     #()	vectorp)
    (string     (vector)   ()                       (vector array sequence t)
     ""		stringp)
    (bit-vector (vector)   ()                       (vector array sequence t)
     #*1	bit-vector-p)
    (character  (t)        ()                       (t)
     #\c	characterp)
   
    (symbol     (t)        (null)                   (t)
     symbol	symbolp)
    (null       (symbol
                 list)     ()                       (symbol list sequence t)
     nil	null)))


;;;
;;; The classes that define the kernel of the metabraid.
;;;
(defclass t () ()
  (:metaclass built-in-class))

(defclass slot-object (t) ()
  (:metaclass slot-class))

(defclass structure-object (slot-object) ()
  (:metaclass structure-class))

(defstruct (structure-object
	     (:constructor |STRUCTURE-OBJECT class constructor|)))

(defclass standard-object (slot-object) ())

(defclass metaobject (standard-object) ())

(defclass specializer (metaobject) 
     ((type
        :initform nil
        :reader specializer-type)))

(defclass definition-source-mixin (standard-object)
     ((source
	:initform (load-truename)
	:reader definition-source
	:initarg :definition-source)))

(defclass plist-mixin (standard-object)
     ((plist
	:initform ()
	:accessor object-plist)))

(defclass documentation-mixin ()
  ((documentation
      :initform NIL
      :initarg :documentation)))

(defclass dependent-update-mixin (plist-mixin)
    ())

;;;
;;; The class CLASS is a specified basic class.  It is the common superclass
;;; of any kind of class.  It holds all of the documented reader functions from
;;; the AMOP.  Any class that can be a metaclass must have the class CLASS
;;; in its class precedence list.
;;; 
(defclass class (documentation-mixin dependent-update-mixin
                 definition-source-mixin specializer)
     ((default-initargs
        :reader class-default-initargs)
      (direct-default-initargs
        :initform nil
        :reader class-direct-default-initargs)
      (direct-slots
        :initform nil
	:reader class-direct-slots)
      (direct-subclasses
        :initform nil
	:reader class-direct-subclasses)
      (direct-superclasses
        :initform nil
	:reader class-direct-superclasses)
      (finalized-p
        :initform nil
        :reader class-finalized-p)
      (name
	:initform nil
	:initarg  :name
	:reader class-name)
      (class-precedence-list
	:reader class-precedence-list)
      (prototype
        :reader class-prototype)
      (slots
	:reader class-slots)))


;;;
;;; The class PCL-CLASS is an implementation-specific common superclass of
;;; all specified subclasses of the class CLASS.
;;; 
(defclass pcl-class (class)
     ((cached-in-generic-functions
        :initform ()
        :reader class-cached-in-generic-functions)
      (can-precede-list
        :initform ()
	:reader class-can-precede-list)
      (class-eq-specializer
        :initform nil
        :reader class-eq-specializer)
      (direct-methods
	:initform (cons nil nil))
      (incompatible-superclass-list
        :initform ()
	:accessor class-incompatible-superclass-list)
      (internal-slotds
        :reader class-internal-slotds
        :documentation
          "List of internal-slotd structure copies of class-slots (for optimization).")
      (wrapper
	:initform nil
	:reader class-wrapper)
      (predicate-name
        :initform nil
	:reader class-predicate-name))
      )

(defclass slot-class (pcl-class)
     ((side-effect-internal-slotds
        :reader class-side-effect-internal-slotds
        :documentation
          "List of internal-slotd structure copies of class-slots whose initfunctions
           may have side-effects (for optimization).")))

;;;
;;; The class STD-CLASS is an implementation-specific common superclass of
;;; the classes STANDARD-CLASS and FUNCALLABLE-STANDARD-CLASS.
;;; 
(defclass std-class (slot-class)
 ())

(defclass standard-class (std-class)
     ())

(defclass funcallable-standard-class (std-class)
     ())
    
(defclass forward-referenced-class (pcl-class) ())

(defclass built-in-class (pcl-class) ())

(defclass structure-class (slot-class)
     ((defstruct-conc-name
        :initform nil
        :reader class-defstruct-conc-name)
      (defstruct-constructor
        :initform nil
	:reader class-defstruct-constructor)
      (from-defclass-p
        :initform nil
	:initarg :from-defclass-p
        :reader class-from-defclass-p)))


(defclass specializer-with-object (specializer) ())

(defclass exact-class-specializer (specializer) ())

(defclass class-eq-specializer (exact-class-specializer specializer-with-object)
  ((object :initarg :class :reader specializer-class :reader specializer-object)))

(defclass eql-specializer (exact-class-specializer specializer-with-object)
  ((object :initarg :object :reader specializer-object 
	   :reader eql-specializer-object)))

(defvar *eql-specializer-table* (make-hash-table :test 'eql))

(defun intern-eql-specializer (object)
  (or (gethash object *eql-specializer-table*)
      (setf (gethash object *eql-specializer-table*)
	    (make-instance 'eql-specializer :object object))))


;;;
;;; Slot definitions.
;;;
;;; Note that throughout PCL, "SLOT-DEFINITION" is abbreviated as "SLOTD".
;;;
(defclass slot-definition (documentation-mixin metaobject) 
     ((name
	:initform nil
	:initarg :name
        :accessor slot-definition-name)
      (initform
	:initform nil
	:initarg :initform
	:accessor slot-definition-initform)
      (initfunction
	:initform nil
        :type     (or function null)
	:initarg :initfunction
	:accessor slot-definition-initfunction)
      (readers
	:initform nil
	:initarg :readers
	:accessor slot-definition-readers)
      (writers
	:initform nil
	:initarg :writers
	:accessor slot-definition-writers)
      (initargs
	:initform nil
	:initarg :initargs
	:accessor slot-definition-initargs)
      (type
	:initform t
	:initarg :type
	:accessor slot-definition-type)
      (class
        :initform nil
	:initarg :class
	:accessor slot-definition-class)
      (initfunction-side-effect-free-p
	:initform nil
	:initarg :initfunction-side-effect-free-p
	:accessor slot-definition-initfunction-side-effect-free-p)))


(defclass standard-slot-definition (slot-definition)
  ((allocation
    :initform :instance
    :initarg :allocation
    :accessor slot-definition-allocation)))

(defclass structure-slot-definition (slot-definition)
  ((defstruct-accessor-symbol 
     :initform nil
     :initarg :defstruct-accessor-symbol
     :accessor slot-definition-defstruct-accessor-symbol)
   (internal-reader-function 
     :initform nil
     :initarg :internal-reader-function
     :accessor slot-definition-internal-reader-function)
   (internal-writer-function 
     :initform nil
     :initarg :internal-writer-function
     :accessor slot-definition-internal-writer-function)))

(defclass direct-slot-definition (slot-definition)
  ())

(defclass effective-slot-definition (slot-definition)
  ((location ; nil, a fixnum, a cons: (slot-name . value)
    :initform nil
    :accessor slot-definition-location)
   (reader-function ; #'(lambda (object) ...)
    :accessor slot-definition-reader-function)
   (writer-function ; #'(lambda (new-value object) ...)
    :accessor slot-definition-writer-function)
   (boundp-function ; #'(lambda (object) ...)
    :accessor slot-definition-boundp-function)
   (accessor-flags
    :initform 0)
   (internal-slotd
    :initform NIL
    :accessor slot-definition-internal-slotd
    :documentation
      "Internal-slotd structure with copies of slot-definition info
       used for optimizations purposes.")))

(defclass standard-direct-slot-definition (standard-slot-definition
					   direct-slot-definition)
  ())

(defclass standard-effective-slot-definition (standard-slot-definition
					      effective-slot-definition)
  ())

(defclass structure-direct-slot-definition (structure-slot-definition
					    direct-slot-definition)
  ())

(defclass structure-effective-slot-definition (structure-slot-definition
					       effective-slot-definition)
  ())



(defun slot-reader-undefined (object)
  (error "slot reader-function undefined for ~S" object))

(defun slot-writer-undefined (new-value object)
  (declare (ignore new-value))
  (error "slot writer-function undefined for ~S" object))

(defun slot-boundp-undefined (object)
  (error "slot boundp-function undefined for ~S" object))

(defstruct (internal-slotd
             (:print-function print-internal-slotd))
 (name            NIL  :type symbol)
 (slot-definition NIL)
 (location        NIL)
 (initargs        ()   :type list)
 (initfunction    ()   :type (or function null))
 (reader-function #'slot-reader-undefined :type compiled-function)
 (writer-function #'slot-writer-undefined :type compiled-function)
 (boundp-function #'slot-boundp-undefined :type compiled-function))

#+akcl
(si::freeze-defstruct 'internal-slotd)

(defun print-internal-slotd (internal-slotd stream depth)
  (declare (ignore depth))
  (printing-random-thing (internal-slotd stream)
    (format stream "internal-slotd ~S"
            (internal-slotd-slot-definition internal-slotd))))


(defparameter *early-class-predicates*
  '((specializer specializerp)
    (exact-class-specializer exact-class-specializer-p)
    (class-eq-specializer class-eq-specializer-p)
    (eql-specializer eql-specializer-p)
    (class classp)
    (standard-class standard-class-p)
    (funcallable-standard-class funcallable-standard-class-p)
    (structure-class structure-class-p)
    (forward-referenced-class forward-referenced-class-p)))

