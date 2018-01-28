;;; -*- Mode: LISP; Syntax: Common-lisp; Package: ZL-USER; Base: 10; Patch-File: T -*-

;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
(SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "SYS:l-COMPILER;OPTIMIZE.LISP.179")
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: Lisp; Package: Compiler; Lowercase: T; Base: 8 -*-")

;;; Does simple constant folding.  This works for everything that doesn't have
;;; side-effects.
;;; ALL operands must be constant.
;;; Note that commutative-constant-folder can hack this case perfectly well
;;; by himself for the functions he handles.
(defun constant-fold-optimizer (form)
  (let ((eval-when-load-p nil))
    (flet ((constant-form-p (x)
	     (when (constant-form-p x)
	       (cond ((and (listp x)
			   (eq (car x) 'quote)
			   (listp (cadr x))
			   (eq (caadr x) eval-at-load-time-marker))
		      (setq eval-when-load-p t)
		      (cdadr x))
		     (t x)))))
      (if (every (cdr form) #'constant-form-p)
	  (if eval-when-load-p
	      (list 'quote
		    (list* eval-at-load-time-marker
			   (car form)
			   (mapcar #'constant-form-p (cdr form))))
	      (condition-case (error-object)
		   (multiple-value-call #'(lambda (&rest values)
					    (if (= (length values) 1)
						`',(first values)
						`(values ,@(mapcar #'(lambda (x) `',x)
								   values))))
					(eval form))
		 (error
		   (phase-1-warning "Constant form left unoptimized: ~S~%because: ~~A~"
				    form error-object)
		   form)))
	  form))))


;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
(SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "SYS:L-COMPILER;COMFILE.LISP.85")
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: Lisp; Package: Compiler; Lowercase: T; Base: 8 -*-")

;;;
;;; The damn compiler doesn't compile random forms that appear at top level.
;;; Its difficult to do because you have to get an associated function spec
;;; to go with those forms.  This handles that by defining a special form,
;;; top-level-form that compiles its body.  It takes a list of eval-when
;;; times just like eval when does.  It also takes a name which it uses
;;; to construct a function spec for the top-level-form function it has
;;; to create.
;;; 
;
;si::
;(defvar *top-level-form-fdefinitions* (cl:make-hash-table :test #'equal))
;
;si::
;(define-function-spec-handler pcl::top-level-form
;			      (operation fspec &optional arg1 arg2)
;  (let ((name (cadr fspec)))
;    (selectq operation
;      (validate-function-spec (and (= (length fspec) 2)
;				   (or (symbolp name)
;				       (listp name))))
;      (fdefine
;       (setf (gethash name *top-level-form-fdefinitions*) arg1))
;      ((fdefinition fdefinedp)
;       (gethash name *top-level-form-fdefinitions*)) 
;      (fdefinition-location 
;       (ferror "It is not possible to get the fdefinition-location of ~s."
;	       fspec))
;      (fundefine (remhash name *top-level-form-fdefinitions*))
;      (otherwise (function-spec-default-handler operation fspec arg1 arg2)))))
;
;;;
;;; This is basically stolen from PROGN (surprised?)
;;; 
;(si:define-special-form pcl::top-level-form (name times
;						  &body body
;						  &environment env)
;  (declare lt:(arg-template . body) (ignore name))
;  (si:check-eval-when-times times)
;  (when (member 'eval times) (si:eval-body body env)))
;
;(defun (:property pcl::top-level-form lt:mapforms) (original-form form usage)
;  (lt::mapforms-list original-form form (cddr form) 'eval usage))

;;; This is the normal function for looking at each form read from the file and calling
;;; *COMPILE-FORM-FUNCTION* on the sub-forms of it.
;;; COMPILE-TIME-TOO means override the normal cases that eval at compile time.  It is
;;; used for recursive calls under (EVAL-WHEN (COMPILE LOAD) ...).
;(DEFUN COMPILE-FROM-STREAM-1 (FORM &OPTIONAL (COMPILE-TIME-TOO NIL))
;  (CATCH-ERROR-RESTART
;     (SYS:ERROR "Skip compiling form ~2,2\COMPILER:SHORT-S-FORMAT\" FORM)
;    (LET ((DEFAULT-CONS-AREA (FUNCALL *COMPILE-FUNCTION* ':CONS-AREA)))
;      (LET ((ERROR-MESSAGE-HOOK
;	      #'(LAMBDA ()
;		  (DECLARE (SYS:DOWNWARD-FUNCTION))
;		  (FORMAT T "~&While processing ~V,V\COMPILER:SHORT-S-FORMAT\"
;			  DBG:*ERROR-MESSAGE-PRINLEVEL*
;			  DBG:*ERROR-MESSAGE-PRINLENGTH*
;			  FORM))))
;	(SETQ FORM (FUNCALL *COMPILE-FUNCTION* ':MACRO-EXPAND FORM)))
;      (WHEN (LISTP FORM)			;Ignore atoms at top-level
;	(LET ((FUNCTION (FIRST FORM)))
;	  (SELECTQ FUNCTION
;	    ((QUOTE))				;and quoted constants e.g. 'COMPILE
;	    ((PROGN)
;	     (DOLIST (FORM (CDR FORM))
;	       (COMPILE-FROM-STREAM-1 FORM COMPILE-TIME-TOO)))
;	    ((EVAL-WHEN)
;	     (SI:CHECK-EVAL-WHEN-TIMES (CADR FORM))
;	     (LET ((COMPILE-P (OR (MEMQ 'COMPILE (CADR FORM))
;				  (AND COMPILE-TIME-TOO (MEMQ 'EVAL (CADR FORM)))))
;		   (LOAD-P (OR (MEMQ 'LOAD (CADR FORM)) (MEMQ 'CL:LOAD (CADR FORM))))
;		   (FORMS (CDDR FORM)))
;	       (COND (LOAD-P
;		      (DOLIST (FORM FORMS)
;			(COMPILE-FROM-STREAM-1 FORM (AND COMPILE-P ':FORCE))))
;		     (COMPILE-P
;		      (DOLIST (FORM FORMS)
;			(FUNCALL *COMPILE-FORM-FUNCTION* FORM ':FORCE NIL))))))
;	    ((DEFUN)
;	     (LET ((TEM (DEFUN-COMPATIBILITY (CDR FORM) :WARN-IF-OBSOLETE T)))
;	       (IF (EQ (CDR TEM) (CDR FORM))
;		   (FUNCALL *COMPILE-FORM-FUNCTION* FORM COMPILE-TIME-TOO T)
;		   (COMPILE-FROM-STREAM-1 TEM COMPILE-TIME-TOO))))
;	    ((MACRO)
;	     (FUNCALL *COMPILE-FORM-FUNCTION* FORM (OR COMPILE-TIME-TOO T) T))
;	    ((DECLARE)
;	     (DOLIST (FORM (CDR FORM))
;	       (FUNCALL *COMPILE-FORM-FUNCTION* FORM (OR COMPILE-TIME-TOO T)
;			;; (DECLARE (SPECIAL ... has load-time action as well.
;			;; All other DECLARE's do not.
;			(MEMQ (CAR FORM) '(SPECIAL ZL:UNSPECIAL)))))
;	    ((COMPILER-LET)
;	     (COMPILER-LET-INTERNAL (CADR FORM) (CDDR FORM)
;				    #'COMPILE-FROM-STREAM-1 COMPILE-TIME-TOO))
;	    ((SI:DEFINE-SPECIAL-FORM)
;	     (FUNCALL *COMPILE-FORM-FUNCTION* FORM COMPILE-TIME-TOO T))
;	    ((MULTIPLE-DEFINITION)
;	     (DESTRUCTURING-BIND (NAME TYPE . BODY) (CDR FORM)
;	       (LET ((NAME-VALID (AND (NOT (NULL NAME))
;				      (OR (SYMBOLP NAME)
;					  (AND (LISTP NAME) (NEQ (CAR NAME) 'QUOTE)))))
;		     (TYPE-VALID (AND (NOT (NULL TYPE)) (SYMBOLP TYPE))))
;		 (UNLESS (AND NAME-VALID TYPE-VALID)
;		   (WARN "(~S ~S ~S ...) is invalid because~@
;			  ~:[~S is not valid as a definition name~;~*~]~
;			  ~:[~&~S is not valid as a definition type~;~*~]"
;			 'MULTIPLE-DEFINITION NAME TYPE NAME-VALID NAME TYPE-VALID TYPE)))
;	       (LET* ((COMPILED-BODY NIL)
;		      (COMPILE-FUNCTION *COMPILE-FUNCTION*)
;		      (*COMPILE-FUNCTION*
;			(LAMBDA (OPERATION &REST ARGS)
;			  (DECLARE (SYS:DOWNWARD-FUNCTION))
;			  (SELECTQ OPERATION
;			    (:DUMP-FORM
;			     (PUSH (FUNCALL COMPILE-FUNCTION :OPTIMIZE-TOP-LEVEL-FORM
;					    (FIRST ARGS))
;				   COMPILED-BODY))
;			    (:INSTALL-DEFINITION
;			     (PUSH (FORM-FOR-DEFINE *COMPILER* (FIRST ARGS) (SECOND ARGS))
;				   COMPILED-BODY))
;			    (OTHERWISE (CL:APPLY COMPILE-FUNCTION OPERATION ARGS)))))
;		      (LOCAL-DECLARATIONS `((FUNCTION-PARENT ,NAME ,TYPE)
;					    ,@LOCAL-DECLARATIONS)))
;		 (DOLIST (FORM BODY)
;		   (COMPILE-FROM-STREAM-1 FORM COMPILE-TIME-TOO))
;		 (FUNCALL COMPILE-FUNCTION :DUMP-FORM
;			  `(LOAD-MULTIPLE-DEFINITION
;			     ',NAME ',TYPE ',(NREVERSE COMPILED-BODY) NIL)))))
;	    ((pcl::top-level-form)
;	     (destructuring-bind (name times . body)
;				 (cdr form)
;	       (si:check-eval-when-times times)
;	       (let ((compile-p (or (memq 'compile times)
;				    (and compile-time-too (memq 'eval times))))
;		     (load-p (or (memq 'load times)
;				 (memq 'cl:load times)))
;		     (fspec `(pcl::top-level-form ,name)))
;		 (cond (load-p
;			(compile-from-stream-1
;			  `(progn (defun ,fspec () . ,body)
;				  (funcall (function ,fspec)))
;			  (and compile-p ':force)))
;		       (compile-p
;			(dolist (b body)
;			  (funcall *compile-form-function* form ':force nil)))))))
;	    (OTHERWISE
;	     (LET ((TEM (AND (SYMBOLP FUNCTION) (GET FUNCTION 'TOP-LEVEL-FORM))))
;	       (IF TEM
;		   (FUNCALL *COMPILE-FORM-FUNCTION* (FUNCALL TEM FORM) COMPILE-TIME-TOO T)
;		   (FUNCALL *COMPILE-FORM-FUNCTION* FORM COMPILE-TIME-TOO T))))))))))
;
;


dw::
(defun symbol-flavor-or-cl-type (symbol)
  (declare (values flavor defstruct-p deftype-fun typep-fun atomic-subtype-parent
		   non-atomic-deftype))
  (multiple-value-bind (result foundp)
      (gethash symbol *flavor-or-cl-type-cache*)
    (let ((frob
	    (if foundp result
	      (setf (gethash symbol *flavor-or-cl-type-cache*)
		    (or (get symbol 'flavor:flavor)
			(let ((class (get symbol 'clos-internals::class-for-name)))
			  (when (and class
				     (not (typep class 'clos:built-in-class)))
			    class))
			(not (null (defstruct-type-p symbol)))
			(let* ((deftype (get symbol 'deftype))
			       (descriptor (symbol-presentation-type-descriptor symbol))
			       (typep
				 (unless (and descriptor
					      (presentation-type-explicit-type-function
						descriptor))
				   ;; Don't override the one defined in the presentation-type.
				   (get symbol 'typep)))
			       (atomic-subtype-parent (find-atomic-subtype-parent symbol))
			       (non-atomic-deftype
				 (when (and (not descriptor) deftype)
				   (not (member (first (type-arglist symbol))
						'(&rest &key &optional))))))
			  (if (or typep (not (atom deftype))
				  non-atomic-deftype
				  ;; deftype overrides atomic-subtype-parent.
				  (and (not deftype) atomic-subtype-parent))
			      (list-in-area *handler-dynamic-area*
					    deftype typep atomic-subtype-parent
					    non-atomic-deftype)
			    deftype)))))))
      (locally (declare (inline compiled-function-p))
        (etypecase frob
	  (array (values frob))
	  (instance (values frob))
	  (null (values nil))
	  ((member t) (values nil t))
	  (compiled-function (values nil nil frob))
	  (lexical-closure (values nil nil frob))
	  (list (destructuring-bind (deftype typep atomic-subtype-parent non-atomic-deftype)
		    frob
		  (values nil nil deftype typep atomic-subtype-parent non-atomic-deftype)))
	  (symbol (values nil nil nil nil frob)))))))


