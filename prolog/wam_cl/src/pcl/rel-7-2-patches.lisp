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
	  (null (values nil))
	  ((member t) (values nil t))
	  (compiled-function (values nil nil frob))
	  (lexical-closure (values nil nil frob))
	  (list (destructuring-bind (deftype typep atomic-subtype-parent non-atomic-deftype)
		    frob
		  (values nil nil deftype typep atomic-subtype-parent non-atomic-deftype)))
	  (symbol (values nil nil nil nil frob)))))))

;;;
;;; The variable zwei::*sectionize-line-lookahead* controls how many lines the parser
;;;  is willing to look ahead while trying to parse a definition.  Even 2 lines is enough
;;;  for just about all cases, but there isn't much overhead, and 10 should be enough
;;;  to satisfy pretty much everyone... but feel free to change it.
;;;        - MT 880921
;;;

zwei:
(defvar *sectionize-line-lookahead* 3)

zwei:
(DEFMETHOD (:SECTIONIZE-BUFFER MAJOR-MODE :DEFAULT)
	   (FIRST-BP LAST-BP BUFFER STREAM INT-STREAM ADDED-COMPLETIONS)
  ADDED-COMPLETIONS ;ignored, obsolete
  (WHEN STREAM
    (SEND-IF-HANDLES STREAM :SET-RETURN-DIAGRAMS-AS-LINES T))
  (INCF *SECTIONIZE-BUFFER*)
  (LET ((BUFFER-TICK (OR (SEND-IF-HANDLES BUFFER :SAVE-TICK) *TICK*))
	OLD-CHANGED-SECTIONS)
    (TICK)
    ;; Flush old section nodes.  Also collect the names of those that are modified, they are
    ;; the ones that will be modified again after a revert buffer.
    (DOLIST (NODE (NODE-INFERIORS BUFFER))
      (AND (> (NODE-TICK NODE) BUFFER-TICK)
	   (PUSH (LIST (SECTION-NODE-FUNCTION-SPEC NODE)
		       (SECTION-NODE-DEFINITION-TYPE NODE))
		 OLD-CHANGED-SECTIONS))
      (FLUSH-BP (INTERVAL-FIRST-BP NODE))
      (FLUSH-BP (INTERVAL-LAST-BP NODE)))
    (DO ((LINE (BP-LINE FIRST-BP) (LINE-NEXT INT-LINE))
	 (LIMIT (BP-LINE LAST-BP))
	 (EOFFLG)
	 (ABNORMAL T)
	 (DEFINITION-LIST NIL)
	 (BP (COPY-BP FIRST-BP))
	 (FUNCTION-SPEC)
	 (DEFINITION-TYPE)
	 (STR)
	 (INT-LINE)
	 (first-time t)
	 (future-line)				; we actually read into future line
	 (future-int-line)
	 (PREV-NODE-START-BP FIRST-BP)
	 (PREV-NODE-DEFINITION-LINE NIL)
	 (PREV-NODE-FUNCTION-SPEC NIL)
	 (PREV-NODE-TYPE 'HEADER)
	 (PREVIOUS-NODE NIL)
	 (NODE-LIST NIL)
	 (STATE (SEND SELF :INITIAL-SECTIONIZATION-STATE)))
	(NIL)
      ;; If we have a stream, read another line.
      (when (AND STREAM (NOT EOFFLG))
	(let ((lookahead (if future-line 1 *sectionize-line-lookahead*)))
	  (dotimes (i lookahead)		; startup lookahead
	    (MULTIPLE-VALUE (future-LINE EOFFLG)
	      (LET ((DEFAULT-CONS-AREA *LINE-AREA*))
		(SEND STREAM ':LINE-IN LINE-LEADER-SIZE)))
	    (IF future-LINE (SETQ future-INT-LINE (FUNCALL INT-STREAM ':LINE-OUT future-LINE)))
	    (when first-time
	      (setq first-time nil)
	      (setq line future-line)
	      (setq int-line future-int-line))
	    (when eofflg
	      (return)))))

      (SETQ INT-LINE LINE)

      (when int-line
	(MOVE-BP BP INT-LINE 0))		;Record as potentially start-bp for a section

      ;; See if the line is the start of a defun.
      (WHEN (AND LINE
		 (LET (ERR)
		   (MULTIPLE-VALUE (FUNCTION-SPEC DEFINITION-TYPE STR ERR STATE)
		     (SEND SELF ':SECTION-NAME INT-LINE BP STATE))
		   (NOT ERR)))
	(PUSH (LIST FUNCTION-SPEC DEFINITION-TYPE) DEFINITION-LIST)
	(SECTION-COMPLETION FUNCTION-SPEC STR NIL)
	;; List methods under both names for user ease.
	(LET ((OTHER-COMPLETION (SEND SELF ':OTHER-SECTION-NAME-COMPLETION
				      FUNCTION-SPEC INT-LINE)))
	  (WHEN OTHER-COMPLETION
	    (SECTION-COMPLETION FUNCTION-SPEC OTHER-COMPLETION NIL)))
	(LET ((PREV-NODE-END-BP (BACKWARD-OVER-COMMENT-LINES BP ':FORM-AS-BLANK)))
	  ;; Don't make a section node if it's completely empty.  This avoids making
	  ;; a useless Buffer Header section node. Just set all the PREV variables
	  ;; so that the next definition provokes the *right thing*
	  (UNLESS (BP-= PREV-NODE-END-BP PREV-NODE-START-BP)
	    (SETQ PREVIOUS-NODE
		  (ADD-SECTION-NODE PREV-NODE-START-BP
				    (SETQ PREV-NODE-START-BP PREV-NODE-END-BP)
				    PREV-NODE-FUNCTION-SPEC PREV-NODE-TYPE
				    PREV-NODE-DEFINITION-LINE BUFFER PREVIOUS-NODE
				    (IF (LOOP FOR (FSPEC TYPE) IN OLD-CHANGED-SECTIONS
					      THEREIS (AND (EQ PREV-NODE-FUNCTION-SPEC FSPEC)
							   (EQ PREV-NODE-TYPE TYPE)))
					*TICK* BUFFER-TICK)
				    BUFFER-TICK))
	    (PUSH PREVIOUS-NODE NODE-LIST)))
	(SETQ PREV-NODE-FUNCTION-SPEC FUNCTION-SPEC
	      PREV-NODE-TYPE DEFINITION-TYPE
	      PREV-NODE-DEFINITION-LINE INT-LINE))
      ;; After processing the last line, exit.
      (WHEN (OR #+ignore EOFFLG (null line) (AND (NULL STREAM) (EQ LINE LIMIT)))
	;; If reading a stream, we should not have inserted a CR
	;; after the eof line.
	(WHEN STREAM
	  (DELETE-INTERVAL (FORWARD-CHAR LAST-BP -1 T) LAST-BP T))
	;; The rest of the buffer is part of the last node
	(UNLESS (SEND SELF ':SECTION-NAME-TRIVIAL-P)
	  ;; ---oh dear, what sort of section will this be? A non-empty HEADER
	  ;; ---node.  Well, ok for now.
	  (PUSH (ADD-SECTION-NODE PREV-NODE-START-BP LAST-BP
				  PREV-NODE-FUNCTION-SPEC PREV-NODE-TYPE
				  PREV-NODE-DEFINITION-LINE BUFFER PREVIOUS-NODE
				  (IF (LOOP FOR (FSPEC TYPE) IN OLD-CHANGED-SECTIONS
					    THEREIS (AND (EQ PREV-NODE-FUNCTION-SPEC FSPEC)
							 (EQ PREV-NODE-TYPE TYPE)))
				      *TICK* BUFFER-TICK)
				  BUFFER-TICK)
		NODE-LIST)
	  (SETF (LINE-NODE (BP-LINE LAST-BP)) (CAR NODE-LIST)))
	(SETF (NODE-INFERIORS BUFFER) (NREVERSE NODE-LIST))
	(SETF (NAMED-BUFFER-WITH-SECTIONS-FIRST-SECTION BUFFER) (CAR (NODE-INFERIORS BUFFER)))
	(SETQ ABNORMAL NIL)			;timing windows here
	;; Speed up completion if enabled.
	(WHEN SI:*ENABLE-AARRAY-SORTING-AFTER-LOADS*
	  (SI:SORT-AARRAY *ZMACS-COMPLETION-AARRAY*))
	(SETQ *ZMACS-COMPLETION-AARRAY*
	      (FOLLOW-STRUCTURE-FORWARDING *ZMACS-COMPLETION-AARRAY*))
	(RETURN
	  (VALUES 
	    (CL:SETF (ZMACS-SECTION-LIST BUFFER)
		     (NREVERSE DEFINITION-LIST))
	    ABNORMAL))))))


