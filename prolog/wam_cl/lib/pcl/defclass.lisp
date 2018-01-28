;;;-*-Mode:LISP; Package: PCL; Base:10; Syntax:Common-lisp -*-
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

;;;
;;; MAKE-TOP-LEVEL-FORM is used by all PCL macros that appear `at top-level'.
;;;
;;; The original motiviation for this function was to deal with the bug in
;;; the Genera compiler that prevents lambda expressions in top-level forms
;;; other than DEFUN from being compiled.
;;;
;;; Now this function is used to grab other functionality as well.  This
;;; includes:
;;;   - Preventing the grouping of top-level forms.  For example, a
;;;     DEFCLASS followed by a DEFMETHOD may not want to be grouped
;;;     into the same top-level form.
;;;   - Telling the programming environment what the pretty version
;;;     of the name of this form is.  This is used by WARN.
;;; 
(defun make-top-level-form (name times form)
  (flet ((definition-name ()
	   (if (and (listp name)
		    (memq (car name) '(defmethod defclass class method method-combination)))
	       (format nil "~A~{ ~S~}"
		       (capitalize-words (car name) ()) (cdr name))
	       (format nil "~S" name))))
    (definition-name)
    #+Genera
    (progn
      #-Genera-Release-8
      (let ((thunk-name (gensym "TOP-LEVEL-FORM")))
	`(eval-when ,times
	   (defun ,thunk-name ()
	     (declare (sys:function-parent
			,(cond ((listp name)
				(case (first name)
				  (defmethod `(method ,@(rest name)))
				  (otherwise (second name))))
			       (t name))
			,(cond ((listp name)
				(case (first name)
				  ((defmethod defgeneric) 'defun)
				  ((defclass) 'defclass)
				  (otherwise (first name))))
			       (t 'defun))))
	     ,form)
	   (,thunk-name)))
      #+Genera-Release-8
      `(compiler-let ((compiler:default-warning-function ',name))
	 (eval-when ,times
	   (funcall #'(lambda ()
			(declare ,(cond ((listp name)
					 (case (first name)
					   ((defclass)
					    `(sys:function-parent ,(second name) defclass))
					   ((defmethod)
					    `(sys:function-name (method ,@(rest name))))
					   ((defgeneric)
					    `(sys:function-name ,(second name)))
					   (otherwise
					     `(sys:function-name ,name))))
					(t
					 `(sys:function-name ,name))))
			,form)))))
    #+LCL3.0
    `(compiler-let ((lucid::*compiler-message-string*
		      (or lucid::*compiler-message-string*
			  ,(definition-name))))
       (eval-when ,times ,form))
    #+cmu
    (if (member 'compile times)
        `(eval-when ,times ,form)
        form)
    #+kcl
    (let* ((*print-pretty* nil)
           (thunk-name (gensym (definition-name))))
      (gensym "G") ; set the prefix back to something less confusing.
      `(eval-when ,times
         (defun ,thunk-name ()
           ,form)
         (,thunk-name)))
    #-(or Genera LCL3.0 cmu kcl)
    (make-progn `',name `(eval-when ,times ,form))))

(defun make-progn (&rest forms)
  (let ((progn-form nil))
    (labels ((collect-forms (forms)
	       (unless (null forms)
		 (collect-forms (cdr forms))
		 (if (and (listp (car forms))
			  (eq (caar forms) 'progn))
		     (collect-forms (cdar forms))
		     (push (car forms) progn-form)))))
      (collect-forms forms)
      (cons 'progn progn-form))))



;;; 
;;; Like the DEFMETHOD macro, the expansion of the DEFCLASS macro is fixed.
;;; DEFCLASS always expands into a call to LOAD-DEFCLASS.  Until the meta-
;;; braid is set up, LOAD-DEFCLASS has a special definition which simply
;;; collects all class definitions up, when the metabraid is initialized it
;;; is done from those class definitions.
;;;
;;; After the metabraid has been setup, and the protocol for defining classes
;;; has been defined, the real definition of LOAD-DEFCLASS is installed by the
;;; file defclass.lisp
;;; 
(defmacro DEFCLASS (name direct-superclasses direct-slots &rest options)
  (declare (indentation 2 4 3 1))
  (expand-defclass name direct-superclasses direct-slots options))

(defun expand-defclass (name supers slots options)
  (declare (special *defclass-times* *boot-state* *the-class-structure-class*))
  (setq supers  (copy-tree supers)
	slots   (copy-tree slots)
	options (copy-tree options))
  (let ((metaclass 'standard-class))
    (dolist (option options)
      (if (not (listp option))
          (error "~S is not a legal defclass option." option)
          (when (eq (car option) ':metaclass)
            (unless (legal-class-name-p (cadr option))
              (error "The value of the :metaclass option (~S) is not a~%~
                      legal class name."
                     (cadr option)))
            (setq metaclass (cadr option))
	    (setf options (remove option options))
	    (return t))))

    (let ((*initfunctions* ())
          (*accessors* ())                         ;Truly a crock, but we got
          (*readers* ())                           ;to have it to live nicely.
          (*writers* ()))
      (declare (special *initfunctions* *accessors* *readers* *writers*))
      (let* ((canonical-slots
	       (mapcar #'(lambda (spec)
			   (canonicalize-slot-specification name spec))
		       slots))
	     (other-initargs
	       (mapcar #'(lambda (option)
			   (canonicalize-defclass-option name option))
		       options))
	      (defstruct-p (and (eq *boot-state* 'complete)
			        (let ((mclass (find-class metaclass nil)))
				  (and mclass
				       (*subtypep mclass 
						  *the-class-structure-class*))))))
	(do-standard-defsetfs-for-defclass *accessors*)
        (let ((defclass-form 
                 (make-top-level-form `(defclass ,name)
                   *defclass-times*
		   `(progn
		      ,@(mapcar #'(lambda (x)
                                    `(proclaim-defgeneric ',x '(self)))
				*readers*)
		      ,@(mapcar #'(lambda (x)
				    #-setf (when (consp x)
					     (setq x (get-setf-function-name (cadr x))))
                                    `(proclaim-defgeneric ',x '(new self)))
				*writers*)
		      (let ,(mapcar #'cdr *initfunctions*)
			(load-defclass ',name
				       ',metaclass
				       ',supers
				       (list ,@canonical-slots)
				       (list ,@(apply #'append 
						      (when defstruct-p
							'(:from-defclass-p t))
						      other-initargs))
				       ',*accessors*))))))
          defclass-form)))))

(defun float-zero ()
  0.0)

(defun make-initfunction (initform)
  (declare (special *initfunctions*))
  (cond ((or (eq initform 't)
	     (equal initform ''t))
	 '(function true))
	((or (eq initform 'nil)
	     (equal initform ''nil))
	 '(function false))
	((or (eql initform '0)
	     (equal initform ''0))
	 '(function zero))
	((or (eql initform '0.0)
	     (equal initform ''0.0))
	 '(function float-zero))
	(t
	 (let ((entry (assoc initform *initfunctions* :test #'equal)))
	   (unless entry
	     (setq entry
                   (list initform
			 (gensym)
                         `(slot-initfunction-storage-form
                            (function (lambda () ,initform)))))
	     (push entry *initfunctions*))
	   (cadr entry)))))

(defun canonicalize-slot-specification (class-name spec)
  (declare (special *accessors* *readers* *writers*))
  (cond ((and (symbolp spec)
	      (not (keywordp spec))
	      (not (memq spec '(t nil))))		   
	 `'(:name ,spec))
	((not (consp spec))
	 (error "~S is not a legal slot specification." spec))
	((null (cdr spec))
	 `'(:name ,(car spec)))
	((null (cddr spec))
	 (error "In DEFCLASS ~S, the slot specification ~S is obsolete.~%~
                 Convert it to ~S"
		class-name spec (list (car spec) :initform (cadr spec))))
	(t
	 (let* ((name (pop spec))
		(readers ())
		(writers ())
		(initargs ())
		(unsupplied (list nil))
		(initform (getf spec :initform unsupplied)))
	   (doplist (key val) spec
	     (case key
	       (:accessor (push val *accessors*)
			  (push val readers)
			  (push `(setf ,val) writers))
	       (:reader   (push val readers))
	       (:writer   (push val writers))
	       (:initarg  (push val initargs))))
	   (loop (unless (remf spec :accessor) (return)))
	   (loop (unless (remf spec :reader)   (return)))
	   (loop (unless (remf spec :writer)   (return)))
	   (loop (unless (remf spec :initarg)  (return)))
           (setq *writers* (append writers *writers*))
           (setq *readers* (append readers *readers*))
	   (setq spec `(:name     ',name
			:readers  ',readers
			:writers  ',writers
			:initargs ',initargs
			',spec))
	   (if (eq initform unsupplied)
	       `(list* ,@spec)
	       `(list* :initfunction ,(make-initfunction initform)
                       :initfunction-side-effect-free-p ,(simple-eval-access-p initform)
                       ,@spec))))))

(defun canonicalize-defclass-option (class-name option)  
  (declare (ignore class-name))
  (case (car option)
    (:default-initargs
      (let ((canonical ()))
	(let (key val (tail (cdr option)))
	  (loop (when (null tail) (return nil))
		(setq key (pop tail)
		      val (pop tail))
		(push ``(,',key ,,(make-initfunction val) ,',val) canonical))
	  `(':direct-default-initargs (list ,@(nreverse canonical))))))
    (:documentation
      `(:documentation ',(cadr option)))
    (otherwise
      `(',(car option) ',(cdr option)))))


;;;
;;; This is the early definition of load-defclass.  It just collects up all
;;; the class definitions in a list.  Later, in the file braid1.lisp, these
;;; are actually defined.
;;;


;;;
;;; Each entry in *early-class-definitions* is an early-class-definition.
;;; 
;;;
(declaim (type list *early-class-definitions*))
(defparameter *early-class-definitions* ())

(defun make-early-class-definition
       (name source metaclass
	superclass-names canonical-slots other-initargs)
  (list 'early-class-definition
	name source metaclass
	superclass-names canonical-slots other-initargs))
  
(defun ecd-class-name        (ecd) (nth 1 ecd))
(defun ecd-source            (ecd) (nth 2 ecd))
(defun ecd-metaclass         (ecd) (nth 3 ecd))
(defun ecd-superclass-names  (ecd) (nth 4 ecd))
(defun ecd-canonical-slots   (ecd) (nth 5 ecd))
(defun ecd-other-initargs    (ecd) (nth 6 ecd))

(proclaim '(notinline load-defclass))
(defun load-defclass
       (name metaclass supers canonical-slots canonical-options accessor-names)
  (setq supers  (copy-tree supers)
	canonical-slots   (copy-tree canonical-slots)
	canonical-options (copy-tree canonical-options))
  (do-standard-defsetfs-for-defclass accessor-names)
  (when (eq metaclass 'standard-class)
    (inform-type-system-about-std-class name))
  (let ((ecd
	  (make-early-class-definition name
				       (load-truename)
				       metaclass
				       supers
				       canonical-slots
				       (apply #'append canonical-options)))
	(existing
	  (find name *early-class-definitions* :key #'ecd-class-name)))
    (setq *early-class-definitions*
	  (cons ecd (remove existing *early-class-definitions*)))
    ecd))

;;;
;;; FIND-CLASS
;;;
;;; This is documented in the CLOS specification.
;;;
(defvar *find-class* (make-hash-table :test #'eq))

(defmacro find-class-cell-class (cell)
  `(car ,cell))

(defmacro find-class-cell-predicate (cell)
  `(cdr ,cell))

(defun find-class-cell (symbol &optional dont-create-p)
  (or (gethash symbol *find-class*)
      (unless dont-create-p
	(unless (legal-class-name-p symbol)
	  (error "~S is not a legal class name." symbol))
	(setf (gethash symbol *find-class*) (make-find-class-cell symbol)))))

(defun found-unknown-class (symbol errorp)
  (cond ((null errorp) nil)
        ((legal-class-name-p symbol)
         (error "No class named: ~S." symbol))
        (t
         (error "~S is not a legal class name." symbol))))

(defmacro find-class-from-cell (symbol cell &optional (errorp t))
  `(or (find-class-cell-class ,cell)
       (if (known-structure-type-p ,symbol)
           (find-structure-class ,symbol)
           #+structure-functions
           (found-unknown-class ,symbol ,errorp)
           #-structure-functions
           (find-maybe-structure-class ,symbol ,errorp))))

#-structure-functions
(defun likely-to-name-structure-p (symbol)
  ;; Returns whether Symbol is likely to name an already-defined
  ;; structure by whether it can find the default make or predicate
  ;; functions that would have been created by Defstruct.  This
  ;; obviously may fail.
  (let ((name (symbol-name symbol)))
    (declare (type simple-string name))
    (and (fboundp (intern (concatenate 'simple-string "MAKE-" name)
                          (symbol-package symbol)))
         (fboundp (intern (concatenate 'simple-string name "-P")
                          (symbol-package symbol))))))

#-structure-functions
(defun find-maybe-structure-class (symbol errorp)
  (if symbol
      (multiple-value-bind (structurep surep)
           (safe-subtypep symbol 'structure)
        (declare (type boolean structurep surep))
        (if structurep
            (find-structure-class symbol :warn T)
            (if surep
                (found-unknown-class symbol errorp)
                (if (likely-to-name-structure-p symbol)
                    (find-structure-class symbol :warn T)
                    (found-unknown-class symbol errorp)))))
     (found-unknown-class symbol errorp)))

(defun find-class-predicate-from-cell (symbol cell &optional (errorp t))
  (unless (find-class-cell-class cell)
    (find-class-from-cell symbol cell errorp))
  (find-class-cell-predicate cell))

(defun legal-class-name-p (x)
  (and (symbolp x)
       (not (keywordp x))))

(defun find-class (symbol &optional (errorp t) environment)
  (declare (ignore environment))
  (find-class-from-cell symbol (gethash symbol *find-class*) errorp))

(defun find-class-predicate (symbol &optional (errorp t) environment)
  (declare (ignore environment))
  (find-class-predicate-from-cell symbol (find-class-cell symbol errorp) errorp))

#-setf
(defsetf find-class (symbol &optional (errorp t) environment) (new-value)
  (declare (ignore errorp environment))
  `(SETF\ PCL\ FIND-CLASS ,new-value ,symbol))

(defun #-setf SETF\ PCL\ FIND-CLASS #+setf (setf find-class) (new-value symbol)
  (if (legal-class-name-p symbol)
      (setf (find-class-cell-class (find-class-cell symbol)) new-value)
      (error "~S is not a legal class name." symbol)))

#-setf
(defsetf find-class-predicate (symbol &optional (errorp t) environment) (new-value)
  (declare (ignore errorp environment))
  `(SETF\ PCL\ FIND-CLASS-PREDICATE ,new-value ,symbol))

(defun #-setf SETF\ PCL\ FIND-CLASS-PREDICATE #+setf (setf find-class-predicate)
          (new-value symbol)
  (if (legal-class-name-p symbol)
      (setf (find-class-cell-predicate (find-class-cell symbol)) new-value)
      (error "~S is not a legal class name." symbol)))

(defun make-find-class-cell (class-name)
  (cons nil
        #'(lambda (x)
            (let* ((class (find-class class-name))
                   (class-predicate (make-class-predicate class)))
              (declare (type compiled-function class-predicate))
              (setf (find-class-predicate class-name) class-predicate)
              (funcall class-predicate x)))))

(defun find-wrapper (symbol)
  (class-wrapper (find-class symbol)))




(declaim (ftype (function (T)
                  (values index index boolean boolean boolean list list))
                analyze-lambda-list))
(defun analyze-lambda-list (lambda-list)
  (declare (values nrequired noptional keysp restp allow-other-keys-p
                   keywords keyword-parameters))
  (flet ((parse-keyword-argument (arg)
	   (if (listp arg)
	       (if (listp (car arg))
		   (caar arg)
		   (make-keyword (car arg)))
	       (make-keyword arg))))
    (let ((nrequired 0)
	  (noptional 0)
	  (keysp nil)
	  (restp nil)
	  (allow-other-keys-p nil)
	  (keywords ())
	  (keyword-parameters ())
	  (state 'required))
      (declare (type index   nrequired noptional)
               (type boolean keysp restp allow-other-keys-p)
               (type list    keywords keyword-parameters))
      (dolist (x lambda-list)
	(if (memq x lambda-list-keywords)
	    (case x
	      (&optional         (setq state 'optional))
	      (&key              (setq keysp 't
				       state 'key))
	      (&allow-other-keys (setq allow-other-keys-p 't))
	      (&rest             (setq restp 't
				       state 'rest))
	      (&aux              (return t))
	      (otherwise
		(error "Encountered the non-standard lambda list keyword ~S." x)))
	    (ecase state
	      (required  (incf nrequired))
	      (optional  (incf noptional))
	      (key       (push (parse-keyword-argument x) keywords)
			 (push x keyword-parameters))
	      (rest      ()))))
      (values nrequired noptional keysp restp allow-other-keys-p
	      (reverse keywords)
	      (reverse keyword-parameters)))))

(defun keyword-spec-name (x)
  (let ((key (if (atom x) x (car x))))
    (if (atom key)
	(intern (symbol-name key) (find-package "KEYWORD"))
	(car key))))

(defun ftype-declaration-from-lambda-list (lambda-list #+cmu name)
  (multiple-value-bind (nrequired noptional keysp restp allow-other-keys-p
				  keys keyword-parameters)
      (analyze-lambda-list lambda-list)
    (declare (type index   nrequired noptional)
             (type boolean keysp restp #+cmu allow-other-keys-p)
             (type list    keys))
    (declare (ignore keyword-parameters #-cmu allow-other-keys-p))
    (let* (#+cmu (old (c::info function type name))
	   #+cmu (old-ftype (if (c::function-type-p old) old nil))
	   #+cmu (old-restp (and old-ftype (c::function-type-rest old-ftype)))
	   #+cmu (old-keys (and old-ftype
				(mapcar #'c::key-info-name
					(c::function-type-keywords old-ftype))))
	   #+cmu (old-keysp (and old-ftype (c::function-type-keyp old-ftype)))
	   #+cmu (old-allowp (and old-ftype (c::function-type-allowp old-ftype)))
	   (keywords #+cmu (union old-keys (mapcar #'keyword-spec-name keys))
		     #-cmu (mapcar #'keyword-spec-name keys)))
      (declare (type list keywords))
      `(function ,(append (make-list nrequired :initial-element 't)
			  (when (plusp noptional)
			    (append '(&optional)
				    (make-list noptional :initial-element 't)))
			  (when (or restp #+cmu old-restp)
			    '(&rest t))
			  (when (or keysp #+cmu old-keysp)
                            #+cmu
			    (append '(&key)
				    (mapcar #'(lambda (key)
						`(,key t))
					    keywords)
				    (when (or allow-other-keys-p #+cmu old-allowp)
				      '(&allow-other-keys)))
                            #-cmu
			    (append '(&key)
                                     (make-list (length keywords)
                                                :initial-element T))))
		 #-(or cmu kcl) T
		 #+(or cmu kcl) *))))

(defun proclaim-function (name lambda-list
                          &optional (return-type #+(or cmu kcl) '*
                                                 #-(or cmu kcl) T))
  (unless (function-ftype-declaimed-p name)
    (eval `(declaim (ftype (function
                              ,(mapcar #'(lambda (param)
                                           (if (memq param lambda-list-keywords)
                                               param
                                               T))
                                       lambda-list)
                              ,return-type)
                            ,name)))
    #+kcl (setf (get name 'compiler::proclaimed-closure) t)))

(defun proclaim-defgeneric (spec lambda-list)
  (when (consp spec)
    (setq spec (get-setf-function-name (cadr spec))))
  (unless (function-ftype-declaimed-p spec)
    (eval
      `(declaim (ftype ,(ftype-declaration-from-lambda-list lambda-list #+cmu spec)
                       ,spec)))
    #+kcl (setf (get spec 'compiler::proclaimed-closure) t)))

