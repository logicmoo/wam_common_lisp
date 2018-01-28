;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;
;;; conditions.lsp
;;;
;;; Originally written by Kent M. Pitman of Symbolics, Inc. and
;;; distributed without any copyright.
;;; This is Version 18.
;;;
;;; KMP's disclaimer:
;;;
;;; This is a sample implementation. It is not in any way intended as the
;;; definition of any aspect of the condition system. It is simply an existence
;;; proof that the condition system can be implemented.
;;;

(in-package "CONDITIONS" :use '("LISP"))


(import 'si::abort)			; defined in "SYSTEM" by setdoc.lsp

(export '(*break-on-signals* *debugger-hook* signal
	  handler-case handler-bind ignore-errors define-condition make-condition
	  with-simple-restart restart-case restart-bind restart-name
	  restart-name find-restart compute-restarts invoke-restart
	  invoke-restart-interactively abort continue muffle-warning
	  store-value use-value invoke-debugger restart condition
	  warning serious-condition simple-condition simple-warning simple-error
	  simple-condition-format-string simple-condition-format-arguments
	  storage-condition stack-overflow storage-exhausted type-error
	  type-error-datum type-error-expected-type simple-type-error
	  program-error control-error stream-error stream-error-stream
	  end-of-file file-error file-error-pathname cell-error
	  unbound-variable undefined-function arithmetic-error
	  arithmetic-error-operation arithmetic-error-operands
	  package-error package-error-package
	  division-by-zero floating-point-overflow floating-point-underflow))

;;; ----------------------------------------------------------------------
;;; Unique Ids

(defmacro unique-id (obj)
  "Generates a unique integer ID for its argument."
  `(sys:pointer ,obj))


;;; Restarts

(defvar *restart-clusters* '())

(defun compute-restarts ()
  (copy-list (apply #'append *restart-clusters*)))

(defun restart-print (restart stream depth)
  (declare (ignore depth))
  (if *print-escape*
      (format stream "#<~s.~d>" (type-of restart) (unique-id restart))
      (restart-report restart stream)))

(defstruct (restart (:print-function restart-print))
  name
  function
  report-function
  interactive-function)

(defun restart-report (restart stream)
  (funcall (or (restart-report-function restart)
               (let ((name (restart-name restart)))
		 #'(lambda (stream)
		     (if name (format stream "~s" name)
			      (format stream "~s" restart)))))
           stream))

(defmacro restart-bind (bindings &body forms)
  `(let ((*restart-clusters* (cons (list ,@(mapcar #'(lambda (binding)
						       `(make-restart
							  :name     ',(car binding)
							  :function ,(cadr binding)
							  ,@(cddr binding)))
						   bindings))
				   *restart-clusters*)))
     ,@forms))

(defun find-restart (name)
  (dolist (restart-cluster *restart-clusters*)
    (dolist (restart restart-cluster)
      (when (or (eq restart name) (eq (restart-name restart) name))
	(return-from find-restart restart)))))
  
(defun invoke-restart (restart &rest values)
  (let ((real-restart (or (find-restart restart)
			  (error "Restart ~S is not active." restart))))
    (apply (restart-function real-restart) values)))

(defun invoke-restart-interactively (restart)
  (let ((real-restart (or (find-restart restart)
			  (error "Restart ~S is not active." restart))))
    (apply (restart-function real-restart)
	   (let ((interactive-function
		   (restart-interactive-function real-restart)))
	     (if interactive-function
		 (funcall interactive-function)
		 '())))))


(defmacro restart-case (expression &body clauses)
  (flet ((transform-keywords (&key report interactive)
	   (let ((keywords '()))
	     (when interactive
	       (setq keywords (list :interactive-function
				    `#',interactive)))
	     (when report
	       (setq keywords (list* :report-function
				     (if (stringp report)
					 `#'(lambda (stream)
					      (write-string ,report stream))
					 `#',report)
				     keywords)))
	     keywords)))
    (let ((block-tag (gensym))
	  (temp-var  (gensym))
	  (data (mapcar #'(lambda (clause)
			    (let (keywords (forms (cddr clause)))
			      (do ()
				  ((null forms))
				(if (keywordp (car forms))
				    (setq keywords (list* (car forms)
							  (cadr forms)
							  keywords)
					  forms (cddr forms))
				    (return)))
			      (list (car clause) 		;Name=0
				    (gensym) 			;Tag=1
				    (apply #'transform-keywords ;Keywords=2
					   keywords)
				    (cadr clause)		;BVL=3
				    forms))) 			;Body=4
			clauses)))
      `(block ,block-tag
	 (let ((,temp-var nil))
	   (tagbody
	     (restart-bind
	       ,(mapcar #'(lambda (datum)
			    (let ((name (nth 0 datum))
				  (tag  (nth 1 datum))
				  (keys (nth 2 datum)))
			      `(,name #'(lambda (&rest temp)
					  (setq ,temp-var temp)
					  (go ,tag))
				,@keys)))
			data)
	       (return-from ,block-tag ,expression))
	     ,@(mapcan #'(lambda (datum)
			   (let ((tag  (nth 1 datum))
				 (bvl  (nth 3 datum))
				 (body (nth 4 datum)))
			     (list tag
				   `(return-from ,block-tag
				      (apply #'(lambda ,bvl ,@body)
					     ,temp-var)))))
		       data)))))))

(defmacro with-simple-restart ((restart-name format-string
					     &rest format-arguments)
			       &body forms)
  `(restart-case (progn ,@forms)
     (,restart-name ()
        :report (lambda (stream)
		  (format stream ,format-string ,@format-arguments))
      (values nil t))))


;;; ----------------------------------------------------------------------
;;; Condition Data Type

(defun condition-print (condition stream depth)
  (declare (ignore depth))
  (if *print-escape*
      (format stream "#<~S.~D>" (type-of condition) (unique-id condition))
      (condition-report condition stream)))

(defstruct (condition :conc-name
                      (:constructor |constructor for condition|)
                      (:predicate nil)
                      (:print-function condition-print))
  (-dummy-slot- nil))

(eval-when (eval compile load)

(defmacro parent-type     (condition-type) `(get ,condition-type 'parent-type))
(defmacro slots           (condition-type) `(get ,condition-type 'slots))
(defmacro conc-name       (condition-type) `(get ,condition-type 'conc-name))
(defmacro report-function (condition-type) `(get ,condition-type 'report-function))
(defmacro make-function   (condition-type) `(get ,condition-type 'make-function))

);nehw-lave

(defun condition-report (condition stream)
  (do ((type (type-of condition) (parent-type type)))
      ((not type) (format stream "The condition ~A occurred."))
    (let ((reporter (report-function type)))
      (when reporter
        (funcall reporter condition stream)
        (return nil)))))

(setf (make-function   'condition) '|constructor for condition|)

(defun make-condition (type &rest slot-initializations)
  (let ((fn (make-function type)))
    (cond ((not fn) (error 'simple-type-error
			   :datum type
			   :expected-type '(satisfies make-function)
			   :format-string "Not a condition type: ~S"
			   :format-arguments (list type)))
          (t (apply fn slot-initializations)))))

(eval-when (eval compile load) ;Some utilities that are used at macro expansion time

(defmacro resolve-function (function expression resolver)
  `(cond ((and ,function ,expression)
          (cerror "Use only the :~A information."
                  "Only one of :~A and :~A is allowed."
                  ',function ',expression))
         (,expression
          (setq ,function ,resolver))))
         
(defun parse-new-and-used-slots (slots parent-type)
  (let ((new '()) (used '()))
    (dolist (slot slots)
      (if (slot-used-p (car slot) parent-type)
          (push slot used)
          (push slot new)))
    (values new used)))

(defun slot-used-p (slot-name type)
  (cond ((eq type 'condition) nil)
        ((not type) (error "The type ~S does not inherit from CONDITION." type))
        ((assoc slot-name (slots type)))
        (t
         (slot-used-p slot-name (parent-type type)))))

);nehw-lave

(defmacro define-condition (name (parent-type) slot-specs &rest options)
  (let ((constructor
	 (let ((*package* (find-package "CONDITIONS"))) ;Bind for the INTERN -and- the FORMAT
	   (intern (format nil "Constructor for ~S" name)))))
    (let ((slots (mapcar #'(lambda (slot-spec)
			     (if (atom slot-spec) (list slot-spec) slot-spec))
			 slot-specs)))
      (multiple-value-bind (new-slots used-slots)
          (parse-new-and-used-slots slots parent-type)
	(let ((conc-name-p     nil)
	      (conc-name       nil)
	      (report-function nil)
	      (documentation   nil))
	  (dolist (option options)
	    (case (car option)		; should be ecase
	      (:conc-name (setq conc-name-p t)
			  (setq conc-name (cadr option)))
	      (:report (setq report-function (if (stringp (cadr option))
						 `(lambda (stream)
						   (write-string ,(cadr option) stream))
						 (cadr option))))
	      (:documentation (setq documentation (cadr option)))
	      (otherwise (cerror "Ignore this DEFINE-CONDITION option."
				 "Invalid DEFINE-CONDITION option: ~S" option))))
	  (unless conc-name-p
	      (setq conc-name (intern (format nil "~A-" name) *PACKAGE*)))
          ;; The following three forms are compile-time side-effects. For now, they affect
          ;; the global environment, but with modified abstractions for PARENT-TYPE, SLOTS, 
          ;; and CONC-NAME, the compiler could easily make them local.
          (setf (parent-type name) parent-type)
          (setf (slots name)       slots)
          (setf (conc-name name)   conc-name)
          ;; Finally, the expansion ...
          `(PROGN (DEFSTRUCT (,NAME
                              (:CONSTRUCTOR ,CONSTRUCTOR)
                              (:PREDICATE NIL)
			      (:COPIER NIL)
                              (:PRINT-FUNCTION CONDITION-PRINT)
                              (:INCLUDE ,PARENT-TYPE ,@USED-SLOTS)
                              (:CONC-NAME ,CONC-NAME))
                    ,@NEW-SLOTS)
		  (SETF (DOCUMENTATION ',NAME 'TYPE) ',DOCUMENTATION)
                  (SETF (PARENT-TYPE ',NAME) ',PARENT-TYPE)
                  (SETF (SLOTS ',NAME) ',SLOTS)
                  (SETF (CONC-NAME ',NAME) ',CONC-NAME)
                  (SETF (REPORT-FUNCTION ',NAME) ,(IF REPORT-FUNCTION `#',REPORT-FUNCTION))
                  (SETF (MAKE-FUNCTION ',NAME) ',CONSTRUCTOR)
                  ',NAME))))))


#| For the moment, do not redefine these. Beppe.
(eval-when (eval compile load)

(defun accumulate-cases (macro-name cases list-is-atom-p)
  (do ((c cases (cdr c))
       (l '()))
      ((null c) (nreverse l))
    (let ((keys (caar c)))
      (cond ((atom keys)
	     (cond ((null keys))
		   ((member keys '(otherwise t))
		    (error "OTHERWISE is not allowed in ~S expressions."
			   macro-name))
		   (t (push keys l))))
	    (list-is-atom-p
	     (push keys l))
	    (t (setq l (append keys l)))))))

);nehw-lave

(defmacro ecase (keyform &rest cases)
  (let ((keys (accumulate-cases 'ecase cases nil)))
    `(case ,keyform
      ,@cases
      (otherwise
       (error 'case-failure :name 'ecase
	:datum ,keyform
	:expected-type '(member ,@keys)
	:possibilities ',keys))))))

(defmacro ccase (keyplace &rest cases)
  (let ((keys (accumulate-cases 'ccase cases nil))
	(tag1 (gensym))
	(tag2 (gensym)))
    `(block ,tag1
       (tagbody ,tag2
	 (return-from ,tag1
	   (case ,keyplace
	     ,@cases
	     (otherwise
	       (restart-case (error 'case-failure
				    :name 'ccase
				    :datum ,keyplace
				    :expected-type '(member ,@keys)
				    :possibilities ',keys)
		 (store-value (value)
		     :report (lambda (stream)
			       (format stream "Supply a new value of ~S."
				       ',keyplace))
		     :interactive read-evaluated-form
		   (setf ,keyplace value)
		   (go ,tag2))))))))))



(defmacro etypecase (keyform &rest cases)
  (let ((types (accumulate-cases 'etypecase cases t)))
    `(typecase ,keyform
      ,@cases
      (otherwise
       (error 'case-failure :name 'etypecase
	:datum ,keyform
	:expected-type '(or ,@types)
	:possibilities ',types)))))

(defmacro ctypecase (keyplace &rest cases)
  (let ((types (accumulate-cases 'ctypecase cases t))
	(tag1 (gensym))
	(tag2 (gensym)))
    `(block ,tag1
       (tagbody ,tag2
	 (return-from ,tag1
	   (typecase ,keyplace
	     ,@cases
	     (otherwise
	       (restart-case (error 'case-failure
				    :name 'ctypecase
				    :datum ,keyplace
				    :expected-type '(or ,@types)
				    :possibilities ',types)
		 (store-value (value)
		     :report (lambda (stream)
			       (format stream "Supply a new value of ~S."
				       ',keyplace))
		     :interactive read-evaluated-form
		   (setf ,keyplace value)
		   (go ,tag2))))))))))

|#


(defun assert-report (names stream)
  (format stream "Retry assertion")
  (if names
      (format stream " with new value~P for ~{~S~^, ~}."
	      (length names) names)
      (format stream ".")))

(defun assert-prompt (name value)
  (cond ((y-or-n-p "The old value of ~S is ~S.~
		  ~%Do you want to supply a new value? "
		   name value)
	 (format *query-io* "~&type a form to be evaluated:~%")
	 (flet ((read-it () (eval (read *query-io*))))
	   (if (symbolp name) ;Help user debug lexical variables
	       (progv (list name) (list value) (read-it))
	       (read-it))))
	(t value)))

(defun simple-assertion-failure (assertion)
  (error 'simple-type-error
	 :datum assertion
	 :expected-type nil		; This needs some work in next revision. -kmp
	 :format-string "The assertion ~S failed."
	 :format-arguments (list assertion)))

(defmacro assert (test-form &optional places datum &rest arguments)
  (let ((tag (gensym)))
    `(tagbody ,tag
       (unless ,test-form
	 (restart-case ,(if datum
			    `(error ,datum ,@arguments)
			    `(simple-assertion-failure ',test-form))
	   (continue ()
	       :report (lambda (stream) (assert-report ',places stream))
	     ,@(mapcar #'(lambda (place)
			   `(setf ,place (assert-prompt ',place ,place)))
		       places)
             (go ,tag)))))))



(defun read-evaluated-form ()
  (format *query-io* "~&Type a form to be evaluated:~%")
  (list (eval (read *query-io*))))

(defmacro check-type (place type &optional type-string)
  (let ((tag1 (gensym))
	(tag2 (gensym)))
    `(block ,tag1
       (tagbody ,tag2
	 (if (typep ,place ',type) (return-from ,tag1 nil))
	 (restart-case ,(if type-string
			    `(error "The value of ~S is ~S, ~
				     which is not ~A."
				    ',place ,place ,type-string)
			    `(error "The value of ~S is ~S, ~
				     which is not of type ~S."
				    ',place ,place ',type))
	   (store-value (value)
	       :report (lambda (stream)
			 (format stream "Supply a new value of ~S."
				 ',place))
	       :interactive read-evaluated-form
	     (setf ,place value)
	     (go ,tag2)))))))

(defvar *handler-clusters* nil)

(defmacro handler-bind (bindings &body forms)
  (unless (every #'(lambda (x) (and (listp x) (= (length x) 2))) bindings)
    (error "Ill-formed handler bindings."))
  `(let ((*handler-clusters* (cons (list ,@(mapcar #'(lambda (x) `(cons ',(car x) ,(cadr x)))
						   bindings))
				   *handler-clusters*)))
     ,@forms))

(defvar *break-on-signals* nil)

(defun signal (datum &rest arguments)
  (let ((condition (coerce-to-condition datum arguments 'simple-condition 'signal))
        (*handler-clusters* *handler-clusters*))
    (if (typep condition *break-on-signals*)
	(break "~A~%Break entered because of *BREAK-ON-SIGNALS*."
	       condition))
    (loop (if (not *handler-clusters*) (return))
          (let ((cluster (pop *handler-clusters*)))
	    (dolist (handler cluster)
	      (when (typep condition (car handler))
		(funcall (cdr handler) condition)
		(return nil) ;?
		))))
    nil))



;;; COERCE-TO-CONDITION
;;;  Internal routine used in ERROR, CERROR, BREAK, and WARN for parsing the
;;;  hairy argument conventions into a single argument that's directly usable 
;;;  by all the other routines.

(defun coerce-to-condition (datum arguments default-type function-name)
  (cond ((typep datum 'condition)
	 (if arguments
	     (cerror "Ignore the additional arguments."
		     'simple-type-error
		     :datum arguments
		     :expected-type 'null
		     :format-string "You may not supply additional arguments ~
				     when giving ~S to ~S."
		     :format-arguments (list datum function-name)))
	 datum)
        ((symbolp datum)                  ;roughly, (subtypep datum 'condition)
         (apply #'make-condition datum arguments))
        ((stringp datum)
	 (make-condition default-type
                         :format-string datum
                         :format-arguments arguments))
        (t
         (error 'simple-type-error
		:datum datum
		:expected-type '(or symbol string)
		:format-string "Bad argument to ~S: ~S"
		:format-arguments (list function-name datum)))))

(defun error (datum &rest arguments)
  (let ((condition (coerce-to-condition datum arguments 'simple-error 'error)))
    (signal condition)
    (invoke-debugger condition)))

(defun cerror (continue-string datum &rest arguments)
  (with-simple-restart (continue "~A" (apply #'format nil continue-string arguments))
    (apply #'error datum arguments))
  nil)

(defun break (&optional (format-string "Break") &rest format-arguments)
  (with-simple-restart (continue "Return from BREAK.")
    (invoke-debugger
      (make-condition 'simple-condition
		      :format-string    format-string
		      :format-arguments format-arguments)))
  nil)

(define-condition warning (condition) ())

(defun warn (datum &rest arguments)
  (let ((condition
	  (coerce-to-condition datum arguments 'simple-warning 'warn)))
    (check-type condition warning "a warning condition")
    (if *break-on-warnings*
	(break "~A~%Break entered because of *BREAK-ON-WARNINGS*."
	       condition))
    (restart-case (signal condition)
      (muffle-warning ()
	  :report "Skip warning."
	(return-from warn nil)))
    (format *error-output* "~&Warning:~%~A~%" condition)
    nil))



(define-condition serious-condition (condition) ())

(define-condition error (serious-condition) ())

(defun simple-condition-printer (condition stream)
  (apply #'format stream (simple-condition-format-string    condition)
	 		 (simple-condition-format-arguments condition)))

(define-condition simple-condition (condition) (format-string (format-arguments '()))
  (:conc-name internal-simple-condition-)
  (:report simple-condition-printer))

(define-condition simple-warning (warning) (format-string (format-arguments '()))
  (:conc-name internal-simple-warning-)
  (:report simple-condition-printer))

(define-condition simple-error (error) (format-string (format-arguments '()))
  (:conc-name internal-simple-error-)
  (:report simple-condition-printer))

(define-condition storage-condition (serious-condition) ())

(define-condition stack-overflow    (storage-condition) ())
(define-condition storage-exhausted (storage-condition) ())

(define-condition type-error (error) (datum expected-type))

(define-condition simple-type-error (type-error) (format-string (format-arguments '()))
  (:conc-name internal-simple-type-error-)
  (:report simple-condition-printer))

(define-condition case-failure (type-error) (name possibilities)
  (:report
    (lambda (condition stream)
      (format stream "~S fell through ~S expression.~%Wanted one of ~:S."
	      (type-error-datum condition)
	      (case-failure-name condition)
	      (case-failure-possibilities condition)))))

(defun simple-condition-format-string (condition)
  (etypecase condition
    (simple-condition  (internal-simple-condition-format-string  condition))
    (simple-warning    (internal-simple-warning-format-string    condition))
    (simple-type-error (internal-simple-type-error-format-string condition))
    (simple-error      (internal-simple-error-format-string      condition))))

(defun simple-condition-format-arguments (condition)
  (etypecase condition
    (simple-condition  (internal-simple-condition-format-arguments  condition))
    (simple-warning    (internal-simple-warning-format-arguments    condition))
    (simple-type-error (internal-simple-type-error-format-arguments condition))
    (simple-error      (internal-simple-error-format-arguments      condition))))

(define-condition program-error (error) ())

(define-condition control-error (error) ())

(define-condition stream-error (error) (stream))

(define-condition end-of-file (stream-error) ())

(define-condition file-error (error) (pathname))

(define-condition package-error (error) (pathname))



(define-condition cell-error (error) (name))

(define-condition unbound-variable (cell-error) ()
  (:report (lambda (condition stream)
	     (format stream "The variable ~S is unbound."
		     (cell-error-name condition)))))
  
(define-condition undefined-function (cell-error) ()
  (:report (lambda (condition stream)
	     (format stream "The function ~S is undefined."
		     (cell-error-name condition)))))

(define-condition arithmetic-error (error) (operation operands))

(define-condition division-by-zero         (arithmetic-error) ())
(define-condition floating-point-overflow  (arithmetic-error) ())
(define-condition floating-point-underflow (arithmetic-error) ())



(defmacro handler-case (form &rest cases)
  (let ((no-error-clause (assoc ':no-error cases)))
    (if no-error-clause
	(let ((normal-return (make-symbol "NORMAL-RETURN"))
	      (error-return  (make-symbol "ERROR-RETURN")))
	  `(block ,error-return
	     (multiple-value-call #'(lambda ,@(cdr no-error-clause))
	       (block ,normal-return
		 (return-from ,error-return
		   (handler-case (return-from ,normal-return ,form)
		     ,@(remove no-error-clause cases)))))))
	(let ((tag (gensym))
	      (var (gensym))
	      (annotated-cases (mapcar #'(lambda (case) (cons (gensym) case))
				       cases)))
	  `(block ,tag
	     (let ((,var nil))
	       (declare (ignore ,var))
	       (tagbody
		 (handler-bind ,(mapcar #'(lambda (annotated-case)
					    (list (cadr annotated-case)
						  `#'(lambda (temp)
						       ,@(if (caddr annotated-case)
							     `((setq ,var temp)))
						       (go ,(car annotated-case)))))
					annotated-cases)
			       (return-from ,tag ,form))
		 ,@(mapcan #'(lambda (annotated-case)
			       (list (car annotated-case)
				     (let ((body (cdddr annotated-case)))
				       `(return-from ,tag
					  ,(cond ((caddr annotated-case)
						  `(let ((,(caaddr annotated-case)
							  ,var))
						     ,@body))
						 ((not (cdr body))
						  (car body))
						 (t
						  `(progn ,@body)))))))
			   annotated-cases))))))))

(defmacro ignore-errors (&rest forms)
  `(handler-case (progn ,@forms)
     (error (condition) (values nil condition))))

(define-condition abort-failure (control-error) ()
  (:report "Abort failed."))

(defun abort          ()      (invoke-restart 'abort)
       			      (error 'abort-failure))
(defun continue       ()      (invoke-restart 'continue))
(defun muffle-warning ()      (invoke-restart 'muffle-warning))
(defun store-value    (value) (invoke-restart 'store-value value))
(defun use-value      (value) (invoke-restart 'use-value   value))
