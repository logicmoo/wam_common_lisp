;;; -*- Mode:LISP; Package:(PCL LISP 1000); Base:10; Syntax:Common-lisp -*-
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
;;; This is the Lucid lisp version of the file portable-low.
;;;
;;; Lucid:               (415)329-8400
;;; 

(in-package 'pcl)

;;; First, import some necessary "internal" or Lucid-specific symbols

(eval-when (eval compile load)

(#-LCL3.0 progn #+LCL3.0 lcl:handler-bind 
    #+LCL3.0 ((lcl:warning #'(lambda (condition)
			       (declare (ignore condition))
			       (lcl:muffle-warning))))
(let ((importer
        #+LCL3.0 #'sys:import-from-lucid-pkg
	#-LCL3.0 (let ((x (find-symbol "IMPORT-FROM-LUCID-PKG" "LUCID")))
		   (if (and x (fboundp x))
		       (symbol-function x)
		       ;; Only the #'(lambda (x) ...) below is really needed, 
		       ;;  but when available, the "internal" function 
		       ;;  'import-from-lucid-pkg' provides better checking.
		       #'(lambda (name)
			   (import (intern name "LUCID")))))))
  ;;
  ;; We need the following "internal", undocumented Lucid goodies:
  (mapc importer '("%POINTER" "DEFSTRUCT-SIMPLE-PREDICATE"
		   #-LCL3.0 "LOGAND&" "%LOGAND&" #+VAX "LOGAND&-VARIABLE"))

  ;;
  ;; For without-interrupts.
  ;; 
  #+LCL3.0
  (mapc importer '("*SCHEDULER-WAKEUP*" "MAYBE-CALL-SCHEDULER"))

  ;;
  ;; We import the following symbols, because in 2.1 Lisps they have to be
  ;;  accessed as SYS:<foo>, whereas in 3.0 lisps, they are homed in the
  ;;  LUCID-COMMON-LISP package.
  (mapc importer '("ARGLIST" "NAMED-LAMBDA" "*PRINT-STRUCTURE*"))
  ;;
  ;; We import the following symbols, because in 2.1 Lisps they have to be
  ;;  accessed as LUCID::<foo>, whereas in 3.0 lisps, they have to be
  ;;  accessed as SYS:<foo>
  (mapc importer '(
		   "NEW-STRUCTURE"   	"STRUCTURE-REF"
		   "STRUCTUREP"         "STRUCTURE-TYPE"  "STRUCTURE-LENGTH"
		   "PROCEDUREP"     	"PROCEDURE-SYMBOL"
		   "PROCEDURE-REF" 	"SET-PROCEDURE-REF" 
		   ))
; ;;
; ;;  The following is for the "patch" to the general defstruct printer.
; (mapc importer '(
; 	           "OUTPUT-STRUCTURE" 	  "DEFSTRUCT-INFO"
;		   "OUTPUT-TERSE-OBJECT"  "DEFAULT-STRUCTURE-PRINT" 
;		   "STRUCTURE-TYPE" 	  "*PRINT-OUTPUT*"
;		   ))
  ;;
  ;; The following is for a "patch" affecting compilation of %logand&.
  ;; On APOLLO, Domain/CommonLISP 2.10 does not include %logand& whereas
  ;; Domain/CommonLISP 2.20 does; Domain/CommonLISP 2.20 includes :DOMAIN/OS
  ;; on *FEATURES*, so this conditionalizes correctly for APOLLO.
  #-(or (and APOLLO DOMAIN/OS) LCL3.0 VAX) 
  (mapc importer '("COPY-STRUCTURE"  "GET-FDESC"  "SET-FDESC"))
  
  nil))

;; end of eval-when

)
	

;;;
;;; Patch up for the fact that the PCL package creation in defsys.lisp
;;;  will probably have an explicit :use list ??
;;;
;;;  #+LCL3.0 (use-package *default-make-package-use-list*)




#+lcl4.0
(progn

(defvar *saved-compilation-speed* 3)

; the production compiler sometimes
; screws up vars within labels

(defmacro dont-use-production-compiler ()
  '(eval-when (compile)
     (setq *saved-compilation-speed* (if LUCID:*USE-SFC* 3 0))
     (proclaim '(optimize (compilation-speed 3)))))

(defmacro use-previous-compiler ()
  `(eval-when (compile)
     (proclaim '(optimize (compilation-speed ,*saved-compilation-speed*)))))

)

(defmacro %logand (x y)
  #-VAX `(%logand& ,x ,y)
  #+VAX `(logand&-variable ,x ,y))

;;; Fix for VAX LCL
#+VAX
(defun logand&-variable (x y)
  (logand&-variable x y))

;;; Fix for other LCLs
#-(or (and APOLLO DOMAIN/OS) LCL3.0 VAX)
(eval-when (compile load eval)

(let* ((logand&-fdesc (get-fdesc 'logand&))
       (%logand&-fdesc (copy-structure logand&-fdesc)))
  (setf (structure-ref %logand&-fdesc 0 t) '%logand&)
  (setf (structure-ref %logand&-fdesc 7 t) nil)
  (setf (structure-ref %logand&-fdesc 8 t) nil)
  (set-fdesc '%logand& %logand&-fdesc))

(eval-when (load)
  (defun %logand& (x y) (%logand& x y)))

(eval-when (eval)
  (compile '%logand& '(lambda (x y) (%logand& x y))))

);#-(or LCL3.0 (and APOLLO DOMAIN/OS) VAX)

;;;
;;; From: JonL
;;; Date: November 28th, 1988
;;; 
;;;  Here's a better attempt to do the without-interrupts macro for LCL3.0.
;;;  For the 2.1  release, maybe you should just ignore it (i.e, turn it 
;;;  into a PROGN and "take your chances") since there isn't a uniform way
;;;  to do inhibition.  2.1 has interrupts, but no multiprocessing.
;;;
;;;  The best bet for protecting the cache is merely to inhibit the
;;;  scheduler, since asynchronous interrupts are only run when "scheduled".
;;;  Of course, there may be other interrupts, which can cons and which 
;;;  could cause a GC; but at least they wouldn't be running PCL type code.
;;;
;;;  Note that INTERRUPTS-ON shouldn't arbitrarily enable scheduling again,
;;;  but rather simply restore it to the state outside the scope of the call
;;;  to WITHOUT-INTERRUPTS.  Note also that an explicit call to 
;;;  MAYBE-CALL-SHEDULER must be done when "turning interrupts back on", if
;;;  there are any interrupts/schedulings pending; at least the test to see
;;;  if any are pending is very fast.

#+LCL3.0
(defmacro without-interrupts (&body body)
  `(macrolet ((interrupts-on  ()
		`(when (null outer-scheduling-state)
		   (setq lcl:*inhibit-scheduling* nil)
		   (when *scheduler-wakeup* (maybe-call-scheduler))))
	      (interrupts-off () 
		'(setq lcl:*inhibit-scheduling* t)))
     (let ((outer-scheduling-state lcl:*inhibit-scheduling*))
       (prog1 (let ((lcl:*inhibit-scheduling* t)) . ,body)
	      (when (and (null outer-scheduling-state) *scheduler-wakeup*)
		(maybe-call-scheduler))))))


;;; The following should override the definitions provided by lucid-low.
;;;
#+(or LCL3.0 (and APOLLO DOMAIN/OS))
(defstruct-simple-predicate  std-instance std-instance-p)


(defun set-function-name-1 (fn new-name ignore)
  (declare (ignore ignore))
  (if (not (procedurep fn))
      (error "~S is not a procedure." fn)
      (if (compiled-function-p fn)
	  ;; This is one of:
	  ;;   compiled-function, funcallable-instance, compiled-closure
	  ;;   or a macro.
	  ;; So just go ahead and set its name.
	  ;; Only change the name when necessary: maybe it is read-only.
	  (unless (eq new-name (procedure-ref fn procedure-symbol))
	    (set-procedure-ref fn procedure-symbol new-name))
	  ;; This is an interpreted function.
	  ;; Seems like any number of different things can happen depending
	  ;; vaguely on what release you are running.  Try to do something
	  ;; reasonable.
	  (let ((symbol (procedure-ref fn procedure-symbol)))
	    (cond ((symbolp symbol)
		   ;; In fact, this is the name of the procedure.
		   ;; Just set it.
		   (set-procedure-ref fn procedure-symbol new-name))
		  ((and (listp symbol)
			(eq (car symbol) 'lambda))
		   (setf (car symbol) 'named-lambda
			 (cdr symbol) (cons new-name (cdr symbol))))
		  ((eq (car symbol) 'named-lambda)
		   (setf (cadr symbol) new-name))))))		  
  fn)

(defun function-arglist (fn)
  (arglist fn))

  ;;   
;;;;;; printing-random-thing-internal
  ;;
(defun printing-random-thing-internal (thing stream)
  (format stream "~O" (%pointer thing)))


;;;
;;; 16-Feb-90 Jon L White
;;;
;;; A Patch provide specifically for the benefit of PCL, in the Lucid 3.0
;;;  release environment.  This adds type optimizers for FUNCALL so that
;;;  forms such as:
;;;
;;;     (FUNCALL (THE PROCEDURE F) ...)
;;;
;;;  and:
;;;
;;;     (LET ((F (Frobulate)))
;;;       (DECLARE (TYPE COMPILED-FUNCTION F))
;;;       (FUNCALL F ...))
;;;
;;;  will just jump directly to the procedure code, rather than waste time
;;;  trying to coerce the functional argument into a procedure.
;;;


(in-package "LUCID")


;;; (DECLARE-MACHINE-CLASS COMMON)
(set-up-compiler-target 'common)


(set-function-descriptor 'FUNCALL
  :TYPE  'LISP
  :PREDS 'NIL
  :EFFECTS 'T
  :OPTIMIZER  #'(lambda (form &optional environment) 
		  (declare (ignore form environment))
		  (let* ((fun (second form))
			 (lambdap (and (consp fun) 
				       (eq (car fun) 'function)
				       (consp (second fun))
				       (memq (car (second fun))
					     '(lambda internal-lambda)))))
		    (if (not lambdap) 
			form
			(alphatize 
			  (cons (second fun) (cddr form)) environment))))
  :FUNCTIONTYPE '(function (function &rest t) (values &rest t))
  :TYPE-DISPATCH `(((PROCEDURE &REST T) (VALUES &REST T)
		    ,#'(lambda (anode fun &rest args) 
			 (declare (ignore anode fun args))
			 `(FAST-FUNCALL ,fun ,@args)))
		   ((COMPILED-FUNCTION &REST T)  (VALUES &REST T)
		    ,#'(lambda (anode fun &rest args) 
			 (declare (ignore anode fun args))
			 `(FAST-FUNCALL ,fun ,@args))))
  :LAMBDALIST '(FN &REST ARGUMENTS)
  :ARGS '(1 NIL)
  :VALUES '(0 NIL)
  )

(def-compiler-macro fast-funcall (&rest args &environment env)
  (if (COMPILER-OPTION-SET-P :READ-SAFETY ENV)
      `(FUNCALL-SUBR . ,args)
      `(&FUNCALL . ,args)))



(setf (symbol-function 'funcall-subr) #'funcall)


;;; (UNDECLARE-MACHINE-CLASS)
(restore-compiler-params)


(in-package 'pcl)

(pushnew :structure-wrapper *features*)
(pushnew :structure-functions *features*)

(defvar *structure-type* nil)
(defvar *structure-length* nil)

(defun known-structure-type-p (type)
  (declare (special lucid::*defstructs*))
  (let ((s-data (gethash type lucid::*defstructs*)))
    (or (and s-data 
	     (eq 'structure (structure-ref s-data 1 'defstruct))) ; type - Fix this
	(and type (eq *structure-type* type)))))

(defun structure-type-included-type-name (type)
  (declare (special lucid::*defstructs*))
  (let ((s-data (gethash type lucid::*defstructs*)))
    (and s-data (structure-ref s-data 6 'defstruct)))) ; include - Fix this

(defun structure-type-slot-description-list (type)
  (declare (special lucid::*defstructs*))
  (let ((s-data (gethash type lucid::*defstructs*)))
    (if s-data
	(nthcdr (the index
                     (let ((include (structure-ref s-data 6 'defstruct)))
		       (if include
		           (let ((inc-s-data (gethash include lucid::*defstructs*)))
		     	     (if inc-s-data
			         (length (structure-ref inc-s-data 7 'defstruct))
			         0))
		           0)))
		(map
                 'list
		 #'(lambda (slotd)
		     (let* ((slot-name (system:structure-ref slotd 0 'lucid::defstruct-slot))
			    (position (system:structure-ref slotd 1 'lucid::defstruct-slot))
			    (accessor (system:structure-ref slotd 2 'lucid::defstruct-slot))
			    (read-only-p (system:structure-ref slotd 5 'lucid::defstruct-slot)))
		       (list slot-name accessor
			     #'(lambda (x)
			         (system:structure-ref x position type))
			     (unless read-only-p
			       #'(lambda (v x)
			           (setf (system:structure-ref x position type)
				         v))))))
		 (structure-ref s-data 7 'defstruct))) ; slots  - Fix this
	(let ((result (make-list (the index *structure-length*))))
	  (dotimes (i *structure-length* result)
	    (let* ((name (format nil "SLOT~D" i))
		   (slot-name (intern name (or (symbol-package type) *package*)))
		   (i i))
	      (setf (elt result i) (list slot-name nil
					 #'(lambda (x)
					     (system:structure-ref x i type))
					 nil))))))))

(defun structure-slotd-name (slotd)
  (first slotd))

(defun structure-slotd-accessor-symbol (slotd)
  (second slotd))

(defun structure-slotd-reader-function (slotd)
  (third slotd))

(defun structure-slotd-writer-function (slotd)
  (fourth slotd))
