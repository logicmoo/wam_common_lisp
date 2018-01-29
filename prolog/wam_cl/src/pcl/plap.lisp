;;;-*-Mode: LISP; Package: PCL; Base:10; Syntax:Common-lisp -*-
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
;;; The portable implementation of the LAP assembler.
;;;
;;; The portable implementation of the LAP assembler works by translating
;;; LAP code back into Lisp code and then compiling that Lisp code.  Note
;;; that this implementation is actually going to get a lot of use.  Some
;;; implementations (KCL) won't implement a native LAP assembler at all.
;;; Other implementations may not implement native LAP assemblers for all
;;; of their ports.  All of this implies that this portable LAP assembler
;;; needs to generate the best code it possibly can.
;;; 


;;;
;;; 
;;;

(defmacro lap-case (operand &body cases)
  (once-only (operand)
    `(ecase (car ,operand)
       ,@(mapcar #'(lambda (case)
		     `(,(car case)
		       (apply #'(lambda ,(cadr case) ,@(cddr case))
			      (cdr ,operand))))
		 cases))))

(defvar *lap-args*)
(defvar *lap-rest-p*)
(defvar *lap-i-regs*)
(defvar *lap-v-regs*)
(defvar *lap-fv-regs*)
(defvar *lap-t-regs*)

(defvar *lap-optimize-declaration* '#.*optimize-speed*)


(eval-when (load eval)
  (setq *make-lap-closure-generator*
	#'(lambda (closure-var-names arg-names index-regs 
		   vector-regs fixnum-vector-regs t-regs lap-code)
	    (compile-lambda
	      (make-lap-closure-generator-lambda
		closure-var-names arg-names index-regs 
		vector-regs fixnum-vector-regs t-regs lap-code)))

	*precompile-lap-closure-generator*
	#'(lambda (cvars args i-regs v-regs fv-regs t-regs lap)
	    `(function
	       ,(make-lap-closure-generator-lambda cvars args i-regs 
		 v-regs fv-regs t-regs lap)))
	*lap-in-lisp*
	#'(lambda (cvars args iregs vregs fvregs tregs lap)
	    (declare (ignore cvars args))
	    (make-lap-prog
	      iregs vregs fvregs tregs 
	      (flatten-lap lap ;(opcode :label 'exit-lap-in-lisp)
			   )))))

(defun make-lap-closure-generator-lambda (cvars args i-regs v-regs fv-regs t-regs lap)
  (let* ((rest (memq '&rest args))
	 (ldiff (and rest (ldiff args rest))))
    (when rest (setq args (append ldiff '(&rest .lap-rest-arg.))))
    (let* ((*lap-args* (if rest ldiff args))
	   (*lap-rest-p* (not (null rest))))
      `(lambda ,cvars
	 #'(lambda ,args
	     #-CMU (declare ,*lap-optimize-declaration*)
	     #-CMU ,(make-lap-prog-internal i-regs v-regs fv-regs t-regs lap)
	     #+CMU
             ;;
             ;; Use LOCALLY instead of a declare on the lambda so that we don't
             ;; suppress arg count checking...
             (locally (declare ,*lap-optimize-declaration*)
	       ,(make-lap-prog-internal i-regs v-regs fv-regs t-regs lap)))))))

(defun make-lap-prog (i-regs v-regs fv-regs t-regs lap)
  (let* ((*lap-args* 'lap-in-lisp)
	 (*lap-rest-p* 'lap-in-lisp))
    (make-lap-prog-internal i-regs v-regs fv-regs t-regs lap)))

(defun make-lap-prog-internal (i-regs v-regs fv-regs t-regs lap)
  (let* ((*lap-i-regs* i-regs)
	 (*lap-v-regs* v-regs)
	 (*lap-fv-regs* fv-regs)
	 (*lap-t-regs* t-regs)
	 (code (mapcar #'lap-opcode lap)))
    `(prog ,(mapcar #'(lambda (reg)
			`(,(lap-reg reg)
			  ,(lap-reg-initial-value-form reg)))
		    (append i-regs v-regs fv-regs t-regs))
	   (declare (type index ,@(mapcar #'lap-reg *lap-i-regs*))
		    (type simple-vector ,@(mapcar #'lap-reg *lap-v-regs*))
		    (type #+structure-wrapper cache-number-vector
		          #-structure-wrapper (simple-array fixnum)
		          ,@(mapcar #'lap-reg *lap-fv-regs*))
	            #-cmu ,*lap-optimize-declaration*)
	   ,.code)))

(defvar *empty-vector* '#())
(defvar *empty-fixnum-vector*
  (make-array 8 :initial-element 0))
 
(defun lap-reg-initial-value-form (reg)
  (cond ((memq reg *lap-i-regs*) 0)
        ((memq reg *lap-v-regs*) '*empty-vector*)
        ((memq reg *lap-fv-regs*) '*empty-fixnum-vector*)
        ((memq reg *lap-t-regs*) nil)
        (t
         (error "What kind of register is ~S?" reg))))

(defun lap-opcode (opcode)    
  (lap-case opcode
    (:move (from to)
     `(setf ,(lap-operand to) ,(lap-operand from)))
      
    ((:eq :neq :fix=) (arg1 arg2 label)
     `(when ,(lap-operands (ecase (car opcode)
			     (:eq 'eq) (:neq 'neq) (:fix= 'RUNTIME\ FIX=))
			   arg1
			   arg2)
	(go ,label)))

    ((:izerop) (arg label)
     `(when ,(lap-operands 'RUNTIME\ IZEROP arg)
	(go ,label)))

    (:std-instance-p (from label)
     `(when ,(lap-operands 'RUNTIME\ STD-INSTANCE-P from) (go ,label)))
    #+pcl-user-instances
    (:user-instance-p (from label)
     `(when ,(lap-operands 'RUNTIME\ USER-INSTANCE-P from) (go ,label)))
    (:fsc-instance-p (from label)
     `(when ,(lap-operands 'RUNTIME\ FSC-INSTANCE-P from) (go ,label)))
    (:built-in-instance-p (from label)
     (declare (ignore from))
     `(when ,t (go ,label)))			                ;***
    (:structure-instance-p (from label)
     `(when ,(lap-operands 'RUNTIME\ STRUCTURE-INSTANCE-P from) (go ,label)))	;***
    
    (:jmp (fn)
     (if (eq *lap-args* 'lap-in-lisp)
	 (error "Can't do a :JMP in LAP-IN-LISP.")
	 `(return
	    ,(if *lap-rest-p*
		 `(RUNTIME\ APPLY ,(lap-operand fn) ,@*lap-args* .lap-rest-arg.)
		 `(RUNTIME\ FUNCALL ,(lap-operand fn) ,@*lap-args*)))))

    (:return (value)
     `(return ,(lap-operand value)))
      
    (:label (label) label)
    (:go   (label)  `(go ,label))

    (:exit-lap-in-lisp () `(go exit-lap-in-lisp))
    
    (:break ()      `(break))
    (:beep  ()      #+Genera`(zl:beep))
    (:print (val)   (lap-operands 'print val))
    ))

(defun lap-operand (operand)
  (lap-case operand
    (:reg (n) (lap-reg n))
    (:cdr (reg) (lap-operands 'cdr reg))
    ((:cvar :arg) (name) name)
    (:constant (c) `',c)
    ((:std-wrapper :fsc-wrapper :built-in-wrapper :structure-wrapper
      :built-in-or-structure-wrapper :std-slots :fsc-slots
      :wrapper-cache-number-vector
      #+pcl-user-instances :user-wrapper
      #+pcl-user-instances :user-slots)
     (x)
     (lap-operands (ecase (car operand)
		     (:std-wrapper       'RUNTIME\ STD-WRAPPER)
		     (:fsc-wrapper       'RUNTIME\ FSC-WRAPPER)
                     #+pcl-user-instances
		     (:user-wrapper      'RUNTIME\ USER-WRAPPER)
		     (:built-in-wrapper  'RUNTIME\ BUILT-IN-WRAPPER)
		     (:structure-wrapper 'RUNTIME\ STRUCTURE-WRAPPER)
		     (:built-in-or-structure-wrapper
		                         'RUNTIME\ BUILT-IN-OR-STRUCTURE-WRAPPER)
		     (:std-slots         'RUNTIME\ STD-SLOTS)
		     (:fsc-slots         'RUNTIME\ FSC-SLOTS)
                     #+pcl-user-instances
		     (:user-slots        'RUNTIME\ USER-SLOTS)
		     (:wrapper-cache-number-vector 
		      'RUNTIME\ WRAPPER-CACHE-NUMBER-VECTOR))
		   x))
    
     
    (:i1+     (index)         (lap-operands 'RUNTIME\ I1+ index))
    (:i+      (index1 index2) (lap-operands 'RUNTIME\ I+ index1 index2))
    (:i-      (index1 index2) (lap-operands 'RUNTIME\ I- index1 index2))
    (:ilogand (index1 index2) (lap-operands 'RUNTIME\ ILOGAND index1 index2))
    (:ilogxor (index1 index2) (lap-operands 'RUNTIME\ ILOGXOR index1 index2))
    
    (:iref    (vector index)       (lap-operands 'RUNTIME\ IREF vector index))
    (:iset    (vector index value) (lap-operands 'RUNTIME\ ISET vector index value))

    (:cref   (vector i)       `(RUNTIME\ SVREF ,(lap-operand vector) ,i))
    (:lisp-variable (symbol) symbol)
    (:lisp          (form)   form)
    ))

(defun lap-operands (fn &rest regs)
  (cons fn (mapcar #'lap-operand regs)))

(defun lap-reg (n) (intern (format nil "REG~D" n) *the-pcl-package*))


;;;
;;; Runtime Implementations of the operands and opcodes.
;;;
;;; In those ports of PCL which choose not to completely re-implement the
;;; LAP code generator, it may still be provident to consider reimplementing
;;; one or more of these to get the compiler to produce better code.  That
;;; is why they are split out.
;;; 
(proclaim '(declaration pcl-fast-call))

(defmacro RUNTIME\ FUNCALL (fn &rest args)
  `(method-function-funcall ,fn ,.args))

(defmacro RUNTIME\ APPLY (fn &rest args)
  `(method-function-apply ,fn ,.args))

(defmacro RUNTIME\ STD-WRAPPER (x)
  `(std-instance-wrapper ,x))

(defmacro RUNTIME\ FSC-WRAPPER (x)
  `(fsc-instance-wrapper ,x))

#+pcl-user-instances
(defmacro RUNTIME\ USER-WRAPPER (x)
  `(get-user-instance-wrapper ,x))

(defmacro RUNTIME\ BUILT-IN-WRAPPER (x)
  `(built-in-wrapper-of ,x))

(defmacro RUNTIME\ STRUCTURE-WRAPPER (x)
  `(built-in-or-structure-wrapper ,x))

(defmacro RUNTIME\ BUILT-IN-OR-STRUCTURE-WRAPPER (x)
  `(built-in-or-structure-wrapper ,x))

(defmacro RUNTIME\ STRUCTURE-INSTANCE-P (x)
  `(structure-instance-p ,x))

(defmacro RUNTIME\ STD-SLOTS (x)
  `(std-instance-slots (the std-instance ,x)))

(defmacro RUNTIME\ FSC-SLOTS (x)
  `(fsc-instance-slots ,x))

#+pcl-user-instances
(defmacro RUNTIME\ USER-SLOTS (x)
  `(get-user-instance-slots ,x))

(defmacro RUNTIME\ WRAPPER-CACHE-NUMBER-VECTOR (x)
  `(wrapper-cache-number-vector ,x))

(defmacro RUNTIME\ STD-INSTANCE-P (x)
  `(std-instance-p ,x))

(defmacro RUNTIME\ FSC-INSTANCE-P (x)
  `(fsc-instance-p ,x))

#+pcl-user-instances
(defmacro RUNTIME\ USER-INSTANCE-P (x)
  `(get-user-instance-p ,x))

(defmacro RUNTIME\ IZEROP (x)
  `(zerop (the index ,x)))

(defmacro RUNTIME\ FIX= (x y)
  `(= (the index ,x) (the index ,y)))

;;;
;;; These are the implementations of the index operands.  The portable
;;; assembler generates Lisp code that uses these macros.  Even though
;;; the variables holding the arguments and results have type declarations
;;; on them, we put type declarations in here.
;;;
;;; Some compilers are so stupid...
;;;
(defmacro RUNTIME\ IREF (vector index)
  `(svref (the simple-vector ,vector) (the index ,index)))

(defmacro RUNTIME\ ISET (vector index value)
  `(setf (svref (the simple-vector ,vector) (the index ,index)) ,value))

(defmacro RUNTIME\ SVREF (vector index)
  `(svref (the simple-vector ,vector) (the index ,index)))

(defmacro RUNTIME\ I+ (index1 index2)
  `(the index (+ (the index ,index1) (the index ,index2))))

(defmacro RUNTIME\ I- (index1 index2)  
  `(the index (- (the index ,index1) (the index ,index2))))

(defmacro RUNTIME\ I1+ (index)
  `(the index (1+ (the index ,index))))

(defmacro RUNTIME\ ILOGAND (index1 index2)
  #-Lucid `(the index (logand (the index ,index1) (the index ,index2)))
  #+Lucid `(%logand ,index1 ,index2))

(defmacro RUNTIME\ ILOGXOR (index1 index2)
  `(the index (logxor (the index ,index1) (the index ,index2))))

;;;
;;; In the portable implementation, indexes are just fixnums.
;;; 

(defconstant index-value-limit most-positive-fixnum)

(defun index-value->index (index-value) index-value)
(defun index->index-value (index) index)

(defun make-index-mask (cache-size line-size)
  (declare (type index cache-size line-size))
  (let ((cache-size-in-bits (floor (log cache-size 2)))
	(line-size-in-bits (floor (log line-size 2)))
	(mask 0))
    (declare (type index cache-size-in-bits line-size-in-bits mask))
    (dotimes (i cache-size-in-bits)
      (setq mask (the index (dpb 1 (byte 1 i) mask))))
    (dotimes (i line-size-in-bits)
      (setq mask (the index (dpb 0 (byte 1 i) mask))))
    mask))


