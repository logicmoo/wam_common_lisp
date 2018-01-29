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
;;; This file defines PCL's interface to the LAP mechanism.
;;;
;;; The file is divided into two parts.  The first part defines the interface
;;; used by PCL to create abstract LAP code vectors.  PCL never creates lists
;;; that represent LAP code directly, it always calls this mechanism to do so.
;;; This provides a layer of error checking on the LAP code before it gets to
;;; the implementation-specific assembler.  Note that this error checking is
;;; syntactic only, but even so is useful to have.  Because of it, no specific
;;; LAP assembler should worry itself with checking the syntax of the LAP code.
;;;
;;; The second part of the file defines the LAP assemblers for each PCL port.
;;; These are included together in the same file to make it easier to change
;;; them all should some random change be made in the LAP mechanism.
;;;

(defvar *make-lap-closure-generator*)
(defvar *precompile-lap-closure-generator*)
(defvar *lap-in-lisp*)

(defun make-lap-closure-generator 
    (closure-variables arguments iregs vregs fvregs tregs lap-code)
  (funcall-function *make-lap-closure-generator*
	            closure-variables arguments iregs 
	            vregs fvregs tregs lap-code))

(defmacro precompile-lap-closure-generator 
    (cvars args i-regs v-regs fv-regs t-regs lap)
  (funcall-function *precompile-lap-closure-generator*
                    cvars args i-regs 
	            v-regs fv-regs t-regs lap))

(defmacro lap-in-lisp (cvars args iregs vregs fvregs tregs lap)
  (declare (ignore cvars args))
  `(locally (declare #.*optimize-speed*)
     ,(make-lap-prog iregs vregs fvregs tregs
		     (flatten-lap lap (opcode :label 'exit-lap-in-lisp)))))


;;;
;;; The following functions and macros are used by PCL when generating LAP
;;; code:
;;;
;;;  GENERATING-LAP
;;;  WITH-LAP-REGISTERS
;;;  ALLOCATE-REGISTER
;;;  DEALLOCATE-REGISTER
;;;  LAP-FLATTEN
;;;  OPCODE
;;;  OPERAND
;;; 
(eval-when (compile eval load)
(proclaim '(special *generating-lap*))		;CAR   - alist of free registers
						;CADR  - alist of allocated registers
						;CADDR - max reg number allocated
						;
						;in each alist, the entries have
						;the form:  (type . (:REG <n>))
						;
)

;;;
;;; This goes around the generation of any lap code.  <body> should return a lap
;;; code sequence, this macro will take care of converting that to a lap closure
;;; generator.
;;; 
(defmacro generating-lap (closure-variables arguments &body body)
  `(let* ((*generating-lap* (list () () -1)))
     (finalize-lap-generation nil ,closure-variables ,arguments (progn ,@body))))

(defmacro generating-lap-in-lisp (closure-variables arguments &body body)
  `(let* ((*generating-lap* (list () () -1)))
     (finalize-lap-generation t ,closure-variables ,arguments (progn ,@body))))

;;;
;;; Each register specification looks like:
;;;
;;;  (<var> <type> &key :reuse <other-reg>)
;;;  
(defmacro with-lap-registers (register-specifications &body body)
  ;;
  ;; Given that, for now, there is only one keyword argument and
  ;; that, for now, we do no error checking, we can be pretty
  ;; sleazy about how this works.
  ;;
  (flet ((make-allocations ()
	   (gathering1 (collecting)
	     (dolist (spec register-specifications)
	       (gather1
		 `(,(car spec) (or ,(cadddr spec) (allocate-register ',(cadr spec))))))))
	 (make-deallocations ()
	   (gathering1 (collecting)
	     (dolist (spec register-specifications)
	       (gather1
		 `(unless ,(cadddr spec) (deallocate-register ,(car spec))))))))
    `(let ,(make-allocations)
       (multiple-value-prog1 (progn ,@body)
			     ,@(make-deallocations)))))

(defun allocate-register (type)
  (destructuring-bind (free allocated) *generating-lap*
    (let ((entry (assoc type free)))
      (cond (entry
	     (setf (car *generating-lap*)  (delete entry free)
		   (cadr *generating-lap*) (cons entry allocated))
	     (cdr entry))
	    (t
	     (let ((new `(,type . (:reg ,(incf (the fixnum (caddr *generating-lap*)))))))
	       (setf (cadr *generating-lap*) (cons new allocated))
	       (cdr new)))))))

(defun deallocate-register (reg)
  (let ((entry (rassoc reg (cadr *generating-lap*))))
    (unless entry (error "Attempt to free an unallocated register."))
    (push entry (car *generating-lap*))
    (setf (cadr *generating-lap*) (delete entry (cadr *generating-lap*)))))

(defvar *precompiling-lap* nil)

(defun finalize-lap-generation (in-lisp-p closure-variables arguments lap-code)
  (when (cadr *generating-lap*) (error "Registers still allocated when lap being finalized."))
  (let ((iregs ())
	(vregs ())
	(fvregs ())
	(tregs ()))
    (dolist (entry (car *generating-lap*))
      (ecase (car entry)
	(index  (push (caddr entry) iregs))
	(vector (push (caddr entry) vregs))
	(fixnum-vector (push (caddr entry) fvregs))
	((t)    (push (caddr entry) tregs))))
    (cond (in-lisp-p
	   `(lap-in-lisp ,closure-variables ,arguments ,iregs 
	                 ,vregs ,fvregs ,tregs ,lap-code))
	  (*precompiling-lap*
	   (values closure-variables arguments iregs 
		   vregs fvregs tregs lap-code))
	  (t
	   (make-lap-closure-generator
	     closure-variables arguments iregs 
	     vregs fvregs tregs lap-code)))))

(defun flatten-lap (&rest opcodes-or-sequences)
  (let ((result ()))
    (dolist (opcode-or-sequence opcodes-or-sequences result)
      (cond ((null opcode-or-sequence))
            ((not (consp (car opcode-or-sequence)))     ;its an opcode
             (setf result (append result (list opcode-or-sequence))))
            (t
             (setf result (append result opcode-or-sequence)))))))

(defmacro flattening-lap ()
  '(let ((result ()))
    (values #'(lambda (value) (push value result))
     #'(lambda () (apply #'flatten-lap (reverse result))))))



;;;
;;; This code deals with the syntax of the individual opcodes and operands.
;;; 
  
;;;
;;; The first two of these variables are documented to all ports.  They are
;;; lists of the symbols which name the lap opcodes and operands.  They can
;;; be useful to determine whether a port has implemented all the required
;;; opcodes and operands.
;;;
;;; The third of these variables is for use of the emitter only.
;;; 
(defvar *lap-operands* ())
(defvar *lap-opcodes*  ())
(defvar *lap-emitters* (make-hash-table :test #'eq :size 30))

(defun opcode (name &rest args)
  (let ((emitter (gethash name *lap-emitters*)))
    (if emitter
	(apply-function (symbol-function emitter) args)
	(error "No opcode named ~S." name))))

(defun operand (name &rest args)
  (let ((emitter (gethash name *lap-emitters*)))
    (if emitter
	(apply-function (symbol-function emitter) args)
	(error "No operand named ~S." name))))

(defmacro defopcode (name types)
  (let ((fn-name (symbol-append "LAP Opcode " name *the-pcl-package*))
	(lambda-list
	  (mapcar #'(lambda (x) (declare (ignore x)) (gensym)) types)))
    `(progn
       (eval-when (load eval) (load-defopcode ',name ',fn-name))
       (defun ,fn-name ,lambda-list
	 #+Genera (declare (sys:function-parent ,name defopcode))
	 (defopcode-1 ',name ',types ,@lambda-list)))))

(defmacro defoperand (name types)
  (let ((fn-name (symbol-append "LAP Operand " name *the-pcl-package*))
	(lambda-list
	  (mapcar #'(lambda (x) (declare (ignore x)) (gensym)) types)))
    `(progn
       (eval-when (load eval) (load-defoperand ',name ',fn-name))
       (defun ,fn-name ,lambda-list
	 #+Genera (declare (sys:function-parent ,name defoperand))
	 (defoperand-1 ',name ',types ,@lambda-list)))))

(defun load-defopcode (name fn-name)
  (if* (memq name *lap-operands*)
       (error "LAP opcodes and operands must have disjoint names.")
       (setf (gethash name *lap-emitters*) fn-name)
       (pushnew name *lap-opcodes*)))

(defun load-defoperand (name fn-name)
  (if* (memq name *lap-opcodes*)
       (error "LAP opcodes and operands must have disjoint names.")
       (setf (gethash name *lap-emitters*) fn-name)
       (pushnew name *lap-operands*)))

(defun defopcode-1 (name operand-types &rest args)
  (iterate ((arg (list-elements args))
	    (type (list-elements operand-types)))
    (check-opcode-arg name arg type))
  (cons name (copy-list args)))

(defun defoperand-1 (name operand-types &rest args)
  (iterate ((arg (list-elements args))
	    (type (list-elements operand-types)))
    (check-operand-arg name arg type))
  (cons name (copy-list args)))

(defun check-opcode-arg (name arg type)
  (labels ((usual (x)
	     (and (consp arg) (eq (car arg) x)))
	   (check (x)
	     (ecase x	       
	       ((:reg :cdr :constant :iref :cvar :arg :lisp :lisp-variable) (usual x))
	       (:label (symbolp arg))
	       (:operand (and (consp arg) (memq (car arg) *lap-operands*))))))
    (unless (if (consp type)
		(if (eq (car type) 'or)
		    (some #'check (cdr type))
		    (error "What type is this?"))
		(check type))
      (error "The argument ~S to the opcode ~A is not of type ~S." arg name type))))

(defun check-operand-arg (name arg type)  
  (flet ((check (x)
	   (ecase x
	     (:symbol           (symbolp arg))
	     (:register-number  (and (integerp arg) (>= (the fixnum arg) 0)))
	     (:t                t)
	     (:reg              (and (consp arg) (eq (car arg) :reg)))
	     (:fixnum           (typep arg 'fixnum)))))
    (unless (if (consp type)
		(if (eq (car type) 'or)
		    (some #'check (cdr type))
		    (error "What type is this?"))
		(check type))
      (error "The argument ~S to the operand ~A is not of type ~S." arg name type))))



;;;
;;; The actual opcodes.
;;;
(defopcode :break ())				;For debugging only.  Not
(defopcode :beep  ())				;all ports are required to
(defopcode :print (:reg))			;implement this.


(defopcode :move (:operand (or :reg :iref :cdr :lisp-variable)))

(defopcode :eq     ((or :reg :constant) (or :reg :constant) :label))
(defopcode :neq    ((or :reg :constant) (or :reg :constant) :label))
(defopcode :fix=   ((or :reg :constant) (or :reg :constant) :label))
(defopcode :izerop (:reg :label))

(defopcode :std-instance-p       (:reg :label))
(defopcode :fsc-instance-p       (:reg :label))
(defopcode :built-in-instance-p  (:reg :label))
(defopcode :structure-instance-p (:reg :label))
#+pcl-user-instances
(defopcode :user-instance-p      (:reg :label))

(defopcode :jmp      ((or :reg :constant)))

(defopcode :label  (:label))
(defopcode :go     (:label))

(defopcode :return ((or :reg :constant)))

(defopcode :exit-lap-in-lisp ())

;;;
;;; The actual operands.
;;;
(defoperand :reg  (:register-number))
(defoperand :cvar (:symbol))
(defoperand :arg  (:symbol))

(defoperand :cdr  (:reg))

(defoperand :constant (:t))

(defoperand :std-wrapper       (:reg))
(defoperand :fsc-wrapper       (:reg))
(defoperand :built-in-wrapper  (:reg))
(defoperand :structure-wrapper (:reg))
(defoperand :other-wrapper     (:reg))
(defoperand :built-in-or-structure-wrapper (:reg))
#+pcl-user-instances
(defoperand :user-wrapper      (:reg))

(defoperand :std-slots (:reg))
(defoperand :fsc-slots (:reg))
#+pcl-user-instances
(defoperand :user-slots (:reg))

(defoperand :wrapper-cache-number-vector (:reg))

(defoperand :cref (:reg :fixnum))

(defoperand :iref (:reg :reg))
(defoperand :iset (:reg :reg :reg))

(defoperand :i1+     (:reg))
(defoperand :i+      (:reg :reg))
(defoperand :i-      (:reg :reg))
(defoperand :ilogand (:reg :reg))
(defoperand :ilogxor (:reg :reg))
(defoperand :ishift  (:reg :fixnum))

(defoperand :lisp (:t))
(defoperand :lisp-variable (:symbol))



;;;
;;; LAP tests (there need to be a lot more of these)
;;;
#|
(defun make-lap-test-closure-1 (result)
  #'(lambda (arg1)
      (declare (pcl-fast-call))
      (declare (ignore arg1))
      result))

(defun make-lap-test-closure-2 (result)
  #'(lambda (arg1 arg2)
      (declare (pcl-fast-call))
      (declare (ignore arg1 arg2))
      result))

(eval-when (eval)
  (compile 'make-lap-test-closure-1)
  (compile 'make-lap-test-closure-2))

(proclaim '(special lap-win lap-lose))
(eval-when (load eval)
  (setq lap-win (make-lap-test-closure-1 'win)
	lap-lose (make-lap-test-closure-1 'lose)))

(defun lap-test-1 ()
  (let* ((cg (generating-lap '(cache)
			     '(arg)
	       (with-lap-registers ((i0 index)
				    (v0 vector)
				    (t0 t))
		 (flatten-lap 
		   (opcode :move (operand :cvar 'cache) v0)
		   (opcode :move (operand :arg 'arg) i0)
		   (opcode :move (operand :iref v0 i0) t0)
		   (opcode :jmp t0)))))
	 
	 (cache (make-array 32))
	 (closure (funcall cg cache))
	 (fn0 (make-lap-test-closure-1 'fn0))
	 (fn1 (make-lap-test-closure-1 'fn1))
	 (fn2 (make-lap-test-closure-1 'fn2))
	 (in0 (index-value->index 2))
	 (in1 (index-value->index 10))
	 (in2 (index-value->index 27)))
    
    (setf (svref cache (index->index-value in0)) fn0
	  (svref cache (index->index-value in1)) fn1
	  (svref cache (index->index-value in2)) fn2)
    
    (unless (and (eq (funcall closure in0) 'fn0)
		 (eq (funcall closure in1) 'fn1)
		 (eq (funcall closure in2) 'fn2))
      (error "LAP TEST 1 failed."))))

(defun lap-test-2 ()            
  (let* ((cg (generating-lap '(cache mask) 
			     '(arg)
	       (with-lap-registers ((i0 index)
				    (i1 index)
				    (i2 index)
				    (v0 vector)
				    (t0 t))

		 (flatten-lap		  
		   (opcode :move (operand :cvar 'cache) v0)
		   (opcode :move (operand :arg 'arg) i0)
		   (opcode :move (operand :cvar 'mask) i1)
		   (opcode :move (operand :ilogand i0 i1) i2)
		   (opcode :move (operand :iref v0 i2) t0)
		   (opcode :jmp t0)))))
	 (cache (make-array 32))
	 (mask #b00110)
	 (closure (funcall cg cache mask))
	 (in0 (index-value->index #b00010))
	 (in1 (index-value->index #b01010))
	 (in2 (index-value->index #b10011)))
    (fill cache lap-lose)
    (setf (svref cache (index->index-value in0)) lap-win)
    
    (unless (and (eq (funcall closure in0) 'win)
		 (eq (funcall closure in1) 'win)
		 (eq (funcall closure in2) 'win))
      (error "LAP TEST 2 failed."))))

(defun lap-test-3 ()            
  (let* ((cg (generating-lap '(addend) '(arg)
	       (with-lap-registers
		 ((i0 index)
		  (i1 index)
		  (i2 index))

		 (flatten-lap		  
		   (opcode :move (operand :cvar 'addend) i0)
		   (opcode :move (operand :arg 'arg) i1)
		   (opcode :move (operand :i+ i0 i1) i2)
		   (opcode :return i2)))))
	 (closure (funcall cg (index-value->index 5))))
    
    (unless (= (index->index-value (funcall closure (index-value->index 2))) 7)
      (error "LAP TEST 3 failed."))))

(defun lap-test-4 ()            
  (let* ((cg (generating-lap '(winner loser) '(arg)
	       (with-lap-registers ((t0 t))
		 (flatten-lap
		   (opcode :move (operand :arg 'arg) t0)
		   (opcode :eq t0 (operand :constant 'foo) 'win)
		   (opcode :move (operand :cvar 'loser) t0)
		   (opcode :jmp t0)
		   (opcode :label 'win)
		   (opcode :move (operand :cvar 'winner) t0)
		   (opcode :jmp t0)))))
	 (closure (funcall cg #'true #'false)))
    (unless (and (eq (funcall closure 'foo) 't)
		 (eq (funcall closure 'bar) 'nil))
      (error "LAP TEST 4 failed."))))

(defun lap-test-5 ()            
  (let* ((cg (generating-lap '(array) '(arg)
	       (with-lap-registers ((r0 vector)
				    (r1 t)
				    (r2 index))
		 (flatten-lap
		   (opcode :move (operand :cvar 'array) r0)
		   (opcode :move (operand :arg 'arg) r1)
		   (opcode :move (operand :constant (index-value->index 0)) r2)
		   (opcode :move r1 (operand :iref r0 r2))
		   (opcode :return r1)))))
	 (array (make-array 1))
	 (closure (funcall cg array)))
    (unless (and (=  (funcall closure 1)    (svref array 0))
		 (eq (funcall closure 'foo) (svref array 0)))
      (error "LAP TEST 5 failed."))))

|#

