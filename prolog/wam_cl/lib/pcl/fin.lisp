;;;-*-Mode:LISP; Package:(PCL Lisp 1000); Base:10; Syntax:Common-lisp -*-
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

  ;;   
;;;;;; FUNCALLABLE INSTANCES
  ;;

#|

Generic functions are instances with meta class funcallable-standard-class.
Instances with this meta class are called funcallable-instances (FINs for
short).  They behave something like lexical closures in that they have data
associated with them (which is used to store the slots) and are funcallable.
When a funcallable instance is funcalled, the function that is invoked is
called the funcallable-instance-function.  The funcallable-instance-function
of a funcallable instance can be changed.

This file implements low level code for manipulating funcallable instances.

It is possible to implement funcallable instances in pure Common Lisp.  A
simple implementation which uses lexical closures as the instances and a
hash table to record that the lexical closures are funcallable instances
is easy to write.  Unfortunately, this implementation adds significant
overhead:

   to generic-function-invocation (1 function call)
   to slot-access (1 function call or one hash table lookup)
   to class-of a generic-function (1 hash-table lookup)

In addition, it would prevent the funcallable instances from being garbage
collected.  In short, the pure Common Lisp implementation really isn't
practical.

Instead, PCL uses a specially tailored implementation for each Common Lisp and
makes no attempt to provide a purely portable implementation.  The specially
tailored implementations are based on the lexical closure's provided by that
implementation and are fairly short and easy to write.

Some of the implementation dependent code in this file was originally written
by someone in the employ of the vendor of that Common Lisp.  That code is
explicitly marked saying who wrote it.

|#

(in-package 'pcl)

;;;
;;; The first part of the file contains the implementation dependent code to
;;; implement funcallable instances.  Each implementation must provide the
;;; following functions and macros:
;;; 
;;;    ALLOCATE-FUNCALLABLE-INSTANCE-1 ()
;;;       should create and return a new funcallable instance.  The
;;;       funcallable-instance-data slots must be initialized to NIL.
;;;       This is called by allocate-funcallable-instance and by the
;;;       bootstrapping code.
;;;
;;;    FUNCALLABLE-INSTANCE-P (x)
;;;       the obvious predicate.  This should be an INLINE function.
;;;       it must be funcallable, but it would be nice if it compiled
;;;       open.
;;;
;;;    SET-FUNCALLABLE-INSTANCE-FUNCTION (fin new-value)
;;;       change the fin so that when it is funcalled, the new-value
;;;       function is called.  Note that it is legal for new-value
;;;       to be copied before it is installed in the fin, specifically
;;;       there is no accessor for a FIN's function so this function
;;;       does not have to preserve the actual new value.  The new-value
;;;       argument can be any funcallable thing, a closure, lambda
;;;       compiled code etc.  This function must coerce those values
;;;       if necessary.
;;;       NOTE: new-value is almost always a compiled closure.  This
;;;             is the important case to optimize.
;;;
;;;    FUNCALLABLE-INSTANCE-DATA-1 (fin data-name)
;;;       should return the value of the data named data-name in the fin.
;;;       data-name is one of the symbols in the list which is the value
;;;       of funcallable-instance-data.  Since data-name is almost always
;;;       a quoted symbol and funcallable-instance-data is a constant, it
;;;       is possible (and worthwhile) to optimize the computation of
;;;       data-name's offset in the data part of the fin.
;;;       This must be SETF'able.
;;;       

(eval-when (compile load eval)
(defconstant funcallable-instance-data
             '(wrapper slots)
  "These are the 'data-slots' which funcallable instances have so that
   the meta-class funcallable-standard-class can store class, and static
   slots in them.")
)

(defmacro funcallable-instance-data-position (data)
  (if (and (consp data)
           (eq (car data) 'quote))
      (or (position (cadr data) funcallable-instance-data :test #'eq)
          (progn
            (warn "Unknown funcallable-instance data: ~S." (cadr data))
            `(error "Unknown funcallable-instance data: ~S." ',(cadr data))))
      `(position ,data funcallable-instance-data :test #'eq)))

(proclaim '(notinline called-fin-without-function))
(defun called-fin-without-function (&rest args)
  (declare (ignore args))
  (error "Attempt to funcall a funcallable-instance without first~%~
          setting its funcallable-instance-function."))




;;;
;;; In Lucid Lisp, compiled functions and compiled closures have the same
;;; representation.  They are called procedures.  A procedure is a basically
;;; just a constants vector, with one slot which points to the CODE.  This
;;; means that constants and closure variables are intermixed in the procedure
;;; vector.
;;;
;;; This code was largely written by JonL@Lucid.com.  Problems with it should
;;; be referred to him.
;;; 
#+Lucid
(progn

(defconstant procedure-is-funcallable-instance-bit-position 10)

(defconstant fin-trampoline-fun-index lucid::procedure-literals)

(defconstant fin-size (+ fin-trampoline-fun-index
			 (length funcallable-instance-data)
			 1))

;;;
;;; The inner closure of this function will have its code vector replaced
;;;  by a hand-coded fast jump to the function that is stored in the 
;;;  captured-lexical variable.  In effect, that code is a hand-
;;;  optimized version of the code for this inner closure function.
;;;
(defun make-trampoline (function)
  (declare (optimize (speed 3) (safety 0)(compilation-speed 0)(space 0)))
  #'(lambda (&rest args)
      (apply function args)))

(eval-when (eval) 
  (compile 'make-trampoline)
  )


(defun binary-assemble (codes)
  (declare (list codes))
  (let* ((ncodes (length codes))
	 (code-vec #-LCL3.0 (lucid::new-code ncodes)
		   #+LCL3.0 (lucid::with-current-area 
				lucid::*READONLY-NON-POINTER-AREA*
			      (lucid::new-code ncodes))))
    (declare (type index ncodes))
    (do ((l codes (cdr l))
	 (i 0 (the index (1+ i))))
	((null l) nil)
      (declare (type index i))
      (setf (lucid::code-ref code-vec i) (car l)))
    code-vec))

;;;
;;; Egad! Binary patching!
;;; See comment following definition of MAKE-TRAMPOLINE -- this is just
;;;  the "hand-optimized" machine instructions to make it work.
;;;
(defvar *mattress-pad-code* 
	(binary-assemble
		#+MC68000
		'(#x2A6D #x11 #x246D #x1 #x4EEA #x5)
		#+SPARC
		(ecase (lucid::procedure-length #'lucid::false)
		  (5
		   '(#xFA07 #x6012 #xDE07 #x7FFE #x81C3 #xFFFE #x100 #x0))
		  (8
		   `(#xFA07 #x601E #xDE07 #x7FFE #x81C3 #xFFFE #x100 #x0)))
		#+(and BSP (not LCL3.0 ))
		'(#xCD33 #x11 #xCDA3 #x1 #xC19A #x5 #xE889)
		#+(and BSP LCL3.0)
		'(#x7733 #x7153 #xC155 #x5 #xE885)
		#+I386
		'(#x87 #xD2 #x8B #x76 #xE #xFF #x66 #xFE)
		#+VAX
		'(#xD0 #xAC #x11 #x5C #xD0 #xAC #x1 #x57 #x17 #xA7 #x5)
		#+PA
		'(#x4891 #x3C #xE461 #x6530 #x48BF #x3FF9)
                #+MIPS
                '(#x8FD4 #x1E #x2785 #x2EEF #xA0 #x8 #x14 #xF000)
                #-(or MC68000 SPARC BSP I386 VAX PA MIPS)
		'(0 0 0 0)))


(lucid::defsubst funcallable-instance-p (x)
  (and (lucid::procedurep x)
       (lucid::logbitp& procedure-is-funcallable-instance-bit-position
                        (lucid::procedure-ref x lucid::procedure-flags))))

(lucid::defsubst set-funcallable-instance-p (x)
  (if (not (lucid::procedurep x))
      (error "Can't make a non-procedure a fin.")
      (setf (lucid::procedure-ref x lucid::procedure-flags)
	    (logior (the index
                         (expt 2 (the index
                                      procedure-is-funcallable-instance-bit-position)))
		    (the index
			 (lucid::procedure-ref x lucid::procedure-flags))))))


(defun allocate-funcallable-instance-1 ()
  #+Prime
  (declare (notinline lucid::new-procedure))    ;fixes a bug in Prime 1.0 in
                                                ;which new-procedure expands
                                                ;incorrectly
  (let ((new-fin (lucid::new-procedure fin-size))
	(fin-index fin-size))
    (declare (type index fin-index)
	     (type lucid::procedure new-fin))
    (dotimes (i (length (the list funcallable-instance-data)) )
      ;; Initialize the new funcallable-instance.  As part of our contract,
      ;; we have to make sure the initial value of all the funcallable
      ;; instance data slots is NIL.
      (setf fin-index (the index (1- fin-index)))
      (setf (lucid::procedure-ref new-fin fin-index) nil))
    ;;
    ;; "Assemble" the initial function by installing a fast "trampoline" code;
    ;; 
    (setf (lucid::procedure-ref new-fin lucid::procedure-code)
	  *mattress-pad-code*)
    ;; Disable argcount checking in the "mattress-pad" code for
    ;;  ports that go through standardized trampolines
    #+PA (setf (sys:procedure-ref new-fin lucid::procedure-arg-count) -1)
    #+MIPS (progn
	     (setf (sys:procedure-ref new-fin lucid::procedure-min-args) 0)
	     (setf (sys:procedure-ref new-fin lucid::procedure-max-args) 
		   (the index call-arguments-limit)))
    ;; but start out with the function to be run as an error call.
    (setf (lucid::procedure-ref new-fin fin-trampoline-fun-index)
	  #'called-fin-without-function)
    ;; Then mark it as a "fin"
    (set-funcallable-instance-p new-fin)
    new-fin))

(defun set-funcallable-instance-function (fin new-value)
  (unless (funcallable-instance-p fin)
    (error "~S is not a funcallable-instance" fin))
  (if (lucid::procedurep new-value)
      (progn
	(setf (lucid::procedure-ref fin fin-trampoline-fun-index) new-value)
	fin)
      (progn 
	(unless (functionp new-value)
	  (error "~S is not a function." new-value))
	;; 'new-value' is an interpreted function.  Install a
	;; trampoline to call the interpreted function.
	(set-funcallable-instance-function fin
					   (make-trampoline new-value)))))

(defmacro funcallable-instance-data-1 (instance data)
  `(lucid::procedure-ref 
	   ,instance
	   (the index
		(- (the index (- (the index fin-size) 1))
		   (the index (funcallable-instance-data-position ,data))))))

);end of #+Lucid


;;;
;;; In Symbolics Common Lisp, a lexical closure is a pair of an environment
;;; and an ordinary compiled function.  The environment is represented as
;;; a CDR-coded list.  I know of no way to add a special bit to say that the
;;; closure is a FIN, so for now, closures are marked as FINS by storing a
;;; special marker in the last cell of the environment.
;;; 
;;;  The new structure of a fin is:
;;;     (lex-env lex-fun *marker* fin-data0 fin-data1)
;;;  The value returned by allocate is a lexical-closure pointing to the start
;;;  of the fin list.  Benefits are: no longer ever have to copy environments,
;;;  fins can be much smaller (5 words instead of 18), old environments never
;;;  get destroyed (so running dcodes dont have the lex env change from under
;;;  them any longer).
;;;
;;;  Most of the fin operations speed up a little (by as much as 30% on a
;;;  3650), at least one nasty bug is fixed, and so far at least I've not
;;;  seen any problems at all with this code.   - mike thome (mthome@bbn.com)
;;;      
#+(and Genera (not Genera-Release-8))
(progn

(defvar *funcallable-instance-marker* (list "Funcallable Instance Marker"))

(defun allocate-funcallable-instance-1 ()
  (let* ((whole-fin (make-list (+ 3 (length funcallable-instance-data))))
	 (new-fin (sys:%make-pointer-offset sys:dtp-lexical-closure
					    whole-fin
					    0)))
    ;;
    ;; note that we DO NOT turn the real lex-closure part of the fin into
    ;; a dotted pair, because (1) the machine doesn't care and (2) if we
    ;; did the garbage collector would reclaim everything after the lexical
    ;; function.
    ;; 
    (setf (sys:%p-contents-offset new-fin 2) *funcallable-instance-marker*)
    (setf (si:lexical-closure-function new-fin)
	  #'(lambda (ignore &rest ignore-them-too)
	      (declare (ignore ignore ignore-them-too))
	      (called-fin-without-function)))
    #+ignore
    (setf (si:lexical-closure-environment new-fin) nil)
    new-fin))

(scl:defsubst funcallable-instance-p (x)
  (declare (inline si:lexical-closure-p))
  (and (si:lexical-closure-p x)
       (= (sys:%p-cdr-code (sys:%make-pointer-offset sys:dtp-compiled-function x 1))
	  sys:cdr-next)
       (eq (sys:%p-contents-offset x 2) *funcallable-instance-marker*)))

(defun set-funcallable-instance-function (fin new-value)
  (cond ((not (funcallable-instance-p fin))
         (error "~S is not a funcallable-instance" fin))
        ((not (or (functionp new-value)
		  (and (consp new-value)
		       (eq (car new-value) 'si:digested-lambda))))
         (error "~S is not a function." new-value))
        ((and (si:lexical-closure-p new-value)
	      (compiled-function-p (si:lexical-closure-function new-value)))
	 (let ((env (si:lexical-closure-environment new-value))
	       (fn  (si:lexical-closure-function new-value)))
	   ;; we only have to copy the pointers!!
	   (setf (si:lexical-closure-environment fin) env
		 (si:lexical-closure-function fin)    fn)
;	   (dbg:set-env->fin env fin)
	   ))
        (t
         (set-funcallable-instance-function fin
                                            (make-trampoline new-value)))))

(defun make-trampoline (function)
  (declare #.*optimize-speed*)
  #'(lambda (&rest args)
      #+Genera (declare (dbg:invisible-frame :pcl-internals))
      (apply function args)))

(defmacro funcallable-instance-data-1 (fin data)
  `(sys:%p-contents-offset ,fin
			   (+ 3 (funcallable-instance-data-position ,data))))

(defsetf funcallable-instance-data-1 (fin data) (new-value)
  `(setf (sys:%p-contents-offset ,fin
				 (+ 3 (funcallable-instance-data-position ,data)))
	 ,new-value))

;;;
;;; Make funcallable instances print out properly.
;;; 
(defvar *print-lexical-closure* nil)

(defun pcl-print-lexical-closure (exp stream slashify-p &optional (depth 0))
  (declare (ignore depth))
  (declare (special *boot-state*))
  (if (or (eq *print-lexical-closure* exp)
	  (neq *boot-state* 'complete)
	  (eq (class-of exp) *the-class-t*))
      (let ((*print-lexical-closure* nil))
	(funcall (original-definition 'si:print-lexical-closure)
		 exp stream slashify-p))
      (let ((*print-escape* slashify-p)
	    (*print-lexical-closure* exp))
	(print-object exp stream))))

(unless (boundp '*boot-state*)
  (setq *boot-state* nil))

(redefine-function 'si:print-lexical-closure 'pcl-print-lexical-closure)

(defvar *function-name-level* 0)

(defun pcl-function-name (function &rest other-args)
  (if (and (eq *boot-state* 'complete)
	   (funcallable-instance-p function)
	   (generic-function-p function)
	   (<= *function-name-level* 2))
      (let ((*function-name-level* (1+ *function-name-level*)))
	(generic-function-name function))
      (apply (original-definition 'si:function-name) function other-args)))

(redefine-function 'si:function-name 'pcl-function-name)

(defun pcl-arglist (function &rest other-args)
  (let ((defn nil))
    (cond ((and (funcallable-instance-p function)
		(generic-function-p function))
	   (generic-function-pretty-arglist function))
	  ((and (sys:validate-function-spec function)
		(sys:fdefinedp function)
		(setq defn (sys:fdefinition function))
		(funcallable-instance-p defn)
		(generic-function-p defn))
	   (generic-function-pretty-arglist defn))
	  (t (apply (original-definition 'zl:arglist) function other-args)))))

(redefine-function 'zl:arglist 'pcl-arglist)


;;;
;;; This code is adapted from frame-lexical-environment and frame-function.
;;;
#||
dbg:
(progn

(defvar *old-frame-function*)

(defvar *inside-new-frame-function* nil)

(defun new-frame-function (frame)
  (let* ((fn (funcall *old-frame-function* frame))
	 (location (%pointer-plus frame #+imach (defstorage-size stack-frame) #-imach 0))
	 (env? #+3600 (location-contents location)
	       #+imach (%memory-read location :cycle-type %memory-scavenge)))
    (or (when (cl:consp env?)
	  (let ((l2 (last2 env?)))
	    (when (eq (car l2) '.this-is-a-dfun.)
	      (cadr l2))))
	fn)))

(defun pcl::doctor-dfun-for-the-debugger (gf dfun)
  (when (sys:lexical-closure-p dfun)
    (let* ((env (si:lexical-closure-environment dfun))
	   (l2 (last2 env)))
      (unless (eq (car l2) '.this-is-a-dfun.)
	(setf (si:lexical-closure-environment dfun)
	      (nconc env (list '.this-is-a-dfun. gf))))))
  dfun)

(defun last2 (l)
  (labels ((scan (2ago tail)
	     (if (null tail)
		 2ago
		 (if (cl:consp tail)
		     (scan (cdr 2ago) (cdr tail))
		     nil))))
    (and (cl:consp l)
	 (cl:consp (cdr l))
	 (scan l (cddr l)))))

(eval-when (load)
  (unless (boundp '*old-frame-function*)
    (setq *old-frame-function* #'frame-function)
    (setf (cl:symbol-function 'frame-function) 'new-frame-function)))

)
||#

);end of #+Genera



;;;
;;; In Genera 8.0, we use a real funcallable instance (from Genera CLOS) for this.
;;; This minimizes the subprimitive mucking around.
;;;
#+(and Genera Genera-Release-8)
(progn

(clos-internals::ensure-class
  'pcl-funcallable-instance
  :direct-superclasses '(clos-internals:funcallable-instance)
  :slots `((:name function
	    :initform #'(lambda (ignore &rest ignore-them-too)
			  (declare (ignore ignore ignore-them-too))
			  (called-fin-without-function))
	    :initfunction ,#'(lambda nil
			       #'(lambda (ignore &rest ignore-them-too)
				   (declare (ignore ignore ignore-them-too))
				   (called-fin-without-function))))
	   ,@(mapcar #'(lambda (slot) `(:name ,slot)) funcallable-instance-data))
  :metaclass 'clos:funcallable-standard-class)

(defun pcl-funcallable-instance-trampoline (extra-arg &rest args)
  (apply (sys:%instance-ref (clos-internals::%dispatch-instance-from-extra-argument extra-arg)
			    3)
	 args))

(defun allocate-funcallable-instance-1 ()
  (let ((fin (clos:make-instance 'pcl-funcallable-instance)))
    (setf (clos-internals::%funcallable-instance-function fin)
	  #'pcl-funcallable-instance-trampoline)
    (setf (clos-internals::%funcallable-instance-extra-argument fin)
	  (sys:%make-pointer sys:dtp-instance
			     (clos-internals::%funcallable-instance-extra-argument fin)))
    (setf (clos:slot-value fin 'clos-internals::funcallable-instance) fin)
    fin))

(scl:defsubst funcallable-instance-p (x)
  (and (sys:funcallable-instance-p x)
       (eq (clos-internals::%funcallable-instance-function x)
	   #'pcl-funcallable-instance-trampoline)))

(defun set-funcallable-instance-function (fin new-value)
  (setf (clos:slot-value fin 'function) new-value))

(defmacro funcallable-instance-data-1 (fin data)
  `(clos-internals:%funcallable-instance-ref
     ,fin (+ 4 (funcallable-instance-data-position ,data))))

(defsetf funcallable-instance-data-1 (fin data) (new-value)
  `(setf (clos-internals:%funcallable-instance-ref
	   ,fin (+ 4 (funcallable-instance-data-position ,data)))
	 ,new-value))

(clos:defmethod clos:print-object ((fin pcl-funcallable-instance) stream)
  (print-object fin stream))

(clos:defmethod clos-internals:debugging-information-function ((fin pcl-funcallable-instance))
  nil)

(clos:defmethod clos-internals:function-name-object ((fin pcl-funcallable-instance))
  (declare (special *boot-state*))
  (if (and (eq *boot-state* 'complete)
	   (generic-function-p fin))
      (generic-function-name fin)
      fin))

(clos:defmethod clos-internals:arglist-object ((fin pcl-funcallable-instance))
  (declare (special *boot-state*))
  (if (and (eq *boot-state* 'complete)
	   (generic-function-p fin))
      (generic-function-pretty-arglist fin)
      '(&rest args)))

);end of #+Genera



#+Cloe-Runtime
(progn

(defconstant funcallable-instance-closure-slots 5)
(defconstant funcallable-instance-closure-size
	     (+ funcallable-instance-closure-slots (length funcallable-instance-data) 1))

#-CLOE-Release-2 (progn

(defun allocate-funcallable-instance-1 ()
  (let ((data (system::make-funcallable-structure 'funcallable-instance
						  funcallable-instance-closure-size)))
    (setf (system::%trampoline-ref data funcallable-instance-closure-slots)
	  'funcallable-instance)
    (set-funcallable-instance-function
      data
      #'(lambda (&rest ignore-them-too)
	  (declare (ignore ignore-them-too))
	  (called-fin-without-function)))
    data))

(proclaim '(inline funcallable-instance-p))
(defun funcallable-instance-p (x)
  (and (typep x 'system::trampoline)
       (= (system::%trampoline-data-length x) funcallable-instance-closure-size)
       (eq (system::%trampoline-ref x funcallable-instance-closure-slots)
	   'funcallable-instance)))

(defun set-funcallable-instance-function (fin new-value)
  (when (not (funcallable-instance-p fin))
    (error "~S is not a funcallable-instance" fin))
  (etypecase new-value
    (system::trampoline
      (let ((length (system::%trampoline-data-length new-value)))
	(cond ((> length funcallable-instance-closure-slots)
	       (set-funcallable-instance-function
		 fin
		 #'(lambda (&rest args)
		     (declare (sys:downward-rest-argument))
		     (apply new-value args))))
	      (t
	       (setf (system::%trampoline-function fin)
		     (system::%trampoline-function new-value))
	       (dotimes (i length)
		 (setf (system::%trampoline-ref fin i)
		       (system::%trampoline-ref new-value i)))))))
    (compiled-function
      (setf (system::%trampoline-function fin) new-value))
    (function
      (set-funcallable-instance-function
	fin
	#'(lambda (&rest args)
	    (declare (sys:downward-rest-argument))
	    (apply new-value args))))))

(defmacro funcallable-instance-data-1 (fin data)
  `(system::%trampoline-ref ,fin (+ funcallable-instance-closure-slots
				    1 (funcallable-instance-data-position ,data))))

(defsetf funcallable-instance-data-1 (fin data) (new-value)
  `(setf (system::%trampoline-ref ,fin (+ funcallable-instance-closure-slots
					  1 (funcallable-instance-data-position ,data)))
	 ,new-value))

)

#+CLOE-Release-2 (progn

(defun allocate-funcallable-instance-1 ()
  (let ((data (si::cons-closure funcallable-instance-closure-size)))
    (setf (si::closure-ref data funcallable-instance-closure-slots) 'funcallable-instance)
    (set-funcallable-instance-function
      data
      #'(lambda (&rest ignore-them-too)
	  (declare (ignore ignore-them-too))
	  (error "Called a FIN without first setting its function.")))
    data))

(proclaim '(inline funcallable-instance-p))
(defun funcallable-instance-p (x)
  (and (si::closurep x)
       (= (si::closure-length x) funcallable-instance-closure-size)
       (eq (si::closure-ref x funcallable-instance-closure-slots) 'funcallable-instance)))

(defun set-funcallable-instance-function (fin new-value)
  (when (not (funcallable-instance-p fin))
    (error "~S is not a funcallable-instance" fin))
  (etypecase new-value
    (si::closure
      (let ((length (si::closure-length new-value)))
	(cond ((> length funcallable-instance-closure-slots)
	       (set-funcallable-instance-function
		 fin
		 #'(lambda (&rest args)
		     (declare (sys:downward-rest-argument))
		     (apply new-value args))))
	      (t
	       (setf (si::closure-function fin) (si::closure-function new-value))
	       (dotimes (i length)
		 (si::object-set fin (+ i 3) (si::object-ref new-value (+ i 3))))))))
    (compiled-function
      (setf (si::closure-function fin) new-value))
    (function
      (set-funcallable-instance-function
	fin
	#'(lambda (&rest args)
	    (declare (sys:downward-rest-argument))
	    (apply new-value args))))))

(defmacro funcallable-instance-data-1 (fin data)
  `(si::closure-ref ,fin (+ funcallable-instance-closure-slots
			    1 (funcallable-instance-data-position ,data))))

(defsetf funcallable-instance-data-1 (fin data) (new-value)
  `(setf (si::closure-ref ,fin (+ funcallable-instance-closure-slots
				  1 (funcallable-instance-data-position ,data)))
	 ,new-value))

)

)


;;;
;;;
;;; In Xerox Common Lisp, a lexical closure is a pair of an environment and
;;; CCODEP.  The environment is represented as a block.  There is space in
;;; the top 8 bits of the pointers to the CCODE and the environment to use
;;; to mark the closure as being a FIN.
;;;
;;; To help the debugger figure out when it has found a FIN on the stack, we
;;; reserve the last element of the closure environment to use to point back
;;; to the actual fin.
;;;
;;; Note that there is code in xerox-low which lets us access the fields of
;;; compiled-closures and which defines the closure-overlay record.  That
;;; code is there because there are some clients of it in that file.
;;;      
#+Xerox
(progn

;; Don't be fooled.  We actually allocate one bigger than this to have a place
;; to store the backpointer to the fin.  -smL
(defconstant funcallable-instance-closure-size 15)

;; This is only used in the file PCL-ENV.
(defvar *fin-env-type*
  (type-of (il:\\allocblock (1+ funcallable-instance-closure-size) t)))

;; Well, Gregor may be too proud to hack xpointers, but bvm and I aren't. -smL

(defstruct fin-env-pointer
  (pointer nil :type il:fullxpointer))

(defun fin-env-fin (fin-env)
  (fin-env-pointer-pointer
   (il:\\getbaseptr fin-env (* funcallable-instance-closure-size 2))))

(defun |set fin-env-fin| (fin-env new-value)
  (il:\\rplptr fin-env (* funcallable-instance-closure-size 2)
	       (make-fin-env-pointer :pointer new-value))
  new-value)

(defsetf fin-env-fin |set fin-env-fin|)

;; The finalization function that will clean up the backpointer from the
;; fin-env to the fin.  This needs to be careful to not cons at all.  This
;; depends on there being no other finalization function on compiled-closures,
;; since there is only one finalization function per datatype.  Too bad.  -smL
(defun finalize-fin (fin)
  ;; This could use the fn funcallable-instance-p, but if we get here we know
  ;; that this is a closure, so we can skip that test.
  (when (il:fetch (closure-overlay funcallable-instance-p) il:of fin)
    (let ((env (il:fetch (il:compiled-closure il:environment) il:of fin)))
      (when env
	(setq env
	      (il:\\getbaseptr env (* funcallable-instance-closure-size 2)))
	(when (il:typep env 'fin-env-pointer) 
	  (setf (fin-env-pointer-pointer env) nil)))))
  nil)					;Return NIL so GC can proceed

(eval-when (load)
  ;; Install the above finalization function.
  (when (fboundp 'finalize-fin)
    (il:\\set.finalization.function 'il:compiled-closure 'finalize-fin)))

(defun allocate-funcallable-instance-1 ()
  (let* ((env (il:\\allocblock (1+ funcallable-instance-closure-size) t))
         (fin (il:make-compiled-closure nil env)))
    (setf (fin-env-fin env) fin)
    (il:replace (closure-overlay funcallable-instance-p) il:of fin il:with 't)
    (set-funcallable-instance-function fin
      #'(lambda (&rest ignore)
          (declare (ignore ignore))
	  (called-fin-without-function)))
    fin))

(xcl:definline funcallable-instance-p (x)
  (and (typep x 'il:compiled-closure)
       (il:fetch (closure-overlay funcallable-instance-p) il:of x)))

(defun set-funcallable-instance-function (fin new)
  (cond ((not (funcallable-instance-p fin))
         (error "~S is not a funcallable-instance" fin))
        ((not (functionp new))
         (error "~S is not a function." new))
        ((typep new 'il:compiled-closure)
         (let* ((fin-env
                  (il:fetch (il:compiled-closure il:environment) il:of fin))
                (new-env
                  (il:fetch (il:compiled-closure il:environment) il:of new))
                (new-env-size (if new-env (il:\\#blockdatacells new-env) 0))
                (fin-env-size (- funcallable-instance-closure-size
                                 (length funcallable-instance-data))))
           (cond ((and new-env
		       (<= new-env-size fin-env-size))
		  (dotimes (i fin-env-size)
		    (il:\\rplptr fin-env
				 (* i 2)
				 (if (< i new-env-size)
				     (il:\\getbaseptr new-env (* i 2))
				     nil)))
		  (setf (compiled-closure-fnheader fin)
			(compiled-closure-fnheader new)))
                 (t
                  (set-funcallable-instance-function
                    fin
                    (make-trampoline new))))))
        (t
         (set-funcallable-instance-function fin
                                            (make-trampoline new)))))

(defun make-trampoline (function)
  #'(lambda (&rest args)
      (apply function args)))

        
(defmacro funcallable-instance-data-1 (fin data)
  `(il:\\getbaseptr (il:fetch (il:compiled-closure il:environment) il:of ,fin)
		    (* (- funcallable-instance-closure-size
			  (funcallable-instance-data-position ,data)
			  1)			;Reserve last element to
						;point back to actual FIN!
		       2)))

(defsetf funcallable-instance-data-1 (fin data) (new-value)
  `(il:\\rplptr (il:fetch (il:compiled-closure il:environment) il:of ,fin)
		(* (- funcallable-instance-closure-size
		      (funcallable-instance-data-position ,data)
		      1)
		   2)
		,new-value))

);end of #+Xerox


;;;
;;; In Franz Common Lisp ExCL
;;; This code was originally written by:
;;;   jkf%franz.uucp@berkeley.edu
;;; and hacked by:
;;;   smh%franz.uucp@berkeley.edu

#+ExCL
(progn

(defconstant funcallable-instance-flag-bit #x1)

(defun funcallable-instance-p (x)
   (and (excl::function-object-p x)
        (eq funcallable-instance-flag-bit
            (logand (excl::fn_flags x)
                    funcallable-instance-flag-bit))))

(defun make-trampoline (function)
  #'(lambda (&rest args)
      (apply function args)))

;; We initialize a fin's procedure function to this because
;; someone might try to funcall it before it has been set up.
(defun init-fin-fun (&rest ignore)
  (declare (ignore ignore))
  (called-fin-without-function))


(eval-when (eval) 
  (compile 'make-trampoline)
  (compile 'init-fin-fun))


;; new style
#+(and gsgc (not sun4) (not cray) (not mips))
(progn
;; set-funcallable-instance-function must work by overwriting the fin itself
;; because the fin must maintain EQ identity.
;; Because the gsgc time needs several of the fields in the function object
;; at gc time in order to walk the stack frame, it is important never to bash
;; a function object that is active in a frame on the stack.  Besides, changing
;; the functions closure vector, not to mention overwriting its constant
;; vector, would scramble it's execution when that stack frame continues.
;; Therefore we represent a fin as a funny compiled-function object.
;; The code vector of this object has some hand-coded instructions which
;; do a very fast jump into the real fin handler function.  The function
;; which is the fin object *never* creates a frame on the stack.
  

(defun allocate-funcallable-instance-1 ()
  (let ((fin (compiler::.primcall 'sys::new-function))
	(init #'init-fin-fun)
	(mattress-fun #'funcallable-instance-mattress-pad))
    (setf (excl::fn_symdef fin) 'anonymous-fin)
    (setf (excl::fn_constant fin) init)
    (setf (excl::fn_code fin)		; this must be before fn_start
	  (excl::fn_code mattress-fun))
    (setf (excl::fn_start fin) (excl::fn_start mattress-fun))
    (setf (excl::fn_flags fin) (logior (excl::fn_flags init)
				       funcallable-instance-flag-bit))
    (setf (excl::fn_closure fin)
      (make-array (length funcallable-instance-data)))

    fin))

;; This function gets its code vector modified with a hand-coded fast jump
;; to the function that is stored in place of its constant vector.
;; This function is never linked in and never appears on the stack.

(defun funcallable-instance-mattress-pad ()
  (declare #.*optimize-speed*)
  'nil)

(eval-when (eval)
  (compile 'funcallable-instance-mattress-pad))


#+(and excl (target-class s))
(eval-when (load eval)
  (let ((codevec (excl::fn_code
		  (symbol-function 'funcallable-instance-mattress-pad))))
    ;; The entire code vector wants to be:
    ;;   move.l  7(a2),a2     ;#x246a0007
    ;;   jmp     1(a2)        ;#x4eea0001
    (setf (aref codevec 0) #x246a
	  (aref codevec 1) #x0007
	  (aref codevec 2) #x4eea
	  (aref codevec 3) #x0001))
)

#+(and excl (target-class a))
(eval-when (load eval)
  (let ((codevec (excl::fn_code
		  (symbol-function 'funcallable-instance-mattress-pad))))
    ;; The entire code vector wants to be:
    ;;   l       r5,15(r5)    ;#x5850500f
    ;;   l       r15,11(r5)   ;#x58f0500b
    ;;   br      r15          ;#x07ff
    (setf (aref codevec 0) #x5850
	  (aref codevec 1) #x500f
	  (aref codevec 2) #x58f0
	  (aref codevec 3) #x500b
	  (aref codevec 4) #x07ff
	  (aref codevec 5) #x0000))
  )

#+(and excl (target-class i))
(eval-when (load eval)
  (let ((codevec (excl::fn_code
		  (symbol-function 'funcallable-instance-mattress-pad))))
    ;; The entire code vector wants to be:
    ;;   movl  7(edx),edx     ;#x07528b
    ;;   jmp   *3(edx)        ;#x0362ff
    (setf (aref codevec 0) #x8b
	  (aref codevec 1) #x52
	  (aref codevec 2) #x07
	  (aref codevec 3) #xff
	  (aref codevec 4) #x62
	  (aref codevec 5) #x03))
)

(defun funcallable-instance-data-1 (instance data)
  (let ((constant (excl::fn_closure instance)))
    (svref constant (funcallable-instance-data-position data))))

(defsetf funcallable-instance-data-1 set-funcallable-instance-data-1)

(defun set-funcallable-instance-data-1 (instance data new-value)
  (let ((constant (excl::fn_closure instance)))
    (setf (svref constant (funcallable-instance-data-position data))
          new-value)))

(defun set-funcallable-instance-function (fin new-function)
  (unless (funcallable-instance-p fin)
    (error "~S is not a funcallable-instance" fin))
  (unless (functionp new-function)
    (error "~S is not a function." new-function))
  (setf (excl::fn_constant fin)
	(if (excl::function-object-p new-function)
	    new-function
	    ;; The new-function is an interpreted function.
	    ;; Install a trampoline to call the interpreted function.
	    (make-trampoline new-function))))


)  ;; end sun3


#+(and gsgc (or sun4 mips))
(progn

(eval-when (compile load eval)
  (defconstant funcallable-instance-constant-count 15)
  )

(defun allocate-funcallable-instance-1 ()
  (let ((new-fin (compiler::.primcall 
		   'sys::new-function
		   funcallable-instance-constant-count)))
    ;; Have to set the procedure function to something for two reasons.
    ;;   1. someone might try to funcall it.
    ;;   2. the flag bit that says the procedure is a funcallable
    ;;      instance is set by set-funcallable-instance-function.
    (set-funcallable-instance-function new-fin #'init-fin-fun)
    new-fin))

(defun set-funcallable-instance-function (fin new-value)
  ;; we actually only check for a function object since
  ;; this is called before the funcallable instance flag is set
  (unless (excl::function-object-p fin)
    (error "~S is not a funcallable-instance" fin))

  (cond ((not (functionp new-value))
         (error "~S is not a function." new-value))
        ((not (excl::function-object-p new-value))
         ;; new-value is an interpreted function.  Install a
         ;; trampoline to call the interpreted function.
         (set-funcallable-instance-function fin (make-trampoline new-value)))
	((> (+ (excl::function-constant-count new-value)
	       (length funcallable-instance-data))
	    funcallable-instance-constant-count)
	 ; can't fit, must trampoline
	 (set-funcallable-instance-function fin (make-trampoline new-value)))
        (t
         ;; tack the instance variables at the end of the constant vector
	 
         (setf (excl::fn_code fin)	; this must be before fn_start
	       (excl::fn_code new-value))
         (setf (excl::fn_start fin) (excl::fn_start new-value))
         
         (setf (excl::fn_closure fin) (excl::fn_closure new-value))
	 ; only replace the symdef slot if the new value is an 
	 ; interned symbol or some other object (like a function spec)
	 (let ((newsym (excl::fn_symdef new-value)))
	   (excl:if* (and newsym (or (not (symbolp newsym))
				(symbol-package newsym)))
	      then (setf (excl::fn_symdef fin) newsym)))
         (setf (excl::fn_formals fin) (excl::fn_formals new-value))
         (setf (excl::fn_cframe-size fin) (excl::fn_cframe-size new-value))
	 (setf (excl::fn_locals fin) (excl::fn_locals new-value))
         (setf (excl::fn_flags fin) (logior (excl::fn_flags new-value)
                                            funcallable-instance-flag-bit))
	 
	 ;; on a sun4 we copy over the constants
	 (dotimes (i (excl::function-constant-count new-value))
	   (setf (excl::function-constant fin i) 
		 (excl::function-constant new-value i)))
	 ;(format t "all done copy from ~s to ~s" new-value fin)
	 )))

(defmacro funcallable-instance-data-1 (instance data)
  `(excl::function-constant ,instance 
			   (- funcallable-instance-constant-count
			      (funcallable-instance-data-position ,data)
			      1)))

) ;; end sun4 or mips

#+(and gsgc cray)
(progn

;; The cray is like the sun4 in that the constant vector is included in the  
;; function object itself.  But a mattress pad must be used anyway, because
;; the function start address is copied in the symbol object, and cannot be
;; updated when the fin is changed.  
;; We place the funcallable-instance-function into the first constant slot,  
;; and leave enough constant slots after that for the instance data.

(eval-when (compile load eval)
  (defconstant fin-fun-slot 0)
  (defconstant fin-instance-data-slot 1)
  )


;; We initialize a fin's procedure function to this because
;; someone might try to funcall it before it has been set up.
(defun init-fin-fun (&rest ignore)
  (declare (ignore ignore))
  (called-fin-without-function))

(defun allocate-funcallable-instance-1 ()
  (let ((fin (compiler::.primcall 'sys::new-function
			(1+ (length funcallable-instance-data))
			"funcallable-instance"))
	(init #'init-fin-fun)
	(mattress-fun #'funcallable-instance-mattress-pad))
    (setf (excl::fn_symdef fin) 'anonymous-fin)
    (setf (excl::function-constant fin fin-fun-slot) init)
    (setf (excl::fn_code fin)		; this must be before fn_start
      (excl::fn_code mattress-fun))
    (setf (excl::fn_start fin) (excl::fn_start mattress-fun))
    (setf (excl::fn_flags fin) (logior (excl::fn_flags init)
				       funcallable-instance-flag-bit))
    
    fin))

;; This function gets its code vector modified with a hand-coded fast jump
;; to the function that is stored in place of its constant vector.
;; This function is never linked in and never appears on the stack.

(defun funcallable-instance-mattress-pad ()
  (declare #.*optimize-speed*)
  'nil)

(eval-when (eval)
  (compile 'funcallable-instance-mattress-pad)
  (compile 'init-fin-fun))

(eval-when (load eval)
  (let ((codevec (excl::fn_code
		  (symbol-function 'funcallable-instance-mattress-pad))))
    ;; The entire code vector wants to be:
    ;;   a1  b77
    ;;   a2  12,a1
    ;;   a1 1,a2
    ;;   b77 a2
    ;;   b76 a1
    ;;   j   b76
    (setf (aref codevec 0) #o024177
	  (aref codevec 1) #o101200 (aref codevec 2) 12
	  (aref codevec 3) #o102100 (aref codevec 4) 1
	  (aref codevec 5) #o025277
	  (aref codevec 6) #o025176
	  (aref codevec 7) #o005076
	  ))
)

(defmacro funcallable-instance-data-1 (instance data)
  `(excl::function-constant ,instance 
			    (+ (funcallable-instance-data-position ,data)
			       fin-instance-dtat-slot)))


(defun set-funcallable-instance-function (fin new-function)
  (unless (funcallable-instance-p fin)
    (error "~S is not a funcallable-instance" fin))
  (unless (functionp new-function)
    (error "~S is not a function." new-function))
  (setf (excl::function-constant fin fin-fun-slot)
    (if (excl::function-object-p new-function)
	new-function
	;; The new-function is an interpreted function.
	;; Install a trampoline to call the interpreted function.
	(make-trampoline new-function))))

) ;; end cray

#-gsgc
(progn

(defun allocate-funcallable-instance-1 ()
  (let ((new-fin (compiler::.primcall 'sys::new-function)))
    ;; Have to set the procedure function to something for two reasons.
    ;;   1. someone might try to funcall it.
    ;;   2. the flag bit that says the procedure is a funcallable
    ;;      instance is set by set-funcallable-instance-function.
    (set-funcallable-instance-function new-fin #'init-fin-fn)
    new-fin))

(defun set-funcallable-instance-function (fin new-value)
  ;; we actually only check for a function object since
  ;; this is called before the funcallable instance flag is set
  (unless (excl::function-object-p fin)
    (error "~S is not a funcallable-instance" fin))
  (cond ((not (functionp new-value))
         (error "~S is not a function." new-value))
        ((not (excl::function-object-p new-value))
         ;; new-value is an interpreted function.  Install a
         ;; trampoline to call the interpreted function.
         (set-funcallable-instance-function fin (make-trampoline new-value)))
        (t
         ;; tack the instance variables at the end of the constant vector
         (setf (excl::fn_start fin) (excl::fn_start new-value))
         (setf (excl::fn_constant fin) (add-instance-vars
                                        (excl::fn_constant new-value)
                                        (excl::fn_constant fin)))
         (setf (excl::fn_closure fin) (excl::fn_closure new-value))
	 ;; In versions prior to 2.0. comment the next line and any other
	 ;; references to fn_symdef or fn_locals.
	 (setf (excl::fn_symdef fin) (excl::fn_symdef new-value))
         (setf (excl::fn_code fin) (excl::fn_code new-value))
         (setf (excl::fn_formals fin) (excl::fn_formals new-value))
         (setf (excl::fn_cframe-size fin) (excl::fn_cframe-size new-value))
	 (setf (excl::fn_locals fin) (excl::fn_locals new-value))
         (setf (excl::fn_flags fin) (logior (excl::fn_flags new-value)
                                            funcallable-instance-flag-bit)))))

(defun add-instance-vars (cvec old-cvec)
  ;; create a constant vector containing everything in the given constant
  ;; vector plus space for the instance variables
  (let* ((nconstants (cond (cvec (length (the simple-vector cvec))) (t 0)))
         (ndata (length funcallable-instance-data))
         (old-cvec-length (if old-cvec (length (the simple-vector old-cvec)) 0))
         (new-cvec nil))
    (declare (fixnum nconstants ndate old-cvec-length))
    (cond ((<= (the fixnum (+ nconstants ndata))  old-cvec-length)
           (setq new-cvec old-cvec))
          (t
           (setq new-cvec (make-array (the fixnum (+ nconstants ndata))))
           (when old-cvec
             (dotimes (i ndata)
               (declare (fixnum i))
               (setf (svref new-cvec (- (the fixnum (+ nconstants ndata)) i 1))
                     (svref old-cvec (- old-cvec-length i 1)))))))
    
    (dotimes (i nconstants) (setf (svref new-cvec i) (svref cvec i)))
    
    new-cvec))

(defun funcallable-instance-data-1 (instance data)
  (let ((constant (excl::fn_constant instance)))
    (declare (simple-vector constant))
    (svref constant (- (the fixnum (length constant))
                       (the fixnum
                            (1+ (the fixnum
                                     (funcallable-instance-data-position data))))))))

(defsetf funcallable-instance-data-1 set-funcallable-instance-data-1)

(defun set-funcallable-instance-data-1 (instance data new-value)
  (let ((constant (excl::fn_constant instance)))
    (setf (svref constant (- (length constant) 
                             (1+ (funcallable-instance-data-position data))))
          new-value)))

);end #-gsgc

);end of #+ExCL


;;;
;;; In Vaxlisp
;;; This code was originally written by:
;;;    vanroggen%bach.DEC@DECWRL.DEC.COM
;;; 
#+(and dec vax common)
(progn

;;; The following works only in Version 2 of VAXLISP, and will have to
;;; be replaced for later versions.

(defun allocate-funcallable-instance-1 ()
  (list 'system::%compiled-closure%
        ()
        #'(lambda (&rest args)
            (declare (ignore args))
	    (called-fin-without-function))
        (make-array (length funcallable-instance-data))))

(proclaim '(inline funcallable-instance-p))
(defun funcallable-instance-p (x)
  (and (consp x)
       (eq (car x) 'system::%compiled-closure%)
       (not (null (cdddr x)))))

(defun set-funcallable-instance-function (fin func)
  (cond ((not (funcallable-instance-p fin))
         (error "~S is not a funcallable-instance" fin))
        ((not (functionp func))
         (error "~S is not a function" func))
        ((and (consp func) (eq (car func) 'system::%compiled-closure%))
         (setf (cadr fin) (cadr func)
               (caddr fin) (caddr func)))
        (t (set-funcallable-instance-function fin
                                              (make-trampoline func)))))

(defun make-trampoline (function)
  #'(lambda (&rest args)
      (apply function args)))

(eval-when (eval) (compile 'make-trampoline))

(defmacro funcallable-instance-data-1 (instance data)
  `(svref (cadddr ,instance)
          (funcallable-instance-data-position ,data)))

);end of Vaxlisp (and dec vax common)


;;;; Implementation of funcallable instances for CMU Common Lisp:
;;;
;;;    We represent a FIN like a closure, but the header has a distinct type
;;; tag.  The FIN data slots are stored at the end of a fixed-length closure
;;; (at FIN-DATA-OFFSET.)  When the function is set to a closure that has no
;;; more than FIN-DATA-OFFSET slots, we can just replace the slots in the FIN
;;; with the closure slots.  If the closure has too many slots, we must
;;; indirect through a trampoline with a rest arg.  For non-closures, we just
;;; set the function slot.
;;;
;;;    We can get away with this efficient and relatively simple scheme because
;;; the compiler currently currently only references closure slots during the
;;; initial call and on entry into the function.  So we don't have to worry
;;; about bad things happening when the FIN is clobbered (the problem JonL
;;; flames about somewhere...)
;;;
;;;    We also stick in a slot for the function name at the end, but before the
;;; data slots.

#+CMU
(import 'kernel:funcallable-instance-p)

#+CMU
(progn

(eval-when (compile load eval)
  ;;; The offset of the function's name & the max number of real closure slots.
  ;;;
  (defconstant fin-name-slot 14)
  
  ;;; The offset of the data slots.
  ;;;
  (defconstant fin-data-offset 15))


;;; ALLOCATE-FUNCALLABLE-INSTANCE-1  --  Interface
;;;
;;;    Allocate a funcallable instance, setting the function to an error
;;; function and initializing the data slots to NIL.
;;;
(defun allocate-funcallable-instance-1 ()
  (let* ((len (+ (length funcallable-instance-data) fin-data-offset))
         (res (kernel:%make-funcallable-instance
               len
               #'called-fin-without-function)))
    (dotimes (i (length funcallable-instance-data))
      (kernel:%set-funcallable-instance-info res (+ i fin-data-offset) nil))
    (kernel:%set-funcallable-instance-info res fin-name-slot nil)
    res))


;;; FUNCALLABLE-INSTANCE-P  --  Interface
;;;
;;;    Return true if X is a funcallable instance.  This is an interpreter
;;; stub; the compiler directly implements this function.
;;;
(defun funcallable-instance-p (x) (funcallable-instance-p x))


;;; SET-FUNCALLABLE-INSTANCE-FUNCTION  --  Interface
;;;
;;;    Set the function that is called when FIN is called.
;;;
(defun set-funcallable-instance-function (fin new-value)
  (declare (type function new-value))
  (assert (funcallable-instance-p fin))
  (ecase (kernel:get-type new-value)
    (#.vm:closure-header-type
     (let ((len (- (kernel:get-closure-length new-value)
                   (1- vm:closure-info-offset))))
       (cond ((> len fin-name-slot)
              (set-funcallable-instance-function
               fin
               #'(lambda (&rest args)
                   (apply new-value args))))
             (t
              (dotimes (i fin-data-offset)
                (kernel:%set-funcallable-instance-info
                 fin i
                 (if (>= i len)
                     nil
                     (kernel:%closure-index-ref new-value i))))
              (kernel:%set-funcallable-instance-function
               fin
               (kernel:%closure-function new-value))))))
    (#.vm:function-header-type
     (kernel:%set-funcallable-instance-function fin new-value)))
  new-value)


;;; FUNCALLABLE-INSTANCE-NAME, SET-FUNCALLABLE-INSTANCE-NAME  --  Interface
;;;
;;;    Read or set the name slot in a funcallable instance.
;;;
(defun funcallable-instance-name (fin)
  (kernel:%closure-index-ref fin fin-name-slot))
;;;
(defun set-funcallable-instance-name (fin new-value)
  (kernel:%set-funcallable-instance-info fin fin-name-slot new-value)
  new-value)
;;;
(defsetf funcallable-instance-name set-funcallable-instance-name)


;;; FUNCALLABLE-INSTANCE-DATA-1  --  Interface
;;;
;;;    If the slot is constant, use CLOSURE-REF with the appropriate offset,
;;; otherwise do a run-time lookup of the slot offset.
;;;
(defmacro funcallable-instance-data-1 (fin slot)
  (if (constantp slot)
      `(sys:%primitive c:closure-ref ,fin
                       (+ (or (position ,slot funcallable-instance-data)
                              (error "Unknown slot: ~S." ,slot))
                          fin-data-offset))
      (ext:once-only ((n-slot slot))
        `(kernel:%closure-index-ref
          ,fin
          (+ (or (position ,n-slot funcallable-instance-data)
                 (error "Unknown slot: ~S." ,n-slot))
             fin-data-offset)))))
;;;
(defmacro %set-funcallable-instance-data-1 (fin slot new-value)
  (ext:once-only ((n-fin fin)
                  (n-slot slot)
                  (n-val new-value))
    `(progn
       (kernel:%set-funcallable-instance-info
        ,n-fin
        (+ (or (position ,n-slot funcallable-instance-data)
               (error "Unknown slot: ~S." ,n-slot))
           fin-data-offset)
        ,n-val)
       ,n-val)))
;;;
(defsetf funcallable-instance-data-1 %set-funcallable-instance-data-1)
                
); End of #+cmu progn


;;;
;;; Kyoto Common Lisp (KCL)
;;;
;;; In KCL, compiled functions and compiled closures are defined as c structs.
;;; This means that in order to access their fields, we have to use C code!
;;; The C code we call and the lisp interface to it is in the file kcl-low.
;;; The lisp interface to this code implements accessors to compiled closures
;;; and compiled functions of about the same level of abstraction as that
;;; which is used by the other implementation dependent versions of FINs in
;;; this file.
;;;

#+(and KCL (not IBCL))
(progn

(defvar *funcallable-instance-marker* (list "Funcallable Instance Marker"))

(defconstant funcallable-instance-closure-size 15)

(defconstant funcallable-instance-closure-size1
  (1- funcallable-instance-closure-size))

(defconstant funcallable-instance-available-size
  (- funcallable-instance-closure-size1
     (length funcallable-instance-data)))

(defmacro funcallable-instance-marker (x)
  `(car (cclosure-env-nthcdr funcallable-instance-closure-size1 ,x)))

(defun allocate-funcallable-instance-1 ()
  (let ((fin (allocate-funcallable-instance-2))
        (env (make-list funcallable-instance-closure-size :initial-element nil)))
    (setf (%cclosure-env fin) env)
    #+:turbo-closure (si:turbo-closure fin)
    (setf (funcallable-instance-marker fin) *funcallable-instance-marker*)
    fin))

(defun allocate-funcallable-instance-2 ()
  (let ((what-a-dumb-closure-variable ()))
    #'(lambda (&rest args)
        (declare (ignore args))
        (called-fin-without-function)
        (setq what-a-dumb-closure-variable
              (dummy-function what-a-dumb-closure-variable)))))

(defun funcallable-instance-p (x)
  (eq *funcallable-instance-marker* (funcallable-instance-marker x)))

(si:define-compiler-macro funcallable-instance-p (x)
  `(eq *funcallable-instance-marker* (funcallable-instance-marker ,x)))

(defun set-funcallable-instance-function (fin new-value)
  (cond ((not (funcallable-instance-p fin))
         (error "~S is not a funcallable-instance" fin))
        ((not (functionp new-value))
         (error "~S is not a function." new-value))
        ((and (cclosurep new-value)
              (<= (the index (length (the list (%cclosure-env new-value))))
                  (the index funcallable-instance-available-size)))
         (%set-cclosure fin new-value funcallable-instance-available-size))
        (t
         (set-funcallable-instance-function
           fin (make-trampoline new-value))))
  fin)

(defmacro funcallable-instance-data-1 (fin data &environment env)
  ;; The compiler won't expand macros before deciding on optimizations,
  ;; so we must do it here.
  (let* ((pos-form (macroexpand `(funcallable-instance-data-position ,data)
                                env))
         (index-form (if (constantp pos-form)
                         (the index
                              (- (the index funcallable-instance-closure-size)
                                 (the index (eval pos-form))
                                 2))
                         `(the index
                               (- (the index funcallable-instance-closure-size)
                                  (the index (funcallable-instance-data-position ,data))
                                  2)))))
    `(car (%cclosure-env-nthcdr ,index-form ,fin))))


#+turbo-closure (clines "#define TURBO_CLOSURE")

(clines "
static make_trampoline_internal();
static make_turbo_trampoline_internal();

static object
make_trampoline(function)
     object function;
{
  vs_push(MMcons(function,Cnil));
#ifdef TURBO_CLOSURE
  if(type_of(function)==t_cclosure)
    {if(function->cc.cc_turbo==NULL)turbo_closure(function);
     vs_head=make_cclosure(make_turbo_trampoline_internal,Cnil,vs_head,Cnil,NULL,0);
     return vs_pop;}
#endif
  vs_head=make_cclosure(make_trampoline_internal,Cnil,vs_head,Cnil,NULL,0);
  return vs_pop;
}

static
make_trampoline_internal(base0)
     object *base0;
{super_funcall_no_event(base0[0]->c.c_car);}

static
make_turbo_trampoline_internal(base0)
     object *base0;
{ object function=base0[0]->c.c_car;
  (*function->cc.cc_self)(function->cc.cc_turbo);
}

")

(defentry make-trampoline (object) (object make_trampoline))
)

#+IBCL
(progn ; From Rainy Day PCL.  

(defvar *funcallable-instance-marker* (list "Funcallable Instance Marker"))

(defconstant funcallable-instance-closure-size 15)

(defun allocate-funcallable-instance-1 ()
  (let ((fin (allocate-funcallable-instance-2))
	(env
	  (make-list funcallable-instance-closure-size :initial-element nil)))
    (set-cclosure-env fin env)
    #+:turbo-closure (si:turbo-closure fin)
    (dotimes (i (1- funcallable-instance-closure-size)) (pop env))
    (setf (car env) *funcallable-instance-marker*)
    fin))

(defun allocate-funcallable-instance-2 ()
  (let ((what-a-dumb-closure-variable ()))
    #'(lambda (&rest args)
	(declare (ignore args))
	(called-fin-without-function)
	(setq what-a-dumb-closure-variable
	      (dummy-function what-a-dumb-closure-variable)))))

(defun funcallable-instance-p (x)
  (and (cclosurep x)
       (let ((env (cclosure-env x)))
	 (when (listp env)
	   (dotimes (i (1- funcallable-instance-closure-size)) (pop env))
	   (eq (car env) *funcallable-instance-marker*)))))

(defun set-funcallable-instance-function (fin new-value)
  (cond ((not (funcallable-instance-p fin))
         (error "~S is not a funcallable-instance" fin))
        ((not (functionp new-value))
         (error "~S is not a function." new-value))
        ((cclosurep new-value)
         (let* ((fin-env (cclosure-env fin))
                (new-env (cclosure-env new-value))
                (new-env-size (length new-env))
                (fin-env-size (- funcallable-instance-closure-size
                                 (length funcallable-instance-data)
				 1)))
           (cond ((<= new-env-size fin-env-size)
		  (do ((i 0 (+ i 1))
		       (new-env-tail new-env (cdr new-env-tail))
		       (fin-env-tail fin-env (cdr fin-env-tail)))
		      ((= i fin-env-size))
		    (setf (car fin-env-tail)
			  (if (< i new-env-size)
			      (car new-env-tail)
			      nil)))		  
		  (set-cclosure-self fin (cclosure-self new-value))
		  (set-cclosure-data fin (cclosure-data new-value))
		  (set-cclosure-start fin (cclosure-start new-value))
		  (set-cclosure-size fin (cclosure-size new-value)))
                 (t                 
                  (set-funcallable-instance-function
                    fin
                    (make-trampoline new-value))))))
	((typep new-value 'compiled-function)
	 ;; Write NILs into the part of the cclosure environment that is
	 ;; not being used to store the funcallable-instance-data.  Then
	 ;; copy over the parts of the compiled function that need to be
	 ;; copied over.
	 (let ((env (cclosure-env fin)))
	   (dotimes (i (- funcallable-instance-closure-size
			  (length funcallable-instance-data)
			  1))
	     (setf (car env) nil)
	     (pop env)))
	 (set-cclosure-self fin (cfun-self new-value))
	 (set-cclosure-data fin (cfun-data new-value))
	 (set-cclosure-start fin (cfun-start new-value))
	 (set-cclosure-size fin (cfun-size new-value)))	 
        (t
         (set-funcallable-instance-function fin
                                            (make-trampoline new-value))))
  fin)


(defun make-trampoline (function)
  #'(lambda (&rest args)
      (apply function args)))

;; this replaces funcallable-instance-data-1, set-funcallable-instance-data-1
;; and the defsetf
(defmacro funcallable-instance-data-1 (fin data &environment env)
  ;; The compiler won't expand macros before deciding on optimizations,
  ;; so we must do it here.
  (let* ((pos-form (macroexpand `(funcallable-instance-data-position ,data)
				env))
	 (index-form (if (constantp pos-form)
			 (- funcallable-instance-closure-size
			    (eval pos-form)
			    2)
			 `(- funcallable-instance-closure-size
			     (funcallable-instance-data-position ,data)
			     2))))
    #+:turbo-closure `(car (tc-cclosure-env-nthcdr ,index-form ,fin))
    #-:turbo-closure `(nth ,index-form (cclosure-env ,fin))))

)


;;; In CLISP, compiled functions (also called compiled closures) are just
;;; a vector of constants, with one slot containing the bytecode. This means
;;; that constants and closure variables are intermixed in the procedure
;;; vector.
;;;
#+CLISP
(progn
  (let* ((mother-fin
           #'(lambda (&rest args) (declare (compile)) (apply '#:G0 args))
         )
         (mother-fin-code
           (sys::make-code-vector (sys::closure-codevec mother-fin))
        ))
    (defun allocate-funcallable-instance-1 ()
      (sys::%make-closure 'FUNCALLABLE-INSTANCE mother-fin-code
                          '#.(make-list (+ 1 (length funcallable-instance-data)) #| :initial-element nil |# )
    ) )
    (proclaim '(inline funcallable-instance-p))
    (defun funcallable-instance-p (obj)
      (and (sys::closurep obj) (eq (sys::%record-ref obj 1) mother-fin-code))
    )
  )
  (defun set-funcallable-instance-function (fin new-value)
    (let ((dummy-sym '#:G0))
      (setf (symbol-function dummy-sym) new-value) ; coerce to a function
      (setf (sys::%record-ref fin 2) (symbol-function dummy-sym))
    )
    new-value
  )
  (defmacro funcallable-instance-data-1 (instance-form data-form)
    (let ((position-form
            (if (and (consp data-form)
                     (eq (car data-form) 'quote)
                     (boundp 'funcallable-instance-data)
                )
              (or (position (cadr data-form) funcallable-instance-data :test #'eq)
                  (progn
                    (warn "Unknown funcallable-instance data: ~S." (cadr data-form))
                    `(error "Unknown funcallable-instance data: ~S." ',(cadr data-form))
              )   )
              `(position ,data-form funcallable-instance-data :test #'eq)
         )) )
      `(sys::%record-ref ,instance-form (+ 3 ,position-form))
  ) )
)



;;;
;;; In H.P. Common Lisp
;;; This code was originally written by:
;;;    kempf@hplabs.hp.com     (James Kempf)
;;;    dsouza@hplabs.hp.com    (Roy D'Souza)
;;;
#+HP-HPLabs
(progn

(defmacro fin-closure-size ()`(prim::@* 6 prim::bytes-per-word))

(defmacro fin-set-mem-hword ()
  `(prim::@set-mem-hword
     (prim::@+ fin (prim::@<< 2 1))
     (prim::@+ (prim::@<< 2 8)
	       (prim::@fundef-info-parms (prim::@fundef-info fundef)))))

(defun allocate-funcallable-instance-1()
  (let* ((fundef
	   #'(lambda (&rest ignore)
	       (declare (ignore ignore))
	       (called-fin-without-function)))
	 (static-link (vector 'lisp::*undefined* NIL NIL NIL NIL NIL))
	 (fin (prim::@make-fundef (fin-closure-size))))
    (fin-set-mem-hword)
    (prim::@set-svref fin 2 fundef)
    (prim::@set-svref fin 3 static-link)
    (prim::@set-svref fin 4 0) 
    (impl::PlantclosureHook fin)
    fin))

(defmacro funcallable-instance-p (possible-fin)
  `(= (fin-closure-size) (prim::@header-inf ,possible-fin)))

(defun set-funcallable-instance-function (fin new-function)
  (cond ((not (funcallable-instance-p fin))
	 (error "~S is not a funcallable instance.~%" fin))
	((not (functionp new-function))
	 (error "~S is not a function." new-function))
	(T
	 (prim::@set-svref fin 2 new-function))))

(defmacro funcallable-instance-data-1 (fin data)
  `(prim::@svref (prim::@closure-static-link ,fin)
		 (+ 2 (funcallable-instance-data-position ,data))))

(defsetf funcallable-instance-data-1 (fin data) (new-value)
  `(prim::@set-svref (prim::@closure-static-link ,fin)
		     (+ (funcallable-instance-data-position ,data) 2)
		     ,new-value))

(defun funcallable-instance-name (fin)
  (prim::@svref (prim::@closure-static-link fin) 1))

(defsetf funcallable-instance-name set-funcallable-instance-name)

(defun set-funcallable-instance-name (fin new-name)
  (prim::@set-svref (prim::@closure-static-link fin) 1 new-name))

);end #+HP



;;;
;;; In Golden Common Lisp.
;;; This code was originally written by:
;;;    dan%acorn@Live-Oak.LCS.MIT.edu     (Dan Jacobs)
;;;
;;; GCLISP supports named structures that are specially marked as funcallable.
;;; This allows FUNCALLABLE-INSTANCE-P to be a normal structure predicate,
;;; and allows ALLOCATE-FUNCALLABLE-INSTANCE-1 to be a normal boa-constructor.
;;; 
#+GCLISP
(progn

(defstruct (%funcallable-instance
	     (:predicate funcallable-instance-p)
	     (:copier nil)
	     (:constructor allocate-funcallable-instance-1 ())
	     (:print-function
	      (lambda (struct stream depth)
		(declare (ignore depth))
		(print-object struct stream))))
  (function	#'(lambda (ignore-this &rest ignore-these-too)
		    (declare (ignore ignore-this ignore-these-too))
		    (called-fin-without-function))
		:type function)
  (%hidden%	'gclisp::funcallable :read-only t)
  (data		(vector nil nil) :type simple-vector :read-only t))

(proclaim '(inline set-funcallable-instance-function))
(defun set-funcallable-instance-function (fin new-value)
  (setf (%funcallable-instance-function fin) new-value))

(defmacro funcallable-instance-data-1 (fin data)
  `(svref (%funcallable-instance-data ,fin)
	  (funcallable-instance-data-position ,data)))

)


;;;
;;; Explorer Common Lisp
;;; This code was originally written by:
;;;    Dussud%Jenner@csl.ti.com
;;;    
#+ti
(progn

#+(or :ti-release-3 (and :ti-release-2 elroy))
(defmacro lexical-closure-environment (l)
  `(cdr (si:%make-pointer si:dtp-list
			  (cdr (si:%make-pointer si:dtp-list ,l)))))

#-(or :ti-release-3 elroy)
(defmacro lexical-closure-environment (l)
  `(caar (si:%make-pointer si:dtp-list
			   (cdr (si:%make-pointer si:dtp-list ,l)))))

(defmacro lexical-closure-function (l)
  `(car (si:%make-pointer si:dtp-list ,l)))


(defvar *funcallable-instance-marker* (list "Funcallable Instance Marker"))

(defconstant funcallable-instance-closure-size 15) ; NOTE: In order to avoid
						   ; hassles with the reader,
(defmacro allocate-funcallable-instance-2 ()       ; these two 15's are the
  (let ((l ()))					   ; same.  Be sure to keep
    (dotimes (i 15)				   ; them consistent.
      (push (list (gensym) nil) l))
    `(let ,l
       #'(lambda (ignore &rest ignore-them-too)
	   (declare (ignore ignore ignore-them-too))
	   (called-fin-without-function)
	   (values . ,(mapcar #'car l))))))

(defun allocate-funcallable-instance-1 ()
  (let* ((new-fin (allocate-funcallable-instance-2)))
    (setf (car (nthcdr (1- funcallable-instance-closure-size)
		       (lexical-closure-environment new-fin)))
	  *funcallable-instance-marker*) 
    new-fin))

(eval-when (eval) (compile 'allocate-funcallable-instance-1))

(proclaim '(inline funcallable-instance-p))
(defun funcallable-instance-p (x)
  (and (typep x #+:ti-release-2 'closure
	        #+:ti-release-3 'si:lexical-closure)
       (let ((env (lexical-closure-environment x)))
	 (eq (nth (1- funcallable-instance-closure-size) env)
	     *funcallable-instance-marker*))))

(defun set-funcallable-instance-function (fin new-value)
  (cond ((not (funcallable-instance-p fin))
	 (error "~S is not a funcallable-instance"))
	((not (functionp new-value))
	 (error "~S is not a function."))
	((typep new-value 'si:lexical-closure)
	 (let* ((fin-env (lexical-closure-environment fin))
		(new-env (lexical-closure-environment new-value))
		(new-env-size (length new-env))
		(fin-env-size (- funcallable-instance-closure-size
				 (length funcallable-instance-data)
				 1)))
	   (cond ((<= new-env-size fin-env-size)
		  (do ((i 0 (+ i 1))
		       (new-env-tail new-env (cdr new-env-tail))
		       (fin-env-tail fin-env (cdr fin-env-tail)))
		      ((= i fin-env-size))
		    (setf (car fin-env-tail)
			  (if (< i new-env-size)
			      (car new-env-tail)
			      nil)))		  
		  (setf (lexical-closure-function fin)
			(lexical-closure-function new-value)))
		 (t
		  (set-funcallable-instance-function
		    fin
		    (make-trampoline new-value))))))
	(t
	 (set-funcallable-instance-function fin
					    (make-trampoline new-value)))))

(defun make-trampoline (function)
  (let ((tmp))
    #'(lambda (&rest args) tmp
	(apply function args))))

(eval-when (eval) (compile 'make-trampoline))
	
(defmacro funcallable-instance-data-1 (fin data)
  `(let ((env (lexical-closure-environment ,fin)))
     (nth (- funcallable-instance-closure-size
	     (funcallable-instance-data-position ,data)
	     2)
	  env)))


(defsetf funcallable-instance-data-1 (fin data) (new-value)
  `(let ((env (lexical-closure-environment ,fin)))
     (setf (car (nthcdr (- funcallable-instance-closure-size
			   (funcallable-instance-data-position ,data)
			   2)
			env))
	   ,new-value)))

);end of code for TI


;;; Implemented by Bein@pyramid -- Tue Aug 25 19:05:17 1987
;;;
;;; A FIN is a distinct type of object which FUNCALL,EVAL, and APPLY
;;; recognize as functions. Both Compiled-Function-P and functionp
;;; recognize FINs as first class functions.
;;;
;;; This does not work with PyrLisp versions earlier than 1.1..

#+pyramid
(progn

(defun make-trampoline (function)
    #'(lambda (&rest args) (apply function args)))

(defun un-initialized-fin (&rest trash)
    (declare (ignore trash))
    (called-fin-without-function))

(eval-when (eval)
    (compile 'make-trampoline)
    (compile 'un-initialized-fin))

(defun allocate-funcallable-instance-1 ()
    (let ((fin (system::alloc-funcallable-instance)))
      (system::set-fin-function fin #'un-initialized-fin)
      fin))
	     
(defun funcallable-instance-p (object)
  (typep object 'lisp::funcallable-instance))

(clc::deftransform funcallable-instance-p trans-fin-p (object)
    `(typep ,object 'lisp::funcallable-instance))

(defun set-funcallable-instance-function (fin new-value)
    (or (funcallable-instance-p fin)
	(error "~S is not a funcallable-instance." fin))
    (cond ((not (functionp new-value))
	   (error "~S is not a function." new-value))
	  ((not (lisp::compiled-function-p new-value))
	   (set-funcallable-instance-function fin
					      (make-trampoline new-value)))
	  (t
	   (system::set-fin-function fin new-value))))

(defun funcallable-instance-data-1 (fin data-name)
  (system::get-fin-data fin
			(funcallable-instance-data-position data-name)))

(defun set-funcallable-instance-data-1 (fin data-name value)
  (system::set-fin-data fin
			(funcallable-instance-data-position data-name)
			value))

(defsetf funcallable-instance-data-1 set-funcallable-instance-data-1)

); End of #+pyramid


;;;
;;; For Coral Lisp
;;;
#+:coral
(progn
  #-:cltl2 
  (progn
    (defconstant ccl::$v_istruct 22)
    (defvar ccl::initial-fin-slots (make-list (length funcallable-instance-data)))
    (defconstant ccl::fin-function 1)
    (defconstant ccl::fin-data (+ ccl::FIN-function 1))
    
    (defun allocate-funcallable-instance-1 ()
      (apply #'ccl::%gvector 
             ccl::$v_istruct
             'ccl::funcallable-instance
             #'(lambda (&rest ignore)
                 (declare (ignore ignore))
	         (called-fin-without-function))
             ccl::initial-fin-slots))
    
    #+:ccl-1.3
    (eval-when (eval compile load)
      
      ;;; Make uvector-based objects (like funcallable instances) print better.
      (defun print-uvector-object (obj stream &optional print-level)
        (declare (ignore print-level))
        (print-object obj stream))
      
      ;;; Inform the print system about funcallable instance uvectors.
      (pushnew (cons 'ccl::funcallable-instance #'print-uvector-object)
	       ccl:*write-uvector-alist*
	       :test #'equal)
      
      )
    
    (defun funcallable-instance-p (x)
      (and (eq (ccl::%type-of x) 'ccl::internal-structure)
           (eq (ccl::%uvref x 0) 'ccl::funcallable-instance)))
    
    (defun set-funcallable-instance-function (fin new-value)
      (unless (funcallable-instance-p fin)
        (error "~S is not a funcallable-instance." fin))
      (unless (functionp new-value)
        (error "~S is not a function." new-value))
      (ccl::%uvset fin ccl::FIN-function new-value))
    
    (defmacro funcallable-instance-data-1 (fin data-name)
      `(ccl::%uvref ,fin 
                    (+ (funcallable-instance-data-position ,data-name)
		       ccl::FIN-data)))
    
    (defsetf funcallable-instance-data-1 (fin data) (new-value)
      `(ccl::%uvset ,fin 
                    (+ (funcallable-instance-data-position ,data) ccl::FIN-data)
                    ,new-value)))
  ) ; end of :coral
  #+(and coral :cltl2) (in-package :ccl)
  #+(and coral :cltl2)
  
  (eval-when (:compile-toplevel :execute)
    
    (require "LISPEQU")
    (require "LAPMACROS"))
  
  #+(and :coral :cltl2)
  (progn
    
    (defun uninitialized-fin-function (&rest rest)
      (error "Uninitialized funcallable instance called with args:~%~s" rest))
    
    (defvar *funcallable-instance-marker* '*funcallable-instance-marker*)
    (declaim (inline internal-allocate-funcallable-instance-1 
                     internal-funcallable-instance-p
                     set-internal-funcallable-instance-function
                     internal-funcallable-instance-data-1
                     set-internal-funcallable-instance-data-1))
    
    (defun internal-allocate-funcallable-instance-1 ()
      ;;;
      ;;;This makes an funcallable instance
      ;;;
      (%make-lfun
   (vector 'funcallable-instance
           #'uninitialized-fin-function
           *funcallable-instance-marker*
           nil)
   '#.(coerce (list #x4ef9 0 1          ; jmp fin-function
                    0 2                 ; *funcallable-instance-marker*
                    0 3                 ; fin-data
                    0 0)                ; function-name
              '(vector (signed-byte 16)))
   '#.(coerce (list 2 $lm_longimm 6 $lm_longimm 10 $lm_longimm 14 $lm_longimm)
              '(vector (signed-byte 16))) 
   (ash 1 $lfbits-rest-bit)   ; bits
   (ash 1 $lfatr-resident-bit)))
    
    (defun internal-funcallable-instance-p (fin)
      (and (functionp fin)
           (lap-inline (*funcallable-instance-marker* fin)
             (move.l arg_z atemp0)
             (move.l nilreg acc)
             (if# (and (eq (cmp.w ($ #x4ef9) @atemp0))
                       (eq (cmp.l (atemp0 6) arg_y)))
               (add.l ($ $t_val) acc)))))
    
    (defmacro require-fin (fin)
      `(unless (internal-funcallable-instance-p ,fin)
         (error "~s is not a funcallable-instance." ,fin)))
    
    (defun set-internal-funcallable-instance-function (fin new-value)
      (require-fin fin)
      (unless (functionp new-value)
        (error "~s is not a function" new-value))
      ; This will make arglist work on funcallable instances
      ; after arglist is fixed by patch 2 for MCL 2.0
      `(let ((bits (lfun-bits fin))
             (new-bits (ccl::lfun-bits new-value)))
         (lfun-bits fin (logior (logand new-bits $lfbits-args-mask)
                                (logand bits (lognot $lfbits-args-mask)))))
      ; Here's where the real work happens
      (lap-inline (fin new-value)
        (move.l arg_y atemp0)
        (move.l arg_z (atemp0 2))
        (sub.l ($ $sym.fapply) atemp0)
        (jsr_subprim $mmu_flush_sym_cache))
      new-value)
  
  (defun internal-funcallable-instance-data-1 (fin)
    (require-fin fin)
    (lap-inline (fin)
      (move.l arg_z atemp0)
      (move.l (atemp0 10) acc)))
  
  (defun set-internal-funcallable-instance-data-1 (fin data)
    (require-fin fin)
    (lap-inline (fin data)
      (move.l arg_y atemp0)
      (move.l arg_z (atemp0 10))
      (movereg arg_z acc)))
) ; end of (and :coral :cltl2)

#+(and :coral :cltl2) (in-package :pcl)
#+(and :coral cltl2)
(progn 


(defmacro allocate-funcallable-instance-1 ()
;;;
;;;This makes a funcallable instance, with a data slot
;;;initialize to a new vector intialized to the size of the 
;;;funcallable-instance-data list
;;;
  `(let ((fin (ccl::internal-allocate-funcallable-instance-1)))
    (ccl::set-internal-funcallable-instance-data-1 fin
                                     (make-array 
                                        (length funcallable-instance-data)  
                                        :initial-element nil))
  fin))                             
                                     
        
(defmacro funcallable-instance-p (fin)
  `(ccl::internal-funcallable-instance-p ,fin))

(defmacro set-funcallable-instance-function (fin new-value)
  `(ccl::set-internal-funcallable-instance-function ,fin ,new-value))

         

(defmacro funcallable-instance-data-1 (fin data-name)
  `(svref (ccl::internal-funcallable-instance-data-1 ,fin) 
         (funcallable-instance-data-position ,data-name)))

(defmacro set-funcallable-instance-data-1 (fin data-name new-value)
  `(setf (svref (ccl::internal-funcallable-instance-data-1 ,fin) 
               (funcallable-instance-data-position ,data-name)) ,new-value))
               

(defsetf funcallable-instance-data-1 set-funcallable-instance-data-1)

)


;;; In Utah Common Lisp, closures (and other things) are represented in the
;;; heap as (1) code that sets up some registers and then jumps to the code
;;; stored in the code pointer and (2) slots that hold the code pointer, heap
;;; pointer, and so on.  We use this "thunk" type to represent FINs.  The code
;;; pointer holds the funcallable-instance-function, the heap pointer holds a
;;; vector with the FIN data.

#+UCL
(progn

(defun allocate-funcallable-instance-1 ()
  (let ((thunk (ucl::make-thunk #'ucl::closure-stub
				'|PCL Generic Function|
				(vector nil nil)
				(ucp::@funtoaddr #'called-fin-without-function)
				nil)))
    thunk))

(defun funcallable-instance-p (fin)
  (and (ucl::conscode-p fin)
       (eq (ucl::conscode-type fin) '|PCL Generic Function|)))

(defun set-funcallable-instance-function (fin new-value)
  (unless (funcallable-instance-p fin)
    (error "~S is not a PCL generic function." fin))
  (ucl::setf-closure-code-ptr fin (ucp::@funtoaddr new-value)))

(defmacro funcallable-instance-data-1 (fin data-name)
  `(svref (ucl::conscode-heap ,fin)
	  (funcallable-instance-data-position ,data-name)))

(defsetf funcallable-instance-data-1 (fin data-name) (new-value)
  `(setf (svref (ucl::conscode-heap ,fin)
		(funcallable-instance-data-position ,data-name))
	 ,new-value))

); End of #+UCL



  
;;;; Slightly Higher-Level stuff built on the implementation-dependent stuff.
;;;
;;;

(defmacro fsc-instance-p (fin)
  `(funcallable-instance-p ,fin))

(defmacro fsc-instance-class (fin)
  `(wrapper-class (funcallable-instance-data-1 ,fin 'wrapper)))

(defmacro fsc-instance-wrapper (fin)
  `(funcallable-instance-data-1 ,fin 'wrapper))

(defmacro fsc-instance-slots (fin)
  `(funcallable-instance-data-1 ,fin 'slots))

(defun allocate-funcallable-instance (wrapper allocate-static-slot-storage-copy)
  (declare (type simple-vector allocate-static-slot-storage-copy))
  (let ((fin (allocate-funcallable-instance-1))
        (slots
          (%allocate-static-slot-storage--class
            allocate-static-slot-storage-copy)))
    (setf (fsc-instance-wrapper fin) wrapper
          (fsc-instance-slots fin) slots)
    fin))

