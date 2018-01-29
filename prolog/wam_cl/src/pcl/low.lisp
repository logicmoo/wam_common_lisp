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
;;; This file contains portable versions of low-level functions and macros
;;; which are ripe for implementation specific customization.  None of the
;;; code in this file *has* to be customized for a particular Common Lisp
;;; implementation. Moreover, in some implementations it may not make any
;;; sense to customize some of this code.
;;;
;;; But, experience suggests that MOST Common Lisp implementors will want
;;; to customize some of the code in this file to make PCL run better in
;;; their implementation.  The code in this file has been separated and
;;; heavily commented to make that easier.
;;;
;;; Implementation-specific version of this file already exist for:
;;; 
;;;    Symbolics Genera family     genera-low.lisp
;;;    Lucid Lisp                  lucid-low.lisp
;;;    Xerox 1100 family           xerox-low.lisp
;;;    ExCL (Franz)                excl-low.lisp
;;;    Kyoto Common Lisp           kcl-low.lisp
;;;    Vaxlisp                     vaxl-low.lisp
;;;    CMU Lisp                    cmu-low.lisp
;;;    H.P. Common Lisp            hp-low.lisp
;;;    Golden Common Lisp          gold-low.lisp
;;;    Ti Explorer                 ti-low.lisp
;;;    
;;;
;;; These implementation-specific files are loaded after this file.  Because
;;; none of the macros defined by this file are used in functions defined by
;;; this file the implementation-specific files can just contain the parts of
;;; this file they want to change.  They don't have to copy this whole file
;;; and then change the parts they want.
;;;
;;; If you make changes or improvements to these files, or if you need some
;;; low-level part of PCL re-modularized to make it more portable to your
;;; system please send mail to CommonLoops.pa@Xerox.com.
;;;
;;; Thanks.
;;; 

(in-package 'pcl)

(eval-when (compile load eval)
(defconstant *optimize-speed*
  #+kcl
  '(optimize)
  #-kcl
  '(optimize (speed 3) (safety 0) (compilation-speed 0) (space 0))
  "List of declarations for locally-optimized internal code.")
)


;;; PCL optimizes slot-value accesses on specialized parameters by caching
;;; methods for each set of classes the method is called on.
;;; *compile-slot-access-method-functions-at-runtime-p* controls whether
;;; July 92 stores the method lambda for such methods and compiles them
;;; at runtime if one of the slot accesses is on a non-:instance allocated
;;; slot, is on a non-standard instance, or has a user-defined
;;; slot-value-using-class method.
;;;   Doing so speeds up slot-accesses because each slot access is directly
;;; coded in the cached method to be compiled, at the cost of extra compilation
;;; at runtime.  store-optimized-method-lambda-p can also be specialized
;;; for the specific kind of generic-function and methods (see methods.lisp).
;;;   The default is normally T, but can be changed.

(declaim (type boolean *compile-slot-access-method-functions-at-runtime-p*))
(defvar *compile-slot-access-method-functions-at-runtime-p* T
  "When T tells PCL to store the lambda source for methods containing slot
   accesses and to compile those slot accesses at runtime in certain cases.")

;;; 
;;;    For optimization purposes, July 92 PCL adds the variable
;;; *compile-all-method-functions-p*, that lets a programmer tell PCL
;;; to make sure that all method functions are compiled, and to trust that
;;; they will be compiled.  The default is NIL, leaving them compiled or
;;; uncompiled as normal, so people can debug their programs more easily 
;;; when loading uncompiled source code.
;;;   It can be set to T by a program that uses PCL, but *must* be set
;;; to T before any user methods are loaded (otherwise their method
;;; functions might have been loaded as uncompiled functions, potentially
;;; causing nasty things to happen when the method dispatch functions later
;;; assume that they're compiled).  It is safe for a user program to set
;;; it to T after PCL itself is loaded, however, because presumably PCL
;;; will have been loaded as compiled.

(declaim (type boolean *compile-all-method-functions-p*))
(defvar *compile-all-method-functions-p* NIL
  "When T tells PCL to compile all cached method-functions and
   to assume that they are compiled.")


;;;    *compile-all-slot-initfunctions-p* tells whether all slot
;;; definition initfunctions should be compiled.  Default is T.
;;;   It can be set to T or NIL by a program that uses PCL, but should
;;; *not* ever later be changed to NIL if PCL has been compiled with it
;;; as T, since the initfunction-funcall's for PCL will have assumed that
;;; they are all compiled.  Later changing it to T after PCL has been
;;; compiled with NIL if you change it here, however, is safe.

(declaim (type boolean *compile-all-slot-initfunctions-p*))
(defvar *compile-all-slot-initfunctions-p* T
  "When T tells PCL to compile all slot initfunctions and assume they are compiled.")

(declaim (type boolean *uncompiled-slot-initfunctions-exists-p*))
(defvar *uncompiled-slot-initfunctions-exist-p* NIL
  "Tells whether there have ever been any uncompiled slot initfunctions.")


;;;   To adhere to the AMOP while retaining maximum efficiency, method
;;; functions are actually stored in two ways: as a (1) METHOD-FUNCTION and
;;; (2) as a METHOD-OPTIMIZED-FUNCTION or METHOD-CLOSURE-GENERATOR.
;;; METHOD-FUNCTION is the documented function of the AMOP.
;;; METHOD-OPTIMIZED-FUNCTION is the optimized function used by PCL in actual
;;; method function invocation (METHOD-FUNCTION-FOR-CACHING). Its arguments are
;;; the actual arguments of method, and it recieves its next-methods by
;;; looking at the global *NEXT-METHODS*.  Alternatively, if the method's
;;; body contains slot-value accesses that can be optimized for caching,
;;; a METHOD-CLOSURE-GENERATOR is stored instead of METHOD-OPTIMIZED-FUNCTION
;;; to generate an optimized caching function for given parameter types.
;;;
;;; To save space and compile time, generic function STORE-METHOD-FUNCTION-P
;;; (generic-function method initargs) can be specialized to return NIL
;;; if the (normally unused) documented method-functions are not needed.
;;; Default returns T.  Variable *standard-store-method-function-p*, which
;;; is what the default store-method-function-p method looks up, can be
;;; set to NIL if it is known that documented method functions will never
;;; be needed.

(declaim (type boolean *standard-store-method-function-p*))
(defvar *standard-store-method-function-p* T
  "Value used by default STORE-METHOD-METHOD-FUNCTION-P method to tell
   whether standard-methods used in standard-generic-functions store
   documented method-functions.")


;;;   Global variables keeping track of whether it is safe to use the
;;; fast slot-value optimizations that directly lookup slot location
;;; from wrapper (wrapper-optimized-slot-value) or not.  PCL changes
;;; the appropriate variable to NIL if the user defines any
;;; slot-value-using-class, (setf slot-value-using-class), or
;;; slot-boundp-using-class that it doesn't know about (i.e. whose
;;; specializers aren't found on the corresponding
;;; *safe-slot-value-using-class-specializers* variable.
;;;   
;;;   Compiled slot-value accesses that are not specialized parameters
;;; inside are generally converted by DEFINE-COMPILER-MACRO to a call
;;; to macro FAST-SLOT-VALUE.  FAST-SLOT-VALUE checks whether it
;;; is safe to use the wrapper optimizations by checking the appropriate
;;; global (e.g. *safe-to-use-slot-value-wrapper-optimizations-p*).
;;; If it is safe, then WRAPPER-OPTIMIZED-SLOT-VALUE is used to
;;; access the slot directly by looking it up in the wrapper.  If it
;;; is not safe, then ACCESSOR-SLOT-VALUE is called to access the
;;; slot through a reader method defined just for that purpose (March 92 PCL).
;;;
;;;   Three optimization hints if defining such slot-value-using-class
;;; method(s):  (1) If the slot-value-using-class method(s) is a
;;; "simple" definition that can be looked up in the standard form
;;; (i.e. only has instance slots stored in the std/fsc/user-instance-slots
;;; vector, etc.), and therefore is still eligible for the slot-value
;;; wrapper optimizations, then one can push the method's list of
;;; specializers method onto the appropriate *safe-slot-value-using-
;;; class-specializers* global to inform PCL that the method is still
;;; "safe" (must be done _before_ defining the method).  See file
;;; user-instances.lisp for an example.
;;;   (2) If you know you are going to define such unsafe s-v-u-c
;;; methods, you can change each of the *safe-to-use-slot-value-
;;; wrapper-optimizations-p* globals defined below to NIL, so that
;;; PCL doesn't generate any of the WRAPPER-OPTIMIZED-SLOT-VALUE
;;; code that it won't be able to use anyway.
;;;   (3) If it is guaranteed that you will *not* define such unsafe
;;; s-v-u-c methods, then *always-safe-to-use-slot-wrapper-optimizations-p*
;;; can be set to T, after which slot-value accesses will be compiled
;;; to assume that slot accesses will always be safe.  This will slightly
;;; optimize slot-value speed and space because it won't have to
;;; check *safe-to-use-slot-value-wrapper-optimizations-p* and it
;;; won't generate the extra code for ACCESSOR-SLOT-VALUE.  If an unsafe
;;; user-defined s-v-u-c methods id defined after this is set to T,
;;; however, then all bets are off.
;;;
;;;   Do *not* change the three *safe-to-use-slot-value-wrapper-optimizations-p*
;;; globals by hand (aside for changing their initial values below).

(declaim (type boolean *safe-to-use-slot-value-wrapper-optimizations-p*
                       *safe-to-use-set-slot-value-wrapper-optimizations-p*
                       *safe-to-use-slot-boundp-wrapper-optimizations-p*
                       *safe-to-use-slot-wrapper-optimizations-p*))

(defvar *safe-to-use-slot-value-wrapper-optimizations-p* T
  "Tells whether it is safe for slot-value to directly access the
   slot through the wrapper for instances of standard-class or
   funcallable-standard-class.  NIL if any non-standard
   slot-value-using-class methods have been defined.")

(defvar *safe-to-use-set-slot-value-wrapper-optimizations-p* T
  "Tells whether it is safe for set-slot-value to directly access the
   slot through the wrapper for instances of standard-class or
   funcallable-standard-class.  NIL if any non-standard
   (setf slot-value-using-class) methods have been defined.")

(defvar *safe-to-use-slot-boundp-wrapper-optimizations-p* T
  "Tells whether it is safe for slot-boundp to directly access the
   slot through the wrapper for instances of standard-class or
   funcallable-standard-class.  NIL if any non-standard
   slot-boundp-using-class methods have been defined.")

(defvar *safe-to-use-slot-wrapper-optimizations-p*
        (and *safe-to-use-slot-value-wrapper-optimizations-p*
             *safe-to-use-set-slot-value-wrapper-optimizations-p*
             *safe-to-use-slot-boundp-wrapper-optimizations-p*)
  "(and *safe-to-use-slot-value-wrapper-optimizations-p*
        *safe-to-use-set-slot-value-wrapper-optimizations-p*
        *safe-to-use-slot-boundp-wrapper-optimizations-p*)")


(defvar *safe-slot-value-using-class-specializers*
    '((std-class standard-object standard-effective-slot-definition)
      (structure-class structure-object structure-effective-slot-definition))
  "List of the names of the standard slot-value-using-class
   method specializers that it is safe to use the slot-value
   wrapper optimizations for.")

(defvar *safe-set-slot-value-using-class-specializers*
  '((T std-class standard-object standard-effective-slot-definition)
    (T structure-class structure-object structure-effective-slot-definition))
  "List of the names of the standard (setf slot-value-using-class)
   method specializers that it is safe to use the slot-value
   wrapper optimizations for.")

(defvar *safe-slot-boundp-using-class-specializers*
   '((std-class standard-object standard-effective-slot-definition)
     (structure-class structure-object structure-effective-slot-definition))
  "List of the names of the standard slot-boundp-using-class
   method specializers that it is safe to use the slot-boundp
   wrapper optimizations for.")


(declaim (type boolean *always-safe-to-use-slot-wrapper-optimizations-p*))
(defvar *always-safe-to-use-slot-wrapper-optimizations-p* NIL
  "Global that programmer can set to T for slight optimizations if it is
   known that it will always be safe to use wrapper-optimized-slot-value
   etc. for slot accesses.  (I.e. there will never be user-defined
   slot-value-using-class methods not in *safe-slot-value-using-class-specializers*.)")


;;;   When compiled with the feature :pcl-user-instances, PCL includes the
;;; hooks built in to allow programmers to define their own USER-INSTANCE
;;; low-level type of instances different from STD-INSTANCE, FSC-INSTANCE,
;;; STRUCTURE-INSTANCE, or BUILT-IN-INSTANCE (see file vector.lisp for more
;;; description, and user-instances.lisp as an example of their use to save
;;; space over standard instances).
;;;   Note: pcl-user-instances do not currently work in excl (Allegro) on
;;; the Sun 4 because of the low-level Sparc lap optimizations in
;;; quadlap.lisp. :pcl-user-instances is therefore not usually compiled as
;;; a feature in excl on the sun4.  If you want to use pcl-user-instances
;;; in excl on the sun4, then the lines defining cpatch and quadlap as
;;; part of the (defsystem pcl ...) in defsys.lisp must be commented out
;;; and the below lines modified.

;#-(and excl sun4)
(pushnew :pcl-user-instances *features*)



(defmacro %svref (vector index)
  `(locally (declare #.*optimize-speed*
                     (inline svref))
            (svref (the simple-vector ,vector) (the index ,index))))

(defsetf %svref (vector index) (new-value)
  `(locally (declare #.*optimize-speed*
                     (inline svref))
     (setf (svref (the simple-vector ,vector) (the index ,index))
           ,new-value)))


(defun method-function-storage-form (function)
  "Converts a function to the form it will be stored as a method-function
   or method-function.  This is equal to itself, except that if it is
   uncompiled and *compile-all-method-functions-p* is T, then it
   will be compiled and that value returned."
  (when function
      (if (compiled-function-p function)
          function
          (if *compile-all-method-functions-p*
              (compile-function function)
              function))))

(defmacro method-function-funcall (f &rest args)
  (if *compile-all-method-functions-p*
      `(funcall-compiled ,f ,@args)
      `(funcall-function ,f ,@args)))

(defmacro method-function-apply (f &rest args)
  (if *compile-all-method-functions-p*
      `(apply-compiled ,f ,@args)
      `(apply-function ,f ,@args)))


(defun slot-initfunction-storage-form (initfunction)
  "Converts a slot-definition initfunction to the form it will be
   stored as.  This is equal to itself, except that if it is uncompiled
   and *compile-all-slot-initfunctions-p* is T, then it will be compiled
   and that value returned."
  (if initfunction
      (if (compiled-function-p initfunction)
          initfunction
          (if *compile-all-slot-initfunctions-p*
              (compile-initfunction initfunction)
              (progn
                (setf *uncompiled-slot-initfunctions-exist-p* T)
                initfunction)))
      (error "No initfunction supplied.")))

(defun compile-initfunction (initfunction)
  ;; This is meant to compile the initfunction if it is not compiled yet,
  ;; which should be straightforward, since it will be in the form
  ;; (function (lambda () ,initform)).  This definitely works in lucid,
  ;; excl, and cmu-lisp. ;However, akcl does something strange to that
  ;; form (i.e. convert it to '(lambda-closure () () () () ,initform)),
  ;; which I've patched, but who knows what the heck other lisps do?  -- TL
  (if (compiled-function-p initfunction)
      initfunction
      (compile-function initfunction)))

(defmacro slot-initfunction-funcall (f &rest args)
  (if (and *compile-all-slot-initfunctions-p*
           (not *uncompiled-slot-initfunctions-exist-p*))
      `(funcall-compiled ,f ,@args)
      `(funcall-function ,f ,@args)))


;;;
;;; without-interrupts
;;; 
;;; OK, Common Lisp doesn't have this and for good reason.  But For all of
;;; the Common Lisp's that PCL runs on today, there is a meaningful way to
;;; implement this.  WHAT I MEAN IS:
;;;
;;; I want the body to be evaluated in such a way that no other code that is
;;; running PCL can be run during that evaluation.  I agree that the body
;;; won't take *long* to evaluate.  That is to say that I will only use
;;; without interrupts around relatively small computations.
;;;
;;; INTERRUPTS-ON should turn interrupts back on if they were on.
;;; INTERRUPTS-OFF should turn interrupts back off.
;;; These are only valid inside the body of WITHOUT-INTERRUPTS.
;;;
;;; OK?
;;;
(defmacro without-interrupts (&body body)
  `(macrolet ((interrupts-on () ())
              (interrupts-off () ()))
     (progn ,.body)))

(defmacro without-interrupts-simple (&body body)
  `(without-interrupts ,.body))


;;; We have to redefine lisp's DEFSTRUCT macro so that pcl knows about
;;; all the structure-classes.

(defmacro pcl-defstruct (name-and-options &body slot-descriptions)
  ;; Excludes structures types created with the :type option
  (if (and (listp name-and-options)
           (find-if #'(lambda (option)
                        (and (consp option) (eq (car option) ':type)))
                    (cdr name-and-options)))
      `(original-defstruct ,name-and-options ,@slot-descriptions)
      (let ((name-and-options
              (if (listp name-and-options)
                  name-and-options
                  (list name-and-options))))
        (unless (or (eq (car name-and-options) 'structure-object)
                    (find-if #'(lambda (option)
                                 (and (consp option)
                                      (eq (car option) ':include)))
                             (cdr name-and-options)))
          (setf name-and-options
                (append name-and-options '((:include structure-object)))))
        `(progn
           (store-defstruct-form '(,name-and-options ,@slot-descriptions))
           (original-defstruct ,name-and-options ,@slot-descriptions)))))

(redefine-macro 'defstruct 'pcl-defstruct)
(setf (macro-function 'original-defstruct) (original-definition 'defstruct))


(defvar *structure-table* (make-hash-table :test 'eq))

(defun store-defstruct-form (defstruct-source)
  (setf (gethash (caar defstruct-source) *structure-table*)
        defstruct-source))

(defun defstruct-form (structure-name)
  (gethash structure-name *structure-table*))

(defun defstruct-form-class-name (defstruct-form)
  ;; Returns the name of the structure defined by the defstruct-form.
  (caar defstruct-form))

(defun defstruct-form-conc-name (defstruct-form)
  ;; Returns a string of the conc-name given by the defstruct source.
  (let ((conc-name-option (assq :conc-name (cdar defstruct-form))))
    (if conc-name-option
        (if (cadr conc-name-option)
            (symbol-name (cadr conc-name-option))
            "")
        (concatenate 'simple-string
                     (symbol-name (defstruct-form-class-name defstruct-form))
                     "-"))))

(defun defstruct-form-predicate-name (defstruct-form)
  ;; Returns a string of the predicate-name given by the defstruct source.
  (let ((predicate-option (assq :predicate (cdar defstruct-form))))
    (if predicate-option
        (cadr predicate-option)
        (let ((class-name (defstruct-form-class-name defstruct-form)))
          (intern (concatenate 'simple-string (symbol-name class-name) "-P")
                  (symbol-package class-name))))))

(defun defstruct-form-constructor (defstruct-form)
  (let ((constructor-option (assq :constructor (cdar defstruct-form))))
    (if constructor-option
        (cdr constructor-option)
        (let ((class-name (defstruct-form-class-name defstruct-form)))
        (list
          (intern (concatenate 'simple-string "MAKE-" (symbol-name class-name))
                  (symbol-package class-name))
          ())))))

#-(or cmu excl ibcl kcl lucid UCL)
(defmacro structurep (x)
  `(typep ,x 'structure))

#-(or cmu excl ibcl kcl lucid UCL)
(defmacro structure-type (x)
  `(type-of ,x))

#-(or cmu kcl lucid UCL)
(defun known-structure-type-p (symbol)
  (not (null (gethash symbol *structure-table*))))

(defmacro structure-instance-p (x)
  "All structures except std-instance structures match."
  (once-only (x)
    `(locally
       (declare #.*optimize-speed*)
       (and (structurep ,x)
            (not (eq (structure-type ,x) 'std-instance))))))

(defun structure-type-included-type-name (symbol)
  (let ((defstruct-source (gethash symbol *structure-table*)))
    (cadr (assq :include (cdar defstruct-source)))))


;;;
;;;  Very Low-Level representation of instances with meta-class standard-class.
;;;
(defstruct (std-instance (:predicate std-instance-p)
                         (:conc-name %std-instance-)
                         (:constructor %%allocate-instance--class ())
                         (:print-function print-std-instance))
  (wrapper nil)
  (slots nil))

(defmacro std-instance-wrapper (x) `(%std-instance-wrapper ,x))
(defmacro std-instance-slots   (x) `(%std-instance-slots ,x))

(defun print-std-instance (instance stream depth) ;A temporary definition used
  (declare (ignore depth))                        ;for debugging the bootstrap
  (printing-random-thing (instance stream)        ;code of PCL (See high.lisp).
    (let ((class (class-of instance)))
      (if (or (eq class (find-class 'standard-class nil))
              (eq class (find-class 'funcallable-standard-class nil))
              (eq class (find-class 'built-in-class nil)))
          (format stream "~a ~a" (early-class-name class)
                  (early-class-name instance))
          (format stream "~a" (early-class-name class))))))

(defmacro %allocate-instance--class (static-slot-storage-copy)
  `(let ((instance (%%allocate-instance--class)))
     (%allocate-instance--class-1 ,static-slot-storage-copy instance)
     instance))

(defmacro %allocate-instance--class-1 (static-slot-storage-copy instance)
  (once-only (instance)
    `(progn 
       (setf (std-instance-slots ,instance)
             (%allocate-static-slot-storage--class ,static-slot-storage-copy)))))

;;;
;;; This is the value that we stick into a slot to tell us that it is unbound.
;;; It may seem gross, but for performance reasons, we make this an interned
;;; symbol.  That means that the fast check to see if a slot is unbound is to
;;; say (EQ <val> '..SLOT-UNBOUND..).  That is considerably faster than looking
;;; at the value of a special variable.  Be careful, there are places in the
;;; code which actually use ..slot-unbound.. rather than this variable.  So
;;; much for modularity
;;; 
(defconstant *slot-unbound* '..slot-unbound..)

;;; As of July 92 PCL, %allocate-static-slot-storage--class does not take
;;; a number of slots.  As per the optimization suggestion in the first two
;;; chapters of the CLOS specification, it instead takes a copy of the slot
;;; storage vector and copies it.  This original vector copy (stored in
;;; wrapper-allocate-static-slot-storage-copy in the wrappers)  holds the
;;; results of the slot :initform forms that neither produce or depend on
;;; side-effects (with any other slots holding *slot-unbound*).

(defmacro %allocate-static-slot-storage--class (static-slot-storage-copy)
  `(locally (declare #.*optimize-speed*)
     (copy-simple-vector ,static-slot-storage-copy)))

(defmacro %allocate-origional-static-slot-storage-copy (no-of-slots)
  `(make-array ,no-of-slots :initial-element *slot-unbound*))

(defmacro std-instance-class (instance)
  `(wrapper-class (std-instance-wrapper ,instance)))



  ;;   
;;;;;; FUNCTION-ARGLIST
  ;;
;;; Given something which is functionp, function-arglist should return the
;;; argument list for it.  PCL does not count on having this available, but
;;; MAKE-SPECIALIZABLE works much better if it is available.  Versions of
;;; function-arglist for each specific port of pcl should be put in the
;;; appropriate xxx-low file. This is what it should look like:
;(defun function-arglist (function)
;  (<system-dependent-arglist-function> function))

(defun function-pretty-arglist (function)
  (declare (ignore function))
  ())

(defsetf function-pretty-arglist set-function-pretty-arglist)

(defun set-function-pretty-arglist (function new-value)
  (declare (ignore function))
  new-value)

;;;
;;; set-function-name
;;; When given a function should give this function the name <new-name>.
;;; Note that <new-name> is sometimes a list.  Some lisps get the upset
;;; in the tummy when they start thinking about functions which have
;;; lists as names.  To deal with that there is set-function-name-intern
;;; which takes a list spec for a function name and turns it into a symbol
;;; if need be.
;;;
;;; When given a funcallable instance, set-function-name MUST side-effect
;;; that FIN to give it the name.  When given any other kind of function
;;; set-function-name is allowed to return new function which is the 'same'
;;; except that it has the name.
;;;
;;; In all cases, set-function-name must return the new (or same) function.
;;; 
(defun set-function-name (function new-name)
  (declare (notinline set-function-name-1 intern-function-name))
  (set-function-name-1 function
                       (intern-function-name new-name)
                       new-name))

(defun set-function-name-1 (function new-name uninterned-name)
  (declare (ignore new-name uninterned-name))
  function)

(defun intern-function-name (name)
  (cond ((symbolp name) name)
        ((listp name)
         (intern (let ((*package* *the-pcl-package*)
                       (*print-case* :upcase)
                       (*print-pretty* nil)
                       (*print-gensym* 't))
                   (format nil "~S" name))
                 *the-pcl-package*))))


;;;
;;; COMPILE-LAMBDA
;;;
;;; This is like the Common Lisp function COMPILE.  In fact, that is what
;;; it ends up calling.  The difference is that it deals with things like
;;; watching out for recursive calls to the compiler or not calling the
;;; compiler in certain cases or allowing the compiler not to be present.
;;;
;;; This starts out with several variables and support functions which 
;;; should be conditionalized for any new port of PCL.  Note that these
;;; default to reasonable values, many new ports won't need to look at
;;; these values at all.
;;;
;;; *COMPILER-PRESENT-P*        NIL means the compiler is not loaded
;;;
;;; *COMPILER-SPEED*            one of :FAST :MEDIUM or :SLOW
;;;
;;; *COMPILER-REENTRANT-P*      T   ==> OK to call compiler recursively
;;;                             NIL ==> not OK
;;;
;;; function IN-THE-COMPILER-P  returns T if in the compiler, NIL otherwise
;;;                             This is not called if *compiler-reentrant-p*
;;;                             is T, so it only needs to be implemented for
;;;                             ports which have non-reentrant compilers.
;;;
;;;
;;; TL: 07/92: Added name optional parameter to compile-lambda to
;;; allow the lambda to be compiled with a given function name.

(defvar *compiler-present-p* t)

(defvar *compiler-speed*
        #+(or KCL IBCL GCLisp) :slow
        #-(or KCL IBCL GCLisp) :fast)

(defvar *compiler-reentrant-p*
        #+(and (not XKCL) (or KCL IBCL UCL)) nil
        #-(and (not XKCL) (or KCL IBCL UCL)) t)

(defun in-the-compiler-p ()
  #+(and (not xkcl) (or KCL IBCL)) compiler::*compiler-in-use*
  #+gclisp (typep (eval '(function (lambda ()))) 'lexical-closure)
  #+UCL (>= ucp::*compile-form-rec-level* 0)
  )

(defun compile-function (function)
  (cond #+kcl
        ((and (listp function) (eq (car function) 'lambda-closure))
         (compile-lambda `(lambda ,@(cddddr function))))
        ((functionp function) (compile-lambda function))
        (T "Function isn't in expected function form -- see
            documentation of PCL::COMPILE-FUNCTION.")))

(defun compile-lambda (lambda &optional (desirability :fast) name)
  (cond ((null *compiler-present-p*)
         (compile-lambda-uncompiled lambda name))
        ((and (null *compiler-reentrant-p*)
              (in-the-compiler-p))
         (compile-lambda-deferred lambda name))
        ((eq desirability :fast)
         (compile name lambda))
        ((and (eq desirability :medium)
              (memq *compiler-speed* '(:fast :medium)))
         (compile name lambda))
        ((and (eq desirability :slow)
              (eq *compiler-speed* ':fast))
         (compile name lambda))
        (t
          (compile-lambda-uncompiled lambda name))))

(defun compile-lambda-uncompiled (uncompiled &optional name)
  (let* ((function (coerce uncompiled 'function))
         (form #'(lambda (&rest args) (apply function args))))
    (if name
        (progn
          (setf (symbol-function name) form)
          name)
        form)))

(defun compile-lambda-deferred (uncompiled &optional name)
  (let* ((function (coerce uncompiled 'function))
         (compiled nil)
         (form
           #'(lambda (&rest args)
              (if compiled
                  (apply (the compiled-function compiled) args)
                  (if (in-the-compiler-p)
                      (apply function args)
                      (progn (setq compiled (compile name uncompiled))
                             (apply (the compiled-function compiled) args)))))))
    (if name
        (progn
          (setf (symbol-function name) form)
          name)
        form)))

(defmacro precompile-random-code-segments (&optional system)
  `(progn
     (eval-when (compile) (before-precompile-random-code-segments))
     (precompile-function-generators ,system)
     (precompile-dfun-constructors ,system)))

(defmacro force-compile (fn-name)
  "If the function named by FN-NAME isn't compiled, then compile it."
  (once-only (fn-name)
    `(unless (really-compiled-function-p (symbol-function ,fn-name))
       #+(and (not XKCL) (or KCL IBCL UCL))
       (if (in-the-compiler-p)
           (in-compiler-force-compile ,fn-name)
           (compile ,fn-name))
       #-(and (not XKCL) (or KCL IBCL UCL))
       (compile ,fn-name))))

(defun in-compiler-force-compile (fn-name)
  (let ((sym-fn (symbol-function fn-name)))
    (if (and (consp sym-fn) (eq (car sym-fn) 'lambda-block))
        (compile-lambda `(lambda ,@(cddr sym-fn)) fn-name)
        (error "Uncompiled SYMBOL-FUNCTION of ~S in unexpected
                form (FORCE-COMPILE)" fn-name))))



(defun record-definition (type spec &rest args)
  (declare (ignore type spec args))
  ())

(defun doctor-dfun-for-the-debugger (gf dfun) (declare (ignore gf)) dfun)


;Low level functions for structures


;Functions on arbitrary objects


