;;; -*- Mode:LISP; Package:PCL; Base:10; Syntax:Common-Lisp -*-
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

#|

This implementation of method lookup was redone in early August of 89.

It has the following properties:

 - It's modularity makes it easy to modify the actual caching algorithm.
   The caching algorithm is almost completely separated into the files
   cache.lisp and dlap.lisp.  This file just contains the various uses
   of it. There will be more tuning as we get more results from Luis'
   measurements of caching behavior.

 - The metacircularity issues have been dealt with properly.  All of
   PCL now grounds out properly.  Moreover, it is now possible to have
   metaobject classes which are themselves not instances of standard
   metaobject classes.

** Modularity of the code **

The actual caching algorithm is isolated in a modest number of functions.
The code which generates cache lookup code is all found in cache.lisp and
dlap.lisp.  Certain non-wrapper-caching special cases are in this file.


** Handling the metacircularity **

In CLOS, method lookup is the potential source of infinite metacircular
regress.  The metaobject protocol specification gives us wide flexibility
in how to address this problem.  PCL uses a technique which handles the
problem not only for the metacircular language described in Chapter 3, but
also for the PCL protocol which includes additional generic functions
which control more aspects of the CLOS implementation.

The source of the metacircular regress can be seen in a number of ways.
One is that the specified method lookup protocol must, as part of doing
the method lookup (or at least the cache miss case), itself call generic
functions.  It is easy to see that if the method lookup for a generic
function ends up calling that same generic function there can be trouble.

Fortunately, there is an easy solution at hand.  The solution is based on 
the restriction that portable code cannot change the class of a specified
metaobject.  This restriction implies that for specified generic functions,
the method lookup protocol they follow is fixed.  

More precisely, for such specified generic functions, most generic functions
that are called during their own method lookup will not run portable methods. 
This allows the implementation to usurp the actual generic function call in
this case.  In short, method lookup of a standard generic function, in the
case where the only applicable methods are themselves standard doesn't
have to do any method lookup to implement itself.

And so, we are saved.

|#



;An alist in which each entry is of the form :
;  (<generator> . (<subentry> ...))
;Each subentry is of the form:
;  (<args> <constructor> <system>)
(defvar *dfun-constructors* ())			

;If this is NIL, then the whole mechanism
;for caching dfun constructors is turned
;off.  The only time that makes sense is
;when debugging LAP code. 
(defvar *enable-dfun-constructor-caching* t)	

(defun show-dfun-constructors ()
  (format t "~&DFUN constructor caching is ~A." 
	  (if *enable-dfun-constructor-caching*
	      "enabled" "disabled"))
  (dolist (generator-entry *dfun-constructors*)
    (dolist (args-entry (cdr generator-entry))
      (format t "~&~S ~S"
	      (cons (car generator-entry) (caar args-entry))
	      (caddr args-entry)))))


(declaim (ftype (function (T &rest T) real-function) get-dfun-constructor))
(defun get-dfun-constructor (generator &rest args)
  (let* ((generator-entry (assq generator *dfun-constructors*))
	 (args-entry (assoc args (cdr generator-entry) :test #'equal)))
    (if (null *enable-dfun-constructor-caching*)
	(apply-function (symbol-function generator) args)
	(or (cadr args-entry)
	    (let ((new (apply-function (symbol-function generator) args)))
	      (if generator-entry
		  (push (list (copy-list args) new nil) (cdr generator-entry))
		  (push (list generator (list (copy-list args) new nil)) *dfun-constructors*))
	      new)))))

(defun load-precompiled-dfun-constructor (generator args system constructor)
  (let* ((generator-entry (assq generator *dfun-constructors*))
	 (args-entry (assoc args (cdr generator-entry) :test #'equal)))
    (unless args-entry
      (if generator-entry
	  (push (list args constructor system) (cdr generator-entry))
	  (push (list generator (list args constructor system)) *dfun-constructors*)))))

(defmacro precompile-dfun-constructors (&optional system)
  #+excl (declare (ignore system))
  #+excl ()
  #-excl
  (let ((*precompiling-lap* t))
    `(progn
       ,@(gathering1 (collecting)
	   (dolist (generator-entry *dfun-constructors*)
	     (dolist (args-entry (cdr generator-entry))
	       (when (or (null (caddr args-entry))
			 (eq (caddr args-entry) system))
		 (when system (setf (caddr args-entry) system))
		 (multiple-value-bind (closure-variables arguments 
							 iregs vregs fvregs tregs lap)
		     (apply-function (symbol-function (car generator-entry))
                                     (car args-entry))
		   (gather1
		     (make-top-level-form `(precompile-dfun-constructor 
					    ,(car generator-entry))
					  '(load)
		       `(load-precompiled-dfun-constructor
			  ',(car generator-entry)
			  ',(car args-entry)
			  ',system
			  (precompile-lap-closure-generator ,closure-variables
							    ,arguments
							    ,iregs
							    ,vregs
			                                    ,fvregs
							    ,tregs
							    ,lap))))))))))))


;;;
;;; When all the methods of a generic function are automatically generated
;;; reader or writer methods a number of special optimizations are possible.
;;; These are important because of the large number of generic functions of
;;; this type.
;;;
;;; There are a number of cases:
;;;
;;;   ONE-CLASS-ACCESSOR
;;;     In this case, the accessor generic function has only been called
;;;     with one class of argument.  There is no cache vector, the wrapper
;;;     of the one class, and the slot index are stored directly as closure
;;;     variables of the discriminating function.  This case can convert to
;;;     either of the next kind.
;;;
;;;   TWO-CLASS-ACCESSOR
;;;     Like above, but two classes.  This is common enough to do specially.
;;;     There is no cache vector.  The two classes are stored a separate
;;;     closure variables.
;;;
;;;   ONE-INDEX-ACCESSOR
;;;     In this case, the accessor generic function has seen more than one
;;;     class of argument, but the index of the slot is the same for all
;;;     the classes that have been seen.  A cache vector is used to store
;;;     the wrappers that have been seen, the slot index is stored directly
;;;     as a closure variable of the discriminating function.  This case
;;;     can convert to the next kind.
;;;
;;;   N-N-ACCESSOR
;;;     This is the most general case.  In this case, the accessor generic
;;;     function has seen more than one class of argument and more than one
;;;     slot index.  A cache vector stores the wrappers and corresponding
;;;     slot indexes.  Because each cache line is more than one element
;;;     long, a cache lock count is used.
;;;
(defstruct (dfun-info
	     (:constructor nil)
	     (:print-function print-dfun-info))
  (cache nil))

(defun print-dfun-info (dfun-info stream depth)
  (declare (ignore depth) (stream stream))
  (printing-random-thing (dfun-info stream)
    (format stream "~A" (type-of dfun-info))))

(defstruct (no-methods
	     (:constructor no-methods-dfun-info ())
	     (:include dfun-info)))

(defstruct (initial-dispatch
	     (:constructor initial-dispatch-dfun-info ())
	     (:include dfun-info)))

(defstruct (dispatch
	     (:constructor dispatch-dfun-info ())
	     (:include dfun-info)))

(defstruct (default-method-only
	     (:constructor default-method-only-dfun-info ())
	     (:include dfun-info)))

;without caching:
;  dispatch one-class two-class default-method-only

;with caching:
;  one-index n-n checking caching

;accessor:
;  one-class two-class one-index n-n
(defstruct (accessor-dfun-info
	     (:constructor nil)
	     (:include dfun-info))
  accessor-type) ; (member reader writer)

(defmacro dfun-info-accessor-type (di)
  `(accessor-dfun-info-accessor-type ,di))

(defstruct (one-index-dfun-info
	     (:constructor nil)
	     (:include accessor-dfun-info))
  index)

(defmacro dfun-info-index (di)
  `(one-index-dfun-info-index ,di))

(defstruct (n-n
	     (:constructor n-n-dfun-info (accessor-type cache))
	     (:include accessor-dfun-info)))

(defstruct (one-class
	     (:constructor one-class-dfun-info (accessor-type index wrapper0))
	     (:include one-index-dfun-info))
  wrapper0)

(defmacro dfun-info-wrapper0 (di)
  `(one-class-wrapper0 ,di))

(defstruct (two-class
	     (:constructor two-class-dfun-info (accessor-type index wrapper0 wrapper1))
	     (:include one-class))
  wrapper1)

(defmacro dfun-info-wrapper1 (di)
  `(two-class-wrapper1 ,di))

(defstruct (one-index
	     (:constructor one-index-dfun-info
			   (accessor-type index cache))
	     (:include one-index-dfun-info)))	     

(defstruct (checking
	     (:constructor checking-dfun-info (function cache))
	     (:include dfun-info))
  function)

(defmacro dfun-info-function (di)
  `(checking-function ,di))

(defstruct (caching
	     (:constructor caching-dfun-info (cache))
	     (:include dfun-info)))

(defstruct (constant-value
	     (:constructor constant-value-dfun-info (cache))
	     (:include dfun-info)))

(defmacro dfun-update (generic-function function &rest args)
  `(multiple-value-bind (dfun cache info)
       (funcall-function ,function ,generic-function ,@args)
     (update-dfun ,generic-function dfun cache info)))

(defun accessor-miss-function (gf dfun-info)
  (ecase (dfun-info-accessor-type dfun-info)
    (reader
      #'(lambda (arg)
	   (declare (pcl-fast-call))
	   (accessor-miss gf nil arg dfun-info)))
    (writer
     #'(lambda (new arg)
	 (declare (pcl-fast-call))
	 (accessor-miss gf new arg dfun-info)))))


(declaim (ftype (function (T T T T) (values function null T))
		make-one-class-accessor-dfun))
(declaim (ftype (function (T T T T T) (values function null T))
		make-two-class-accessor-dfun))
(declaim (ftype (function (T T T &optional T) (values function cache T))
		make-one-index-accessor-dfun))
(declaim (ftype (function (T T T T) (values function cache T))
		make-final-one-index-accessor-dfun))
(declaim (ftype (function (T T &optional T) (values function cache T))
		make-n-n-accessor-dfun))
(declaim (ftype (function (T T T) (values function cache T))
		make-final-n-n-accessor-dfun))
(declaim (ftype (function (T T &optional T) (values function (or cache null) T))
		make-checking-dfun))
(declaim (ftype (function (T T T T) (values function (or cache null) T))
		make-final-checking-dfun))
(declaim (ftype (function (T &optional T) (values function (or cache null) T))
		make-caching-dfun))
(declaim (ftype (function (T T T) (values function (or cache null) T))
		make-final-caching-dfun))
(declaim (ftype (function (T &optional T) (values function cache T))
		make-constant-value-dfun))
(declaim (ftype (function (T T T) (values function cache T))
		make-final-constant-value-dfun))
(declaim (ftype (function (T) (values function null T))
		make-dispatch-dfun
		make-final-dispatch-dfun))
(declaim (ftype (function (T &optional T) (values function (or cache null) T))
		make-final-dfun-internal))

(declaim (ftype (function (T) boolean)
		use-caching-dfun-p
		use-dispatch-dfun-p))

;;;
;;; ONE-CLASS-ACCESSOR
;;;
(declaim (ftype (function (T T T T) (values function null T))
		make-one-class-accessor-dfun))
(defun make-one-class-accessor-dfun (gf type wrapper index)
  (let ((emit (if (eq type 'reader) 'emit-one-class-reader 'emit-one-class-writer))
	(dfun-info (one-class-dfun-info type index wrapper)))
    (values
     (funcall (get-dfun-constructor emit (consp index))
	      wrapper index
	      (accessor-miss-function gf dfun-info))
     nil
     dfun-info)))

;;;
;;; TWO-CLASS-ACCESSOR
;;;
(declaim (ftype (function (T T T T T) (values function null T))
		make-two-class-accessor-dfun))
(defun make-two-class-accessor-dfun (gf type w0 w1 index)
  (let ((emit (if (eq type 'reader) 'emit-two-class-reader 'emit-two-class-writer))
	(dfun-info (two-class-dfun-info type index w0 w1)))
    (values
     (funcall (get-dfun-constructor emit (consp index))
	      w0 w1 index
	      (accessor-miss-function gf dfun-info))
     nil
     dfun-info)))

;;;
;;; std accessors same index dfun
;;;
(declaim (ftype (function (T T T &optional T) (values function cache T))
		make-one-index-accessor-dfun))
(defun make-one-index-accessor-dfun (gf type index &optional cache)
  (let* ((emit (if (eq type 'reader) 'emit-one-index-readers 'emit-one-index-writers))
	 (cache (or cache (get-cache 1 nil #'one-index-limit-fn 4)))
	 (dfun-info (one-index-dfun-info type index cache)))
    (declare (type cache cache))
    (values
     (funcall (get-dfun-constructor emit (consp index))
	      (cache-field cache) (cache-vector cache) 
	      (cache-mask cache) (cache-size cache)
	      index
	      (accessor-miss-function gf dfun-info))
     cache
     dfun-info)))

(declaim (ftype (function (T T T T) (values function cache T))
		make-final-one-index-accessor-dfun))
(defun make-final-one-index-accessor-dfun (gf type index table)
  (let ((cache (fill-dfun-cache table nil 1 #'one-index-limit-fn)))
    (make-one-index-accessor-dfun gf type index cache)))				

(defun one-index-limit-fn (nlines)
  (default-limit-fn nlines))


(declaim (ftype (function (T T &optional T) (values function cache T))
		make-n-n-accessor-dfun))
(defun make-n-n-accessor-dfun (gf type &optional cache)
  (let* ((emit (if (eq type 'reader) 'emit-n-n-readers 'emit-n-n-writers))
	 (cache (or cache (get-cache 1 t #'n-n-accessors-limit-fn 2)))
	 (dfun-info (n-n-dfun-info type cache)))
    (declare (type cache cache))
    (values
     (funcall (get-dfun-constructor emit)
	      (cache-field cache) (cache-vector cache) 
	      (cache-mask cache) (cache-size cache)
	      (accessor-miss-function gf dfun-info))
     cache
     dfun-info)))

(declaim (ftype (function (T T T) (values function cache T))
		make-final-n-n-accessor-dfun))
(defun make-final-n-n-accessor-dfun (gf type table)
  (let ((cache (fill-dfun-cache table t 1 #'n-n-accessors-limit-fn)))
    (make-n-n-accessor-dfun gf type cache)))

(defun n-n-accessors-limit-fn (nlines)
  (default-limit-fn nlines))

(defun fill-dfun-cache (table valuep nkeys limit-fn &optional cache)
  (let ((cache
          (or cache
              (get-cache
                 nkeys valuep limit-fn
		 (the index (+ (the index (hash-table-count table)) 3))))))
    (maphash #'(lambda (classes value)
		 (setq cache (fill-cache cache
					 (class-wrapper classes)
					 value
					 t)))
	     table)
    cache))


;;;
;;;
;;;


(defun make-checking-dfun (generic-function function &optional cache)
  (unless cache
    (when (some #'(lambda (method)
		    (method-closure-generator method))
		(generic-function-methods generic-function))
      (return-from make-checking-dfun (make-caching-dfun generic-function)))
    (when (use-dispatch-dfun-p generic-function)
      (return-from make-checking-dfun (make-dispatch-dfun generic-function))))
  (let* ((arg-info (gf-arg-info generic-function))
	 (metatypes (arg-info-metatypes arg-info))
	 (applyp (arg-info-applyp arg-info))
	 (nkeys (arg-info-nkeys arg-info)))
    (declare (type boolean applyp) (type index nkeys))
    (if (every #'(lambda (mt) (eq mt 't)) metatypes)
	(values function nil (default-method-only-dfun-info))
	(let* ((cache (or cache (get-cache nkeys nil #'checking-limit-fn 2)))
	       (dfun-info (checking-dfun-info function cache)))
	  (values
	   (funcall (get-dfun-constructor 'emit-checking metatypes applyp)
		    (cache-field cache) (cache-vector cache) 
		    (cache-mask cache) (cache-size cache)
		    function 
		    #'(lambda (&rest args)
			(declare (pcl-fast-call))
			(checking-miss generic-function args dfun-info)))
	   cache
	   dfun-info)))))

(defun make-final-checking-dfun (generic-function function
						  classes-list new-class)
  (let ((metatypes (arg-info-metatypes (gf-arg-info generic-function))))
    (if (every #'(lambda (mt) (eq mt 't)) metatypes)
	(values function nil (default-method-only-dfun-info))
	(let ((cache (make-final-ordinary-dfun-internal 
		      generic-function nil #'checking-limit-fn 
		      classes-list new-class)))
	  (make-checking-dfun generic-function function cache)))))

(defun checking-limit-fn (nlines)
  (default-limit-fn nlines))


;;;
;;;
;;;
(defun make-caching-dfun (generic-function &optional cache)
  (unless cache
    (when (use-constant-value-dfun-p generic-function)
      (return-from make-caching-dfun (make-constant-value-dfun generic-function)))
    (when (use-dispatch-dfun-p generic-function)
      (return-from make-caching-dfun (make-dispatch-dfun generic-function))))
  (let* ((arg-info (gf-arg-info generic-function))
	 (metatypes (arg-info-metatypes arg-info))
	 (applyp (arg-info-applyp arg-info))
	 (nkeys (arg-info-nkeys arg-info))
	 (cache (or cache (get-cache nkeys t #'caching-limit-fn 2)))
	 (dfun-info (caching-dfun-info cache)))
    (declare (type boolean applyp) (type index nkeys))
    (values
     (funcall (get-dfun-constructor 'emit-caching metatypes applyp)
	      (cache-field cache) (cache-vector cache) 
	      (cache-mask cache) (cache-size cache)
	      #'(lambda (&rest args)
		  (declare (pcl-fast-call))
		  (caching-miss generic-function args dfun-info)))
     cache
     dfun-info)))

(defun make-final-caching-dfun (generic-function classes-list new-class)
  (let ((cache (make-final-ordinary-dfun-internal 
		generic-function t #'caching-limit-fn
		classes-list new-class)))
    (make-caching-dfun generic-function cache)))

(defun caching-limit-fn (nlines)
  (default-limit-fn nlines))

(defun use-constant-value-dfun-p (gf)
  (let ((methods (generic-function-methods gf))
	(default '(unknown)))
    (declare (type list methods))
    (and (null (arg-info-applyp (gf-arg-info gf)))
	 (compute-applicable-methods-emf-std-p gf)
	 (< 1 (length methods))
	 (some #'(lambda (method)
		   (every #'(lambda (specl) (eq specl *the-class-t*))
			  (method-specializers method)))
	       methods)
	 (notany #'(lambda (method)
		     (or (some #'eql-specializer-p
			       (method-specializers method))
			 (eq (plist-value method :constant-value default)
			     default)))
		methods))))

(defun make-constant-value-dfun (generic-function &optional cache)
  (let* ((arg-info (gf-arg-info generic-function))
	 (metatypes (arg-info-metatypes arg-info))
	 (nkeys (arg-info-nkeys arg-info))
	 (cache (or cache (get-cache nkeys t #'caching-limit-fn 2)))
	 (dfun-info (constant-value-dfun-info cache)))
    (values
     (funcall (get-dfun-constructor 'emit-constant-value metatypes)
	      (cache-field cache) (cache-vector cache) 
	      (cache-mask cache) (cache-size cache)
	      #'(lambda (&rest args)
		  (declare (pcl-fast-call))
		  (constant-value-miss generic-function args dfun-info)))
     cache
     dfun-info)))

(defun make-final-constant-value-dfun (generic-function classes-list new-class)
  (let ((cache (make-final-ordinary-dfun-internal 
		generic-function :constant-value #'caching-limit-fn
		classes-list new-class)))
    (make-constant-value-dfun generic-function cache)))

(defun make-final-ordinary-dfun-internal (generic-function valuep limit-fn
					  classes-list new-class)
  (let* ((arg-info (gf-arg-info generic-function))
	 (nkeys (arg-info-nkeys arg-info))
	 (new-class (and new-class
			 (equal (type-of (gf-dfun-info generic-function))
				(cond ((eq valuep t) 'caching)
				      ((eq valuep :constant-value) 'constant-value)
				      ((null valuep) 'checking)))
			 new-class))
	 (cache (if new-class
		    (copy-cache (gf-dfun-cache generic-function))
		    (get-cache nkeys (not (null valuep)) limit-fn 4))))
      (declare (type index nkeys))
      (make-emf-cache generic-function valuep cache classes-list new-class)))

(defun use-caching-dfun-p (gf)
  (some #'method-function-for-caching-p (generic-function-methods gf)))

(defun use-dispatch-dfun-p (gf)
  (unless (use-caching-dfun-p gf)
    (let* ((methods (generic-function-methods gf))
	   (arg-info (gf-arg-info gf))
	   (mt (arg-info-metatypes arg-info))
	   (nreq (length mt)))
      (declare (type list methods mt) (type index nreq))
      ;;Is there a position at which every specializer is eql or non-standard?
      (dotimes (i nreq nil)
	(when (not (eq 't (nth i mt)))
	  (let ((some-std-class-specl-p nil))
	    (dolist (method methods)
	      (let ((specl (nth i (method-specializers method))))
		(when (and (not (eql-specializer-p specl))
                           (class-standard-p (specializer-class specl)))
		  (setq some-std-class-specl-p t))))
	    (unless some-std-class-specl-p
	      (return-from use-dispatch-dfun-p t))))))))

(defvar *lazy-dispatch-dfun-compute-p* t)
(defvar *lazy-dfun-compute-p* nil)

(defun make-dispatch-dfun (gf)
  (values (get-dispatch-function gf) nil (dispatch-dfun-info)))

(defun make-final-dispatch-dfun (gf)
  (if *lazy-dispatch-dfun-compute-p*
      (values (let ((*lazy-dfun-compute-p* t))
		(make-initial-dfun gf))
	      nil (initial-dispatch-dfun-info))
      (make-dispatch-dfun gf)))

(defun before-precompile-random-code-segments ()
  (dolist (gf (gfs-of-type '(dispatch initial-dispatch)))
    (dfun-update gf #'make-dispatch-dfun)))

(defvar *dfun-miss-gfs-on-stack* ())

(defmacro dfun-miss ((gf args wrappers invalidp nfunction 
		      &optional type index caching-p applicable)
		     &body body)
  (unless applicable (setq applicable (gensym)))
  `(multiple-value-bind (,wrappers ,invalidp ,nfunction ,applicable 
				   ,@(when type `(,type ,index)))
       (cache-miss-values ,gf ,args ',(cond (type 'accessor)
					    (caching-p 'caching)
					    (t 'checking)))
     (declare (type boolean ,invalidp))
     (when (and ,applicable (not (memq ,gf *dfun-miss-gfs-on-stack*)))
       (let ((*dfun-miss-gfs-on-stack* (cons ,gf *dfun-miss-gfs-on-stack*)))
	 ,@body))
     (method-function-apply ,nfunction ,args)))

;;;
;;; The dynamically adaptive method lookup algorithm is implemented is
;;; implemented as a kind of state machine.  The kinds of discriminating
;;; function is the state, the various kinds of reasons for a cache miss
;;; are the state transitions.
;;;
;;; The code which implements the transitions is all in the miss handlers
;;; for each kind of dfun.  Those appear here.
;;;
;;; Note that within the states that cache, there are dfun updates which
;;; simply select a new cache or cache field.  Those are not considered
;;; as state transitions.
;;; 
(defun make-initial-dfun (gf)
  (if (or *lazy-dfun-compute-p* (not (compute-applicable-methods-emf-std-p gf)))
      #'(lambda (&rest args)
	  #+Genera (declare (dbg:invisible-frame :pcl-internals))
	  (initial-dfun args gf))
      (make-final-dfun gf (precompute-effective-methods gf t))))

(defun initial-dfun (args generic-function)
  #+Genera (declare (dbg:invisible-frame :pcl-internals))
  (dfun-miss (generic-function args wrappers invalidp nfunction ntype nindex)
    (cond (invalidp)
	  ((and ntype nindex)
	   (dfun-update generic-function
			#'make-one-class-accessor-dfun ntype wrappers nindex))
	  (t
	   (dfun-update generic-function #'make-checking-dfun
			;; nfunction is suitable only for caching, have to do this:
			(multiple-value-bind (w i function)
			    (cache-miss-values generic-function args 'checking)
			  (declare (ignore w i))
			  function))))))

(defun make-final-dfun (gf &optional classes-list)
  (multiple-value-bind (dfun cache info)
      (make-final-dfun-internal gf classes-list)
    (set-dfun gf dfun cache info)))

(defun make-final-dfun-internal (gf &optional classes-list)
  (let ((methods (generic-function-methods gf)) type
	(new-class *new-class*) (*new-class* nil))
    (cond ((null methods)
	   (values
	    #'(lambda (&rest args)
		(apply #'no-applicable-method gf args))
	    nil
	    (no-methods-dfun-info)))
	  ((setq type (cond ((every #'standard-reader-method-p methods)
			     'reader)
			    ((every #'standard-writer-method-p methods)
			     'writer)))
	   (with-eq-hash-table (table)
	     (multiple-value-bind (table all-index first second size no-class-slots-p)
		 (make-accessor-table gf type table)
               (declare (type boolean no-class-slots-p))
	       (if table
		   (cond ((= (the index size) 1)
			  (let ((w (class-wrapper first)))
			    (make-one-class-accessor-dfun gf type w all-index)))
			 ((and (= (the index size) 2)
                               (or (integerp all-index) (consp all-index)))
			  (let ((w0 (class-wrapper first))
				(w1 (class-wrapper second)))
			    (make-two-class-accessor-dfun gf type w0 w1 all-index)))
			 ((or (integerp all-index) (consp all-index))
			  (make-final-one-index-accessor-dfun 
			   gf type all-index table))
			 (no-class-slots-p
			  (make-final-n-n-accessor-dfun gf type table))
			 (t
			  (make-final-caching-dfun gf classes-list new-class)))
		   (make-final-caching-dfun gf classes-list new-class)))))
	  ((use-constant-value-dfun-p gf)
	   (make-final-constant-value-dfun gf classes-list new-class))
	  ((use-dispatch-dfun-p gf)
	   (make-final-dispatch-dfun gf))
	  ((let ((specls (method-specializers (car methods))))
	     (and (every #'(lambda (method)
			     (and (equal specls (method-specializers method))))
			 methods)
		  (not (use-caching-dfun-p gf))))
	   (let ((function (get-secondary-dispatch-function gf methods nil)))
	     (make-final-checking-dfun gf function classes-list new-class)))
	  (t
	   (make-final-caching-dfun gf classes-list new-class)))))

(defun accessor-miss (gf new object dfun-info)
  (let* ((ostate (type-of dfun-info))
	 (otype (dfun-info-accessor-type dfun-info))
	 oindex ow0 ow1 cache
	 (args (ecase otype			;The congruence rules assure
		(reader (list object))		;us that this is safe despite
		(writer (list new object)))))	;not knowing the new type yet.
    (dfun-miss (gf args wrappers invalidp nfunction ntype nindex)
      ;;
      ;; The following lexical functions change the state of the
      ;; dfun to that which is their name.  They accept arguments
      ;; which are the parameters of the new state, and get other
      ;; information from the lexical variables bound above.
      ;; 
      (flet ((two-class (index w0 w1)
	       (when (zerop (the index (random 2))) (psetf w0 w1 w1 w0))
	       (dfun-update gf #'make-two-class-accessor-dfun ntype w0 w1 index))
	     (one-index (index &optional cache)
	       (dfun-update gf #'make-one-index-accessor-dfun ntype index cache))
	     (n-n (&optional cache)
	       (if (consp nindex)
		   (dfun-update gf #'make-checking-dfun nfunction)
		   (dfun-update gf #'make-n-n-accessor-dfun ntype cache)))
	     (caching () ; because cached accessor emfs are much faster for accessors
	       (dfun-update gf #'make-caching-dfun))
	     ;;
	     (do-fill (update-fn)
	       (let ((ncache (fill-cache cache wrappers nindex)))
		 (unless (eq ncache cache)
		   (funcall-function update-fn ncache)))))
	(cond ((null ntype)
	       (caching))
	      ((or invalidp
		   (null nindex)))
	      ((not (or (std-instance-p object)
			(fsc-instance-p object)
                        #+pcl-user-instances
                        (user-instance-p object)))
	       (caching))
	      ((or (neq ntype otype) (listp wrappers))
	       (caching))
	      (t
	       (ecase ostate
		 (one-class
		  (setq oindex (dfun-info-index dfun-info))
		  (setq ow0 (dfun-info-wrapper0 dfun-info))
		  (unless (eq ow0 wrappers)
		    (if (eql nindex oindex)
			(two-class nindex ow0 wrappers)
			(n-n))))
		 (two-class
		  (setq oindex (dfun-info-index dfun-info))
		  (setq ow0 (dfun-info-wrapper0 dfun-info))
		  (setq ow1 (dfun-info-wrapper1 dfun-info))
		  (unless (or (eq ow0 wrappers) (eq ow1 wrappers))
		    (if (eql nindex oindex)
			(one-index nindex)
			(n-n))))
		 (one-index
		  (setq oindex (dfun-info-index dfun-info))
		  (setq cache (dfun-info-cache dfun-info))
		  (if (eql nindex oindex)
		      (do-fill #'(lambda (ncache)
				   (one-index nindex ncache)))
		      (n-n)))
		 (n-n
		  (setq cache (dfun-info-cache dfun-info))
		  (if (consp nindex)
		      (caching)
		      (do-fill #'n-n))))))))))

(defun checking-miss (generic-function args dfun-info)
  (let ((ofunction (dfun-info-function dfun-info))
	(cache (dfun-info-cache dfun-info)))
    (dfun-miss (generic-function args wrappers invalidp nfunction)
      (cond (invalidp)
	    ((eq ofunction nfunction)
	     (let ((ncache (fill-cache cache wrappers nil)))
	       (unless (eq ncache cache)
		 (dfun-update generic-function #'make-checking-dfun 
			      nfunction ncache))))
	    (t
	     (dfun-update generic-function #'make-caching-dfun))))))

(defun caching-miss (generic-function args dfun-info)
  (let ((ocache (dfun-info-cache dfun-info)))
    (dfun-miss (generic-function args wrappers invalidp function nil nil t)
      (cond (invalidp)
	    (t
	     (let ((ncache (fill-cache ocache wrappers function)))
	       (unless (eq ncache ocache)
		 (dfun-update generic-function 
			      #'make-caching-dfun ncache))))))))

(defun constant-value-miss (generic-function args dfun-info)
  (let ((ocache (dfun-info-cache dfun-info)))
    (dfun-miss (generic-function args wrappers invalidp function nil nil t)
      (cond (invalidp)
	    (t
	     (let* ((value (method-constant-value (method-function-method function)))
		    (ncache (fill-cache ocache wrappers value)))
	       (unless (eq ncache ocache)
		 (dfun-update generic-function
			      #'make-constant-value-dfun ncache))))))))



(defvar dfun-count nil)
(defvar dfun-list nil)
(defvar *minimum-cache-size-to-list*)

(defun list-dfun (gf)
  (let* ((sym (type-of (gf-dfun-info gf)))
	 (a (assq sym dfun-list)))
    (unless a
      (push (setq a (list sym)) dfun-list))
    (push (generic-function-name gf) (cdr a))))

(defun list-all-dfuns ()
  (setq dfun-list nil)
  (map-all-generic-functions #'list-dfun)
  dfun-list)

(defun list-large-cache (gf)
  (let* ((sym (type-of (gf-dfun-info gf)))
	 (cache (gf-dfun-cache gf)))
    (when cache
      (let ((size (cache-size cache)))
        (declare (type index size))
	(when (>= size (the index *minimum-cache-size-to-list*))
	  (let ((a (assoc size dfun-list)))
	    (unless a
	      (push (setq a (list size)) dfun-list))
	    (push (let ((name (generic-function-name gf)))
		    (if (eq sym 'caching) name (list name sym)))
		  (cdr a))))))))

(defun list-large-caches (&optional (*minimum-cache-size-to-list* 130))
  (setq dfun-list nil)
  (map-all-generic-functions #'list-large-cache)
  (setq dfun-list (sort dfun-list #'< :key #'car))
  (mapc #'print dfun-list)
  (values))


(defun count-dfun (gf)
  (let* ((sym (type-of (gf-dfun-info gf)))
	 (cache (gf-dfun-cache gf))
	 (a (assq sym dfun-count)))
    (unless a
      (push (setq a (list sym 0 nil)) dfun-count))
    (setf (cadr a) (the index (1+ (the index (cadr a)))))
    (when cache
      (let* ((size (cache-size cache))
	     (b (assoc size (third a))))
	(unless b 
	  (push (setq b (cons size 0)) (third a)))
        (setf (cdr b) (the index (1+ (the index (cdr b)))))))))

(defun count-all-dfuns ()
  (setq dfun-count (mapcar #'(lambda (type) (list type 0 nil))
			   '(ONE-CLASS TWO-CLASS DEFAULT-METHOD-ONLY
			     ONE-INDEX N-N CHECKING CACHING 
			     DISPATCH)))
  (map-all-generic-functions #'count-dfun)
  (mapc #'(lambda (type+count+sizes)
	    (setf (third type+count+sizes)
		  (sort (third type+count+sizes) #'< :key #'car)))
	dfun-count)
  (mapc #'(lambda (type+count+sizes)
	    (format t "~&There are ~4d dfuns of type ~s"
		    (cadr type+count+sizes) (car type+count+sizes))
	    (format t "~%   ~S~%" (caddr type+count+sizes)))
	dfun-count)
  (values))

(defun gfs-of-type (type)
  (unless (consp type) (setq type (list type)))
  (let ((gf-list nil))
    (map-all-generic-functions #'(lambda (gf)
				   (when (memq (type-of (gf-dfun-info gf)) type)
				     (push gf gf-list))))
    gf-list))
