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
;;; The basics of the PCL wrapper cache mechanism.
;;;

(in-package 'pcl)
;;;
;;; The caching algorithm implemented:
;;;
;;; << put a paper here >>
;;;
;;; For now, understand that as far as most of this code goes, a cache has
;;; two important properties.  The first is the number of wrappers used as
;;; keys in each cache line.  Throughout this code, this value is always
;;; called NKEYS.  The second is whether or not the cache lines of a cache
;;; store a value.  Throughout this code, this always called VALUEP.
;;;
;;; Depending on these values, there are three kinds of caches.
;;;
;;; NKEYS = 1, VALUEP = NIL
;;;
;;; In this kind of cache, each line is 1 word long.  No cache locking is
;;; needed since all read's in the cache are a single value.  Nevertheless
;;; line 0 (location 0) is reserved, to ensure that invalid wrappers will
;;; not get a first probe hit.
;;;
;;; To keep the code simpler, a cache lock count does appear in location 0
;;; of these caches, that count is incremented whenever data is written to
;;; the cache.  But, the actual lookup code (see make-dlap) doesn't need to
;;; do locking when reading the cache.
;;; 
;;;
;;; NKEYS = 1, VALUEP = T
;;;
;;; In this kind of cache, each line is 2 words long.  Cache locking must
;;; be done to ensure the synchronization of cache reads.  Line 0 of the
;;; cache (location 0) is reserved for the cache lock count.  Location 1
;;; of the cache is unused (in effect wasted).
;;; 
;;; NKEYS > 1
;;;
;;; In this kind of cache, the 0 word of the cache holds the lock count.
;;; The 1 word of the cache is line 0.  Line 0 of these caches is not
;;; reserved.
;;;
;;; This is done because in this sort of cache, the overhead of doing the
;;; cache probe is high enough that the 1+ required to offset the location
;;; is not a significant cost.  In addition, because of the larger line
;;; sizes, the space that would be wasted by reserving line 0 to hold the
;;; lock count is more significant.
;;;

(declaim (ftype (function () index)
		get-wrapper-cache-number))
(declaim (ftype (function (T T T) (values index index index index))
                compute-cache-parameters))
(declaim (ftype (function (T T T) index)
		compute-primary-cache-location
		compute-primary-cache-location-from-location))
(declaim (ftype (function (T) index)
		cache-count))
(declaim (ftype (function (T T T T) boolean)
		fill-cache-p
		fill-cache-from-cache-p))
(declaim (ftype (function (T T &optional T) (values T boolean))
		find-free-cache-line))
(declaim (ftype (function (index) index)
		compute-line-size
		default-limit-fn
		power-of-two-ceiling))
(declaim (ftype (function (T) boolean)
		free-cache-vector))


;;;
;;; Caches
;;;
;;; A cache is essentially just a vector.  The use of the individual `words'
;;; in the vector depends on particular properties of the cache as described
;;; above.
;;;
;;; This defines an abstraction for caches in terms of their most obvious
;;; implementation as simple vectors.  But, please notice that part of the
;;; implementation of this abstraction, is the function lap-out-cache-ref.
;;; This means that most port-specific modifications to the implementation
;;; of caches will require corresponding port-specific modifications to the
;;; lap code assembler.
;;;
(defmacro cache-vector-ref (cache-vector location)
  `(svref (the simple-vector ,cache-vector)
          (#-cmu the #+cmu ext:truly-the fixnum ,location)))

(defun emit-cache-vector-ref (cache-vector-operand location-operand)
  (operand :iref cache-vector-operand location-operand))


(defmacro cache-vector-size (cache-vector)
  `(array-dimension (the simple-vector ,cache-vector) 0))

(defun allocate-cache-vector (size)
  (declare (type index size))
  (make-array size :adjustable nil))

(defmacro cache-vector-lock-count (cache-vector)
  `(cache-vector-ref ,cache-vector 0))

(defun flush-cache-vector-internal (cache-vector)
  (without-interrupts-simple
    (fill (the simple-vector cache-vector) nil)
    (setf (cache-vector-lock-count cache-vector) 0))
  cache-vector)

(defmacro modify-cache (cache-vector &body body)
  `(without-interrupts-simple
     (multiple-value-prog1
       (progn ,@body)
       (let ((old-count (cache-vector-lock-count ,cache-vector)))
	 (declare (type index old-count))
	 (setf (cache-vector-lock-count ,cache-vector)
               (the index
	            (if (= old-count most-positive-fixnum)
		        1
                        (the index (1+ old-count)))))))))

(deftype field-type ()
  '(integer 0    ;#.(position 'number wrapper-layout)
            7))  ;#.(position 'number wrapper-layout :from-end t)

(eval-when (compile load eval)
(defun power-of-two-ceiling (x)
  (declare (type index x))
  ;;(expt 2 (ceiling (log x 2)))
  (the index (ash 1 (integer-length (1- x)))))

(defconstant *nkeys-limit* 256)
)

(defstruct (cache
	     (:print-function print-cache)
	     (:constructor make-cache ())
	     (:copier copy-cache-internal))
  (nkeys 1 :type (integer 1 #.*nkeys-limit*))
  (valuep nil :type boolean)
  (nlines 0 :type index)
  (field 0 :type field-type)
  (limit-fn #'default-limit-fn :type real-function)
  (mask 0 :type index)
  (size 0 :type index)
  (line-size 1 :type (integer 1 #.(power-of-two-ceiling (1+ *nkeys-limit*))))
  (max-location 0 :type index)
  (vector '#() :type simple-vector)
  (overflow nil :type list))

(defun print-cache (cache stream depth)
  (declare (ignore depth))
  (printing-random-thing (cache stream)
    (format stream "cache ~D ~S ~D" 
	    (cache-nkeys cache) (cache-valuep cache) (cache-nlines cache))))

#+akcl
(si::freeze-defstruct 'cache)

(defmacro cache-lock-count (cache)
  `(cache-vector-lock-count (cache-vector ,cache)))


;;;
;;; Some facilities for allocation and freeing caches as they are needed.
;;; This is done on the assumption that a better port of PCL will arrange
;;; to cons these all the same static area.  Given that, the fact that
;;; PCL tries to reuse them should be a win.
;;; 
(defvar *free-cache-vectors* (make-hash-table :size 16 :test 'eql))

;;;
;;; Return a cache that has had flush-cache-internal called on it.  This
;;; returns a cache of exactly the size requested, it won't ever return a
;;; larger cache.
;;; 
(defun get-cache-vector (size)
  (let ((entry (gethash size *free-cache-vectors*)))
    (without-interrupts-simple
      (cond ((null entry)
	     (setf (gethash size *free-cache-vectors*) (cons 0 nil))
	     (get-cache-vector size))
	    ((null (cdr entry))
             (setf (car entry) (the fixnum (1+ (the fixnum (car entry)))))
	     (flush-cache-vector-internal (allocate-cache-vector size)))
	    (t
	     (let ((cache (cdr entry)))
	       (setf (cdr entry) (cache-vector-ref cache 0))
	       (flush-cache-vector-internal cache)))))))

(defun free-cache-vector (cache-vector)
  (let ((entry (gethash (cache-vector-size cache-vector) *free-cache-vectors*)))
    (without-interrupts-simple
      (if (null entry)
	  (error "Attempt to free a cache-vector not allocated by GET-CACHE-VECTOR.")
	  (let ((thread (cdr entry)))
	    (loop (unless thread (return))
		  (when (eq thread cache-vector) (error "Freeing a cache twice."))
		  (setq thread (cache-vector-ref thread 0)))	  
	    (flush-cache-vector-internal cache-vector)		;Help the GC
	    (setf (cache-vector-ref cache-vector 0) (cdr entry))
	    (setf (cdr entry) cache-vector)
	    nil)))))

;;;
;;; This is just for debugging and analysis.  It shows the state of the free
;;; cache resource.
;;; 
(defun show-free-cache-vectors ()
  (let ((elements ()))
    (maphash #'(lambda (s e) (push (list s e) elements)) *free-cache-vectors*)
    (setq elements (sort elements #'< :key #'car))
    (dolist (e elements)
      (let* ((size (car e))
	     (entry (cadr e))
	     (allocated (car entry))
	     (head (cdr entry))
	     (free 0))
        (declare (type index allocated free))
	(loop (when (null head) (return t))
	      (setq head (cache-vector-ref head 0))
	      (incf free))
	(format t
		"~&There  ~4D are caches of size ~4D. (~D free  ~3D%)"
		allocated
		size
		free
		(floor (* 100 (/ free (float allocated)))))))))


;;;
;;; Wrapper cache numbers
;;; 

;;;
;;; The constant WRAPPER-CACHE-NUMBER-ADDS-OK controls the number of non-zero
;;; bits wrapper cache numbers will have.
;;;
;;; The value of this constant is the number of wrapper cache numbers which
;;; can be added and still be certain the result will be a fixnum.  This is
;;; used by all the code that computes primary cache locations from multiple
;;; wrappers.
;;;
;;; The value of this constant is used to derive the next two which are the
;;; forms of this constant which it is more convenient for the runtime code
;;; to use.
;;; 
(eval-when (compile load eval)

(defconstant wrapper-cache-number-adds-ok 4)

(defconstant wrapper-cache-number-length
	     (- (integer-length most-positive-fixnum)
		wrapper-cache-number-adds-ok))

(defconstant wrapper-cache-number-mask
	     (1- (expt 2 wrapper-cache-number-length)))

(defvar *get-wrapper-cache-number* (make-random-state))

(defun get-wrapper-cache-number ()
  (let ((n 0))
    (declare (type index n))
    (loop
      (setq n
	    (logand (the index wrapper-cache-number-mask)
		    (the index (random most-positive-fixnum
                                       *get-wrapper-cache-number*))))
      (unless (zerop n) (return n)))))


(unless (> wrapper-cache-number-length 8)
  (error "In this implementation of Common Lisp, fixnums are so small that~@
          wrapper cache numbers end up being only ~D bits long.  This does~@
          not actually keep PCL from running, but it may degrade cache~@
          performance.~@
          You may want to consider changing the value of the constant~@
          WRAPPER-CACHE-NUMBER-ADDS-OK.")))


;;;
;;; wrappers themselves
;;;
;;; This caching algorithm requires that wrappers have more than one wrapper
;;; cache number.  You should think of these multiple numbers as being in
;;; columns.  That is, for a given cache, the same column of wrapper cache
;;; numbers will be used.
;;;
;;; If at some point the cache distribution of a cache gets bad, the cache
;;; can be rehashed by switching to a different column.
;;;
;;; The columns are referred to by field number which is that number which,
;;; when used as a second argument to wrapper-ref, will return that column
;;; of wrapper cache number.
;;;
;;; This code is written to allow flexibility as to how many wrapper cache
;;; numbers will be in each wrapper, and where they will be located.  It is
;;; also set up to allow port specific modifications to `pack' the wrapper
;;; cache numbers on machines where the addressing modes make that a good
;;; idea.
;;; 
;;; For July 92, the wrapper field UNRESERVED-FIELD, accessable by macro
;;; WRAPPER-UNRESERVED-FIELD, has been created to allow a programmer to
;;; store his own items in the wrapper if so desired.  (Since there is
;;; only one wrapper per class, this could be added at minimal cost).
;;; It would be nice for this kind of low level hook to be part of the
;;; MOP.  -- TL.

(defconstant *temporary-static-slot-storage-copy* (make-array 1))

#-structure-wrapper
(eval-when (compile load eval)
(defconstant wrapper-layout
	     '(number
	       number
	       number
	       number
	       number
	       number
	       number
	       number
               wrapper-identifier
	       state
	       instance-slots-layout
	       class-slots
	       class
	       class-precedence-list
               allocate-static-slot-storage-copy
               unreserved-field))

(defconstant wrapper-length 16)          ; #.(length wrapper-layout)

(deftype wrapper () `(simple-vector 16)) ; #.(length wrapper-layout)
)

#-structure-wrapper
(progn

(eval-when (compile load eval)

(declaim (ftype (function (T) index) wrapper-field))
(defun wrapper-field (type)
  (posq type wrapper-layout))

(declaim (ftype (function (index) (or index null)) next-wrapper-field))
(defun next-wrapper-field (field-number)
  (declare (type index field-number))
  (position (nth field-number wrapper-layout)
	    wrapper-layout
	    :start (1+ field-number)))

(defmacro first-wrapper-cache-number-index ()
  `(the field-type (wrapper-field 'number)))

(defmacro next-wrapper-cache-number-index (field-number)
  `(next-wrapper-field ,field-number))

);eval-when

(defmacro wrapper-cache-number-vector (wrapper)
  wrapper)

(defmacro cache-number-vector-ref (cnv n)
  `(svref ,cnv ,n))

(defconstant *wrapper-identifier-symbol* (gensym "WRAPPER-IDENTIFIER"))

(defmacro wrapper-ref (wrapper n)
  `(svref (the wrapper ,wrapper) (the index ,n)))

(defun emit-wrapper-ref (wrapper-operand field-operand)
  (operand :iref wrapper-operand field-operand))


(defmacro wrapper-p (x)
  (once-only (x)
    `(locally (declare #.*optimize-speed*)
       (and (simple-vector-p ,x)
            (= (the index (length (the simple-vector ,x)))
               wrapper-length)
            (eq (wrapper-ref ,x ,(wrapper-field 'wrapper-identifier))
                *wrapper-identifier-symbol*)))))

(defmacro wrapper-state (wrapper)
  `(wrapper-ref ,wrapper ,(wrapper-field 'state)))

(defmacro wrapper-instance-slots-layout (wrapper)
  `(the list
        (wrapper-ref ,wrapper ,(wrapper-field 'instance-slots-layout))))

(defmacro wrapper-class-slots (wrapper)
  `(the list
        (wrapper-ref ,wrapper ,(wrapper-field 'class-slots))))

(defmacro wrapper-class (wrapper)
  `(wrapper-ref ,wrapper ,(wrapper-field 'class)))

(defmacro wrapper-class-precedence-list (wrapper)
  `(the list
        (wrapper-ref ,wrapper ,(wrapper-field 'class-precedence-list))))

(defmacro wrapper-allocate-static-slot-storage-copy (wrapper)
  `(the simple-vector
        (wrapper-ref ,wrapper ,(wrapper-field 'allocate-static-slot-storage-copy))))

(defmacro wrapper-unreserved-field (wrapper)
  "Field unreserved by PCL.  May be used by user programs."
  `(wrapper-ref ,wrapper ,(wrapper-field 'unreserved-field)))


(defmacro make-wrapper-internal ()
  `(let ((wrapper
           (make-array ,(length (the list wrapper-layout)) :adjustable nil)))
     (declare (type wrapper wrapper))
     ,@(gathering1 (collecting)
	 (iterate ((i (interval :from 0))
		   (desc (list-elements wrapper-layout)))
	   (ecase desc
             (wrapper-identifier
              (gather1 `(setf (wrapper-ref wrapper ,i)
                              *wrapper-identifier-symbol*)))
	     (number
	      (gather1 `(setf (wrapper-ref wrapper ,i)
			      (the index (get-wrapper-cache-number)))))
             (allocate-static-slot-storage-copy
              (gather1 `(setf (wrapper-ref wrapper ,i)
                              *temporary-static-slot-storage-copy*)))
	     ((state instance-slots-layout class-slots class 
               class-precedence-list unreserved-field)))))
     (setf (wrapper-state wrapper) 't)
     wrapper))

(defun make-wrapper (class)
  (let ((wrapper (make-wrapper-internal)))
    (setf (wrapper-class wrapper) class)
    wrapper))

) ;#-structure-wrapper


; In CMUCL we want to do type checking as early as possible; structures help this.
#+structure-wrapper
(eval-when (compile load eval)

(defconstant wrapper-cache-number-vector-length 8)

(deftype cache-number-vector ()
  `(simple-array fixnum (8)))

(defconstant wrapper-layout (make-list wrapper-cache-number-vector-length
				       :initial-element 'number))

)

#+structure-wrapper
(progn

(defun make-wrapper-cache-number-vector ()
  (let ((cnv (make-array wrapper-cache-number-vector-length)))
    (dotimes (i #.wrapper-cache-number-vector-length)
      (setf (svref cnv i) (get-wrapper-cache-number)))
    cnv))

(defstruct (wrapper
	     (:print-function print-wrapper)
	     (:constructor make-wrapper (class)))
  (cache-number-vector (make-wrapper-cache-number-vector)
		       :type cache-number-vector)
  (state t :type (or (member t) cons)) ; a cons whose car is one of: flush or obsolete
  (instance-slots-layout nil :type list)
  (class-slots nil :type list)
  (class *the-class-t* :type class)
  (class-precedence-list nil :type list)
  (allocate-static-slot-storage-copy *temporary-static-slot-storage-copy*
                                     :type simple-vector)
  (unreserved-field nil))

#+akcl
(si::freeze-defstruct 'cache)

(defun print-wrapper (wrapper stream depth)
  (declare (ignore depth))
  (printing-random-thing (wrapper stream)
    (format stream "wrapper ~S" (wrapper-class wrapper))))

(defmacro first-wrapper-cache-number-index ()
  0)

(defmacro next-wrapper-cache-number-index (field-number)
  `(and (< (the index ,field-number) #.(1- wrapper-cache-number-vector-length))
        (1+ (the index ,field-number))))

(defmacro cache-number-vector-ref (cnv n)
  `(svref ,cnv ,n))

(defun emit-wrapper-cache-number-vector (wrapper-operand)
  (operand :wrapper-cache-number-vector wrapper-operand))

(defun emit-cache-number-vector-ref (cnv-operand field-operand)
  (operand :iref cnv-operand field-operand))

) ;#+structure-wrapper

(defmacro wrapper-cache-number-vector-ref (wrapper n)
  `(svref (wrapper-cache-number-vector ,wrapper) ,n))


;;;
;;;
;;;

(defvar *built-in-or-structure-wrapper-table*
  (make-hash-table :test 'eq))

(defvar wft-type1 nil)
(defvar wft-wrapper1 nil)
(defvar wft-type2 nil)
(defvar wft-wrapper2 nil)

(defun wrapper-for-structure (x)
  (let ((type (structure-type x)))
    (when (symbolp type)
      (cond ((eq type wft-type1)
             (return-from wrapper-for-structure wft-wrapper1))
            ((eq type wft-type2)
             (return-from wrapper-for-structure wft-wrapper2))
            (t (setq wft-type2 wft-type1  wft-wrapper2 wft-wrapper1))))
    (let* ((cell (find-class-cell type))
           (class (or (find-class-cell-class cell)
                      (let* (#+lucid
                             (*structure-type* type)
                             #+lucid
                             (*structure-length* (structure-length x type)))
                        (find-class-from-cell type cell nil))))
           (wrapper (if class (class-wrapper class) *the-wrapper-of-t*)))
     (when (symbolp type)
        (setq wft-type1 type  wft-wrapper1 wrapper))
      wrapper)))

(defmacro built-in-or-structure-wrapper (x)
  (once-only (x)
    `(if (structurep ,x)
         (wrapper-for-structure ,x)
         (if (symbolp ,x)
	     (if ,x *the-wrapper-of-symbol* *the-wrapper-of-null*)
	     (built-in-wrapper-of ,x)))))

(defun built-in-or-structure-wrapper-fun (x)
  (built-in-or-structure-wrapper x))

(defmacro fast-wrapper-of (x)
  (once-only (x)
    `(cond ((std-instance-p ,x)
	    (std-instance-wrapper ,x))
           ((fsc-instance-p ,x)
	    (fsc-instance-wrapper ,x))
           #+pcl-user-instances
           ((get-user-instance-p ,x)
	    (get-user-instance-wrapper ,x))
           (T (built-in-or-structure-wrapper ,x)))))

;;;
;;; The wrapper cache machinery provides general mechanism for trapping on
;;; the next access to any instance of a given class.  This mechanism is
;;; used to implement the updating of instances when the class is redefined
;;; (make-instances-obsolete).  The same mechanism is also used to update
;;; generic function caches when there is a change to the supers of a class.
;;;
;;; Basically, a given wrapper can be valid or invalid.  If it is invalid,
;;; it means that any attempt to do a wrapper cache lookup using the wrapper
;;; should trap.  Also, methods on slot-value-using-class check the wrapper
;;; validity as well.  This is done by calling check-wrapper-validity.
;;; 

(defmacro invalid-wrapper-p (wrapper)
  `(neq (wrapper-state ,wrapper) 't))

(defvar *previous-nwrappers* (make-hash-table))

(defun invalidate-wrapper (owrapper state nwrapper)
  (ecase state
    ((flush obsolete)
     (let ((new-previous ()))
       ;;
       ;; First off, a previous call to invalidate-wrapper may have recorded
       ;; owrapper as an nwrapper to update to.  Since owrapper is about to
       ;; be invalid, it no longer makes sense to update to it.
       ;;
       ;; We go back and change the previously invalidated wrappers so that
       ;; they will now update directly to nwrapper.  This corresponds to a
       ;; kind of transitivity of wrapper updates.
       ;; 
       (dolist (previous (gethash owrapper *previous-nwrappers*))
	 (when (eq state 'obsolete)
	   (setf (car previous) 'obsolete))
	 (setf (cadr previous) nwrapper)
	 (push previous new-previous))
       
       (let ((ocnv (wrapper-cache-number-vector owrapper)))
	 (iterate ((type (list-elements wrapper-layout))
		   (i (interval :from 0)))
           (when (eq type 'number) (setf (cache-number-vector-ref ocnv i) 0))))
       (push (setf (wrapper-state owrapper) (list state nwrapper))
	     new-previous)
       
       (setf (gethash owrapper *previous-nwrappers*) ()
	     (gethash nwrapper *previous-nwrappers*) new-previous)))))

(defun wrapper-state-trap (wrapper object)
  (let ((state (wrapper-state wrapper)))
    (ecase (car state)
      (flush
        (flush-cache-trap wrapper (cadr state) object))
      (obsolete
        (obsolete-instance-trap wrapper (cadr state) object)))))

(defmacro fast-check-wrapper-validity (object
                                       &optional (wrapper-fn 'fast-wrapper-of))
  (let ((owrapper (gensym "OWRAPPER")))
    (once-only (object)
      `(let ((,owrapper (,wrapper-fn ,object)))
         (if (eq 't (wrapper-state ,owrapper))
	     ,owrapper
	     (wrapper-state-trap ,owrapper ,object))))))

(defun check-wrapper-validity (instance)
  (fast-check-wrapper-validity instance wrapper-of))




(defvar *free-caches* nil)

(defun get-cache (nkeys valuep limit-fn nlines)
  (declare (type index         nkeys)
           (type boolean       valuep)
           (type real-function limit-fn)
           (type index         nlines))
  (let ((cache (or (without-interrupts-simple (pop *free-caches*))
                   (make-cache))))
    (declare (type cache cache))
    (multiple-value-bind (cache-mask actual-size line-size nlines)
	(compute-cache-parameters nkeys valuep nlines)
      (declare (type index cache-mask actual-size line-size nlines))
      (setf (cache-nkeys cache) nkeys
	    (cache-valuep cache) valuep
	    (cache-nlines cache) nlines
	    (cache-field cache) (first-wrapper-cache-number-index)
	    (cache-limit-fn cache) limit-fn
	    (cache-mask cache) cache-mask
	    (cache-size cache) actual-size
	    (cache-line-size cache) line-size
	    (cache-max-location cache)
              (the index (let ((line (1- nlines)))
                            (declare (type index line))
			    (if (= nkeys 1)
			        (the index (* line line-size))
			        (the index (1+ (the index (* line line-size)))))))
	    (cache-vector cache) (get-cache-vector actual-size)
	    (cache-overflow cache) nil)
      cache)))

(defun get-cache-from-cache (old-cache new-nlines 
			     &optional (new-field (first-wrapper-cache-number-index)))
  (declare (type index new-nlines) (type field-type new-field))
  (let ((nkeys (cache-nkeys old-cache))
	(valuep (cache-valuep old-cache))
	(cache (or (without-interrupts-simple (pop *free-caches*))
                   (make-cache))))
    (declare (type cache cache) (type index nkeys)
             (type boolean valuep))
    (multiple-value-bind (cache-mask actual-size line-size nlines)
	(if (= new-nlines (cache-nlines old-cache))
	    (values (cache-mask old-cache) (cache-size old-cache) 
		    (cache-line-size old-cache) (cache-nlines old-cache))
	    (compute-cache-parameters nkeys valuep new-nlines))
      (declare (type index cache-mask actual-size line-size nlines))
      (setf (cache-nkeys cache) nkeys
	    (cache-valuep cache) valuep
	    (cache-nlines cache) nlines
	    (cache-field cache) new-field
	    (cache-limit-fn cache) (cache-limit-fn old-cache)
	    (cache-mask cache) cache-mask
	    (cache-size cache) actual-size
	    (cache-line-size cache) line-size
	    (cache-max-location cache)
              (the index (let ((line (1- nlines)))
                            (declare (type index line))
			    (if (= nkeys 1)
			        (the index (* line line-size))
			        (the index (1+ (the index (* line line-size)))))))
	    (cache-vector cache) (get-cache-vector actual-size)
	    (cache-overflow cache) nil)
      cache)))

(defun copy-cache (old-cache)
  (let* ((new-cache (copy-cache-internal old-cache))
	 (size (cache-size old-cache))
	 (old-vector (cache-vector old-cache))
	 (new-vector (get-cache-vector size)))
    (declare (type simple-vector old-vector new-vector)
             (type index         size))
    (dotimes (i size)
      (setf (svref new-vector i) (svref old-vector i)))
    (setf (cache-vector new-cache) new-vector)
    new-cache))

(defun free-cache (cache)
  (free-cache-vector (cache-vector cache))
  (setf (cache-vector cache) '#())
  (push cache *free-caches*)
  nil)


       

(defun compute-line-size (x)
  (declare (type index x))
  (power-of-two-ceiling x))

(defun compute-cache-parameters (nkeys valuep nlines-or-cache-vector)
  (declare (values cache-mask actual-size line-size nlines))
  (declare (type index nkeys))
  (if (= nkeys 1)
      (let* ((line-size (if valuep 2 1))
	     (cache-size (if (typep nlines-or-cache-vector 'fixnum)
			     (the index
				  (* line-size
				     (power-of-two-ceiling 
					(the index nlines-or-cache-vector))))
			     (cache-vector-size nlines-or-cache-vector))))
	(declare (type index line-size cache-size))
	(values (logxor (the index (1- cache-size)) (the index (1- line-size)))
		cache-size
		line-size
		(the index (floor cache-size line-size))))
      (let* ((line-size (power-of-two-ceiling (if valuep (1+ nkeys) nkeys)))
	     (cache-size (if (typep nlines-or-cache-vector 'fixnum)
			     (the index
				  (* line-size
				     (the index
					  (power-of-two-ceiling 
					    nlines-or-cache-vector))))
			     (1- (cache-vector-size nlines-or-cache-vector)))))
	(declare (type index line-size cache-size))
	(values (logxor (the index (1- cache-size)) (the index (1- line-size)))
		(the index (1+ cache-size))
		line-size
		(the index (floor cache-size line-size))))))



;;;
;;; The various implementations of computing a primary cache location from
;;; wrappers.  Because some implementations of this must run fast there are
;;; several implementations of the same algorithm.
;;;
;;; The algorithm is:
;;;
;;;  SUM       over the wrapper cache numbers,
;;;  ENSURING  that the result is a fixnum
;;;  MASK      the result against the mask argument.
;;;
;;;

;;;
;;; COMPUTE-PRIMARY-CACHE-LOCATION
;;; 
;;; The basic functional version.  This is used by the cache miss code to
;;; compute the primary location of an entry.  
;;;
(defun compute-primary-cache-location (field mask wrappers)
  (declare (type field-type field) (type index mask))
  (if (not (listp wrappers))
      (the index
          (logand mask (the index (wrapper-cache-number-vector-ref wrappers field))))
      (let ((location 0) (i 0))
	(declare (type index location i))
	(dolist (wrapper wrappers)
	  ;;
	  ;; First add the cache number of this wrapper to location.
	  ;; 
	  (let ((wrapper-cache-number (wrapper-cache-number-vector-ref wrapper field)))
	    (declare (type index wrapper-cache-number))
	    (if (zerop wrapper-cache-number)
		(return-from compute-primary-cache-location 0)
		(setq location (the index (+ location wrapper-cache-number)))))
	  ;;
	  ;; Then, if we are working with lots of wrappers, deal with
	  ;; the wrapper-cache-number-mask stuff.
	  ;; 
	  (when (and (not (zerop i))
		     (zerop (mod i wrapper-cache-number-adds-ok)))
	    (setq location
		  (the index (logand location wrapper-cache-number-mask))))
	  (setf i (the index (1+ i))))
	(the index (1+ (the index (logand mask location)))))))

;;;
;;; COMPUTE-PRIMARY-CACHE-LOCATION-FROM-LOCATION
;;;
;;; This version is called on a cache line.  It fetches the wrappers from
;;; the cache line and determines the primary location.  Various parts of
;;; the cache filling code call this to determine whether it is appropriate
;;; to displace a given cache entry.
;;; 
;;; If this comes across a wrapper whose cache-no is 0, it returns the symbol
;;; invalid to suggest to its caller that it would be provident to blow away
;;; the cache line in question.
;;;
(defun compute-primary-cache-location-from-location (field cache location)
  (declare (type field-type field) (type index location))
  (let ((result 0)
	(cache-vector (cache-vector cache))
	(mask (cache-mask cache))
	(nkeys (cache-nkeys cache)))
    (declare (type index result mask nkeys) (simple-vector cache-vector))
    (dotimes (i nkeys)
      (let* ((wrapper (cache-vector-ref cache-vector (+ i location)))
	     (wcn (wrapper-cache-number-vector-ref wrapper field)))
	(declare (type index wcn))
	(setq result (the index (+ result wcn))))
      (when (and (not (zerop i))
		 (zerop (mod i wrapper-cache-number-adds-ok)))
	(setq result (the index (logand result wrapper-cache-number-mask)))))
    (if (= nkeys 1)
	(logand mask result)
	(the index (1+ (the index (logand mask result)))))))

(defun emit-1-wrapper-compute-primary-cache-location (wrapper primary wrapper-cache-no)
  (with-lap-registers ((mask index) 
		       #+structure-wrapper (cnv fixnum-vector))
    (let ((field wrapper-cache-no))
      (flatten-lap
        (opcode :move (operand :cvar 'mask) mask)
        (opcode :move (operand :cvar 'field) field)
	#-structure-wrapper
        (opcode :move (emit-wrapper-ref wrapper field) wrapper-cache-no)
	#+structure-wrapper
	(opcode :move (emit-wrapper-cache-number-vector wrapper) cnv)
	#+structure-wrapper
	(opcode :move (emit-cache-number-vector-ref cnv field) wrapper-cache-no)
        (opcode :move (operand :ilogand wrapper-cache-no mask) primary)))))

(defun emit-n-wrapper-compute-primary-cache-location (wrappers primary miss-label)
  (with-lap-registers ((field index)
		       (mask index))
    (let ((add-wrapper-cache-numbers
	   (flatten-lap
	    (gathering1 (flattening-lap)
	       (iterate ((wrapper (list-elements wrappers))
			 (i (interval :from 1)))
		 (gather1
		  (with-lap-registers ((wrapper-cache-no index)
				       #+structure-wrapper (cnv fixnum-vector))
		    (flatten-lap
		     #-structure-wrapper
		     (opcode :move (emit-wrapper-ref wrapper field) wrapper-cache-no)
		     #+structure-wrapper
		     (opcode :move (emit-wrapper-cache-number-vector wrapper) cnv)
		     #+structure-wrapper
		     (opcode :move (emit-cache-number-vector-ref cnv field)
			     wrapper-cache-no)
		     (opcode :izerop wrapper-cache-no miss-label)
		     (opcode :move (operand :i+ primary wrapper-cache-no) primary)
		     (when (zerop (the index (mod (the index i)
                                                  wrapper-cache-number-adds-ok)))
		       (opcode :move (operand :ilogand primary mask) primary))))))))))
      (flatten-lap
       (opcode :move (operand :constant 0) primary)
       (opcode :move (operand :cvar 'field) field)
       (opcode :move (operand :cvar 'mask) mask)
       add-wrapper-cache-numbers
       (opcode :move (operand :ilogand primary mask) primary)
       (opcode :move (operand :i1+ primary) primary)))))



;;;
;;;  NIL              means nothing so far, no actual arg info has NILs
;;;                   in the metatype
;;;  CLASS            seen all sorts of metaclasses
;;;                   (specifically, more than one of the next 4 values)
;;;  T                means everything so far is the class T
;;;  STANDARD-CLASS   seen only standard classes
;;;  BUILT-IN-CLASS   seen only built in classes
;;;  STRUCTURE-CLASS  seen only structure classes
;;;  
(defun raise-metatype (metatype new-specializer)
  (let ((slot      (find-class 'slot-class))
	(standard  (find-class 'standard-class))
	(fsc       (find-class 'funcallable-standard-class))
	(structure (find-class 'structure-class))
	(built-in  (find-class 'built-in-class)))
    (flet ((specializer->metatype (x)
	     (let ((meta-specializer 
		     (if (eq *boot-state* 'complete)
			 (class-of (specializer-class x))
			 (class-of x))))
	       (cond ((eq x *the-class-t*) t)
		     ((*subtypep meta-specializer standard)  'standard-instance)
		     ((*subtypep meta-specializer fsc)       'standard-instance)
		     ((*subtypep meta-specializer structure) 'structure-instance)
		     ((*subtypep meta-specializer built-in)  'built-in-instance)
		     ((*subtypep meta-specializer slot)      'slot-instance)
		     (t (error "PCL can not handle the specializer ~S (meta-specializer ~S)."
			       new-specializer meta-specializer))))))
      ;;
      ;; We implement the following table.  The notation is
      ;; that X and Y are distinct meta specializer names.
      ;; 
      ;;   NIL    <anything>    ===>  <anything>
      ;;    X      X            ===>      X
      ;;    X      Y            ===>    CLASS
      ;;    
      (let ((new-metatype (specializer->metatype new-specializer)))
	(cond ((eq new-metatype 'slot-instance) 'class)
	      ((null metatype) new-metatype)
	      ((eq metatype new-metatype) new-metatype)
	      (t 'class))))))



(defun emit-fetch-wrapper (metatype argument dest miss-label &optional slot)
  (let ((exit-emit-fetch-wrapper (make-symbol "exit-emit-fetch-wrapper")))
    (with-lap-registers ((arg t))
      (ecase metatype
	(standard-instance
	  (let ((get-std-inst-wrapper (make-symbol "get-std-inst-wrapper"))
		(get-fsc-inst-wrapper (make-symbol "get-fsc-inst-wrapper"))
                #+pcl-user-instances
		(get-user-inst-wrapper (make-symbol "get-user-inst-wrapper")))
	    (flatten-lap
	      (opcode :move (operand :arg argument) arg)
	      (opcode :std-instance-p arg get-std-inst-wrapper)	   ;is it a std wrapper?
	      (opcode :fsc-instance-p arg get-fsc-inst-wrapper)	   ;is it a fsc wrapper?
              #+pcl-user-instances
	      (opcode :user-instance-p arg get-user-inst-wrapper)  ;is it a user wrapper?
	      (opcode :go miss-label)
              #+pcl-user-instances
	      (opcode :label get-user-inst-wrapper)
              #+pcl-user-instances
	      (opcode :move (operand :user-wrapper arg) dest)	   ;get user wrapper
              #+pcl-user-instances
	      (and slot
		   (opcode :move (operand :user-slots arg) slot))
              #+pcl-user-instances
	      (opcode :go exit-emit-fetch-wrapper)
	      (opcode :label get-fsc-inst-wrapper)
	      (opcode :move (operand :fsc-wrapper arg) dest)	   ;get fsc wrapper
	      (and slot
		   (opcode :move (operand :fsc-slots arg) slot))
	      (opcode :go exit-emit-fetch-wrapper)
	      (opcode :label get-std-inst-wrapper)
	      (opcode :move (operand :std-wrapper arg) dest)	   ;get std wrapper
	      (and slot
		   (opcode :move (operand :std-slots arg) slot))
	      (opcode :label exit-emit-fetch-wrapper))))
	(class
	  (when slot (error "Can't do a slot reg for this metatype."))
	  (let ((get-std-inst-wrapper (make-symbol "get-std-inst-wrapper"))
		(get-fsc-inst-wrapper (make-symbol "get-fsc-inst-wrapper"))
                #+pcl-user-instances
		(get-user-inst-wrapper (make-symbol "get-user-inst-wrapper")))
	    (flatten-lap
	      (opcode :move (operand :arg argument) arg)
	      (opcode :std-instance-p arg get-std-inst-wrapper)
	      (opcode :fsc-instance-p arg get-fsc-inst-wrapper)
              #+pcl-user-instances
	      (opcode :user-instance-p arg get-user-inst-wrapper)
	      (opcode :move (operand :built-in-or-structure-wrapper arg) dest)
	      (opcode :go exit-emit-fetch-wrapper)
              #+pcl-user-instances
	      (opcode :label get-user-inst-wrapper)
              #+pcl-user-instances
	      (opcode :move (operand :user-wrapper arg) dest)
              #+pcl-user-instances
	      (opcode :go exit-emit-fetch-wrapper)
	      (opcode :label get-fsc-inst-wrapper)
	      (opcode :move (operand :fsc-wrapper arg) dest)
	      (opcode :go exit-emit-fetch-wrapper)
	      (opcode :label get-std-inst-wrapper)
	      (opcode :move (operand :std-wrapper arg) dest)
	      (opcode :label exit-emit-fetch-wrapper))))
	((built-in-instance structure-instance)
	  (when slot (error "Can't do a slot reg for this metatype."))
	  (let ()
	    (flatten-lap
	      (opcode :move (operand :arg argument) arg)
	      (opcode :std-instance-p arg miss-label)
	      (opcode :fsc-instance-p arg miss-label)
	      (opcode :move (operand :built-in-or-structure-wrapper arg) dest))))))))


;;;
;;; Some support stuff for getting a hold of symbols that we need when
;;; building the discriminator codes.  Its ok for these to be interned
;;; symbols because we don't capture any user code in the scope in which
;;; these symbols are bound.
;;; 

(defvar *dfun-arg-symbols* '(.ARG0. .ARG1. .ARG2. .ARG3.))

(defun dfun-arg-symbol (arg-number)
  (declare (type index arg-number))
  (or (nth arg-number (the list *dfun-arg-symbols*))
      (intern (format nil ".ARG~A." arg-number) *the-pcl-package*)))

(defvar *slot-vector-symbols* '(.SLOTS0. .SLOTS1. .SLOTS2. .SLOTS3.))

(defun slot-vector-symbol (arg-number)
  (declare (type index arg-number))
  (or (nth arg-number (the list *slot-vector-symbols*))
      (intern (format nil ".SLOTS~A." arg-number) *the-pcl-package*)))

(defun make-dfun-lambda-list (metatypes applyp)
  (gathering1 (collecting)
    (iterate ((i (interval :from 0))
	      (s (list-elements metatypes)))
      (progn s)
      (gather1 (dfun-arg-symbol i)))
    (when applyp
      (gather1 '&rest)
      (gather1 '.dfun-rest-arg.))))

(defun make-dlap-lambda-list (metatypes applyp)
  (gathering1 (collecting)
    (iterate ((i (interval :from 0))
	      (s (list-elements metatypes)))
      (progn s)
      (gather1 (dfun-arg-symbol i)))
    (when applyp
      (gather1 '&rest))))

(defun make-dfun-call (metatypes applyp fn-variable)
  (let ((required
	  (gathering1 (collecting)
	    (iterate ((i (interval :from 0))
		      (s (list-elements metatypes)))
	      (progn s)
	      (gather1 (dfun-arg-symbol i))))))
    (if applyp
	`(method-function-apply   ,fn-variable ,@required .dfun-rest-arg.)
	`(method-function-funcall ,fn-variable ,@required))))


;;;
;;; Its too bad Common Lisp compilers freak out when you have a defun with
;;; a lot of LABELS in it.  If I could do that I could make this code much
;;; easier to read and work with.
;;;
;;; Ahh Scheme...
;;; 
;;; In the absence of that, the following little macro makes the code that
;;; follows a little bit more reasonable.  I would like to add that having
;;; to practically write my own compiler in order to get just this simple
;;; thing is something of a drag.
;;;
(eval-when (compile load eval)

(defvar *cache* nil)

(defconstant *local-cache-functions*
  '((cache () .cache.)
    (nkeys () (cache-nkeys .cache.))
    (line-size () (cache-line-size .cache.))
    (pcl-vector () (cache-vector .cache.))
    (valuep () (cache-valuep .cache.))
    (nlines () (cache-nlines .cache.))
    (max-location () (cache-max-location .cache.))
    (limit-fn () (cache-limit-fn .cache.))
    (size () (cache-size .cache.))
    (mask () (cache-mask .cache.))
    (field () (cache-field .cache.))

    ;;
    ;; Return T IFF this cache location is reserved.  The only time
    ;; this is true is for line number 0 of an nkeys=1 cache.  
    ;;
    (line-reserved-p (line)
      (declare (type index line))
      (and (= (nkeys) 1)
           (= line 0)))
    ;;
    (location-reserved-p (location)
      (declare (type index location))
      (and (= (nkeys) 1)
           (= location 0)))
    ;;
    ;; Given a line number, return the cache location.  This is the
    ;; value that is the second argument to cache-vector-ref.  Basically,
    ;; this deals with the offset of nkeys>1 caches and multiplies
    ;; by line size.  
    ;; 	  
    (line-location (line)
      (declare (type index line))
      (when (line-reserved-p line)
        (error "line is reserved"))
      (if (= (nkeys) 1)
	  (the index (* line (line-size)))
	  (the index (1+ (the index (* line (line-size)))))))
    ;;
    ;; Given a cache location, return the line.  This is the inverse
    ;; of LINE-LOCATION.
    ;; 	  
    (location-line (location)
      (declare (type index location))
      (if (= (nkeys) 1)
	  (floor location (line-size))
	  (floor (the index (1- location)) (line-size))))
    ;;
    ;; Given a line number, return the wrappers stored at that line.
    ;; As usual, if nkeys=1, this returns a single value.  Only when
    ;; nkeys>1 does it return a list.  An error is signalled if the
    ;; line is reserved.
    ;;
    (line-wrappers (line)
      (declare (type index line))
      (when (line-reserved-p line) (error "Line is reserved."))
      (location-wrappers (line-location line)))
    ;;
    (location-wrappers (location) ; avoid multiplies caused by line-location
      (declare (type index location))
      (if (= (nkeys) 1)
	  (cache-vector-ref (pcl-vector) location)
	  (let ((list (make-list (nkeys)))
		(pcl-vector (pcl-vector)))
	    (declare (simple-vector pcl-vector))
	    (dotimes (i (nkeys) list)
	      (setf (nth i list) (cache-vector-ref pcl-vector (+ location i)))))))
    ;;
    ;; Given a line number, return true IFF the line's
    ;; wrappers are the same as wrappers.
    ;;
    (line-matches-wrappers-p (line wrappers)
      (declare (type index line))
      (and (not (line-reserved-p line))
           (location-matches-wrappers-p (line-location line) wrappers)))
    ;;
    (location-matches-wrappers-p (loc wrappers) ; must not be reserved
      (declare (type index loc))
      (let ((cache-vector (pcl-vector)))
	(declare (simple-vector cache-vector))
	(if (= (nkeys) 1)
	    (eq wrappers (cache-vector-ref cache-vector loc))
	    (dotimes (i (nkeys) t)
	      (unless (eq (pop wrappers) (cache-vector-ref cache-vector (+ loc i)))
		(return nil))))))
    ;;
    ;; Given a line number, return the value stored at that line.
    ;; If valuep is NIL, this returns NIL.  As with line-wrappers,
    ;; an error is signalled if the line is reserved.
    ;; 
    (line-value (line)
      (declare (type index line))
      (when (line-reserved-p line) (error "Line is reserved."))
      (location-value (line-location line)))
    ;;
    (location-value (loc)
      (declare (type index loc))
      (and (valuep)
           (cache-vector-ref (pcl-vector) (+ loc (nkeys)))))
    ;;
    ;; Given a line number, return true IFF that line has data in
    ;; it.  The state of the wrappers stored in the line is not
    ;; checked.  An error is signalled if line is reserved.
    (line-full-p (line)
      (when (line-reserved-p line) (error "Line is reserved."))
      (not (null (cache-vector-ref (pcl-vector) (line-location line)))))
    ;;
    ;; Given a line number, return true IFF the line is full and
    ;; there are no invalid wrappers in the line, and the line's
    ;; wrappers are different from wrappers.
    ;; An error is signalled if the line is reserved.
    ;;
    (line-valid-p (line wrappers)
      (declare (type index line))
      (when (line-reserved-p line) (error "Line is reserved."))
      (location-valid-p (line-location line) wrappers))
    ;;
    (location-valid-p (loc wrappers)
      (declare (type index loc))
      (let ((cache-vector (pcl-vector))
	    (wrappers-mismatch-p (null wrappers)))
	(declare (simple-vector cache-vector))
	(dotimes (i (nkeys) wrappers-mismatch-p)
	  (let ((wrapper (cache-vector-ref cache-vector (+ loc i))))
	    (when (or (null wrapper)
		      (invalid-wrapper-p wrapper))
	      (return nil))
	    (unless (and wrappers
			 (eq wrapper
			     (if (consp wrappers) (pop wrappers) wrappers)))
	      (setq wrappers-mismatch-p t))))))
    ;;
    ;; How many unreserved lines separate line-1 and line-2.
    ;;
    (line-separation (line-1 line-2)
     (declare (type index line-1 line-2))
     (let ((diff (the fixnum (- line-2 line-1))))
       (declare (type fixnum diff))
       (when (minusp diff)
	 (setq diff (+ diff (nlines)))
	 (when (line-reserved-p 0)
	   (setq diff (1- diff))))
       diff))
    ;;
    ;; Given a cache line, get the next cache line.  This will not
    ;; return a reserved line.
    ;; 
    (next-line (line)
     (declare (type index line))
     (if (= line (the index (1- (nlines))))
	 (if (line-reserved-p 0) 1 0)
	 (the index (1+ line))))
    ;;
    (next-location (loc)
      (declare (type index loc))
      (if (= loc (max-location))
	  (if (= (nkeys) 1)
	      (line-size)
	      1)
	  (the index (+ loc (line-size)))))
    ;;
    ;; Given a line which has a valid entry in it, this will return
    ;; the primary cache line of the wrappers in that line.  We just
    ;; call COMPUTE-PRIMARY-CACHE-LOCATION-FROM-LOCATION, this is an
    ;; easier packaging up of the call to it.
    ;; 
    (line-primary (field line)
      (declare (type index field line))
      (location-line (line-primary-location field line)))
    ;;
    (line-primary-location (field line)
     (declare (type index field line))
     (compute-primary-cache-location-from-location
       field (cache) (line-location line)))
    ))
) ;eval-when

(eval-when (compile load eval)
(defmacro with-local-cache-functions ((cache) &body body)
  `(let ((.cache. ,cache))
     (declare (type cache .cache.))
     (macrolet ,(mapcar #'(lambda (fn)
			    `(,(car fn) ,(cadr fn)
			        `(let (,,@(mapcar #'(lambda (var)
						      ``(,',var ,,var))
					          (cadr fn)))
				    ,@',(cddr fn))))
			*local-cache-functions*)
       ,@body)))
) ;eval-when


;;;
;;; Here is where we actually fill, recache and expand caches.
;;;
;;; The functions FILL-CACHE and PROBE-CACHE are the ONLY external
;;; entrypoints into this code.
;;;
;;; FILL-CACHE returns 1 value: a new cache
;;;
;;;   a wrapper field number
;;;   a cache
;;;   a mask
;;;   an absolute cache size (the size of the actual vector)
;;; It tries to re-adjust the cache every time it makes a new fill.  The
;;; intuition here is that we want uniformity in the number of probes needed to
;;; find an entry.  Furthermore, adjusting has the nice property of throwing out
;;; any entries that are invalid.
;;;
(defvar *cache-expand-threshold* 1.25)

(defun cache-count (cache)
  (with-local-cache-functions (cache)
    (let ((count 0)(location (if (= (nkeys) 1) (line-size) 1)))
      (declare (type index count))
      (dotimes (i (nlines) count)
	(unless (or (location-reserved-p location)
		    (not (location-valid-p location nil)))
	  (setq count (the index (1+ count))))
	(setq location (next-location location))))))

#|
(defun entry-in-cache-p (cache wrappers value)
  (declare (ignore value))
  (with-local-cache-functions (cache)
    (dotimes (i (nlines))
      (unless (line-reserved-p i)
	(when (equal (line-wrappers i) wrappers)
	  (return t))))))
|#

(defun fill-cache (cache wrappers value &optional free-cache-p)
  (declare (values cache))
  (unless wrappers ; fill-cache won't return if wrappers is nil, might as well check.
    (error "fill-cache: wrappers arg is NIL!"))
  (or (fill-cache-p nil cache wrappers value)
      (and (< (the index (ceiling (* (cache-count cache) 1.25)))
	      (the index
                   (if (= (cache-nkeys cache) 1)
		       (1- (cache-nlines cache))
		       (cache-nlines cache))))
	   (adjust-cache cache wrappers value free-cache-p))
      (expand-cache cache wrappers value free-cache-p)))

(defun probe-cache (cache wrappers &optional default)
  (declare (values value))
  (unless wrappers (error "probe-cache: wrappers arg is NIL!"))
  (with-local-cache-functions (cache)
    (let* ((location (compute-primary-cache-location (field) (mask) wrappers))
	   (limit (funcall-function (limit-fn) (nlines))))
      (declare (type index location limit))
      (when (location-reserved-p location)
	(setq location (next-location location)))
      (dotimes (i limit default)
	(when (location-matches-wrappers-p location wrappers)
	  (return (or (not (valuep)) (location-value location))))
	(setq location (next-location location))))))

(defun map-cache (function cache &optional set-p)
  (declare (type real-function function))
  (with-local-cache-functions (cache)
    (let ((set-p (and set-p (valuep))))
      (dotimes (i (nlines) cache)
	(unless (or (line-reserved-p i) (not (line-valid-p i nil)))
	  (let ((value (funcall function (line-wrappers i) (line-value i))))
	    (when set-p
	      (setf (cache-vector-ref (pcl-vector) (+ (line-location i) (nkeys)))
		    value))))))))


;;;
;;; returns T or NIL
;;;

(defun fill-cache-p (forcep cache wrappers value)
  (with-local-cache-functions (cache)
    (let* ((location (compute-primary-cache-location (field) (mask) wrappers))
	   (primary (location-line location)))
      (declare (type index location primary))
      (multiple-value-bind (free emptyp)
	  (find-free-cache-line primary cache wrappers)
	(when (or forcep emptyp)
	  (when (not emptyp)
	    (push (cons (line-wrappers free) (line-value free)) 
		  (cache-overflow cache)))
	  ;;(fill-line free wrappers value)
	  (let ((line free))
	    (declare (type index line))
	    (when (line-reserved-p line)
	      (error "Attempt to fill a reserved line."))
	    (let ((loc (line-location line))
		  (cache-vector (pcl-vector)))
	      (declare (type index loc) (simple-vector cache-vector))
	      (cond ((= (nkeys) 1)
		     (setf (cache-vector-ref cache-vector loc) wrappers)
		     (when (valuep)
		       (setf (cache-vector-ref cache-vector (1+ loc)) value)))
		    (t
		     (let ((i 0))
		       (declare (type index i))
		       (dolist (w wrappers)
			 (setf (cache-vector-ref cache-vector (+ loc i)) w)
			 (setq i (the index (1+ i)))))
		     (when (valuep)
		       (setf (cache-vector-ref cache-vector (+ loc (nkeys)))
			     value))))))
	  cache)))))

(defun fill-cache-from-cache-p (forcep cache from-cache from-line)
  (declare (type index from-line))
  (with-local-cache-functions (from-cache)
    (let ((primary (line-primary (field) from-line)))
      (declare (type index primary))
      (multiple-value-bind (free emptyp)
	  (find-free-cache-line primary cache)
	(when (or forcep emptyp)
	  (when (not emptyp)
	    (push (cons (line-wrappers free) (line-value free))
		  (cache-overflow cache)))
	  ;;(transfer-line from-cache-vector from-line cache-vector free)
	  (let ((from-cache-vector (pcl-vector))
		(to-cache-vector (cache-vector cache))
		(to-line free))
	    (declare (type index to-line))
	    (if (line-reserved-p to-line)
		(error "transfering something into a reserved cache line.")
		(let ((from-loc (line-location from-line))
		      (to-loc (line-location to-line)))
		  (declare (type index from-loc to-loc))
		  (modify-cache to-cache-vector
				(dotimes (i (line-size))
				  (setf (cache-vector-ref to-cache-vector
							  (+ to-loc i))
					(cache-vector-ref from-cache-vector
							  (+ from-loc i))))))))
	  cache)))))

;;;
;;; Returns NIL or (values <field> <cache>)
;;; 
;;; This is only called when it isn't possible to put the entry in the cache
;;; the easy way.  That is, this function assumes that FILL-CACHE-P has been
;;; called as returned NIL.
;;;
;;; If this returns NIL, it means that it wasn't possible to find a wrapper
;;; field for which all of the entries could be put in the cache (within the
;;; limit).  
;;;
(defun adjust-cache (cache wrappers value free-old-cache-p)
  (with-local-cache-functions (cache)
    (let ((ncache (get-cache-from-cache cache (nlines) (field))))
      (do ((nfield (cache-field ncache) (next-wrapper-cache-number-index nfield)))
	  ((null nfield) (free-cache ncache) nil)
	(setf (cache-field ncache) (the index nfield))
	(labels ((try-one-fill-from-line (line)
		   (fill-cache-from-cache-p nil ncache cache line))
		 (try-one-fill (wrappers value)
		   (fill-cache-p nil ncache wrappers value)))
	  (if (and (dotimes (i (nlines) t)
		     (when (and (null (line-reserved-p i))
				(line-valid-p i wrappers))
		       (unless (try-one-fill-from-line i) (return nil))))
		   (dolist (wrappers+value (cache-overflow cache) t)
		     (unless (try-one-fill (car wrappers+value) (cdr wrappers+value))
		       (return nil)))
		   (try-one-fill wrappers value))
	      (progn (when free-old-cache-p (free-cache cache))
		     (return ncache))
	      (flush-cache-vector-internal (cache-vector ncache))))))))

		       
;;;
;;; returns: (values <cache>)
;;;
(defun expand-cache (cache wrappers value free-old-cache-p)
  (declare (values cache))
  (with-local-cache-functions (cache)
    (let ((ncache (get-cache-from-cache cache (the index (* (nlines) 2)))))
      (labels ((do-one-fill-from-line (line)
		 (unless (fill-cache-from-cache-p nil ncache cache line)
		   (do-one-fill (line-wrappers line) (line-value line))))
	       (do-one-fill (wrappers value)
		 (setq ncache (or (adjust-cache ncache wrappers value t)
				  (fill-cache-p t ncache wrappers value))))
	       (try-one-fill (wrappers value)
		 (fill-cache-p nil ncache wrappers value)))
	(dotimes (i (nlines))
	  (when (and (null (line-reserved-p i))
		     (line-valid-p i wrappers))
	    (do-one-fill-from-line i)))
	(dolist (wrappers+value (cache-overflow cache))
	  (unless (try-one-fill (car wrappers+value) (cdr wrappers+value))
	    (do-one-fill (car wrappers+value) (cdr wrappers+value))))
	(unless (try-one-fill wrappers value)
	  (do-one-fill wrappers value))
	(when free-old-cache-p (free-cache cache))
	ncache))))


;;;
;;; This is the heart of the cache filling mechanism.  It implements the decisions
;;; about where entries are placed.
;;; 
;;; Find a line in the cache at which a new entry can be inserted.
;;;
;;;   <line>
;;;   <empty?>           is <line> in fact empty?
;;;
(defun find-free-cache-line (primary cache &optional wrappers)
  (declare (values line empty?))
  (declare (type index primary))
  (with-local-cache-functions (cache)
    (when (line-reserved-p primary) (setq primary (next-line primary)))
    (let ((limit (funcall-function (limit-fn) (nlines)))
	  (field (field))
	  (wrappedp nil)
	  (lines nil)
	  (p primary) (s primary)
	  (successp nil))
      (declare (type index p s limit) (type field-type field)
               (type boolean wrappedp successp))
      (block find-free
	(loop
	 ;; Try to find a free line starting at <s>.  <p> is the
	 ;; primary line of the entry we are finding a free
	 ;; line for, it is used to compute the seperations.
	 (do* ((line s (next-line line))
	       (nsep (line-separation p s) (the index (1+ nsep))))
	      (())
	   (declare (type index line nsep))
	   (when (null (line-valid-p line wrappers)) ;If this line is empty or
	     (push line lines)		;invalid, just use it.
	     (return-from find-free (setq successp t)))
	   (let ((osep (line-separation (line-primary field line) line)))
	     (when (and wrappedp (>= line primary))
	       ;; have gone all the way around the cache, time to quit
	       (push line lines)
	       (return-from find-free (setq successp nil)))
	     (when (cond ((= nsep limit) t)
			 ((= nsep osep) (zerop (the index (random 2))))
			 ((> nsep osep) t)
			 (t nil))
	       ;; See if we can displace what is in this line so that we
	       ;; can use the line.
	       (when (= line (the index (1- (nlines)))) (setq wrappedp t))
	       (setq p (line-primary field line))
	       (setq s (next-line line))
	       (push line lines)
	       (return nil)))
	   (when (= line (the index (1- (nlines)))) (setq wrappedp t)))))
      ;; Do all the displacing.
      (loop 
       (when (null (cdr lines)) (return nil))
       (let ((dline (pop lines))
	     (line (car lines)))
	 (declare (type index dline line))
	 (when successp
	   ;;Copy from line to dline (dline is known to be free).
	   (let ((from-loc (line-location line))
		 (to-loc (line-location dline))
		 (cache-vector (pcl-vector)))
	     (declare (type index from-loc to-loc) (simple-vector cache-vector))
	     (modify-cache cache-vector
			   (dotimes (i (line-size))
			     (setf (cache-vector-ref cache-vector (+ to-loc i))
				   (cache-vector-ref cache-vector (+ from-loc i)))
			     (setf (cache-vector-ref cache-vector (+ from-loc i))
				   nil)))))))
      (values (car lines) successp))))

(defun default-limit-fn (nlines)
  (declare (type index nlines))
  (case nlines
    ((1 2 4) 1)
    ((8 16)  4)
    (otherwise 6)))

#-*lisp-simulator
(declaim (type cache *empty-cache*))
(defvar *empty-cache* (make-cache)) ; for defstruct slot initial value forms



;;;
;;; pre-allocate generic function caches.  The hope is that this will put
;;; them nicely together in memory, and that that may be a win.  Of course
;;; the first gc copy will probably blow that out, this really wants to be
;;; wrapped in something that declares the area static.
;;;
;;; This preallocation only creates about 25% more caches than PCL itself
;;; uses.  Some ports may want to preallocate some more of these.
;;; 
(eval-when (load)
  (dolist (n-size '((1 513)(3 257)(3 129)(14 128)(6 65)(2 64)(7 33)(16 32)
		    (16 17)(32 16)(64 9)(64 8)(6 5)(128 4)(35 2)))
    (let ((n (car n-size))
	  (size (cadr n-size)))
      (mapcar #'free-cache-vector
	      (mapcar #'get-cache-vector
		      (make-list n :initial-element size))))))

(defun caches-to-allocate ()
  (sort (let ((l nil))
	  (maphash #'(lambda (size entry)
		       (push (list (car entry) size) l))
		   pcl::*free-caches*)
	  l)
	#'> :key #'cadr))


