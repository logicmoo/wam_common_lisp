;;;-*-Mode:LISP; Package: PCL; Base:10; Syntax:Common-lisp; -*-

;;;
;;; *************************************************************************
;;;
;;;   File: user-instances.lisp.
;;;
;;;     by Trent E. Lange, Effective Date 06-02-92
;;;
;;;
;;;  This file contains a metaclass (User-Vector-Class) whose instances
;;; are stored as simple-vectors, saving space over PCL's standard instance
;;; representations of PCL at the cost of some class redefinition flexibiliity.
;;;
;;; Permission is granted to any individual or institution to use, copy,
;;; modify and distribute this document.
;;;
;;; Suggestions, bugs, criticism and questions to lange@cs.ucla.edu
;;; *************************************************************************
;;;

(in-package 'pcl)

;;;   This file builds on the PCL-USER-INSTANCES feature of July 92 PCL
;;; to define the USER-VECTOR-CLASS metaclass whose instances are simple
;;; vectors.  The first element of the instance vector is the instance's
;;; class wrapper (providing internal PCL information about the instance's
;;; class).  The remaining elements of the instance vector are the instance's
;;; slots themselves.
;;;
;;;   The space overhead of user-vector-instances is only two vector cells
;;; (one for the vector, one for the wrapper).  This is contrast to standard
;;; PCL instances, which have a total overhead of four cells.  (Standard
;;; instances in PCL are represented as instances of structure STD-INSTANCE
;;; having two slots, one for the wrapper and one holding a simple-vector
;;; which is the instance's slots).  This two-cell space savings per instance
;;; comes at the cost of losing some class redefinition flexibility, since
;;; simple-vectors cannot have their sizes changed dynamically.
;;; All current instances of user-instance-vectors therefore become
;;; permanently obsolete if the classes' instance slots change.
;;;
;;;   This code requires July 92 PCL or later compiled with the
;;; PCL-USER-INSTANCES feature turned on (see PCL's low.lisp file).
;;;

#-pcl-user-instances
(eval-when (compile load eval)
(error "Cannot use user-instances, since PCL was compiled without
        PCL-USER-INSTANCES on the *features* list (see pcl file low.lisp.)")
)

(eval-when (compile load eval)
(defclass user-vector-class-mixin () ()
  (:documentation
    "Use this mixin for metaclasses whose instances are USER-INSTANCES
     instantiated as simple-vectors.  This saves space over the standard
     instances used by standard-class, at the cost of losing the ability to
     redefine the slots in a class and still have old instances updated correctly."))

(defclass user-vector-class (user-vector-class-mixin standard-class) ()
  (:documentation
    "A metaclass whose instances are USER-INSTANCES instantiated as simple-vectors.
     This saves space over the standard instances used by standard-class, at the
     cost of losing the ability to redefine the slots in a class and still have old
     instances updated correctly."))

(defmethod validate-superclass ((class user-vector-class-mixin)
                                (new-super T))
  (or (typep new-super 'user-vector-class-mixin)
      (eq new-super (find-class 'standard-object))))

(defclass user-vector-object (standard-object) ()
  (:metaclass user-vector-class))
)

;;;
;;;
;;; Instance allocation stuff.
;;;

(defmacro user-vector-instance-p (object)
  (once-only (object)
    `(the boolean
          (and (simple-vector-p ,object)
               (plusp (length (the simple-vector ,object)))
               (wrapper-p (%svref ,object 0))))))

(defmacro user-vector-instance-wrapper (object)
  `(%svref ,object 0))

(defsetf user-vector-instance-wrapper (object) (new-value)
  `(setf (%svref ,object 0) ,new-value))

(defmacro user-vector-instance-slots (instance)
  ;; The slots vector of user-vector instances is the instance itself.
  instance)

(defmacro set-user-vector-instance-slots (instance new-value)
  `(progn
     (warn "Attempt to set user-vector-instance-slots of ~S to ~S"
           ,instance ,new-value)
     ,new-value))

(defun user-instance-p (x)
  "Is X a user instance, specifically a user-vector-instance?"
  (user-vector-instance-p x))

(defun user-instance-slots (x)
  "Return the slots of this user-vector-instance."
  (user-vector-instance-slots x))

(defun user-instance-wrapper (x)
  "Return the wrapper of this user-vector-instance."
  (user-vector-instance-wrapper x))

(defun set-user-instance-wrapper (x new)
  (setf (user-vector-instance-wrapper x) new))

(defmacro get-user-instance-p (x)
  `(user-vector-instance-p ,x))

(defmacro get-user-instance-wrapper (x)
  `(user-vector-instance-wrapper ,x))

(defmacro get-user-instance-slots (x)
  `(user-vector-instance-slots ,x))

(eval-when (eval #+cmu load)
  (force-compile 'user-instance-p)
  (force-compile 'user-instance-slots)
  (force-compile 'user-instance-wrapper)
  (force-compile 'set-user-instance-wrapper))


;;;
;;; Methods needed for user-vector-class-mixin.
;;;

(defconstant *not-a-slot* (gensym "NOT-A-SLOT"))

(defmethod allocate-instance ((class user-vector-class-mixin) &rest initargs)
  (declare (ignore initargs))
  (unless (class-finalized-p class) (finalize-inheritance class))
  (let* ((class-wrapper (class-wrapper class))
         (copy-instance (wrapper-allocate-static-slot-storage-copy
                           class-wrapper))
         (instance      (copy-simple-vector copy-instance)))
    (declare (type simple-vector copy-instance instance))
    (setf (user-vector-instance-wrapper instance) class-wrapper)
    instance))

(defmethod make-instances-obsolete ((class user-vector-class-mixin))
  "The slots of user-vector-instances are stored in the instance vector
   themselves (a simple-vector), so old instances cannot be updated properly."
  (setf (slot-value class 'prototype) NIL)
  (warn "Obsoleting user-vector class ~A, all current instances will be invalid..."
        class))

(defmethod compute-layout :around ((class user-vector-class-mixin)
                                    cpl instance-eslotds)
  ;; First element of user-vector-instance is actually its wrapper.
  (declare (ignore cpl instance-eslotds))
  (cons *not-a-slot* (call-next-method)))

(defmethod compute-instance-layout :around ((class user-vector-class-mixin)
                                            instance-eslotds)
  ;; First element of user-vector-instance is actually its wrapper.
  (declare (ignore instance-eslotds))
  (cons *not-a-slot* (call-next-method)))

(defmethod wrapper-fetcher ((class user-vector-class-mixin))
  'user-vector-instance-wrapper)

(defmethod slots-fetcher ((class user-vector-class-mixin))
  'user-vector-instance-slots)

(defmethod raw-instance-allocator ((class user-vector-class-mixin))
  'allocate-user-vector-instance)


;;; Inform PCL that it is still safe to use its standard slot-value
;;; optimizations with user-vector-class-mixin's slot-value-using-class
;;; methods:

(pushnew
  '(user-vector-class-mixin standard-object standard-effective-slot-definition)
   *safe-slot-value-using-class-specializers*)

(pushnew
  '(T user-vector-class-mixin standard-object standard-effective-slot-definition)
   *safe-set-slot-value-using-class-specializers*)

(pushnew
  '(user-vector-class-mixin standard-object standard-effective-slot-definition)
  *safe-slot-boundp-using-class-specializers*)

(defmethod slot-value-using-class
  ((class user-vector-class-mixin)
   (object standard-object)
   (slotd standard-effective-slot-definition))
  (let* ((location (slot-definition-location slotd))
	 (value
           (typecase location
	      (fixnum
                (%svref (user-vector-instance-slots object) location))
	      (cons
	        (cdr location))
	      (t
	       (error
                 "The slot ~s has neither :instance nor :class allocation, ~@
                              so it can't be read by the default ~s method."
		  slotd 'slot-value-using-class)))))
    (if (eq value *slot-unbound*)
	(slot-unbound class object (slot-definition-name slotd))
	value)))

(defmethod (setf slot-value-using-class)
	   (new-value (class user-vector-class-mixin)
                      (object standard-object)
		      (slotd standard-effective-slot-definition))
  (let ((location (slot-definition-location slotd)))
    (typecase location
      (fixnum
        (setf (%svref (user-vector-instance-slots object) location) new-value))
      (cons
        (setf (cdr location) new-value))
      (t
       (error "The slot ~s has neither :instance nor :class allocation, ~@
                           so it can't be written by the default ~s method."
	      slotd '(setf slot-value-using-class))))))

(defmethod slot-boundp-using-class
  ((class user-vector-class-mixin)
   (object standard-object)
   (slotd standard-effective-slot-definition))
  (let* ((location (slot-definition-location slotd))
	 (value
           (typecase location
	     (fixnum
               (%svref (user-vector-instance-slots object) location))
	     (cons
	       (cdr location))
	     (t
	       (error
                 "The slot ~s has neither :instance nor :class allocation, ~@
                              so it can't be read by the default ~s method."
		 slotd 'slot-boundp-using-class)))))
    (not (eq value *slot-unbound*))))



;;;
;;; The following functions and methods are not strictly necessary for
;;; user-vector-instances, but do speed things up a bit.
;;;

(defmethod make-optimized-reader-method-function
           ((class user-vector-class-mixin)
            generic-function
            reader-method-prototype
            slot-name)
  (declare (ignore generic-function reader-method-prototype))
  (make-user-vector-instance-reader-method-function slot-name))

(defmethod make-optimized-writer-method-function
           ((class user-vector-class-mixin)
            generic-function
            reader-method-prototype
            slot-name)
  (declare (ignore generic-function reader-method-prototype))
  (make-user-vector-instance-writer-method-function slot-name))

(defmethod make-optimized-method-function
           ((class user-vector-class-mixin)
            generic-function
            boundp-method-prototype
            slot-name)
  (declare (ignore generic-function boundp-method-prototype))
  (make-user-vector-instance-boundp-method-function slot-name))

(defun make-user-vector-instance-reader-method-function (slot-name)
  (declare #.*optimize-speed*)
  #'(lambda (instance)
      (user-instance-slot-value instance slot-name)))

(defun make-user-vector-instance-writer-method-function (slot-name)
  (declare #.*optimize-speed*)
  #'(lambda (nv instance)
      (setf (user-instance-slot-value instance slot-name) nv)))

(defun make-user-vector-instance-boundp-method-function (slot-name)
  (declare #.*optimize-speed*)
  #'(lambda (instance)
      (user-instance-slot-boundp instance slot-name)))


(defun make-optimized-user-reader-method-function (slot-name index)
  (declare #.*optimize-speed*)
  (progn slot-name)
  #'(lambda (instance)
      (let ((value (%svref (user-vector-instance-slots instance) index)))
        (if (eq value *slot-unbound*)
            (slot-unbound (class-of instance) instance slot-name)
            value))))

(defun make-optimized-user-writer-method-function (index)
  (declare #.*optimize-speed*)
  #'(lambda (nv instance)
      (setf (%svref (user-vector-instance-slots instance) index) nv)))

(defun make-optimized-user-boundp-method-function (index)
  (declare #.*optimize-speed*)
  #'(lambda (instance)
      (not (eq (%svref (user-vector-instance-slots instance) index)
               *slot-unbound*))))



(defmacro with-user-instance-slots (slot-entries instance-form &body body)
  "Optimized version of With-Slots that assumes that the instance-form
   evaluates to a user-vector-instance.  The result is undefined if it does not.
   With-user-vector-instance-slots is faster than With-Slots because it factors
   out functions common to all slot accesses on the instance.  It has two
   extensions to With-Slots: (1) the second value of slot-entries are
   evaluated as forms rather than considered to be hard slot-names, allowing
   access of variable slot-names.  (2) if a :variable-instance keyword is
   the first part of the body, then the instance-form is treated as a variable
   form, which is always expected to return an instance of the same class.
   The value of the keyword must be an instance that is the same class as
   instance-form will always return."
  (build-with-optimized-slots-form slot-entries instance-form body 'user-instance))



;;;
;;;  Lisp and CLOS print compatability functions:
;;;
;;;  This gets really ugly because most lisps don't use PRINT-OBJECT
;;;  for the printed representation of their objects like they're supposed
;;;  to.  (And if the lisp did, it wouldn't be using PCL.).  And since
;;;  user-vector-instances are implemented as simple-vectors, the only
;;;  way to get their printed representations to look right is to make
;;;  PRINT-OBJECT object to work.
;;;    We therefore have to patch the standard lisp printing functions.
;;;  If all goes well, then everything is honky-dory.  If it doesn't, then
;;;  debugging can get pretty messy since we were screwing with the standard
;;;  printing functions.  Things should work, but if they don't, then calling
;;;  RESTORE-LISP-PRINTERS will get things back to normal.

(defvar *old-write* NIL)
(defvar *old-princ* NIL)
(defvar *old-prin1* NIL)
(defvar *old-print* NIL)

;; Structure dummy-print-instance is a structure whose sole purpose
;; in life is to act as a placeholder to allow the print-object of 
;; user-vector-class objects to be printed.

(defstruct (dummy-print-instance
              (:print-function print-dummy-print-instance))
  (print-object-string nil))

(declaim (type list *dummy-print-instance-garbage*))
(defvar      *dummy-print-instance-garbage* NIL)
(defconstant *dummy-print-instance-garbage-limit* 100)

(defmacro pure-array-p (x &optional (test-user-vector-instance-p T))
  "Returns whether item is a 'pure' array -- i.e. not a string, and
   not something holding a CLOS instance."
  (once-only (x)
    `(the boolean
          (locally (declare (inline arrayp stringp typep))
            (and (arrayp ,x)
                 (not (stringp ,x))
                 #-(or cmu (and lucid pcl))
                 (not (typep ,x 'structure))
                 ,@(when test-user-vector-instance-p
                     `((not (user-vector-instance-p ,x))))
                 #-(or cmu (and lucid pcl))
                 (not (typep ,x 'standard-object)))))))

(defun copy-any-array (old-array &rest keys-passed &key key dimensions)
  ;; Returns a copy of old-array.  If :key is provided, then the
  ;; elements of the new-array are the result of key applied to
  ;; old-array's elements.  If :dimensions is provided, and it is
  ;; different than old-array's dimensions, then the new-array is created
  ;; with those dimensions, and everything that can be copied from
  ;; old-array is copied into it.  It is an error if the rank of
  ;; the array specified by dimensionss is different than that of the
  ;; old-array.
  (declare (type array              old-array)
           (type (or function null) key)
           (type list               dimensions keys-passed))
  (cond
    ((simple-vector-p old-array)
     (apply #'copy-array-contents
            old-array
            (make-array (the index
                             (if dimensions
                                 (car dimensions)
                               (length (the simple-vector old-array)))))
            keys-passed))
    ((vectorp old-array)
     (apply #'copy-array-contents
            old-array
            (make-array (the index
                             (if dimensions
                                 (car dimensions)
                               (length (the vector old-array))))
                        :element-type (array-element-type old-array)
                        :adjustable   (adjustable-array-p old-array))
            keys-passed))
    ((arrayp old-array)
     (let* ((old-dimensions (array-dimensions   old-array))
            (new-dimensions (or dimensions old-dimensions))
            (element-type   (array-element-type old-array))
            (new-array
              (make-array new-dimensions
                          :element-type element-type
                          :adjustable   (adjustable-array-p old-array))))
       (declare (type list  old-dimensions new-dimensions)
                (type array new-array))
       (if (or (null dimensions) (equal new-dimensions old-dimensions))
           (let* ((displaced-old-array
                    (make-array (array-total-size old-array)
                      :element-type element-type
                      :displaced-to old-array))
                  (displaced-new-array
                    (make-array (array-total-size new-array)
                      :element-type element-type
                      :displaced-to new-array)))
             (declare (type array displaced-old-array displaced-new-array))
             (copy-array-contents displaced-old-array
                                  displaced-new-array
                                  :key key))
         (let ((first-dimension
                  (min (the index (car new-dimensions))
                       (the index (car old-dimensions)))))
            (declare (type index first-dimension))
            (walk-dimensions
              (mapcar #'min (cdr new-dimensions) (cdr old-dimensions))
              #'(lambda (post-indices)
                 (copy-array-contents old-array new-array
                                      :key key
                                      :length first-dimension
                                      :post-indices post-indices)))))
       new-array))))

(defun copy-array-contents
       (old-array new-array &key key length post-indices &allow-other-keys)
  ;; Copies the contents of old-array into new-array, using key if
  ;; supplied.  Only the first :length items are copied (defaulting
  ;; to the length of the old-array).  If :post-indices are passed, then
  ;; they are used as "post" indices to an aref.
  (macrolet
   ((do-copy (aref old new key key-type len post-indices)
     (let ((atype (if (eq aref #'svref) 'simple-vector 'array)))
       `(dotimes (i (the index ,len))
          (setf ,(if post-indices
                     `(apply #'aref (the ,atype ,new) i ,post-indices)
                    `(,aref (the ,atype ,new) i))
                ,(if key-type
                     `(funcall
                        (the ,key-type ,key)
                        ,(if post-indices
                             `(apply #'aref (the ,atype ,old)
                                     i ,post-indices)
                           `(,aref (the ,atype ,old) i)))
                   (if post-indices
                      `(apply #'aref (the ,atype ,old) i ,post-indices)
                    `(,aref (the ,atype ,old) i)))))))
     (expand-on-key (aref key old new len post-ind)
       `(cond
         ((null ,key)
          (do-copy ,aref ,old ,new ,key NIL ,len ,post-ind))
         ((compiled-function-p ,key)
          (do-copy ,aref ,old ,new ,key compiled-function ,len ,post-ind))
         (T
          (do-copy ,aref ,old ,new ,key function ,len ,post-ind)))))
    (if (simple-vector-p old-array)
        (progn
          (when post-indices
            (error "Can't pass post-indices given to COPY-ARRAY-CONTENTS
                    from simple-vector"))
          
          (unless length
            (setf length (min (length (the simple-vector old-array))
                              (length (the simple-vector new-array)))))
          (expand-on-key svref key old-array new-array length NIL))
       (progn
         (unless length
           (setf length (min (the index (car (array-dimensions old-array)))
                             (the index (car (array-dimensions new-array))))))
         (if post-indices
             (expand-on-key #'aref key old-array new-array length post-indices)
             (expand-on-key aref key old-array new-array length NIL)))))
  new-array)

(declaim (ftype (function (list function) T) walk-dimensions))
(defun walk-dimensions (dimensions fn)
  (declare (type list     dimensions)
           (type function fn))
  ;; Given a list of dimensions (e.g. '(3 2 8)), this function walks
  ;; through every possible combination from 0 to 1- each of those
  ;; dimensions, and calling fn on each of them.
  (let ((compiled-p (compiled-function-p fn)))
    (labels
      ((doit (dims apply-dims)
         (declare (type list dims apply-dims))
         (if (cdr dims)
             (let ((last-dim  NIL)
                   (dims-left NIL))
                (loop (when (null (cdr dims))
                        (setf last-dim (car dims))
                        (return))
                      (if dims-left
                          (nconc dims-left (list (car dims)))
                        (setf dims-left (list (car dims))))
                      (setf dims (cdr dims)))
                 (dotimes (i (the index last-dim))
                   (doit dims-left (cons i apply-dims))))
           (if compiled-p 
               (dotimes (i (the index (car dims)))
                 (funcall (the compiled-function fn) (cons i apply-dims)))
             (dotimes (i (the index (car dims)))
               (funcall fn (cons i apply-dims)))))))
      (doit dimensions NIL))))

(defmacro funcall-printer (applyer print-function object keys)
  `(progn
     (if (or (arrayp ,object) (consp ,object))
         (multiple-value-bind (converted-item garbage)
           (convert-user-vector-instances-to-dummy-print-instances ,object)
           (,applyer (the compiled-function ,print-function)
                     converted-item ,keys)
           (deallocate-dummy-print-instances garbage))
       (,applyer (the compiled-function ,print-function)
                ,object ,keys))
     ,object))

(defun print-dummy-print-instance (instance stream depth)
  (declare (ignore depth))
  (let ((*print-pretty* NIL))
    (funcall (the compiled-function *old-princ*)
             (dummy-print-instance-print-object-string instance)
             stream)))

(defun allocate-dummy-print-instance (print-object-string)
  (if *dummy-print-instance-garbage*
      (let ((instance (pop *dummy-print-instance-garbage*)))
        (setf (dummy-print-instance-print-object-string instance)
              print-object-string)
        instance)
    (make-dummy-print-instance :print-object-string print-object-string)))

(defun dummy-print-instance-of (user-vector-instance)
  (allocate-dummy-print-instance 
    (with-output-to-string (str)
      (print-object user-vector-instance str))))

(defun deallocate-dummy-print-instances (dummies)
  (let ((count (length *dummy-print-instance-garbage*)))
    (declare (type index count))
    (dolist (dummy dummies)
      (when (> count *dummy-print-instance-garbage-limit*)
        (return))
      (push dummy *dummy-print-instance-garbage*)
      (setf count (the index (1+ count))))))
   
(defun convert-user-vector-instances-to-dummy-print-instances (item)
  (let ((print-length
          (or *print-length* 1000))
        (print-level
          (or *print-level* 1000))
        (dummy-print-instances-used NIL))
    (declare (fixnum print-length print-level))
    (labels
      ((doit (item level length)
         (declare (fixnum level length))
         (labels
           ((user-vector-instance-visible-within-p (item level length)
              (declare (fixnum level length))
              (cond
                ((>= length print-length) NIL)
                ((> level print-level) NIL)
                ((= level print-level) (user-vector-instance-p item))
                (T (cond
                    ((user-vector-instance-p item) T)
                    ((consp item)
                     (or (user-vector-instance-visible-within-p
                           (car item) (the fixnum (1+ level)) 0)
                         (user-vector-instance-visible-within-p
                           (cdr item) level (the fixnum (1+ length)))))
                    ((and *print-array* (pure-array-p item))
                     (let ((next-level (the fixnum (1+ level))))
                       (declare (fixnum next-level))
                       (dotimes (i (1- (length (the array item))) NIL)
                         (unless (< i print-length)
                           (return NIL))
                         (if (user-vector-instance-visible-within-p
                               (aref item i) next-level 0)
                             (return T))))))))))
            ;; doit body
            (cond
              ((user-vector-instance-p item)
               (let ((dummy (dummy-print-instance-of item)))
                 (push dummy dummy-print-instances-used)
                 dummy))
              ((consp item)
               (if (user-vector-instance-visible-within-p item level length)
                   (cons (doit (car item) (the fixnum (1+ level)) length)
                         (doit (cdr item) level (the fixnum (1+ length))))
                 item))
              ((and *print-array* (pure-array-p item NIL))
               (if (user-vector-instance-visible-within-p item level length)
                   (copy-any-array
                     item
                     :key
                     #'(lambda (item)
                         (if (user-vector-instance-p item)
                             (let ((dummy (dummy-print-instance-of item)))
                               (push dummy dummy-print-instances-used)
                               dummy)
                           item))
                     :dimensions
                       (mapcar #'1+ (array-dimensions item)))
                    item))
              (T item)))))

      ;; convert-user-vector-instances-to-dummy-print-instances body

      (let ((converted (doit item 0 0)))
        (values converted dummy-print-instances-used)))))

(force-compile 'convert-user-vector-instances-to-dummy-print-instances)

(unless *old-write* (setf *old-write* (symbol-function 'write)))
(defun new-write (object &rest keys-passed)
  (declare (list keys-passed))
  (funcall-printer apply *old-write* object keys-passed))
(force-compile 'write)
(setf (symbol-function 'write) (symbol-function 'new-write))

(unless *old-princ* (setf *old-princ* (symbol-function 'princ)))
(defun princ (object &optional stream)
  (funcall-printer funcall *old-princ* object stream))
(force-compile 'princ)

(unless *old-prin1* (setf *old-prin1* (symbol-function 'prin1)))
(defun prin1 (object &optional stream)
  (funcall-printer funcall *old-prin1* object stream))
(force-compile 'prin1)

(unless *old-print* (setf *old-print* (symbol-function 'print)))
(defun print (object &optional stream)
  (funcall-printer funcall *old-print* object stream))
(force-compile 'print)

(defun new-write-to-string (object &rest keys-passed)
  (declare (list keys-passed))
  (with-output-to-string (string-stream)
    (apply #'write object :stream string-stream keys-passed)))
(force-compile 'write-to-string)
(setf (symbol-function 'write-to-string)
      (symbol-function 'new-write-to-string))

(defun princ-to-string (object)
  (with-output-to-string (string-stream)
    (funcall-printer funcall *old-princ* object string-stream)
    string-stream))
(force-compile 'princ-to-string)

(defun prin1-to-string (object)
  (with-output-to-string (string-stream)
    (funcall-printer funcall *old-prin1* object string-stream)
    string-stream))
(force-compile 'prin1-to-string)
 
(defun restore-lisp-printers ()
  (setf (symbol-function 'write) *old-write*)
  (setf (symbol-function 'princ) *old-princ*)
  (setf (symbol-function 'prin1) *old-prin1*)
  (setf (symbol-function 'print) *old-print*))

