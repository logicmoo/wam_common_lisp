;-*- Mode:     Lisp -*-
;;;; Author:   Marius Gerbershagen
;;;; Created:  Sat Dec 19 12:27:21 2020
;;;; Contains: Tests which check that the compiler and loader correctly order load time forms


;;; Ordering constraints
;;;
;;; CLHS make-load-form:
;;;
;;; > Each initialization form is evaluated as soon as possible after
;;;   its associated creation form, as determined by data flow. If the
;;;   initialization form for an object does not reference any other
;;;   objects not referenced earlier in the file and processed by the
;;;   file compiler using make-load-form, the initialization form is
;;;   evaluated immediately after the creation form.
;;;
;;; > If a creation or initialization form F does contain references
;;;   to such objects, the creation forms for those other objects are
;;;   evaluated before F, and the initialization forms for those other
;;;   objects are also evaluated before F whenever they do not depend
;;;   on the object created or initialized by F.

;;; Note:
;;;
;;; Many implementations do not strictly follow the spec in this
;;; regard, in particular for the ordering of initialization forms.
;;; This is usually not a big problem, because load time forms which
;;; need a proper implementation of the spec in order to correctly
;;; reconstruct objects (for example due to initialization forms that
;;; access slots of previously created load time forms) are uncommon
;;; in practice.


(in-package :cl-test)

(defclass load-form-test-object ()
  ((name :initarg :name :reader load-form-test-name)
   (make-dependency1 :initarg :make-dependency1 :accessor load-form-test-make-dependency1 :initform nil)
   (make-dependency2 :initarg :make-dependency2 :accessor load-form-test-make-dependency2 :initform nil)
   (init-dependency1 :initarg :init-dependency1 :accessor load-form-test-init-dependency1 :initform nil)
   (init-dependency2 :initarg :init-dependency2 :accessor load-form-test-init-dependency2 :initform nil)))

(defvar *load-form-test-order* nil)

(defmethod make-load-form ((x load-form-test-object) &optional environment)
  (declare (ignore environment))
  (values
   `(progn
      (push :creating *load-form-test-order*)
      (push ',(load-form-test-name x) *load-form-test-order*)
      (make-instance ',(class-of x) :name ',(load-form-test-name x)
                                    :make-dependency1 ',(slot-value x 'make-dependency1)
                                    :make-dependency2 ',(slot-value x 'make-dependency2)))
   `(progn
      (push :initializing *load-form-test-order*)
      (push ',(load-form-test-name x) *load-form-test-order*)
      (setf (load-form-test-init-dependency1 ',x) ',(slot-value x 'init-dependency1))
      (setf (load-form-test-init-dependency2 ',x) ',(slot-value x 'init-dependency2)))))

(defmethod print-object ((obj load-form-test-object) stream)
  (write (load-form-test-name obj) :stream stream))

(defun write-text-compile-and-load (text)
  (let ((file "make-load-form-test.lsp"))
    (with-open-file (s file :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string "(in-package #:cl-test)" s)
      (write-string text s))
    (let ((compiled-file (compile-file file :verbose nil :print nil)))
      (if compiled-file
          (load compiled-file)
          (error "Compilation failed")))))

(defmacro def-load-form-test (name text checks expected-ordering)
  `(deftest ,name
       (progn (setf *load-form-test-order* nil)
              (write-text-compile-and-load ,text)
              (let ((results (list ,@checks
                                   (reverse *load-form-test-order*))))
                (mapcar #'makunbound '(*a* *b* *c* *aa* *bb* *cc*))
                (values-list results)))
     ,@(loop repeat (length checks) collect t)
     ,expected-ordering))

;;; no dependencies: load time forms are evaluated in the same order
;;; as the definition of the literal objects
(def-load-form-test make-load-form.order.1
  "(defparameter *a* #.(make-instance 'load-form-test-object :name 'a))"
  ((eql (load-form-test-name *a*) 'a)
   (eql (load-form-test-make-dependency1 *a*) nil)
   (eql (load-form-test-make-dependency2 *a*) nil)
   (eql (load-form-test-init-dependency1 *a*) nil)
   (eql (load-form-test-init-dependency2 *a*) nil))
  (:creating a :initializing a))

(def-load-form-test make-load-form.order.2
  "(defparameter *a* #.(make-instance 'load-form-test-object :name 'a))
   (defparameter *b* #.(make-instance 'load-form-test-object :name 'b))"
  ((eql (load-form-test-name *a*) 'a)
   (eql (load-form-test-make-dependency1 *a*) nil)
   (eql (load-form-test-make-dependency2 *a*) nil)
   (eql (load-form-test-init-dependency1 *a*) nil)
   (eql (load-form-test-init-dependency2 *a*) nil)
   (eql (load-form-test-name *b*) 'b)
   (eql (load-form-test-make-dependency1 *b*) nil)
   (eql (load-form-test-make-dependency2 *b*) nil)
   (eql (load-form-test-init-dependency1 *b*) nil)
   (eql (load-form-test-init-dependency2 *b*) nil))
  (:creating a :initializing a :creating b :initializing b))

;;; dependencies only in init form
(def-load-form-test make-load-form.order.3
  "(eval-when (:compile-toplevel)
     (defparameter *aa* (make-instance 'load-form-test-object :name 'a))
     (defparameter *bb* (make-instance 'load-form-test-object :name 'b))
     (setf (load-form-test-init-dependency1 *bb*) *aa*))
   (defparameter *a* #.*aa*)
   (defparameter *b* #.*bb*)"
  ((eql (load-form-test-name *a*) 'a)
   (eql (load-form-test-make-dependency1 *a*) nil)
   (eql (load-form-test-make-dependency2 *a*) nil)
   (eql (load-form-test-init-dependency1 *a*) nil)
   (eql (load-form-test-init-dependency2 *a*) nil)
   (eql (load-form-test-name *b*) 'b)
   (eql (load-form-test-make-dependency1 *b*) nil)
   (eql (load-form-test-make-dependency2 *b*) nil)
   (eql (load-form-test-init-dependency1 *b*) *a*)
   (eql (load-form-test-init-dependency2 *b*) nil))
  (:creating a :initializing a      ; no dependencies for a
   :creating b :initializing b))

;; make-load-form.order.3 in reverse order, (defparameter *b* #.*bb*) before (defparameter *a* #.*aa*)
(def-load-form-test make-load-form.order.4
  "(eval-when (:compile-toplevel)
     (defparameter *aa* (make-instance 'load-form-test-object :name 'a))
     (defparameter *bb* (make-instance 'load-form-test-object :name 'b))
     (setf (load-form-test-init-dependency1 *bb*) *aa*))
   (defparameter *b* #.*bb*)
   (defparameter *a* #.*aa*)"
  ((eql (load-form-test-name *a*) 'a)
   (eql (load-form-test-make-dependency1 *a*) nil)
   (eql (load-form-test-make-dependency2 *a*) nil)
   (eql (load-form-test-init-dependency1 *a*) nil)
   (eql (load-form-test-init-dependency2 *a*) nil)
   (eql (load-form-test-name *b*) 'b)
   (eql (load-form-test-make-dependency1 *b*) nil)
   (eql (load-form-test-make-dependency2 *b*) nil)
   (eql (load-form-test-init-dependency1 *b*) *a*)
   (eql (load-form-test-init-dependency2 *b*) nil))
  (:creating b         ; no dependency for creation form of b
   :creating a         ; *a* must be created before *b* is initialized
   :initializing a     ; no dependency for init form of a, evaluate immediately after creating a
   :initializing b))

;;; dependencies only in make form
(def-load-form-test make-load-form.order.5
  "(eval-when (:compile-toplevel)
     (defparameter *aa* (make-instance 'load-form-test-object :name 'a))
     (defparameter *bb* (make-instance 'load-form-test-object :name 'b))
     (setf (load-form-test-make-dependency1 *bb*) *aa*))
   (defparameter *a* #.*aa*)
   (defparameter *b* #.*bb*)"
  ((eql (load-form-test-name *a*) 'a)
   (eql (load-form-test-make-dependency1 *a*) nil)
   (eql (load-form-test-make-dependency2 *a*) nil)
   (eql (load-form-test-init-dependency1 *a*) nil)
   (eql (load-form-test-init-dependency2 *a*) nil)
   (eql (load-form-test-name *b*) 'b)
   (eql (load-form-test-make-dependency1 *b*) *a*)
   (eql (load-form-test-make-dependency2 *b*) nil)
   (eql (load-form-test-init-dependency1 *b*) nil)
   (eql (load-form-test-init-dependency2 *b*) nil))
  (:creating a :initializing a :creating b :initializing b))

;; make-load-form.order.5 in reverse order
(def-load-form-test make-load-form.order.6
  "(eval-when (:compile-toplevel)
     (defparameter *aa* (make-instance 'load-form-test-object :name 'a))
     (defparameter *bb* (make-instance 'load-form-test-object :name 'b))
     (setf (load-form-test-make-dependency1 *bb*) *aa*))
   (defparameter *b* #.*bb*)
   (defparameter *a* #.*aa*)"
  ((eql (load-form-test-name *a*) 'a)
   (eql (load-form-test-make-dependency1 *a*) nil)
   (eql (load-form-test-make-dependency2 *a*) nil)
   (eql (load-form-test-init-dependency1 *a*) nil)
   (eql (load-form-test-init-dependency1 *a*) nil)
   (eql (load-form-test-name *b*) 'b)
   (eql (load-form-test-make-dependency1 *b*) *a*)
   (eql (load-form-test-make-dependency2 *b*) nil)
   (eql (load-form-test-init-dependency1 *b*) nil)
   (eql (load-form-test-init-dependency2 *b*) nil))
  (:creating a                      ; *a* must be created before *b* is created
   :initializing a                  ; no dependency for init form of a, evaluate immediately after creating a
   :creating b :initializing b))

;;; circular dependency
(def-load-form-test make-load-form.order.7
  "(eval-when (:compile-toplevel)
     (defparameter *aa* (make-instance 'load-form-test-object :name 'a))
     (defparameter *bb* (make-instance 'load-form-test-object :name 'b))
     (setf (load-form-test-make-dependency1 *bb*) *aa*)
     (setf (load-form-test-init-dependency1 *aa*) *bb*))
   (defparameter *a* #.*aa*)
   (defparameter *b* #.*bb*)"
  ((eql (load-form-test-name *a*) 'a)
   (eql (load-form-test-make-dependency1 *a*) nil)
   (eql (load-form-test-make-dependency2 *a*) nil)
   (eql (load-form-test-init-dependency1 *a*) *b*)
   (eql (load-form-test-init-dependency2 *a*) nil)
   (eql (load-form-test-name *b*) 'b)
   (eql (load-form-test-make-dependency1 *b*) *a*)
   (eql (load-form-test-make-dependency2 *b*) nil)
   (eql (load-form-test-init-dependency1 *b*) nil)
   (eql (load-form-test-init-dependency2 *b*) nil))
  (:creating a                      ; no dependencies for creation form of a
   :creating b                      ; *b* must be created *b* before *a* is initialized
   :initializing b                  ; no dependencies for init form of b, evaluate immediately after creating a
   :initializing a))

;; make-load-form.order.7 in reverse order
(def-load-form-test make-load-form.order.8
  "(eval-when (:compile-toplevel)
     (defparameter *aa* (make-instance 'load-form-test-object :name 'a))
     (defparameter *bb* (make-instance 'load-form-test-object :name 'b))
     (setf (load-form-test-make-dependency1 *bb*) *aa*)
     (setf (load-form-test-init-dependency1 *aa*) *bb*))
   (defparameter *b* #.*bb*)
   (defparameter *a* #.*aa*)"
  ((eql (load-form-test-name *a*) 'a)
   (eql (load-form-test-make-dependency1 *a*) nil)
   (eql (load-form-test-make-dependency2 *a*) nil)
   (eql (load-form-test-init-dependency1 *a*) *b*)
   (eql (load-form-test-init-dependency2 *a*) nil)
   (eql (load-form-test-name *b*) 'b)
   (eql (load-form-test-make-dependency1 *b*) *a*)
   (eql (load-form-test-make-dependency2 *b*) nil)
   (eql (load-form-test-init-dependency1 *b*) nil)
   (eql (load-form-test-init-dependency2 *b*) nil))
  (:creating a :creating b :initializing b :initializing a))

;;; 3 load forms, transitive make dependencies
(def-load-form-test make-load-form.order.9
  "(eval-when (:compile-toplevel)
     (defparameter *aa* (make-instance 'load-form-test-object :name 'a))
     (defparameter *bb* (make-instance 'load-form-test-object :name 'b))
     (defparameter *cc* (make-instance 'load-form-test-object :name 'c))
     (setf (load-form-test-make-dependency1 *bb*) *aa*)
     (setf (load-form-test-make-dependency1 *cc*) *bb*)
     (setf (load-form-test-make-dependency2 *cc*) *aa*))
   (defparameter *a* #.*aa*)
   (defparameter *b* #.*bb*)
   (defparameter *c* #.*cc*)"
  ((eql (load-form-test-name *a*) 'a)
   (eql (load-form-test-make-dependency1 *a*) nil)
   (eql (load-form-test-make-dependency2 *a*) nil)
   (eql (load-form-test-init-dependency1 *a*) nil)
   (eql (load-form-test-init-dependency2 *a*) nil)
   (eql (load-form-test-name *b*) 'b)
   (eql (load-form-test-make-dependency1 *b*) *a*)
   (eql (load-form-test-make-dependency2 *b*) nil)
   (eql (load-form-test-init-dependency1 *b*) nil)
   (eql (load-form-test-init-dependency2 *b*) nil)
   (eql (load-form-test-name *c*) 'c)
   (eql (load-form-test-make-dependency1 *c*) *b*)
   (eql (load-form-test-make-dependency2 *c*) *a*)
   (eql (load-form-test-init-dependency1 *c*) nil)
   (eql (load-form-test-init-dependency2 *c*) nil))
  (:creating a :initializing a :creating b :initializing b :creating c :initializing c))

;; make-load-form.order.9 in reverse order
(def-load-form-test make-load-form.order.10
  "(eval-when (:compile-toplevel)
     (defparameter *aa* (make-instance 'load-form-test-object :name 'a))
     (defparameter *bb* (make-instance 'load-form-test-object :name 'b))
     (defparameter *cc* (make-instance 'load-form-test-object :name 'c))
     (setf (load-form-test-make-dependency1 *bb*) *aa*)
     (setf (load-form-test-make-dependency1 *cc*) *bb*)
     (setf (load-form-test-make-dependency2 *cc*) *aa*))
   (defparameter *c* #.*cc*)
   (defparameter *b* #.*bb*)
   (defparameter *a* #.*aa*)"
  ((eql (load-form-test-name *a*) 'a)
   (eql (load-form-test-make-dependency1 *a*) nil)
   (eql (load-form-test-make-dependency2 *a*) nil)
   (eql (load-form-test-init-dependency1 *a*) nil)
   (eql (load-form-test-init-dependency2 *a*) nil)
   (eql (load-form-test-name *b*) 'b)
   (eql (load-form-test-make-dependency1 *b*) *a*)
   (eql (load-form-test-make-dependency2 *b*) nil)
   (eql (load-form-test-init-dependency1 *b*) nil)
   (eql (load-form-test-init-dependency2 *b*) nil)
   (eql (load-form-test-name *c*) 'c)
   (eql (load-form-test-make-dependency1 *c*) *b*)
   (eql (load-form-test-make-dependency2 *c*) *a*)
   (eql (load-form-test-init-dependency1 *c*) nil)
   (eql (load-form-test-init-dependency2 *c*) nil))
  (:creating a :initializing a :creating b :initializing b :creating c :initializing c))

;;; 3 load forms, transitive init dependencies
(def-load-form-test make-load-form.order.11
  "(eval-when (:compile-toplevel)
     (defparameter *aa* (make-instance 'load-form-test-object :name 'a))
     (defparameter *bb* (make-instance 'load-form-test-object :name 'b))
     (defparameter *cc* (make-instance 'load-form-test-object :name 'c))
     (setf (load-form-test-init-dependency1 *bb*) *aa*)
     (setf (load-form-test-init-dependency1 *cc*) *bb*)
     (setf (load-form-test-init-dependency2 *cc*) *aa*))
   (defparameter *a* #.*aa*)
   (defparameter *b* #.*bb*)
   (defparameter *c* #.*cc*)"
  ((eql (load-form-test-name *a*) 'a)
   (eql (load-form-test-make-dependency1 *a*) nil)
   (eql (load-form-test-make-dependency2 *a*) nil)
   (eql (load-form-test-init-dependency1 *a*) nil)
   (eql (load-form-test-init-dependency2 *a*) nil)
   (eql (load-form-test-name *b*) 'b)
   (eql (load-form-test-make-dependency1 *b*) nil)
   (eql (load-form-test-make-dependency2 *b*) nil)
   (eql (load-form-test-init-dependency1 *b*) *a*)
   (eql (load-form-test-init-dependency2 *b*) nil)
   (eql (load-form-test-name *c*) 'c)
   (eql (load-form-test-make-dependency1 *c*) nil)
   (eql (load-form-test-make-dependency2 *c*) nil)
   (eql (load-form-test-init-dependency1 *c*) *b*)
   (eql (load-form-test-init-dependency2 *c*) *a*))
  (:creating a :initializing a :creating b :initializing b :creating c :initializing c))

;; make-load-form.order.11 in reverse order
(def-load-form-test make-load-form.order.12
  "(eval-when (:compile-toplevel)
     (defparameter *aa* (make-instance 'load-form-test-object :name 'a))
     (defparameter *bb* (make-instance 'load-form-test-object :name 'b))
     (defparameter *cc* (make-instance 'load-form-test-object :name 'c))
     (setf (load-form-test-init-dependency1 *bb*) *aa*)
     (setf (load-form-test-init-dependency1 *cc*) *bb*)
     (setf (load-form-test-init-dependency2 *cc*) *aa*))
   (defparameter *c* #.*cc*)
   (defparameter *b* #.*bb*)
   (defparameter *a* #.*aa*)"
  ((eql (load-form-test-name *a*) 'a)
   (eql (load-form-test-make-dependency1 *a*) nil)
   (eql (load-form-test-make-dependency2 *a*) nil)
   (eql (load-form-test-init-dependency1 *a*) nil)
   (eql (load-form-test-init-dependency2 *a*) nil)
   (eql (load-form-test-name *b*) 'b)
   (eql (load-form-test-make-dependency1 *b*) nil)
   (eql (load-form-test-make-dependency2 *b*) nil)
   (eql (load-form-test-init-dependency1 *b*) *a*)
   (eql (load-form-test-init-dependency2 *b*) nil)
   (eql (load-form-test-name *c*) 'c)
   (eql (load-form-test-make-dependency1 *c*) nil)
   (eql (load-form-test-make-dependency2 *c*) nil)
   (eql (load-form-test-init-dependency1 *c*) *b*)
   (eql (load-form-test-init-dependency2 *c*) *a*))
  (:creating c :creating b :creating a :initializing a :initializing b :initializing c))

;;; 3 load forms, circular dependencies
(def-load-form-test make-load-form.order.13
  "(eval-when (:compile-toplevel)
     (defparameter *aa* (make-instance 'load-form-test-object :name 'a))
     (defparameter *bb* (make-instance 'load-form-test-object :name 'b))
     (defparameter *cc* (make-instance 'load-form-test-object :name 'c))
     (setf (load-form-test-make-dependency1 *bb*) *aa*)
     (setf (load-form-test-make-dependency1 *cc*) *bb*)
     (setf (load-form-test-make-dependency2 *cc*) *aa*)
     (setf (load-form-test-init-dependency1 *bb*) *cc*)
     (setf (load-form-test-init-dependency1 *aa*) *cc*)
     (setf (load-form-test-init-dependency2 *aa*) *bb*))
   (defparameter *a* #.*aa*)
   (defparameter *b* #.*bb*)
   (defparameter *c* #.*cc*)"
  ((eql (load-form-test-name *a*) 'a)
   (eql (load-form-test-make-dependency1 *a*) nil)
   (eql (load-form-test-make-dependency2 *a*) nil)
   (eql (load-form-test-init-dependency1 *a*) *c*)
   (eql (load-form-test-init-dependency2 *a*) *b*)
   (eql (load-form-test-name *b*) 'b)
   (eql (load-form-test-make-dependency1 *b*) *a*)
   (eql (load-form-test-make-dependency2 *b*) nil)
   (eql (load-form-test-init-dependency1 *b*) *c*)
   (eql (load-form-test-init-dependency2 *b*) nil)
   (eql (load-form-test-name *c*) 'c)
   (eql (load-form-test-make-dependency1 *c*) *b*)
   (eql (load-form-test-make-dependency2 *c*) *a*)
   (eql (load-form-test-init-dependency1 *c*) nil)
   (eql (load-form-test-init-dependency2 *c*) nil))
  (:creating a :creating b :creating c :initializing c :initializing b :initializing a))

;; make-load-form.order.13 in reverse order
(def-load-form-test make-load-form.order.14
  "(eval-when (:compile-toplevel)
     (defparameter *aa* (make-instance 'load-form-test-object :name 'a))
     (defparameter *bb* (make-instance 'load-form-test-object :name 'b))
     (defparameter *cc* (make-instance 'load-form-test-object :name 'c))
     (setf (load-form-test-make-dependency1 *bb*) *aa*)
     (setf (load-form-test-make-dependency1 *cc*) *bb*)
     (setf (load-form-test-make-dependency2 *cc*) *aa*)
     (setf (load-form-test-init-dependency1 *bb*) *cc*)
     (setf (load-form-test-init-dependency1 *aa*) *cc*)
     (setf (load-form-test-init-dependency2 *aa*) *bb*))
   (defparameter *c* #.*cc*)
   (defparameter *b* #.*bb*)
   (defparameter *a* #.*aa*)"
  ((eql (load-form-test-name *a*) 'a)
   (eql (load-form-test-make-dependency1 *a*) nil)
   (eql (load-form-test-make-dependency2 *a*) nil)
   (eql (load-form-test-init-dependency1 *a*) *c*)
   (eql (load-form-test-init-dependency2 *a*) *b*)
   (eql (load-form-test-name *b*) 'b)
   (eql (load-form-test-make-dependency1 *b*) *a*)
   (eql (load-form-test-make-dependency2 *b*) nil)
   (eql (load-form-test-init-dependency1 *b*) *c*)
   (eql (load-form-test-init-dependency2 *b*) nil)
   (eql (load-form-test-name *c*) 'c)
   (eql (load-form-test-make-dependency1 *c*) *b*)
   (eql (load-form-test-make-dependency2 *c*) *a*)
   (eql (load-form-test-init-dependency1 *c*) nil)
   (eql (load-form-test-init-dependency2 *c*) nil))
  (:creating a :creating b :creating c :initializing c :initializing b :initializing a))
