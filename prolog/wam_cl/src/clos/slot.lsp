;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package 'clos)

;;; ----------------------------------------------------------------------
;;; SLOT descriptors
;;;

(defvar *initform-unsupplied* nil)

(defvar *class-slot-unbound* nil)

(defstruct (slotd (:type list))
  name initargs initform accessors readers writers allocation type
  documentation)

(defun PARSE-CLASS-SLOT (slot)
  (let ((name nil)
        (initargs nil)
        (initform '*initform-unsupplied*)	; default
        (accessors ())
        (readers ())
        (writers ())
        (allocation ':instance)
        (type 'T)				; default
	(documentation nil)
	(slotd (make-slotd)))

    (cond ((symbolp slot)     (setq name slot))
          ((null (cdr slot))  (setq name (car slot)))
          (t
           (setq name (car slot))
           (let ((options (cdr slot))
		 (option nil)
		 (value nil))
             (loop (when (null options) (return t))
		   (setq option (pop options)
			 value (pop options))
		   (unless (legal-slot-option-p option)
		     (error "In the slot description ~S,~%~
                             the option ~S is not legal."
			    slot option))
                   (case option
                     (:initarg    (push value initargs))
                     (:initform   (setq initform value))
                     (:accessor   (push value accessors))
                     (:reader     (push value readers))
                     (:writer     (push value writers))
                     (:allocation (setq allocation value))
                     (:type       (setq type (if (null value) (error "In the slot description ~S, ~%~
                                                                      the type option is not complete")
					       value)))
                     (:documentation     (push value documentation)))))))

    (setf (slotd-name slotd)       name
;	  (slotd-keyword slotd)    (make-keyword name)
	  (slotd-initargs slotd)   initargs
	  (slotd-initform slotd)   initform
	  (slotd-accessors slotd)  accessors
	  (slotd-readers slotd)    readers
	  (slotd-writers slotd)    writers
	  (slotd-allocation slotd) allocation
	  (slotd-type slotd)       type)
	  (slotd-documentation slotd)    documentation

    slotd))

(defun parse-class-slots (slots)
  (do ((scan slots (cdr scan))
       (slot) (collect))
      ((null scan) (nreverse collect))
    (push (PARSE-CLASS-SLOT (first scan)) collect)))

(defun LEGAL-SLOT-OPTION-P (option)
  (member option
	  '(:accessor :reader :writer :allocation :initarg :initform :type
		      :documentation)))

;;; ----------------------------------------------------------------------
