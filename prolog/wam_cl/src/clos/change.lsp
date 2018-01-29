;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package 'clos)

;;; The mechanism for updating classes.

;;; ----------------------------------------------------------------------
;;; Invalid Class
;;; ----------------------------------------------------------------------

(defclass invalid () ())

(defmethod OPTIMIZE-SLOT-VALUE ((class class) form) form)
    
(defmethod OPTIMIZE-SET-SLOT-VALUE ((class class) form) form)

(defmethod slot-value ((object invalid) slot-name)
  ;; first update the instance
  (update-instance object)
  ;; now access the slot
  (slot-value object slot-name))

(defmethod (setf slot-value) (val (object invalid) slot-name)
  ;; first update the instance
  (update-instance object)
  ;; now modify the slot
  (setf (slot-value object slot-name) val))

;;; ----------------------------------------------------------------------

(defun update-instance (instance)
  (let* ((old-class (class-of instance))
	 (new-class (slot-value old-class 'forward)) ; was saved here by change-class
	 (old-instance-slot-names
	  (mapcar #'slotd-name (class-slots old-class)))
	 (old-class-slot-names
	  (mapcar #'slotd-name (class-class-slots old-class)))
	 (new-instance-slot-names
	  (mapcar #'slotd-name (class-slots new-class)))
	 (new-class-slot-names
	  (mapcar #'slotd-name (class-class-slots new-class)))
	 discarded-slots
	 added-slots
	 retained-instance-slots
	 property-list
	 position)
    ;; dont (declare (fixnum position)) otherwise if position will fail.
    (unless (equal old-instance-slot-names new-instance-slot-names)
      (setq discarded-slots
	    (set-difference old-instance-slot-names
			    new-instance-slot-names))
      ;; compute the property list
      (dolist (slot-name discarded-slots)
	(setq position
	      (position slot-name old-instance-slot-names :test #'eq))
	(push (cons slot-name (si:instance-ref instance position))
	      property-list))

      ;; go through all the new instance slots to retain old values
      (do* ((new-slots new-instance-slot-names (cdr new-slots))
	    (new-slot (car new-slots) (car new-slots))
	    (i 0 (1+ i)))
	   ((null new-slots))
	(declare (fixnum i))
	(setq position (position new-slot old-instance-slot-names :test #'eq))
	(if position
	    ;; for an old instance slot retain its value
	    (push (cons i (si:instance-ref instance position)) 
		  retained-instance-slots)
	    ;; else
	    (unless (member new-slot old-class-slot-names)
	      ;; for an old shared slot the value is already retained
	      (push new-slot added-slots)))))

    ;; modify the structure of the instance
    (si:change-instance instance new-class (length new-instance-slot-names))

    ;; restore local retained values
    ;; each entry is of type (slot-position . slot-value)
    (dolist (l retained-instance-slots)
      (si:instance-set instance (the fixnum (car l)) (cdr l)))

    ;; restore class retained values
    (dolist (class-slot-info (class-class-slots-values old-class))
      ;; class-slot-info is a pair (name . value)
      (let ((slot-name (car class-slot-info)))
	(if (setq position 
		  (position slot-name new-instance-slot-names :test #'eq))
	    ;; it has become an instance slot
	    (si:instance-set instance position (cdr class-slot-info))
	    ;; else
	    (when (member slot-name new-class-slot-names)
	      ;; it is still a class slot
	      (push (cons slot-name (cdr class-slot-info))
		    (class-class-slots-values new-class))))))

    ;; initialize newly added slots
    (update-instance-for-redefined-class instance added-slots
					 discarded-slots property-list)
    ))

(defun remove-optional-slot-accessors (class)
  (let ((class-name (class-name class)))
    (dolist (slotd (class-slots class))
      
      (dolist (accessor (slotd-accessors slotd))
	(let* ((gfun (symbol-function accessor))
	       (gf-object (si:gfun-instance gfun))
	       (setf-accessor (list 'setf accessor))
	       (setf-gfun (symbol-function setf-accessor))
	       (setf-gf-object (si:gfun-instance setf-gfun))
	       found)
	  ;; primary reader method
	  (when (setq found
		      (find-method gf-object nil (list class-name) nil))
	    (remove-method gf-object found))
	  ;; before reader method
	  (when (setq found
		      (find-method gf-object ':before (list class-name) nil))
	    (remove-method gf-object found))
	  ;; after reader method
	  (when (setq found
		      (find-method gf-object ':after (list class-name) nil))
	    (remove-method gf-object found))
	  (when (null (methods gf-object))
	    (fmakunbound accessor))
	  ;; primary writer method
	  (when (setq found
		      (find-method setf-gf-object nil (list nil class-name) nil))
	    (remove-method setf-gf-object found))
	  ;; before writer method
	  (when (setq found
		      (find-method setf-gf-object ':before (list nil class-name) nil))
	    (remove-method setf-gf-object found))
	  ;; after writer method
	  (when (setq found
		      (find-method setf-gf-object ':after (list nil class-name) nil))
	    (remove-method setf-gf-object found))
	  (when (null (methods gf-object))
	    (fmakunbound setf-accessor))))
      
      ;; remove previous defined reader methods
      (dolist (reader (slotd-readers slotd))
	(let* ((gfun (symbol-function reader))
	       (gf-object (si:gfun-instance gfun))
	       found)
	  ;; primary method
	  (when (setq found
		      (find-method gf-object nil (list class-name) nil))
	    (remove-method gf-object found))
	  ;; before method
	  (when (setq found
		      (find-method gf-object ':before (list class-name) nil))
	    (remove-method gf-object found))
	  ;; after method
	  (when (setq found
		      (find-method gf-object ':after (list class-name) nil))
	    (remove-method gf-object found))
	(when (null (methods gf-object))
	  (fmakunbound reader))))
      
      ;; remove previous defined writer methods
      (dolist (writer (slotd-writers slotd))
	(let* ((gfun (symbol-function writer))
	       (gf-object (si:gfun-instance gfun))
	       found)
	  ;; primary method
	  (when (setq found
		      (find-method gf-object nil (list class-name) nil))
	    (remove-method gf-object found))
	  ;; before method
	  (when (setq found
		      (find-method gf-object ':before (list class-name) nil))
	    (remove-method gf-object found))
	  ;; after method
	  (when (setq found
		      (find-method gf-object ':after (list class-name) nil))
	    (remove-method gf-object found))
	(when (null (methods gf-object))
	  (fmakunbound writer)))))))


;;; ----------------------------------------------------------------------
