;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package 'clos)

;;; new methods for slot-boundp and slot-value

(defmethod slot-value ((class t) slot-name)
  (multiple-value-bind (val condition)
    (general-instance-get class slot-name)
    (ecase condition
      (:VALUE val)
      (:UNBOUND (slot-unbound (si:instance-class class) class slot-name))
      (:MISSING (slot-missing (si:instance-class class) class slot-name
			      'SLOT-VALUE))
      )))

(defmethod slot-value ((class standard-class) slot-name)
  (multiple-value-bind (val condition)
    (general-instance-get class slot-name)
    (ecase condition
      (:VALUE val)
      (:UNBOUND (slot-unbound (si:instance-class class) class slot-name))
      (:MISSING (slot-missing (si:instance-class class) class slot-name
			      'SLOT-VALUE))
      )))

(defmethod slot-boundp ((class standard-class) slot-name)
  (multiple-value-bind (val condition)
    (general-instance-get class slot-name)
    (ecase condition
      (:VALUE t)
      (:UNBOUND nil)
      (:MISSING (slot-missing (si:instance-class class) class slot-name
			      'SLOT-BOUNDP))
      )))

(defmethod slot-boundp ((class t) slot-name)
  (multiple-value-bind (val condition)
    (general-instance-get class slot-name)
    (ecase condition
      (:VALUE t)
      (:UNBOUND nil)
      (:MISSING (slot-missing (si:instance-class class) class slot-name
			      'SLOT-BOUNDP))
      )))

(defmethod select-clos-N ((instance standard-object))
  (let* ((class (si:instance-class instance))
	 (local-slotds (slot-value class 'CLOS::SLOTS))
	 (class-slotds (slot-value class 'CLOS::CLASS-CLASS-SLOTS)))
        (if local-slotds
	    (progn
	      (si::inspect-indent)
	      (format t "The local slots are:~%")
	      (incf si::*inspect-level*)
	      (dolist (slotd local-slotds)
		(si::inspect-indent-1)
		(format t "name : ~S" (clos::slotd-name slotd))
		(if (slot-boundp instance (clos::slotd-name slotd))
		    (si::inspect-recursively "value:"
		       (slot-value instance (clos::slotd-name slotd))
		       (slot-value instance (clos::slotd-name slotd)))
		    (si::inspect-print "value: Unbound"
		       nil
		       (slot-value instance (clos::slotd-name slotd)))))
	      (decf si::*inspect-level*))
	    (progn
	      (si::inspect-indent)
	      (format t "It has no local slots.~%")))
	(if class-slotds
	    (progn
	      (si::inspect-indent)
	      (format t "The class slots are:~%")
	      (incf si::*inspect-level*)
	      (dolist (slotd class-slotds)
		(si::inspect-indent-1)
		(format t "name : ~S" (clos::slotd-name slotd))
		(if (slot-boundp instance (clos::slotd-name slotd))
		    (si::inspect-recursively "value:"
		       (slot-value instance (clos::slotd-name slotd))
		       (slot-value instance (clos::slotd-name slotd)))
		    (si::inspect-print "value: Unbound"
		       nil
		       (slot-value instance (clos::slotd-name slotd)))))
	      (decf si::*inspect-level*))
	    (progn
	      (si::inspect-indent)
	      (format t "It has no class slots.~%")))))

(defmethod select-clos-N ((instance standard-class))
  (let* ((class (si:instance-class instance))
	 (local-slotds (slot-value class 'CLOS::SLOTS)))
        (if local-slotds
	    (progn
	      (si::inspect-indent)
	      (format t "The (local) slots are:~%")
	      (incf si::*inspect-level*)
	      (dolist (slotd local-slotds)
		(si::inspect-indent-1)
		(format t "name : ~S" (clos::slotd-name slotd))
		(if (slot-boundp instance (clos::slotd-name slotd))
		    (si::inspect-recursively "value:"
		       (slot-value instance (clos::slotd-name slotd))
;		       (slot-value instance (clos::slotd-name slotd))
		       )
		    (si::inspect-print "value: Unbound"
		       nil
;		       (slot-value instance (clos::slotd-name slotd))
		       )))
	      (decf si::*inspect-level*))
	    (progn
	      (si::inspect-indent)
	      (format t "It has no (local) slots.~%")))))

(defmethod select-clos-N ((instance t))
  (let* ((class (si:instance-class instance))
	 (local-slotds (slot-value class 'CLOS::SLOTS)))
        (if local-slotds
	    (progn
	      (si::inspect-indent)
	      (format t "The (local) slots are:~%")
	      (incf si::*inspect-level*)
	      (dolist (slotd local-slotds)
		(si::inspect-indent-1)
		(format t "name : ~S" (clos::slotd-name slotd))
		(if (slot-boundp instance (clos::slotd-name slotd))
		    (si::inspect-recursively "value:"
		       (slot-value instance (clos::slotd-name slotd))
;		       (slot-value instance (clos::slotd-name slotd))
		       )
		    (si::inspect-print "value: Unbound"
		       nil
;		       (slot-value instance (clos::slotd-name slotd))
		       )))
	      (decf si::*inspect-level*))
	    (progn
	      (si::inspect-indent)
	      (format t "It has no (local) slots.~%")))))

(defmethod select-clos-L ((instance standard-object))
  (let* ((class (si:instance-class instance))
	 (local-slotds (slot-value class 'CLOS::SLOTS))
	 (class-slotds (slot-value class 'CLOS::CLASS-CLASS-SLOTS)))
        (terpri)
	(if local-slotds
	    (progn
	      (format t "The names of the local slots are:~%")
	      (dolist (slotd local-slotds)
		(format t "  ~S~%" (clos::slotd-name slotd))))
	    (progn
	      (format t "It has no local slots.~%")))
	(terpri)
	(if class-slotds
	    (progn
	      (format t "The names of the class slots are:~%")
	      (dolist (slotd class-slotds)
		(format t "  ~S~%" (clos::slotd-name slotd))))
	    (progn
	      (format t "It has no class slots.~%")))
	(terpri)))

(defmethod select-clos-L ((instance standard-class))
  (let* ((class (si:instance-class instance))
	 (local-slotds (slot-value class 'CLOS::SLOTS)))
        (terpri)
	(if local-slotds
	    (progn
	      (format t "The names of the (local) slots are:~%")
	      (dolist (slotd local-slotds)
		      (format t "  ~S~%" (clos::slotd-name slotd))))
	    (progn
	      (format t "It has no (local) slots.~%")))
	(terpri)))

(defmethod select-clos-L ((instance t))
  (let* ((class (si:instance-class instance))
	 (local-slotds (slot-value class 'CLOS::SLOTS)))
        (terpri)
	(if local-slotds
	    (progn
	      (format t "The names of the (local) slots are:~%")
	      (dolist (slotd local-slotds)
		      (format t "  ~S~%" (clos::slotd-name slotd))))
	    (progn
	      (format t "It has no (local) slots.~%")))
	(terpri)))

(defmethod select-clos-J ((instance standard-object))
  (let* ((class (si:instance-class instance))
	 (local-slotds (slot-value class 'CLOS::SLOTS))
	 (class-slotds (slot-value class 'CLOS::CLASS-CLASS-SLOTS))
	 (slotd (car (member (prog1
			       (read-preserving-whitespace *query-io*)
			       (si::inspect-read-line))
			     (append local-slotds class-slotds)
			     :key #'clos::slotd-name
			     :test #'eq))))
        (if slotd
	    (progn
	      (incf si::*inspect-level*)
	      (si::inspect-indent-1)
	      (format t "name : ~S" (clos::slotd-name slotd))
	      (if (slot-boundp instance (clos::slotd-name slotd))
		  (si::inspect-recursively "value:"
		     (slot-value instance (clos::slotd-name slotd))
		     (slot-value instance (clos::slotd-name slotd)))
		  (si::inspect-print "value: Unbound"
		     nil
		     (slot-value instance (clos::slotd-name slotd))))
	      (decf si::*inspect-level*))
	    (progn
	      (terpri)
	      (format t "~S is not a slot of the instance." (slotd-name slotd))
	      (terpri)
	      (terpri)))))

(defmethod select-clos-J ((instance standard-class))
  (let* ((class (si:instance-class instance))
	 (local-slotds (slot-value class 'CLOS::SLOTS))
	 (slotd (car (member (prog1
			       (read-preserving-whitespace *query-io*)
			       (si::inspect-read-line))
			     local-slotds
			     :key #'clos::slotd-name
			     :test #'eq))))
        (if slotd
	    (progn
	      (incf si::*inspect-level*)
	      (si::inspect-indent-1)
	      (format t "name : ~S" (clos::slotd-name slotd))
	      (if (slot-boundp instance (clos::slotd-name slotd))
		  (si::inspect-recursively "value:"
		     (slot-value instance (clos::slotd-name slotd))
;		     (slot-value instance (clos::slotd-name slotd))
		     )
		  (si::inspect-print "value: Unbound"
		     nil
;		     (slot-value instance (clos::slotd-name slotd))
		     ))
	      (decf si::*inspect-level*))
	    (progn
	      (terpri)
	      (format t "~S is not a slot of the instance." (slotd-name slotd))
	      (terpri)
	      (terpri)))))

(defmethod select-clos-J ((instance t))
  (let* ((class (si:instance-class instance))
	 (local-slotds (slot-value class 'CLOS::SLOTS))
	 (slotd (car (member (prog1
			       (read-preserving-whitespace *query-io*)
			       (si::inspect-read-line))
			     local-slotds
			     :key #'clos::slotd-name
			     :test #'eq))))
        (if slotd
	    (progn
	      (incf si::*inspect-level*)
	      (si::inspect-indent-1)
	      (format t "name : ~S" (clos::slotd-name slotd))
	      (if (slot-boundp instance (clos::slotd-name slotd))
		  (si::inspect-recursively "value:"
		     (slot-value instance (clos::slotd-name slotd))
;		     (slot-value instance (clos::slotd-name slotd))
		     )
		  (si::inspect-print "value: Unbound"
		     nil
;		     (slot-value instance (clos::slotd-name slotd))
		     ))
	      (decf si::*inspect-level*))
	    (progn
	      (terpri)
	      (format t "~S is not a slot of the instance." (slotd-name slotd))
	      (terpri)
	      (terpri)))))

(defun select-clos-? ()
  (terpri)
  (format t
	  "Inspect commands for clos instances:~%~
n (or N or Newline):  inspects all slots of the class (recursively).~%~
s (or S):             skips the field.~%~
p (or P):             pretty-prints the field.~%~
a (or A):             aborts the inspection of the rest of the fields.~%~
e (or E) form:        evaluates and prints the form.~%~
l (or L):             show the names of all slots.~%~
j (or J) slot-name:   inspect the slot with the name requested.~%~
q (or Q):             quits the inspection.~%~
?:                    prints this.~%~%"
	  ))

(defmethod inspect-obj ((instance standard-object))
  (unless (eq (si:instance-class (si:instance-class instance))
              (find-class 'STANDARD-CLASS))
          (terpri)
          (format t "No applicable method CLOS::INSPECT-OBJ for an instance~%")
          (format t "of class ~S" (si:instance-class instance))
          (throw 'SI::ABORT-INSPECT nil))
  (decf si::*inspect-level*)
  (let* ((class (si:instance-class instance))
	 (local-slotds (slot-value class 'CLOS::SLOTS))
	 (class-slotds (slot-value class 'CLOS::CLASS-CLASS-SLOTS)))
    (loop
      (format t "~S - clos object:" instance)
      (incf si::*inspect-level*)
      (si::inspect-indent)
      (format t "- it is an instance of class named ~S,"
	      (class-name class))
      (si::inspect-indent)
      (format t "- it has ~A local slots and ~A class slots: "
	      (length local-slotds) (length class-slotds))
      (force-output)
      (case (do ((char (read-char *query-io*) (read-char *query-io*)))
		((and (char/= char #\Space) (char/= #\Tab)) char))
	    ((#\Newline #\Return)
	     (select-clos-N instance)
	     (return nil))
	    ((#\n #\N)
	     (si::inspect-read-line)
	     (select-clos-N instance)
	     (return nil))
	    ((#\s #\S)
	     (si::inspect-read-line)
	     (return nil))
	    ((#\p #\P)
	     (si::inspect-read-line)
	     (si::select-P instance))
	    ((#\a #\A)
	     (si::inspect-read-line)
	     (throw 'SI::ABORT-INSPECT nil))
	    ((#\e #\E)
	     (si::select-E))
	    ((#\q #\Q)
	     (si::inspect-read-line)
	     (throw 'SI::QUIT-INSPECT nil))
	    ((#\l #\L)
	     (si::inspect-read-line)
	     (select-clos-L instance))
	    ((#\j #\J)
	     (select-clos-J instance))
	    ((#\?)
	     (si::inspect-read-line)
	     (select-clos-?))
	    (t
	     (si::inspect-read-line)))
      (decf si::*inspect-level*)
      (si::inspect-indent)))
  (incf si::*inspect-level*))

(defmethod inspect-obj ((instance standard-class))
  (decf si::*inspect-level*)
  (let* ((class (si:instance-class instance))
	 (local-slotds (slot-value class 'CLOS::SLOTS)))
    (loop
      (format t "~S - clos object:" instance)
      (incf si::*inspect-level*)
      (si::inspect-indent)
      (format t "- it is an instance of class named ~S,"
	      (class-name class))
      (si::inspect-indent)
      (format t "- it has ~A local slots: " (length local-slotds))
      (force-output)
      (case (do ((char (read-char *query-io*) (read-char *query-io*)))
		((and (char/= char #\Space) (char/= #\Tab)) char))
	    ((#\Newline #\Return)
	     (select-clos-N instance)
	     (return nil))
	    ((#\n #\N)
	     (si::inspect-read-line)
	     (select-clos-N instance)
	     (return nil))
	    ((#\s #\S)
	     (si::inspect-read-line)
	     (return nil))
	    ((#\p #\P)
	     (si::inspect-read-line)
	     (si::select-P instance))
	    ((#\a #\A)
	     (si::inspect-read-line)
	     (throw 'SI::ABORT-INSPECT nil))
	    ((#\e #\E)
	     (si::select-E))
	    ((#\q #\Q)
	     (si::inspect-read-line)
	     (throw 'SI::QUIT-INSPECT nil))
	    ((#\l #\L)
	     (si::inspect-read-line)
	     (select-clos-L instance))
	    ((#\j #\J)
	     (select-clos-J instance))
	    ((#\?)
	     (si::inspect-read-line)
	     (select-clos-?))
	    (t
	     (si::inspect-read-line)))
      (decf si::*inspect-level*)
      (si::inspect-indent)))
  (incf si::*inspect-level*))

(defmethod inspect-obj ((instance t))
  (decf si::*inspect-level*)
  (let* ((class (si:instance-class instance))
	 (local-slotds (slot-value class 'CLOS::SLOTS)))
    (loop
      (format t "~S - clos object:" instance)
      (incf si::*inspect-level*)
      (si::inspect-indent)
      (format t "- it is an instance of class named ~S,"
	      (class-name class))
      (si::inspect-indent)
      (format t "- it has ~A local slots: " (length local-slotds))
      (force-output)
      (case (do ((char (read-char *query-io*) (read-char *query-io*)))
		((and (char/= char #\Space) (char/= #\Tab)) char))
	    ((#\Newline #\Return)
	     (select-clos-N instance)
	     (return nil))
	    ((#\n #\N)
	     (si::inspect-read-line)
	     (select-clos-N instance)
	     (return nil))
	    ((#\s #\S)
	     (si::inspect-read-line)
	     (return nil))
	    ((#\p #\P)
	     (si::inspect-read-line)
	     (si::select-P instance))
	    ((#\a #\A)
	     (si::inspect-read-line)
	     (throw 'SI::ABORT-INSPECT nil))
	    ((#\e #\E)
	     (si::select-E))
	    ((#\q #\Q)
	     (si::inspect-read-line)
	     (throw 'SI::QUIT-INSPECT nil))
	    ((#\l #\L)
	     (si::inspect-read-line)
	     (select-clos-L instance))
	    ((#\j #\J)
	     (select-clos-J instance))
	    ((#\?)
	     (si::inspect-read-line)
	     (select-clos-?))
	    (t
	     (si::inspect-read-line)))
      (decf si::*inspect-level*)
      (si::inspect-indent)))
  (incf si::*inspect-level*))

;;; -------------------------------------------------------------------------
