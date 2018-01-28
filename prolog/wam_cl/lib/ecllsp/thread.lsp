;;;;  thread.lsp -- thread top level and utilities
;;;;
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "CL")
(export '(MAKE-THREAD
	  DEACTIVATE
	  REACTIVATE
	  KILL-THREAD
	  CURRENT-THREAD
	  THREAD-STATUS
	  THREAD-LIST
	  MAKE-CONTINUATION
	  THREAD-OF
	  CONTINUATION-OF
	  RESUME
	  %disable-scheduler
	  %enable-scheduler))

(in-package "SYSTEM")

(export '(let/cc
	  pass
	  spawn
	  without-scheduling
	  wait-in
	  wait
	  wait-or
	  *scheduler-disabled-in-error*
	  *break-level*))

;;; ----------------------------------------------------------------------
;;; Top level

(defvar *scheduler-disabled-in-error* t)

(defun universal-error-handler
  (error-name correctable function-name
	      continue-format-string error-format-string
	      args)
  (declare (ignore error-name))
  (unwind-protect
      (if (or (plusp *break-level*) (not *scheduler-disabled-in-error*))
	  (let* ((*print-pretty* nil)
		 (*print-level* 4)
		 (*print-length* 4)
		 (*print-case* :upcase))
	    (format *error-output* "~&~:[E~;Correctable e~]rror: " correctable)
	    (let ((*indent-formatted-output* t))
	      (format *error-output* "~?~%Signalled by ~A." error-format-string args
		      (or function-name "an anonymous function")))
	    (when correctable
		  (format *error-output* "~&~:[Continuing~;If continued~]: "
			  *break-enable*)
		  (let ((*indent-formatted-output* t))
		    (format *error-output* "~?" continue-format-string args)))
	    (terpri *error-output*)
	    (break-level correctable
			 (format nil "~?" error-format-string args)))
;;;	(let ((break-exit nil))
	  (let* ((*print-pretty* nil)
		 (*print-level* 4)
		 (*print-length* 4)
		 (*print-case* :upcase))
	    (thread-break-in)
	    (format *error-output* "~&~:[E~;Correctable e~]rror: " correctable)
	    (let ((*indent-formatted-output* t))
	      (format *error-output* "~?~%Signalled by ~A." error-format-string args
		      (or function-name "an anonymous function")))
	    (when correctable
		  (format *error-output* "~&~:[Continuing~;If continued~]: "
			  *break-enable*)
		  (let ((*indent-formatted-output* t))
		    (format *error-output* "~?" continue-format-string args)))
	    (terpri *error-output*)
;;;	    (setq break-exit
	    (break-level correctable
			 (format nil "~?" error-format-string args))))
;;;))
    (thread-break-resume)))

(defun thread-top-level (fun &rest args)
  (let*((*quit-tag* (cons nil nil))
	*quit-tags*
	(*break-level* 0)
	(*tpl-level* 0)
	(*ihs-base* 1)
	(*ihs-top* 1)
	(*break-enable* t))
    (catch 'THREAD-TOP			; catch (quit)
      (catch *quit-tag*			; catch quit from errors
	(apply fun args)))
    nil					; dont remove
    )
  )


;;; ----------------------------------------------------------------------
;;; Utilities

(defmacro spawn (function &rest args)
  `(resume (make-continuation (make-thread ,function)) ,@ args))

(defun pass (&rest args)
  (%disable-scheduler)
  (apply 'resume args)
  (%suspend))

(defmacro let/cc (cont body)
  `(let ((,cont (make-continuation (current-thread))))
    ,@body
    (%suspend)))

(defmacro without-scheduling (&rest body)
  `(unwind-protect
       (progn 
	 (%disable-scheduler)
	 (progn ,@body))
     (%enable-scheduler)))

(defmacro wait-in (place)
  `(progn
     (setf ,place (make-continuation (current-thread)))
     (%suspend)))

(defun wait (&rest threads)
  (labels ((wait-all-internal (threads)
	     (or (null threads)
		 (and (eq (thread-status (first threads)) 'DEAD)
		      (wait-all-internal (rest threads))))))
    (funcall #'%thread-wait #'wait-all-internal threads)))

(defun wait-some (&rest threads)
  (labels ((wait-some-internal (threads)
	     (or (null threads)
		 (eq (thread-status (first threads)) 'DEAD)
		 (wait-some-internal (rest threads)))))
    (funcall #'%thread-wait #'wait-some-internal threads)))

;;; ----------------------------------------------------------------------
;;; Examples
#|
(defvar *producer* (make-thread 'producer))
(defvar *consumer* (make-thread 'consumer))

(defun producer ()
  (dotimes (i 20)
    (print 'producer)
    ;; produce
    (resume (make-continuation *consumer*) i)
    (%suspend)))

(defun consumer ()
  (let (i)
    (loop
     (print 'consumer)
     (resume (make-continuation *producer*))
     (setq i (%suspend))
     ;; consume
     (print i))))

(resume (make-continuation *producer*))
|#
