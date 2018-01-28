;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;  thread top level and utilities

(in-package 'system)

(export '(spawn
	  pass
	  without-scheduling
	  wait-in
	  wait
	  wait-or))

(eval-when (compile) (proclaim '(optimize (safety 2) (space 3))))

(defun thread-top-level (fun &rest args)
  (let ((*quit-tag* (cons nil nil))
	*quit-tags*
	(*break-level* 0)
	(*tpl-level* 0)
	(*ihs-base* 1)
	(*ihs-top* 1)
	(*break-enable* t))
    (catch 'thread-top			; catch (bye)
      (catch *quit-tag*			; catch quit from errors
	(apply fun args))
      )
;    (%enable-scheduler)		; moved to lwp.c Beppe
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
  (labels ((wait-and-internal (threads)
	     (cond
	       ((null threads) t)
	       (t   (and
		     (eql (thread-status (first threads)) 'DEAD)
		     (wait-and-internal (rest threads)))))))
    (funcall #'%thread-wait #'wait-and-internal threads)))

(defun wait-or (&rest threads)
  (labels ((wait-or-internal (threads)
	     (cond
	       ((null threads) nil)
	       (t   (or
		     (eql (thread-status (first threads)) 'DEAD)
		     (wait-or-internal (rest threads)))))))
    (funcall #'%thread-wait #'wait-or-internal threads)))

