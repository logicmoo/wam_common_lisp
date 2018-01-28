;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;        Tracer package for Common Lisp

(in-package "SYSTEM")

(defvar *trace-level* 0)
(defvar *trace-list* nil)
(defconstant +tracing-block+ (gensym))

(defmacro trace (&rest r)
"Syntax: (trace ({function-name | ({function-name}+)} {keyword [form]\}*)
Begins tracing the specified functions.  With no FUNCTION-NAMEs, returns a
list of functions currently being traced. The printed information consists of
the name of function followed at entry by its arguments and on exit by its
return values.
The keywords allow to control when and how tracing is performed.
The possible keywords are:

 :BREAK		a breakpoint is entered after printing the entry trace
		information, but before applying the traced function to its
		arguments, if form evaluates to non-nil
 :BREAK-AFTER 	like :BREAK but the breakpoint is entered after the function
		has been executed and the exit trace information has been
		printed and before control returns
 :COND-BEFORE	information is printed upon entry if form evaluates to non-nil
 :COND-AFTER	information is printed upon exit if form evaluates to non-nil
 :COND		specifies a single condition for both entry and exit
 :PRINT		prints the values of the forms in the list upon entry.
		They are preceeded by a backslash (\\)
 :PRINT-AFTER	prints the values of the forms in the list upon exit from the
		function. They are preceeded by a backslash (\\)
 :STEP		turns on the stepping facility

Forms can refer to the list of arguments of the function through the variable
SI::ARGS."
  `(trace* ',r))

(defun trace* (r)
  (if (null r)
    *trace-list*
    (mapc #'trace-one r)))

(defmacro untrace (&rest r)
  `(untrace* ',r))

(defun untrace* (r)
  (mapc #'untrace-one (if (null r) *trace-list* r)))

(defvar *inside-trace* nil)

(defun trace-one (spec)
  (let* (break exitbreak (entrycond t) (exitcond t) entry exit
	       step (barfp t) fname oldf)
    (cond ((atom spec)
	   (setq fname spec))
	  ((eq 'SETF (car spec))
	   (setq fname (get-sysprop (cadr spec) 'SETF-SYMBOL)))
	  ((atom (car spec))
	   (setq fname (car spec))
	   (do ((specs (cdr spec) (cdr specs)))
	       ((null specs))
	     (case (car specs)
	       (:break (setq barfp specs specs (cdr specs) break (car specs)))
	       (:break-after (setq barfp specs specs (cdr specs) exitbreak (car specs)))
	       (:step (setq step t))
	       (:cond (setq barfp specs specs (cdr specs))
		      (setq exitcond (setq entrycond (car specs))))
	       (:cond-before (setq barfp specs specs (cdr specs) entrycond (car specs)))
	       (:cond-after (setq barfp specs specs (cdr specs) exitcond (car specs)))
	       (:print (setq barfp specs specs (cdr specs) entry (car specs)))
	       (:print-after (setq barfp specs specs (cdr specs) exit (car specs)))
	       (t (error "Meaningless TRACE keyword: ~S" (car specs))))
	     (unless barfp (error "Parameter missing"))))
	  ((eq 'SETF (caar spec))
	   (return-from trace-one
	     (trace-one `(,(get-sysprop (cadar spec) 'SETF-SYMBOL) . ,(cdr spec)))))
	  (t
	   (let (results)
	       (dolist (fname (car spec))
		 (push (trace-one `(,fname . ,(cdr spec))) results))
	       (return-from trace-one (nreverse results)))))
    (when (null (fboundp fname))
      (format *trace-output* "The function ~S is not defined.~%" fname)
      (return-from trace-one nil))
    (when (special-operator-p fname)
      (format *trace-output* "~S is a special form.~%" fname)
      (return-from trace-one nil))
    (when (macro-function fname)
      (format *trace-output* "~S is a macro.~%" fname)
      (return-from trace-one nil))
    (when (get-sysprop fname 'TRACED)
      (cond ((tracing-body fname)
	     (format *trace-output*
		     "The function ~S is already traced.~%" fname)
	     (return-from trace-one nil))
	    (t (untrace-one fname))))
    (sys:fset (setq oldf (gensym)) (symbol-function fname))
    (put-sysprop fname 'TRACED oldf)
    (eval
     `(defun ,fname (&rest args)
	(block ,+tracing-block+		; used to recognize traced functions
	  (let* (values (*trace-level* (1+ *trace-level*)))
	    (if *inside-trace*
		(setq values (multiple-value-list (apply ',oldf args)))
		(let ((*inside-trace* t))
		  ,@(when entrycond
		       (if (eq t entrycond)
			 `((trace-print 'ENTER ',fname args ,@entry))
			 `((when ,entrycond
			     (trace-print 'ENTER ',fname args ,@entry)))))
		  ,@(when break
		      `((when ,break (let (*inside-trace*)
				       (break "tracing ~S" ',fname)))))
		  (setq values
			(let (*inside-trace*)
			  (multiple-value-list
			      (apply ',oldf args)
			      #+nil
			      ,(if step
				   `(let (*step-quit*)
				     (applyhook ',oldf args #'stepper nil))
				   `(apply ',oldf args)))))
		  ,@(when exitcond
		      (if (eq t exitcond)
			  `((trace-print 'EXIT ',fname values ,@exit))
			  `((when ,exitcond
			      (trace-print 'EXIT ',fname values ,@exit)))))
		  ,@(when exitbreak
		      `((when ,exitbreak
			  (let (*inside-trace*)
			    (break "after tracing ~S" ',fname)))))))
	    (values-list values)))))
  (push fname *trace-list*)
  (cons fname nil)))

(defun trace-print (direction fname vals &rest extras)
  (let ((indent (min (* (1- *trace-level*) 2) 20)))
    (fresh-line *trace-output*)
    (case direction
      (ENTER
       (multiple-value-bind (bars rem)
	   (floor indent 4)
	 (dotimes (i bars) (princ "|   " *trace-output*))
	 (when (plusp rem) (format *trace-output* "~V,,,' A" rem "|")))
       (format *trace-output*
	       "~D> (~S~{ ~S~})~%"
	       *trace-level* fname vals))
      (EXIT
       (multiple-value-bind (bars rem)
	   (floor indent 4)
	 (dotimes (i bars) (princ "|   " *trace-output*))
	 (when (plusp rem) (format *trace-output* "~V,,,' A" rem "|")))
       (format *trace-output*
	       "<~D (~S~{ ~S~})~%"
	       *trace-level*
	       fname vals)
       ))
    (when extras
      (multiple-value-bind (bars rem)
	  (floor indent 4)
	(dotimes (i bars) (princ "|   " *trace-output*))
	(when (plusp rem) (format *trace-output* "~V,,,' A" rem "|")))
      (format *trace-output*
	      "~0,4@T\\\\ ~{ ~S~}~%"
	      extras))))

(defun untrace-one (fname)
  (cond ((get-sysprop fname 'TRACED)
         (if (tracing-body fname)
	   (sys:fset fname (symbol-function (get-sysprop fname 'TRACED)))
	   (format *trace-output*
		   "The function ~S was traced, but redefined.~%"
		   fname))
         (rem-sysprop fname 'TRACED)
         (setq *trace-list* (delete fname *trace-list* :test #'eq))
         (list fname))
        (t
         (format *trace-output* "The function ~S is not traced.~%" fname)
         nil)))

(defun tracing-body (fname &aux (fun (symbol-function fname)))
  (when (functionp fun)
    (let ((bytecodes (si::bc-split fun)))
      (when bytecodes
	(dotimes (i (length bytecodes))
	  (when (eq (aref bytecodes i) +tracing-block+)
	    (return-from tracing-body t))))))
  nil)

#+nil
(progn
(defvar *step-level* 0)
(defvar *step-quit* nil)
(defvar *step-function* nil)		; skip stepping until this function
(defvar *step-form*)
(defvar *step-env*)
(defvar *step-tag* (cons nil nil))

(defconstant step-commands
  `("Stepper commands"
     ((:newline) (step-next) :constant
      "newline		Advance to the next form"
      "newline						[Stepper command]~@
	~@
	Step to next form.~%")
     ((:s :skip) step-skip nil
      ":s(kip)		Skip current form or until function"
      ":skip &optional arg				[Stepper command]~@
	:s &optional arg				[Abbreviation]~@
	~@
	Continue evaluation without stepping.  Without argument, resume
	stepping after the current form.  With numeric argument (n),
	resume stepping at the n-th level above.  With function name, resume
	when given function is called.~%")
     ((:b :back) (tpl-pop-command) :constant
      ":b(ack)		Step backward"
      ":back						[Stepper command]~@
	:b						[Abbreviation]~@
	~@
	Go back one step.~%")
     ((:pr :print) (step-print) :constant
      ":pr(int)	Pretty print current form"
      ":print						[Stepper command]~@
	:p						[Abbreviation]~@
	~@
	Pretty print current form.~%")
     ((:form) *step-form* :constant
      ":form		Current form"
      ":form						[Stepper command]~@
	~@
	Return the current form.  Nothing is done, but the current form~@
	is returned as the value of this command.  As a consequence,~@
	it is printed by the top level in the usual way and saved in~@
	the variable *.  The main purpose of this command is to allow~@
	the current form to be examined further by accessing *.~%")
     ((:ret :return) step-return :eval
      ":ret(urn)	Return without evaluating current form"
      ":return &eval &rest values			[Stepper command]~@
	:ret &eval &rest values				[Abbreviation]~@
	~@
	Return from current form without evaluating it.~%")
     ((:x :exit) (step-quit) :constant
      ":x or :exit	Finish evaluation and exit stepper"
      ":exit						[Stepper command]~@
       :x						[Abbreviation]~@
       ~@
       Finish evaluation without stepping.~%")
     ))

(defmacro step (form)
"Syntax: (step form)
Evaluates FORM in the Stepper mode and returns all its values.  See ECL Report
for Stepper mode commands."
  `(step* ',form))

(defun step* (form)
  (let* ((*step-quit* nil)
	 (*step-function* nil)
	 (*step-level* 0))
    (stepper form nil)))

(defun stepper (form &optional env)
  (when (eq *step-quit* t)
    (return-from stepper (evalhook form nil nil env)))
  ;; skip the encapsulation of traced functions:
  (when (and (consp form)
	     (symbolp (car form))
	     (get-sysprop (car form) 'TRACED)
	     (tracing-body (car form)))
    (do ((args (cdr form) (cdr args))
	 (values))
	((null args)
	 (return-from stepper
	   (applyhook (car form) (nreverse values) #'stepper nil env)))
      (push (evalhook (car args) #'stepper nil env) values)))
  (when (numberp *step-quit*)
    (if (>= *step-level* *step-quit*)
      (return-from stepper (evalhook form nil nil env))
      (setq *step-quit* nil)))
  (when *step-function*
    (if (and (consp form) (eq (car form) *step-function*))
      (let ((*step-function* nil))
	(return-from stepper (stepper form env)))
      (return-from stepper (evalhook form #'stepper nil env))))
  (let* ((*step-level* (1+ *step-level*))
	 (*step-form* form)
	 (*step-env* env)
	 values indent prompt)
    (setq indent (min (* *tpl-level* 2) 20))
    (setq prompt
	  #'(lambda ()
	      (format *debug-io* "~VT" indent)
	      (write form :stream *debug-io* :pretty nil
		     :level 2 :length 2)
	      (princ #\space *debug-io*)
	      (princ #\- *debug-io*)))
    (if (constantp form)
      (progn
	(format *debug-io* "~VT" indent)
	(write form :stream *debug-io* :pretty nil
	       :level 2 :length 2)
	(princ #\space *debug-io*)
	(princ #\= *debug-io*)
	(setq values (multiple-value-list (evalhook form nil nil env)))
	(dolist (v values)
	  (princ #\space *debug-io*)
	  (write v :stream *debug-io* :pretty nil :level 2 :length 2))
	(terpri *debug-io*))
      (progn
	(setq values
	      (catch *step-tag*
		(tpl :quiet t
		     :commands (adjoin step-commands
				       (adjoin break-commands *tpl-commands*))
		     :prompt-hook prompt)))
	(if (endp values)
	  (format *debug-io* "~V@T=~%" indent)
	  (do ((l values (cdr l))
	       (b t nil))
	      ((endp l))
	    (format *debug-io* "~V@T~C " indent (if b #\= #\&) (car l))
	    (write (car l) :stream *debug-io* :pretty nil
		   :level 2 :length 2)
	    (terpri *debug-io*)))))
    (values-list values)))

(defun step-next ()
  (throw *step-tag*
	 (multiple-value-list
	  (locally (declare (notinline evalhook))
		   (evalhook *step-form* #'stepper nil *step-env*)))))

(defun step-skip (&optional (when 0))
  (throw *step-tag*
	 (multiple-value-list
	  (locally (declare (notinline evalhook))
	   (cond ((symbolp when)
		  (let ((*step-function* when))
		    (evalhook *step-form* #'stepper nil *step-env*)))
		 ((integerp when)
		  (setq *step-quit* (- *step-level* when))
		  (evalhook *step-form* nil nil *step-env*))
		 (t
		  (error "Skip: argument must be integer or symbol.")))))))

(defun step-print ()
  (write *step-form* :stream *debug-io* :pretty t :level nil :length nil)
  (terpri)
  (values))

(defun step-quit ()
  (setq *step-quit* t)
  (throw *step-tag*
	 (multiple-value-list
	  (locally (declare (notinline evalhook))
		   (evalhook *step-form* nil nil *step-env*)))))

(defun step-return (&rest values)
  (throw *step-tag* values))
)
;(provide 'TRACE)
