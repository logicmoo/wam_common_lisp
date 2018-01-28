;;; -*- Mode:Lisp; Package:USER; Base:10; Syntax:Common-lisp -*-

(in-package 'user)

(setq c::optimize-speed 3)
(setq c::optimize-safety 0)
(setq c::optimize-space 0)

(remprop 'macroexpand 'c::fdesc)
(remprop 'macroexpand-1 'c::fdesc)


;;; this is here to fix the printer so it will find the print
;;; functions on structures that have 'em.

(in-package 'lisp)

(defun %write-structure (struct output-stream print-vars level)
  (let* ((name (svref struct 0))
	 (pfun (or (let ((temp (get name 'structure-descriptor)))
	 	     (and temp (dd-print-function temp)))
	 	   (get name :print-function))))
    (declare (symbol name))
    (cond
      (pfun
	(funcall pfun struct output-stream level))
      ((and (pv-level print-vars) (>= level (pv-level print-vars)))
       (write-char #\# output-stream))
      ((and (pv-circle print-vars)
            (%write-circle struct output-stream (pv-circle print-vars))))
      (t
       (let ((pv-length (pv-length print-vars))
	     (pv-pretty (pv-pretty print-vars)))
	 (when pv-pretty
	   (pp-push-level pv-pretty))
	 (incf level)
	 (write-string "#s(" output-stream)
	 (cond
	  ((and pv-length (>= 0 pv-length))
	   (write-string "..."))
	  (t
	   (%write-symbol name output-stream print-vars)
	   (do ((i 0 (1+ i))
		(n 0)
		(slots (dd-slots (get name 'structure-descriptor))
		       (rest slots)))
	       ((endp slots))
	     (declare (fixnum i n) (list slots))
	     (when pv-pretty
	       (pp-insert-break pv-pretty *structure-keyword-slot-spec* t))
	     (write-char #\space output-stream)
	     (when (and pv-length (>= (incf n) pv-length))
	       (write-string "..." output-stream)
	       (return))
	     (write-char #\: output-stream)
	     (%write-symbol-name
	      (symbol-name (dsd-name (first slots))) output-stream print-vars)
	     (when pv-pretty
	       (pp-insert-break pv-pretty *structure-data-slot-spec* nil))
	     (write-char #\space output-stream)
	     (when (and pv-length (>= (incf n) pv-length))
	       (write-string "..." output-stream)
	       (return))
	     (%write-object
	      (svref struct (dsd-index (first slots)))
	      output-stream print-vars level))))
	 (write-char #\) output-stream)
	 (when pv-pretty
	   (pp-pop-level pv-pretty)))))))

(eval-when (eval) (compile '%write-structure))

;;;
;;; Apparently, whoever implemented the TIME macro didn't consider that
;;; someone might want to use it in a non-null lexical environment.  Of
;;; course this fix is a loser since it binds a whole mess of variables
;;; around the evaluation of form, but it will do for now.
;;;
(in-package 'lisp)

(DEFmacro TIME (FORM)
  `(LET (IGNORE START FINISH S-HSEC F-HSEC S-SEC F-SEC S-MIN F-MIN VALS)
     (FORMAT *trace-output* "~&Evaluating: ~A" ,form)
     ;; read the start time.
     (MULTIPLE-VALUE-SETQ (IGNORE IGNORE IGNORE S-MIN START)
       (SYS::%SYSINT #X21 #X2C00 0 0 0))
     ;; Eval the form.
     (SETQ VALS (MULTIPLE-VALUE-LIST (progn ,form)))
     ;; Read the end time.
     (MULTIPLE-VALUE-SETQ (IGNORE IGNORE IGNORE F-MIN FINISH)
       (SYS::%SYSINT #X21 #X2C00 0 0 0))
     ;; Unpack start and end times.
     (SETQ S-HSEC (LOGAND START #X0FF)
	   F-HSEC (LOGAND FINISH #X0FF)
	   S-SEC (LSH START -8)
           F-SEC (LSH FINISH -8)
	   S-MIN (LOGAND #X0FF S-MIN)
	   F-MIN (LOGAND #X0FF F-MIN))
     (SETQ F-HSEC (- F-HSEC S-HSEC))			; calc hundreths
     (IF (MINUSP F-HSEC)
         (SETQ F-HSEC (+ F-HSEC 100)
	       F-SEC (1- F-SEC)))
     (SETQ F-SEC (- F-SEC S-SEC))			; calc seconds
     (IF (MINUSP F-SEC)
         (SETQ F-SEC (+ F-SEC 60)
	       F-MIN (1- F-MIN)))
     (SETQ F-MIN (- F-MIN S-MIN))			; calc minutes
     (IF (MINUSP F-MIN) (INCF F-MIN 60))
     (FORMAT *trace-output* "~&Elapsed time: ~D:~:[~D~;0~D~].~:[~D~;0~D~]~%"
       F-MIN (< F-SEC 10.) F-SEC (< F-HSEC 10) F-HSEC)
     (VALUES-LIST VALS)))

;;;
;;; Patch to PROGV
;;; 
(in-package sys::*compiler-package-load*)

;;; This is a fully portable (though not very efficient)
;;; implementation of PROGV as a macro.  It does its own special
;;; binding (shallow binding) by saving the original values in a
;;; list, and marking things that were originally unbound.

(defun PORTABLE-PROGV-BIND (symbol old-vals place-holder)
  (let ((val-to-save '#:value-to-save))
    `(let ((,val-to-save (if (boundp ,symbol)
			     (symbol-value ,symbol)
			     ,place-holder)))
       (if ,old-vals
	   (rplacd (last ,old-vals) (ncons ,val-to-save))
	   (setq ,old-vals (ncons ,val-to-save))))))

(defun PORTABLE-PROGV-UNBIND (symbol old-vals place-holder)
  (let ((val-to-restore '#:value-to-restore))
    `(let ((,val-to-restore (pop ,old-vals)))
       (if (eq ,val-to-restore ,place-holder)
	   (makunbound ,symbol)
	   (setf (symbol-value ,symbol) ,val-to-restore)))))
  

(deftransform PROGV PORTABLE-PROGV-TRANSFORM
	      (symbols-form values-form &rest body)
  (let ((symbols-lst '#:symbols-list)
	(values-lst '#:values-list)
	(syms '#:symbols)
	(vals '#:values)
	(sym '#:symbol)
	(old-vals '#:old-values)
	(unbound-holder ''#:unbound-holder))
    `(let ((,symbols-lst ,symbols-form)
	   (,values-lst ,values-form)
	   (,old-vals nil))
       (unless (and (listp ,symbols-lst) (listp ,values-lst))
	 (error "PROGV: Both symbols and values must be lists"))
       (unwind-protect
	   (do ((,syms ,symbols-lst (cdr ,syms))
		(,vals ,values-lst (cdr ,vals))
		(,sym nil))
	       ((null ,syms) (progn ,@body))
	     (setq ,sym (car ,syms))
	     (if (symbolp ,sym)
		 ,(PORTABLE-PROGV-BIND sym old-vals unbound-holder)
		 (error "PROGV: Object to be bound not a symbol: ~S" ,sym))
	     (if ,vals
		 (setf (symbol-value ,sym) (first ,vals))
		 (makunbound ,sym)))
	 (dolist (,sym ,symbols-lst)
	   ,(PORTABLE-PROGV-UNBIND sym old-vals unbound-holder))))))

