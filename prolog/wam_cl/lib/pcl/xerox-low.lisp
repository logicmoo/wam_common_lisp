;;; -*- Mode:LISP; Package:(PCL Lisp 1000); Base:10.; Syntax:Common-lisp -*-
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
;;; This is the 1100 (Xerox version) of the file portable-low.
;;;

(in-package 'pcl)

(defmacro load-time-eval (form)
  `(il:LOADTIMECONSTANT ,form))

;;;
;;; make the pointer from an instance to its class wrapper be an xpointer.
;;; this prevents instance creation from spending a lot of time incrementing
;;; the large refcount of the class-wrapper.  This is safe because there will
;;; always be some other pointer to the wrapper to keep it around.
;;; 
#+Xerox-Medley
(defstruct (std-instance (:predicate std-instance-p)
			 (:conc-name %std-instance-)
			 (:constructor %%allocate-instance--class ())
			 (:fast-accessors t)
			 (:print-function %print-std-instance))
  (wrapper nil :type il:fullxpointer)
  (slots nil))

#+Xerox-Lyric
(eval-when (eval load compile)
  (il:datatype std-instance
	       ((wrapper il:fullxpointer)
	        slots))

  (xcl:definline std-instance-p (x)
    (typep x 'std-instance))
  
  (xcl:definline %%allocate-instance--class ()
    (il:create std-instance))

  (xcl:definline %std-instance-wrapper (x) 
    (il:fetch (std-instance wrapper) il:of x))

  (xcl:definline %std-instance-slots (x) 
    (il:fetch (std-instance slots) il:of x))

  (xcl:definline set-%std-instance-wrapper (x value) 
    (il:replace (std-instance wrapper) il:of x il:with value))

  (xcl:definline set-%std-instance-slots (x value) 
    (il:replace (std-instance slots) il:of x il:with value))

  (defsetf %std-instance-wrapper set-%std-instance-wrapper)

  (defsetf %std-instance-slots set-%std-instance-slots)

  (il:defprint 'std-instance '%print-std-instance)

  )

(defun %print-std-instance (instance &optional stream depth)  
  ;; See the IRM, section 25.3.3.  Unfortunatly, that documentation is
  ;; not correct.  In particular, it makes no mention of the third argument.
  (cond ((streamp stream)
	 ;; Use the standard PCL printing method, then return T to tell
	 ;; the printer that we have done the printing ourselves.
	 (print-std-instance instance stream depth)
	 t)
	(t 
	 ;; Internal printing (again, see the IRM section 25.3.3). 
	 ;; Return a list containing the string of characters that
	 ;; would be printed, if the object were being printed for
	 ;; real.
	 (list (with-output-to-string (stream)
		 (print-std-instance instance stream depth))))))

  ;;   
;;;;;; FUNCTION-ARGLIST
  ;;

(defun function-arglist (x)
  ;; Xerox lisp has the bad habit of returning a symbol to mean &rest, and
  ;; strings instead of symbols.  How silly.
  (let ((arglist (il:arglist x)))
    (when (symbolp arglist)
      ;; This could be due to trying to extract the arglist of an interpreted
      ;; function (though why that should be hard is beyond me).  On the other
      ;; hand, if the function is compiled, it helps to ask for the "smart"
      ;; arglist.
      (setq arglist 
	    (if (consp (symbol-function x))
		(second (symbol-function x))
		(il:arglist x t))))
    (if (symbolp arglist)
	;; Probably never get here, but just in case
	(list '&rest 'rest)
	;; Make sure there are no strings where there should be symbols
	(if (some #'stringp arglist)
	    (mapcar #'(lambda (a) (if (symbolp a) a (intern a))) arglist)
	    arglist))))

(defun printing-random-thing-internal (thing stream)
  (let ((*print-base* 8))
    (princ (il:\\hiloc thing) stream)
    (princ "," stream)
    (princ (il:\\loloc thing) stream)))

(defun record-definition (name type &optional parent-name parent-type)
  (declare (ignore type parent-name))
  ())


;;;
;;; FIN uses this too!
;;;
(eval-when (compile load eval)
  (il:datatype il:compiled-closure (il:fnheader il:environment))

  (il:blockrecord closure-overlay ((funcallable-instance-p il:flag)))  

  )

(defun compiled-closure-fnheader (compiled-closure)
  (il:fetch (il:compiled-closure il:fnheader) il:of compiled-closure))

(defun set-compiled-closure-fnheader (compiled-closure nv)
  (il:replace (il:compiled-closure il:fnheader) il:of compiled-closure nv))

(defsetf compiled-closure-fnheader set-compiled-closure-fnheader)

;;;
;;; In Lyric, and until the format of FNHEADER changes, getting the name from
;;; a compiled closure looks like this:
;;; 
;;; (fetchfield '(nil 4 pointer)
;;;             (fetch (compiled-closure fnheader) closure))
;;;
;;; Of course this is completely non-robust, but it will work for now.  This
;;; is not the place to go into a long tyrade about what is wrong with having
;;; record package definitions go away when you ship the sysout; there isn't
;;; enough diskspace.
;;;             
(defun set-function-name-1 (fn new-name uninterned-name)
  (cond ((typep fn 'il:compiled-closure)
	 (il:\\rplptr (compiled-closure-fnheader fn) 4 new-name)
	 (when (and (consp uninterned-name)
		    (eq (car uninterned-name) 'method))
	   (let ((debug (si::compiled-function-debugging-info fn)))
	     (when debug (setf (cdr debug) uninterned-name)))))
	(t nil))
  fn)

