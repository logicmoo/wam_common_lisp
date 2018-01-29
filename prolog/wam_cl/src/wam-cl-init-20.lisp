;;; #+BUILTIN Means to ignore since it should already be defined
;;; #+WAM-CL Means we want it
;;; #+LISP500 Means probably we dont want it
;;; #+ALT Alternative definition
;;; #+ABCL From ABCL
;;; #+SBCL From SBCL
;;; #+ECL From ECL
;;; #+SICL From SICL


(in-package "SYSTEM")



(defmacro=sourceinfo psetq (&rest rest)
  (let ((inits nil)
	(sets nil)
	(list rest))
    (tagbody
     start
       (when (cddr list)
	 (push (list (gensym) (cadr list)) inits)
	 (setq list (cddr list))
	 (go start)))
    (setq list inits)
    (tagbody
     start
       (when (cddr rest)
	 (push (caar list) sets)
	 (push (car rest) sets)
	 (setq list (cdr list))
	 (setq rest (cddr rest))
	 (go start)))
    `(let ,(reverse inits)
      (setq ,@sets ,@rest))))


(defmacro=sourceinfo return (&optional result)
  `(return-from nil ,result))

(defmacro=sourceinfo when (test-form &rest forms)
  `(if ,test-form (progn ,@forms)))

(defmacro=sourceinfo unless (test-form &rest forms)
  `(if (not ,test-form) (progn ,@forms)))

(defmacro=sourceinfo and (&rest forms)
  (if forms
      (if (cdr forms)
	  `(when ,(car forms) (and ,@(cdr forms)))
	(car forms))
    `t))

(defmacro=sourceinfo or (&rest forms)
  (if forms
      (if (cdr forms)
	  (let ((temp (gensym)))
	    `(let ((,temp ,(car forms)))
	      (if ,temp
		  ,temp
		(or ,@(cdr forms)))))
	(car forms))
    `nil))

(defmacro=sourceinfo cond (&rest clauses)
  (when clauses
    (if (cdar clauses)
	`(if ,(caar clauses)
	     (progn ,@(cdar clauses))
	     (cond ,@(cdr clauses)))
	`(or ,(caar clauses)
	     (cond ,@(cdr clauses))))))


(defmacro=sourceinfo case (keyform &rest clauses)
  (let ((temp (gensym)))
    (labels ((recur (clauses)
	       (when clauses
		 (if (member (caar clauses) '(otherwise t))
		     `(progn ,@(cdar clauses))
		     `(if ,(if (listp (caar clauses))
			       `(member ,temp ',(caar clauses))
			       `(eql ,temp ',(caar clauses)))
		          (progn ,@(cdar clauses))
		          ,(recur (cdr clauses)))))))
      `(let ((,temp ,keyform))
	,(recur clauses)))))


(defmacro=sourceinfo ecase (keyform &rest clauses)
  (let ((temp (gensym)))
    `(let ((,temp ,keyform))
      (case ,temp ,@clauses
	    (error 'type-error :datum ,temp
		   :expected-type `(member ,@(mapcan #'(lambda (x)
							 (if (listp (car x))
							     (car x)
							     (list (car x))))
						     clauses)))))))

(defmacro=sourceinfo multiple-value-bind (vars values-form &rest forms)
  `(multiple-value-call #'(lambda (&optional ,@vars &rest ,(gensym))
			    ,@forms)
                        ,values-form))

(defmacro=sourceinfo multiple-value-list (form)
  `(multiple-value-call #'list ,form))

(defun=sourceinfo values-list (list) (apply #'values list))

(defmacro=sourceinfo prog (inits &rest forms)
  `(block nil
    (let ,inits
      (tagbody ,@forms))))
(defmacro=sourceinfo prog* (inits &rest forms)
  `(block nil
    (let* ,inits
      (tagbody ,@forms))))
(defmacro=sourceinfo prog1 (first-form &rest forms)
  (let ((temp (gensym)))
    `(let ((,temp ,first-form))
      ,@forms
      ,temp)))
(defmacro=sourceinfo prog2 (first-form second-form &rest forms)
  (let ((temp (gensym)))
    `(progn
      ,first-form
      (let ((,temp ,second-form))
	,@forms
	,temp))))

#+BUILTIN
#+(or WAM-CL LISP500)
(defun equal (a b)
  (or (eql a b)
      (cond
	((not a) nil)
	((consp a) (and (consp b)
			(equal (car a) (car b))
			(equal (cdr a) (cdr b))))
	((stringp a) (and (stringp b)
			  (string= a b)))
	((bit-vector-p a) (and (bit-vector-p b)
			       (= (length a) (length b))
			       (dotimes (i (length a) t)
				 (when (/= (aref a i) (aref b i))
				   (return))))))))

#+(or WAM-CL LISP500)
(defun identity (object) object)

#+(or WAM-CL LISP500)
(defun complement (function)
  #'(lambda (&rest rest) (not (apply function rest))))

#+(or WAM-CL LISP500)
(defun constantly (value) #'(lambda (&rest rest) value))



(defmacro=sourceinfo dotimes ((var count-form &optional result-form) &rest forms)
  (let ((start (gensym))
	(count (gensym)))
    `(block nil
      (let ((,var 0)
	    (,count ,count-form))
	(tagbody
	   ,start
	   (when (< ,var ,count)
	     ,@forms
	     (incf ,var)
	     (go ,start)))
	,result-form))))



(defmacro=sourceinfo do (vars (end-test-form &rest result-forms) &rest forms)
  (let ((start (gensym))
	(inits nil)
	(steps nil))
  `(block nil
    (let ,(dolist (var vars (reverse inits))
	    (push (if (consp var)
		      (list (car var) (cadr var))
		      (list var)) inits))
      (tagbody
	 ,start
	 (if ,end-test-form (return (progn ,@result-forms)))
	 ,@forms
	 ,@(dolist (var vars (when steps `((psetq ,@(reverse steps)))))
	     (when (and (consp var) (cddr var))
	       (push (car var) steps)
	       (push (caddr var) steps)))
	 (go ,start))))))
(defmacro=sourceinfo do* (vars (end-test-form &rest result-forms) &rest forms)
  (let ((start (gensym))
	(inits nil)
	(steps nil))
  `(block nil
    (let* ,(dolist (var vars (reverse inits))
	     (push (if (consp var)
		       (list (car var) (cadr var))
		       (list var)) inits))
      (tagbody
	 ,start
	 (if ,end-test-form (return (progn ,@result-forms)))
	 ,@forms
	 ,@(dolist (var vars (when steps `((setq ,@(reverse steps)))))
	     (when (and (consp var) (cddr var))
	       (push (car var) steps)
	       (push (caddr var) steps)))
	 (go ,start))))))



(defmacro=sourceinfo dolist ((var list-form &optional result-form) &rest forms)
  (let ((start (gensym))
	(list (gensym)))
    `(block nil
      (let ((,list ,list-form)
	    (,var nil))
	(tagbody
	   ,start
	   (unless ,list
	     (setf ,var nil)
	     (return-from nil ,result-form))
	   (setf ,var (car ,list))
	   (setf ,list (cdr ,list))
	   ,@forms
	   (go ,start))))))




#+(or WAM-CL LISP500)
(defun designator-condition (default-type datum arguments)
  (if (symbolp datum)
      (apply #'make-condition datum arguments)
      (if (or (stringp datum) (functionp datum))
	  (make-condition default-type
			  :format-control datum
			  :format-arguments arguments)
	  datum)))


#+(or WAM-CL LISP500) 
(defun invoke-debugger (condition)
  (let ((debugger-hook *debugger-hook*)
	(*debugger-hook* nil))
    (when debugger-hook
      (funcall debugger-hook condition debugger-hook))
    (format *debug-io* "Entering debugger.~%")
    (princ condition *debug-io*)
    (terpri *debug-io*)
    (let ((restarts (compute-restarts condition))
	  (stack (makef))
	  (frame-depth 0)
	  (active-frame nil))
      (let ((count 0))
	(dolist (restart restarts)
	  (format *debug-io* "~A: " count)
	  (princ restart *debug-io*)
	  (terpri *debug-io*)
	  (incf count)))
      (setq active-frame (next-function-frame (- stack 20)))
      (show-frame active-frame 0)
      (tagbody
       start
	 (format *debug-io* ";~A> " frame-depth)
	 (let ((form (read)))
	   (case form
	     (:help (format *debug-io* "Type :help to get help.~%")
		    (format *debug-io* "Type :continue <index> to invoke the indexed restart.~%"))
	     (:back (do ((frame (next-function-frame (- stack 20))
				(next-function-frame frame))
			 (index 0 (+ 1 index)))
			((not frame))
		      (show-frame frame index)))
	     (:up (if (plusp frame-depth)
		      (progn
			(decf frame-depth)
			(do ((frame (next-function-frame (- stack 20))
				    (next-function-frame frame))
			     (index 0 (+ 1 index)))
			    ((= index frame-depth) (setq active-frame frame)))
			(show-frame active-frame frame-depth))
		      (format *debug-io* "Top of stack.~%")))
	     (:down (let ((frame (next-function-frame active-frame)))
		      (if frame
			  (progn
			    (incf frame-depth)
			    (setq active-frame frame)
			    (show-frame active-frame frame-depth))
			  (format *debug-io* "Bottom of stack.~%"))))
	     (:locals (do ((env (fref (- active-frame 1)) (cdr env)))
			  ((not env))
			(when (symbolp (caar env))
			  (format *debug-io* "~A~%" (caar env)))))
	     (:continue (let ((index (read)))
			  (invoke-restart-interactively (nth index restarts))))
	     (t (let ((values (multiple-value-list
			       (eval form (fref (- active-frame 1)))))
		      (count 0))
		  (if values
		      (dolist (value values)
			(format *debug-io* ";~A: ~S~%" count value)
			(incf count))
		      (format *debug-io* ";No values.~%")))))
	   (go start))))))





#+(or WAM-CL LISP500) 
(defparameter *debugger-hook* nil)

#+(or WAM-CL LISP500) 
(defparameter *break-on-signals* nil)

#+(or WAM-CL LISP500) 
(defparameter *handlers* nil)

#+(or WAM-CL LISP500) 
(defun invoke-handler (condition)
  (dolist (handler *handlers*)
    (when (typep condition (car handler))
      (setq *handlers* (caddr handler))
      (funcall (cadr handler) condition))))


#+(or WAM-CL LISP500) 
(defmacro handler-bind (bindings &rest forms)
  (let ((form '*handlers*)
	(handlers (gensym)))
    (dolist (binding (reverse bindings))
      (setq form
	    `(cons (list ',(car binding) ,(cadr binding) ',handlers) ,form)))
    `(let ((handlers *handlers*)
	   (*handlers* ,form))
      ,@forms)))

#+(or WAM-CL LISP500) 
(defmacro handler-case (expression &rest clauses)
  (let ((tag (gensym))
	(bindings nil))
    `(handler-bind
      ,(dolist (clause clauses (reverse bindings))
	 (let ((typespec (car clause))
	       (var-list (cadr clause))
	       (forms (cddr clauses)))
	   (push `(typespec #'(lambda (,(if var-list (car var-list) (gensym)))
				(return-from tag (progn ,@forms))))
		 bindings)))
      ,expression)))

#+(or WAM-CL LISP500) 
(defmacro ignore-errors (&rest forms)
  `(handler-case (progn ,@forms)
    (error (condition) (values nil condition))))

#+(or WAM-CL LISP500) 
(defparameter *restarts* nil)

#+(or WAM-CL LISP500) 
(defun compute-restarts (&optional condition)
  "FIXME restarts associated with conditions"
  (if condition
      *restarts*
      *restarts*))

#+(or WAM-CL LISP500) 
(defun find-restart (identifier &optional condition)
  (dolist (restart *restarts*)
    (when (eq restart identifier)
      (return restart))
    (when (eq (restart-name restart) identifier)
      (return restart))))



#+(or WAM-CL LISP500) 
(defun designator-restart (designator)
  (if (restartp designator)
      designator
      (dolist (restart *restarts* (error 'type-error :datum designator
					 :expected-type 'restart))
	(when (eq (restart-name restart) designator)
	  (return restart)))))


#+(or WAM-CL LISP500) 
(defun invoke-restart (restart &rest arguments)
  (setq restart (designator-restart restart))
  (apply (restart-function restart) arguments))

#+(or WAM-CL LISP500) 
(defun invoke-restart-interactively (restart)
  (setq restart (designator-restart restart))
  (apply (restart-function restart)
	 (funcall (restart-interactive-function restart))))

#+(or WAM-CL LISP500) 
(defmacro restart-bind (restart-bindings &rest forms)
  (let ((form '*restarts*))
    (dolist (binding (reverse restart-bindings))
      (setq form
	    `(cons (make-restart ',(car binding) ,@(cdr binding)) ,form)))
    `(let ((*restarts* ,form))
      ,@forms)))


#+(or WAM-CL LISP500) 
(defmacro restart-case (restartable-form &rest clauses)
  (let ((catch-tag (gensym))
	(bindings nil))
    `(catch ',catch-tag
      (restart-bind
	  ,(dolist (clause clauses (reverse bindings))
	     (let ((name (car clause))
		   (lambda-list (cadr clause))
		   (rest (cddr clause))
		   (interactive '#'(lambda () nil))
		   (report '#'(lambda (stream)
				(format stream "~A" (car clause))))
		   (test '#'(lambda (condition) t)))
	       (tagbody
		start
		  (when (member (car rest) '(:interactive :report :test))
		    (let ((value (cadr rest)))
		      (case (car rest)
			(:interactive (setq interactive `(function ,value)))
			(:report (setq report
				       (if (stringp value)
					   `#'(lambda (stream)
						(write-string ,value stream))
					   `(function ,value))))
			(:test (setq test `(function ,value)))))
		    (setq rest (cddr rest))
		    (go start)))
	       (push `(,(car clause)
		       #'(lambda ,(cadr clause)
			   (throw ',catch-tag (progn ,@rest)))
		       :interactive-function ,interactive
		       :report-function ,report
		       :test-function ,test)
		     bindings)))
	,restartable-form))))




#+(or WAM-CL LISP500) 
(defun warn (datum &rest arguments)
  (restart-case
      (let ((warning (if (symbolp datum)
			 (apply #'make-condition 'warning datum arguments)
			 datum)))
	(signal warning)
	(print-object warning *error-output*))
    (muffle-warning () nil))
  nil)


#+(or WAM-CL LISP500) 
(defun error (datum &rest arguments)
  (let ((condition (designator-condition 'simple-error datum arguments)))
    (when (typep condition *break-on-signals*)
      (invoke-debugger condition))
    (invoke-handler condition)
    (invoke-debugger condition)))


#+(or WAM-CL LISP500) 
(defun cerror (continue-format-control datum &rest arguments)
  (with-simple-restart (continue continue-format-control)
    (apply #'error datum arguments)))


#+(or WAM-CL LISP500) 
(defun signal (datum &rest arguments)
  (let ((condition (designator-condition 'simple-condition datum arguments)))
    (when (typep condition *break-on-signals*)
      (invoke-debugger condition))
    (invoke-handler condition)
    nil))



