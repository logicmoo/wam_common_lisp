
(in-package 'clos)

(defmacro pre-make-caching-discriminating-functions (specs)
  `(progn ,.(mapcar #'(lambda (s)
			`(pre-make-templated-function-constructor
			   caching-discriminating-function
			   ,.s))
		    specs)))

(defmacro pre-make-templated-function-constructor (name
						   &rest template-parameters)
  (let* ((params (get name 'templated-fn-params))
	 (template-params (car params))
	 (instance-params (cadr params))
	 (body (cddr params))
	 (form 
	  (progv template-params
		 template-parameters
		 `(let ((entry
			 (or (assoc ',template-parameters 
				    (get ',name 'templated-fn-constructors)
				    :test #'equal)
			     (let ((new-entry
				    (list ',template-parameters () () ())))
			       (push new-entry
				     (get ',name 'templated-fn-constructors))
			       new-entry))))
		    (setf (caddr entry) 'compiled)
		    (setf (cadr entry)
			  (function (lambda ,(eval instance-params)
				      ,(eval (cons 'progn body)))))))))
    form))

(eval-when (load)
  (pre-make-caching-discriminating-functions
    ((1 NIL (0) 32)
     (2 NIL (0) 32)
     (2 NIL (1) 32)		;setf of accessor gfuns
     (2 NIL (0 1) 32)
     (3 NIL (0) 32)
     (3 NIL (1) 32)
     (3 NIL (0 1) 32)
     (4 NIL (0) 32)
     (4 NIL (1) 32)
     (5 NIL (0) 32)
     (5 NIL (0 1) 32)
     (6 NIL (0) 32)
     (6 NIL (0 1) 32)
     (7 NIL (0) 32)
     
     (1 T (0) 32)
     (2 T (0) 32)
     (2 T (0 1) 32)
     (3 T (0) 32)
     (4 T (0) 32))))

(eval-when (compile eval)
  (defmacro precompile-effective-method-templates (templates)
    `(progn ,@(mapcar #'(lambda (x)
			  `(precompile-effective-method-template-1 ,x))
		      templates))))

(precompile-effective-method-templates 
  (
   (_call-method_)				      ;1 or more :around
						      ;methods with 0 or
						      ;more next methods
						      ;This case happens
						      ;whenever there are
						      ;:around methods


   (PROGN (_call-method_)			      ;1 :before 1 :after
	  (MULTIPLE-VALUE-PROG1			      ;*** NO PRIMARY, COMMON
	    (error "No applicable primary method.")   ;*** INTERMEDIATE STATE
	    (_call-method_)))

   (PROGN (_call-method_)			      ;1 :before 0 :after
	  (MULTIPLE-VALUE-PROG1			      ;*** NO PRIMARY, COMMON
	    (error "No applicable primary method."))) ;*** INTERMEDIATE STATE


   (PROGN ()					      ;0 :before 1 :after
	  (MULTIPLE-VALUE-PROG1			      ;*** NO PRIMARY, COMMON
	    (error "No applicable primary method.")   ;*** INTERMEDIATE STATE
	    (_call-method_)))


   (PROGN (_call-method_)			      ;1 :before 1 :after
	  (MULTIPLE-VALUE-PROG1 (_call-method_)
				(_call-method_)))

   (PROGN ()					      ;0 :before 1 :after
	  (MULTIPLE-VALUE-PROG1 (_call-method_)
				(_call-method_)))

   (PROGN (_call-method_)			      ;1 :before 0 :after
	  (MULTIPLE-VALUE-PROG1 (_call-method_)
				()))


   (PROGN (_call-method_)			      ;2 :befores 2 :after
	  (_call-method_)
	  (MULTIPLE-VALUE-PROG1 (_call-method_)
				(_call-method_)
				(_call-method_)))


   (PROGN (_call-method_)			      ;2 :befores 1 :after
	  (_call-method_)
	  (MULTIPLE-VALUE-PROG1 (_call-method_)
				(_call-method_)))

   (PROGN (_call-method_)			      ;1 :before 2 :after
	  (MULTIPLE-VALUE-PROG1 (_call-method_)
				(_call-method_)
				(_call-method_)))


   (PROGN (_call-method_)			      ;2 :befores no :after
	  (_call-method_)
	  (MULTIPLE-VALUE-PROG1 (_call-method_)
				()))

   (PROGN (_call-method_)			      ;0 :before 2 :after
	  (MULTIPLE-VALUE-PROG1 (_call-method_)
				(_call-method_)
				(_call-method_)))
  
   ))
