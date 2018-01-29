(defun compile-file
 (&rest system::args &aux (*print-pretty* nil) (*package* *package*))
 (labels ((get-ofile (list)
		   (if (null list)
		       (error "No :output-file specification")
		     (if (eq (car list) :output-file)
			 (cadr list)
		       (get-ofile (cddr list))))))
       (compiler::init-env)
       (funcall #'compiler::compile-file1
		(first system::args)
		:c-file t :h-file t :data-file t
		:o-file (get-ofile (cdr system::args)))))
