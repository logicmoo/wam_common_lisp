(defvar std-compile (symbol-function 'compile-file))
(defun compile-file (file &rest args)
 (labels ((get-ofile (list)
	    (if (null list)
		(merge-pathnames file ".o")
		(if (eq (car list) :output-file)
		    (cadr list)
		    (get-ofile (cddr list))))))
       (funcall std-compile
		file
		:c-file t :h-file t :data-file t
		:o-file (get-ofile (cdr args)))))
