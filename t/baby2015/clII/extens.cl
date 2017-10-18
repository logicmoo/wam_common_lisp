(in-package "LISP")

(defmacro declaim (&rest decl-specs)
  `(PROGN
     ,@(mapcar #'(lambda (decl-spec) 
                   `(PROCLAIM (QUOTE ,decl-spec))) decl-specs)))

(export 'declaim)

;;; eof

