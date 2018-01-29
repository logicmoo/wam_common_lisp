;;;
;;; **********************************************************************
;;; (c) Copyright G. Attardi, 1993.  All rights reserved.
;;; **********************************************************************
;;;
;;; A simple minded System Builder Tool.
;;;

;;; ----------------------------------------------------------------------
;;; Use:
;;;
;;; (defsystem name
;;;
;;;   :modules
;;;   -----------------------------------------------------------------
;;;    file     |   load           |   compile       | files which force
;;;             | environment      | environment     | recompilations of
;;;   -----------------------------------------------------------------
;;;   `(,@patches
;;;   (pkg          ,(car patches)  ,(car patches)         ())
;;;   (macros       (pkg macros)    (pkg macros)           ())
;;;   (low          (pkg walk)      (pkg macros)           (macros))
;;;   (,xxx-low     (low)           (macros low)           (low))
;;;   (boot         (,xxx)          (macros low ,xxx)      (low ,xxx))
;;;   (last         t               t                      (boot)))
;;;
;;;   If the value specified for :directory is a CONS, then the CAR is
;;;   used as the source file directory and the CDR is used as the binary
;;;   file directory.
;;;   :directory "/usr/src/"
;;;
;;;   :pathname-types ("lisp" . "bin"))
;;;
;;; ----------------------------------------------------------------------

(in-package 'sbt :use '(lisp))

(export '(defsystem
	   build-system
	   compile-system
	   load-system))

(defmacro defsystem (name &key modules directory pathname-types)
  `(defparameter ,name			; rather then defvar
     (make-system :name ',name
		  :modules ,modules
		  :source-directory ,directory
		  :pathname-types ,pathname-types)))

;;; ----------------------------------------------------------------------

(defstruct (system (:type vector) :named)
  name
  modules
  source-directory
  (pathname-types '("lsp"  "o")))

(defun make-source-pathname (name system)
  (make-pathname-internal name system :source))

(defun make-binary-pathname (name system)
  (make-pathname-internal name system :binary))

(defun make-pathname-internal (name system type)
  (let* ((extension (ecase type
		      (:SOURCE (car (system-pathname-types system)))
		      (:BINARY (cdr (system-pathname-types system)))))
	 (directory (etypecase (system-source-directory system)
		      (STRING (pathname (system-source-directory system)))
		      (CONS (ecase type
			      (:SOURCE (pathname
					(car (system-source-directory system))))
			      (:BINARY (pathname
					(cdr (system-source-directory system))))))))
	 (pathname
	  (make-pathname
	   :name #-VMS (string-downcase (string name))
	   #+VMS (string-downcase (substitute #\_ #\- (string name)))
	   :type extension
	   :defaults directory)))

    pathname))

;;; ----------------------------------------------------------------------
;;; Operations on modules
;;; 

(defstruct (module (:TYPE vector) :NAMED
		   (:CONSTRUCTOR make-module (name))
;                   (:PRINT-FUNCTION
;                     (lambda (m s d)
;                       (declare (ignore d))
;                       (format s "#<Module ~A>" (module-name m))))
		   )
  name
  load-env
  comp-env
  recomp-reasons)

(defun make-modules (system-description)
  (let ((modules ()))
    (labels ((get-module (name)
               (or (find name modules :key #'module-name)
                   (progn (setq modules (cons (make-module name) modules))
                          (car modules))))
             (parse-spec (spec)
               (if (eq spec 't)
                   (reverse (cdr modules))
                   (mapcar #'get-module spec))))
      (dolist (file system-description)
        (let* ((name (car file))
               (module (get-module name)))
          (setf (module-load-env module) (parse-spec (second file))
                (module-comp-env module) (parse-spec (third file))
                (module-recomp-reasons module) (parse-spec (fourth file))))))
    (reverse modules)))

(defun make-transformations (system filter make-transform)
  (let ((transforms (list nil)))
    (dolist (m (make-modules (system-modules system)))
      (when (funcall filter system m transforms)
        (funcall make-transform m transforms)))
    (nreverse (cdr transforms))))

(defun make-compile-transformation (module transforms)
  (unless (dolist (trans transforms)
            (and (eq (car trans) :COMPILE)
                 (eq (second trans) module)
                 (return trans)))    
    (dolist (c (module-comp-env module))
      (make-load-transformation c transforms))
    (push `(:COMPILE ,module) (cdr transforms))))

(defun make-load-transformation (module transforms)
  (unless (dolist (trans transforms)
            (when (eq (second trans) module)
              (case (car trans)
		(:COMPILE (return nil))
		(:LOAD    (return trans)))))
    (dolist (l (module-load-env module))
      (make-load-transformation l transforms))
    (push `(:LOAD ,module) (cdr transforms))))


(defun make-load-without-dependencies-transformation (module transforms)
  (unless (dolist (trans transforms)
            (and (eq (car trans) :LOAD)
                 (eq (second trans) module)
                 (return trans)))
    (push `(:LOAD ,module) (cdr transforms))))

(defun compile-filter (system module transforms)
  (or (dolist (r (module-recomp-reasons module))
        (when (dolist (transform transforms)
                (when (and (eq (car transform) :COMPILE)
                           (eq (second transform) r))
                  (return t)))
          (return t)))
      (null (probe-file (make-binary-pathname (module-name module) system)))
      (> (file-write-date (make-source-pathname (module-name module) system))
         (file-write-date (make-binary-pathname (module-name module) system)))))

(defun operate-on-system (system mode &optional arg print-only)
  (let ((transformations ()))
    (flet ((load-module (m s)
             (let ((name (module-name m))
                   (*load-verbose* nil))
               (if (or (eq mode :source)
		       (dolist (trans transformations)
			       (and (eq (first trans) :compile)
				    (eq (second trans) m)
				    (return trans))))
                   (progn (format t "~&Loading source of ~A..." name)
                          (unless print-only
				  (load (make-source-pathname name s))))
                   (progn (format t "~&Loading binary of ~A..." name)
                          (unless print-only
				  (load (make-binary-pathname name s)))))))
           (compile-module (m s)
             (format t "~&Compiling ~A..." (module-name m))
             (unless print-only
	       (let  ((name (module-name m)))
                 (compile-file (make-source-pathname name s)
		   :output-file (make-binary-pathname name s)))))
	   (true (&rest ignore) (declare (ignore ignore)) 't))
      (setq transformations
        (ecase mode
          (:COMPILE
            (make-transformations system
                                  #'compile-filter
                                  #'make-compile-transformation))
          (:RECOMPILE
            (make-transformations system
				  #'true
                                  #'make-compile-transformation))
          (:QUERY-COMPILE
            (make-transformations system
                                  #'(lambda (s m transforms)
                                      (or (compile-filter s m transforms)
                                          (y-or-n-p "Compile ~A?"
                                                    (module-name m))))
                                  #'make-compile-transformation))
          (:COMPILE-FROM
            (make-transformations system
                                  #'(lambda (s m transforms)
                                      (or (member (module-name m) arg)
                                          (compile-filter s m transforms)))
                                  #'make-compile-transformation))
          ((:LOAD :SOURCE)
            (make-transformations system
				  #'true
                                  #'make-load-transformation))
          (:QUERY-LOAD
            (make-transformations system
              #'(lambda (s m transforms)
		  (declare (ignore s transforms))
                  (y-or-n-p "Load ~A?" (module-name m)))
              #'make-load-without-dependencies-transformation))))
      
      (loop (when (null transformations) (return t))
	    (let ((transform (pop transformations)))
	      (ecase (first transform)
		(:COMPILE (compile-module (second transform) system))
		(:LOAD (load-module (second transform) system))))))))


(defun compile-system (system &optional m)
  (cond ((null m)      (operate-on-system system :COMPILE))
	((eq m 't)     (operate-on-system system :RECOMPILE))
	((eq m :PRINT) (operate-on-system system :COMPILE () t))
	((eq m :QUERY) (operate-on-system system :QUERY-COMPILE))
	((symbolp m)   (operate-on-system system :COMPILE-FROM (list m)))
	((listp m)     (operate-on-system system :COMPILE-FROM m))))

(defun load-system (system &optional mode)
  (case mode
    ((NIL) (operate-on-system system :LOAD))
    (:SOURCE (operate-on-system system :SOURCE))
    (:QUERY-LOAD (operate-on-system system :QUERY-LOAD))))


;;;----------------------------------------------------------------------
;;; User interface

(defmacro build-system (system &optional op mode)
  (case op
	(:LOAD
	 `(load-system ,system ,(case mode
				      (:QUERY :QUERY-LOAD)
				      (:SOURCE :SOURCE))))
	(:COMPILE
	 `(compile-system ,system ,(case mode
					 (:QUERY :QUERY-COMPILE)
					 (:FORCE :RECOMPILE))))
	(:PRINT
	 `(compile-system ,system :PRINT))
	(otherwise
	 `(load-system ,system))))
