;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPMAIN  Compiler main program.

;;;		**** Caution ****
;;;	This file is machine/OS dependant.
;;;		*****************


(in-package 'compiler)

(export '(*compile-print* *compile-verbose* *cc* *cc-optimize*))

(defvar *cmpinclude* "<ecl.h>")
;;; This is copied into each .h file generated, EXCEPT for system-p calls.
;;; The constant string *include-string* is the content of file "ecl.h".
;;; Here we use just a placeholder: it will be replaced with sed.
(eval-when (load)
  (defconstant *include-string* "REPLACE ECL.H
"))					; the newline here is needed by the
					; sed in Makefile.
(eval-when (eval)
  (with-open-file (st "../h/ecl.h")
    (let* ((len (file-length st))
           (tem (make-array len :element-type 'STRING-CHAR)))
      (when (plusp (sys::read-bytes st tem 0 len))
         (defconstant *include-string* tem)))))

(eval-when (eval)
  (defvar *cc* "gcc -I."))
(eval-when (load)
  (defvar *cc* "CCOMPILER -I."))
(defvar *cc-optimize* "-O")		; C compiler otimization flag

(defvar *compiler-in-use* nil)
(defvar *compiler-input*)
(defvar *compiler-output1*)
(defvar *compiler-output2*)
(defvar *compiler-output-data*)

(defvar *error-p* nil)

(defvar *compile-print* t)
(defvar *compile-verbose* t)

(eval-when (compile eval)
  (defmacro get-output-pathname (file ext)
    `(make-pathname :directory (or (and ,file
				       (not (eq ,file t))
				       (pathname-directory ,file))
				  dir)
		   :name (or (and ,file
				  (not (eq ,file t))
				  (pathname-name ,file))
			     name)
		   :type ,ext))
  )

(defun compile-file (input-pathname
                      &key (output-file input-pathname)
                           (o-file t)
                           (c-file nil)
                           (h-file nil)
                           (data-file nil)
                           (system-p nil)
                           (load nil)
                      &aux (*standard-output* *standard-output*)
                           (*error-output* *error-output*)
                           (*compiler-in-use* *compiler-in-use*)
                           (*package* *package*)
			   (*print-pretty* nil)
                           (*error-count* 0)
			   #+PDE sys:*source-pathname*)
  (declare (notinline compiler-cc))

  (setq input-pathname (merge-pathnames input-pathname #".lsp"))

  #+PDE (setq sys:*source-pathname* (truename input-pathname))

  (when *compiler-in-use*
    (format t "~&;;; The compiler was called recursively.~%~
Cannot compile ~a."
	    (namestring input-pathname))
    (setq *error-p* t)
    (return-from compile-file (values)))

  (setq *error-p* nil
	*compiler-in-use* t)

  (unless (probe-file input-pathname)
    (format t "~&;;; The source file ~a is not found.~%"
            (namestring input-pathname))
    (setq *error-p* t)
    (return-from compile-file (values)))

  (when *compile-verbose*
    (format t "~&;;; Compiling ~a."
            (namestring input-pathname)))

  (let* ((eof (cons nil nil))
         (dir (or (and output-file
                       (pathname-directory output-file))
                  (pathname-directory input-pathname)))

         (name (or (and output-file
                        (pathname-name output-file))
                   (pathname-name input-pathname)))

         (o-pathname (get-output-pathname o-file "o"))
         (c-pathname (get-output-pathname c-file "c"))
         (h-pathname (get-output-pathname h-file "h"))
         (data-pathname (get-output-pathname data-file "data"))
         )

    (init-env)

    (when (probe-file "./cmpinit.lsp")
      (load "./cmpinit.lsp"
            :verbose *compile-verbose*))

    (with-open-file (*compiler-output-data*
                     data-pathname
                     :direction :output)
      (wt-data-begin)

      (with-open-file
          (*compiler-input* input-pathname)
        (let* ((rtb *readtable*)
               (prev (and (eq (get-macro-character #\# rtb)
                              (get-macro-character
                                #\# (copy-readtable nil)))
                          (get-dispatch-macro-character #\# #\, rtb))))
          (if (and prev (eq prev (get-dispatch-macro-character
                                   #\# #\, (copy-readtable nil))))
              (set-dispatch-macro-character #\# #\,
                'SYS:SHARP-COMMA-READER-FOR-COMPILER rtb)
              (setq prev nil))
          (unwind-protect
            (do ((form (read *compiler-input* nil eof)
                       (read *compiler-input* nil eof)))
                ((eq form eof))
              (t1expr form))
            (when prev (set-dispatch-macro-character #\# #\, prev rtb)))))

      (when (zerop *error-count*)
        (when *compile-verbose* (format t "~&;;; End of Pass 1.  "))
        (compiler-pass2 c-pathname h-pathname system-p
                        (if system-p
                            (pathname-name input-pathname)
                            "code")))

      (wt-data-end)

      ) ;;; *compiler-output-data* closed.

    (init-env)

    (if (zerop *error-count*)
        (progn
          (cond (o-file
		 (when *compile-verbose*
		   (format t "~&;;; Calling the C compiler... "))
                 (compiler-cc c-pathname o-pathname)
                 (cond ((probe-file o-pathname)
                        (cat-data-file o-pathname data-pathname)
                        (when load (load o-pathname))
                        (when *compile-verbose*
			  (print-compiler-info)
			  (format t "~&;;; Finished compiling ~a."
				  (namestring input-pathname))))
		       #+(or SYSTEM-V APOLLO) ;tito 
		       ((probe-file (setq ob-name
					  (format nil "~a.o"
						  (pathname-name o-pathname))))
			(system (format nil "mv ~A ~A" (namestring ob-name)
					(namestring o-pathname)))
                        (cat-data-file o-pathname data-pathname)
                        (when load (load o-pathname))
                        (when *compile-verbose*
			  (print-compiler-info)
			  (format t "~&;;; Finished compiling ~a."
				  (namestring input-pathname))))	       
                       (t (format t "~&;;; The C compiler failed to compile the intermediate file.~%")
                          (setq *error-p* t))))
		(*compile-verbose*
		 (print-compiler-info)
		 (format t "~&;;; Finished compiling ~a."
			 (namestring input-pathname))))
          (unless c-file (delete-file c-pathname))
          (unless h-file (delete-file h-pathname))
          (unless data-file (delete-file data-pathname))
	  o-pathname)

        (progn
          (when (probe-file c-pathname) (delete-file c-pathname))
          (when (probe-file h-pathname) (delete-file h-pathname))
          (when (probe-file data-pathname) (delete-file data-pathname))
          (format t "~&;;; No FASL generated.~%")
          (setq *error-p* t)
	  (values))
        ))
  )

(defun compile (name &optional (def nil supplied-p)
                      &aux form gazonk-name
                      data-pathname
                      (*compiler-in-use* *compiler-in-use*)
                      (*standard-output* *standard-output*)
                      (*error-output* *error-output*)
                      (*package* *package*)
                      (*compile-print* nil)
		      (*print-pretty* nil)
                      (*error-count* 0))

  (unless (symbolp name) (error "~s is not a symbol." name))

  (when *compiler-in-use*
    (format t "~&;;; The compiler was called recursively.~
		~%Cannot compile ~s." name)
    (setq *error-p* t)
    (return-from compile))

  (setq *error-p* nil
	*compiler-in-use* t)

  (cond ((and supplied-p def)
         (unless (and (consp def) (eq (car def) 'LAMBDA))
                 (error "~s is invalid lambda expression." def))
         (setq form (if name
                        `(defun ,name ,@(cdr def))
                        `(set 'GAZONK #',def))))
        ((and (fboundp name)
              (consp (setq def (symbol-function name))))
         (cond ((and (eq (car def) 'LAMBDA-BLOCK)
                     (consp (cdr def)) (consp (cddr def)))
                (if (eq (cadr def) name)
                    (setq form `(defun ,name ,@(cddr def)))
                    (setq form `(defun ,name ,(caddr def)
                                  (block ,(cadr def) ,@(cdddr def))))))
               ((eq (car def) 'LAMBDA)
                (setq form `(defun ,name ,@(cdr def))))
               ((and (eq (car def) 'LAMBDA-CLOSURE)
                     (consp (cdr def)) (null (cadr def))
                     (consp (cddr def)) (null (caddr def))
                     (consp (cdddr def)) (null (cadddr def)))
                (setq form `(defun ,name ,@(cddddr def))))
               ((and (eq (car def) 'LAMBDA-BLOCK-CLOSURE)
                     (consp (cdr def)) (null (cadr def))
                     (consp (cddr def)) (null (caddr def))
                     (consp (cdddr def)) (null (cadddr def))
                     (consp (cddddr def)))
                (setq form `(defun ,name
                              (block ,(car (cddddr def))
                                ,@(cdr (cddddr def))))))
               (t (error "I cannot compile such ~Ss, sorry." (car def)))))
        (t (error "No lambda expression is assigned to the symbol ~s." name)))

  (dotimes (n 1000
              (progn
                (format t "~&;;; The name space for GAZONK files exhausted.~%~
;;; Delete one of your GAZONK*** files before compiling ~s." name)
                (setq *error-p* t)
                (return-from compile (values))))
    (setq gazonk-name (format nil "gazonk~3,'0d" n))
    (setq data-pathname (make-pathname :name gazonk-name :type "data"))
    (unless (probe-file data-pathname)
      (return)))

  (let ((c-pathname (make-pathname :name gazonk-name :type "c"))
        (h-pathname (make-pathname :name gazonk-name :type "h"))
        (o-pathname (make-pathname :name gazonk-name :type "o")))

    (init-env)

    (with-open-file (*compiler-output-data* data-pathname
					    :direction :output)
      (wt-data-begin)

      (t1expr form)

      (when (zerop *error-count*)
        (when *compile-verbose* (format t "~&;;; End of Pass 1.  "))
        (compiler-pass2 c-pathname h-pathname nil "code"))

      (wt-data-end)
      ) ;;; *compiler-output-data* closed.

    (init-env)

    (if (zerop *error-count*)
        (progn
          (when *compile-verbose*
	    (format t "~&;;; Calling the C compiler... "))
          (compiler-cc c-pathname o-pathname)
          (delete-file c-pathname)
          (delete-file h-pathname)
          (cond ((probe-file o-pathname)
                 (cat-data-file o-pathname data-pathname)
                 (load o-pathname :verbose nil)
                 (when *compile-verbose* (print-compiler-info))
                 (delete-file o-pathname)
                 (delete-file data-pathname))
                (t (delete-file data-pathname)
                   (format t "~&;;; The C compiler failed to compile~
			~the intermediate code for ~s.~%" name)
                   (setq *error-p* t)))
	  (or name (symbol-value 'GAZONK)))

        (progn
          (when (probe-file c-pathname) (delete-file c-pathname))
          (when (probe-file h-pathname) (delete-file h-pathname))
          (when (probe-file data-pathname) (delete-file data-pathname))
          (format t "~&;;; Failed to compile ~s.~%" name)
          (setq *error-p* t)
          name))))

(defun disassemble (&optional (thing nil)
			      &key (h-file nil) (data-file nil)
			      &aux def disassembled-form
			      (*compiler-in-use* *compiler-in-use*)
			      (*print-pretty* nil))
 (when *compiler-in-use*
   (format t "~&;;; The compiler was called recursively.~
                   ~%Cannot disassemble ~a." thing)
   (setq *error-p* t)
   (return-from disassemble))
 (setq *error-p* nil
       *compiler-in-use* t)

 (cond ((null thing))
       ((symbolp thing)
	(setq def (symbol-function thing))
	(when (macro-function thing)
	  (setq def (cdr def)))
	(if (and (consp def)
		 (eq (car def) 'LAMBDA-BLOCK)
		 (consp (cdr def)))
	    (setq disassembled-form `(defun ,thing ,@(cddr def)))
	    (error "The function object ~s cannot be disassembled." def)))
       ((and (consp thing) (eq (car thing) 'LAMBDA))
	(setq disassembled-form `(defun gazonk ,@(cdr thing))))
       (t (setq disassembled-form thing)))

  (let* ((null-stream (make-broadcast-stream))
         (*compiler-output1* null-stream)
         (*compiler-output2* (if h-file
				 (open h-file :direction :output)
				 null-stream))
         (*compiler-output-data* (if data-file
				     (open data-file :direction :output)
				     null-stream))
         (*error-count* 0)
         (t3local-fun (symbol-function 'T3LOCAL-FUN))
	 (t3fun (get 'DEFUN 'T3)))
    (unwind-protect
      (progn
        (setf (get 'DEFUN 'T3)
              #'(lambda (&rest args)
                 (let ((*compiler-output1* *standard-output*))
                   (apply t3fun args))))
        (setf (symbol-function 'T3LOCAL-FUN)
              #'(lambda (&rest args)
                 (let ((*compiler-output1* *standard-output*))
                   (apply t3local-fun args))))
        (init-env)
        (when data-file (wt-data-begin))
        (t1expr disassembled-form)
        (if (zerop *error-count*)
          (catch *cmperr-tag* (ctop-write "code"))
          (setq *error-p* t))
	(when data-file (wt-data-end))
        )
      (setf (get 'DEFUN 'T3) t3fun)
      (setf (symbol-function 'T3LOCAL-FUN) t3local-fun)
      (when h-file (close *compiler-output2*))
      (when data-file (close *compiler-output-data*))))
  (values)
  )

(defun compiler-pass2 (c-pathname h-pathname system-p init-name)
  (with-open-file (*compiler-output1* c-pathname :direction :output)
    (with-open-file (*compiler-output2* h-pathname :direction :output)
      (when system-p
        (wt-nl1
	 "/* (c) Copyright G. Attardi, 1993. */")
        (wt-h
	 "/* (c) Copyright G. Attardi, 1993. */"))

      (if (and *include-string* (not system-p))
	  (sys::write-bytes *compiler-output2* *include-string*
			  0 (length *include-string*))
	  (wt-nl1 "#include " *cmpinclude*))

      (wt-nl1 "#include \"" (namestring h-pathname) "\"")
      (catch *cmperr-tag* (ctop-write init-name system-p))

      (terpri *compiler-output1*)
      ;; write ctl-z at end to make sure preprocessor stops!
;      #+ms-dos (write-char (code-char 26) *compiler-output1*)
      (terpri *compiler-output2*))))

(eval-when (load)
  (defvar *cc-format* "~A ~:[~*~;~A~] LSPCFLAGS -w -c ~A -o ~A"))
(eval-when (eval)
  (defvar *cc-format* "~A ~:[~*~;~A~] -w -c ~A -o ~A"))

(defun compiler-cc (c-pathname o-pathname)
  (flet ((safe-system (string)
	   (let ((result (system string)))
	     (unless (zerop result)
	       (cerror "Continues anyway."
		       "(SYSTEM ~S) returned a non-zero value ~D."
		       string
		       result)
	       (setq *error-p* t))
	     result)))
  (safe-system
   (format nil
	   *cc-format*
	   *cc* (>= *speed* 2) *cc-optimize*
	   (namestring c-pathname)
	   (namestring o-pathname))
; Since the SUN4 assembler loops with big files, you might want to use this:
;   (format nil
;	   "~A ~@[~*-O1~] -S -I. -I~A -w ~A ; as -o ~A ~A"
;	   *cc* (>= *speed* 2)
;          *include-directory*
;	   (namestring c-pathname)
;	   (namestring o-pathname)
;	   (namestring s-pathname))
   )))

(defun cat-data-file (o-pathname data-pathname)
  (with-open-file (o-file (namestring o-pathname)
			  :direction :output
			  :if-exists :append)
    ;; cat data-file >> o-file
    (with-open-file (data-file (namestring data-pathname))
      (do ((buffer (make-string 256))
	   (n 0))
	  ((zerop (setq n (sys::read-bytes data-file buffer 0 256))))
	(declare (fixnum n))
	(sys::write-bytes o-file buffer 0 n)))))

(defun print-compiler-info ()
  (format t "~&;;; OPTIMIZE levels: Safety=~d~:[ (No runtime error checking)~;~], Space=~d, Speed=~d~%"
          (cond ((null *compiler-check-args*) 0)
                ((null *safe-compile*) 1)
                ((null *compiler-push-events*) 2)
                (t 3))
          *safe-compile* *space* *speed*))

;;; ----------------------------------------------------------------------
(provide "compiler")
