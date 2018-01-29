;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;        The IO library.

(in-package "SYSTEM")

(defmacro with-open-stream ((var stream) &rest body)
  "Syntax: (with-open-stream (var stream-form) {decl}* {form}*)
Evaluates FORMs with VAR bound to the value of STREAM-FORM.  The stream is
automatically closed on exit."
  (multiple-value-bind (ds b)
      (find-declarations body)
    `(LET ((,var ,stream))
       ,@ds
       (UNWIND-PROTECT
         (PROGN ,@b)
         (CLOSE ,var)))))

(defmacro with-input-from-string ((var string &key index start end) &rest body)
  "Syntax: (with-input-from-string (var string-form {keyword value}*)
           {decl}* {form}*)
Evaluates FORMs with VAR bound to a string input stream from the string that
is the value of STRING-FORM.  The stream is automatically closed on exit.
Possible keywords are :INDEX, :START, and :END."
  (if index
      (multiple-value-bind (ds b)
          (find-declarations body)
        `(LET ((,var (MAKE-STRING-INPUT-STREAM ,string ,start ,end)))
           ,@ds
           (UNWIND-PROTECT
             (PROGN ,@b)
             (SETF ,index (SYS:GET-STRING-INPUT-STREAM-INDEX ,var)))))
      `(LET ((,var (MAKE-STRING-INPUT-STREAM ,string ,start ,end)))
         ,@body)))

(defmacro with-output-to-string ((var &optional string) &rest body)
  "Syntax: (with-output-to-string (var [string-form]) {decl}* {form}*)
Evaluates FORMs with VAR bound to a string output stream to the string that is
the value of STRING-FORM.  If STRING-FORM is not given, a new string is used.
The stream is automatically closed on exit and the string is returned."
  (if string
      `(LET ((,var (MAKE-STRING-OUTPUT-STREAM-FROM-STRING ,string)))
         ,@body)
      `(LET ((,var (MAKE-STRING-OUTPUT-STREAM)))
         ,@body
         (GET-OUTPUT-STREAM-STRING ,var))))

(defun read-from-string (string
                         &optional (eof-error-p t) eof-value
                         &key (start 0) (end (length string))
                              preserve-whitespace)
  "Args: (string &optional (eof-error-p t) (eof-value nil)
              &key (start 0) (end (length string)) (preserve-whitespace nil))
Reads an object from STRING and returns the object.  As the second value,
returns the index to the character next to the object's representation.
PRESERVE-WHITESPACE specifies whether to leave the character next to the
object's representation."
  (let ((stream (make-string-input-stream string start end)))
    (if preserve-whitespace
        (values (read-preserving-whitespace stream eof-error-p eof-value)
                (sys::get-string-input-stream-index stream))
        (values (read stream eof-error-p eof-value)
                (sys::get-string-input-stream-index stream)))))

(defun write-to-string (object &rest rest
                        &key escape radix base
                             circle pretty level length
                             case gensym array
                        &aux (stream (make-string-output-stream)))
  "Args: (object &key (escape *print-escape*) (radix *print-radix*)
                   (base *print-base*) (circle *print-circle*)
                   (pretty *print-pretty*) (level *print-level*)
                   (length *print-length*) (case *print-case*)
                   (array *print-array*) (gensym *print-gensym*))
Returns as a string the printed representation of OBJECT in the specified
mode.  See the variable docs of *PRINT-...* for the mode."
  (declare (ignore escape radix base
                   circle pretty level length
                   case gensym array))
  (apply #'write object :stream stream rest)
  (get-output-stream-string stream))

(defun prin1-to-string (object
                        &aux (stream (make-string-output-stream)))
  "Args: (object)
PRIN1s OBJECT to a new string and returns the result.  Equivalent to
(WRITE-TO-STRING OBJECT :ESCAPE T)."
   (prin1 object stream)
   (get-output-stream-string stream))

(defun princ-to-string (object
                        &aux (stream (make-string-output-stream)))
  "Args: (object)
PRINCs OBJECT to a new string and returns the result.  Equivalent to
(WRITE-TO-STRING OBJECT :ESCAPE NIL)."
  (princ object stream)
  (get-output-stream-string stream))

(defmacro with-open-file ((stream . filespec) &rest body)
  "Syntax: (with-open-file (var filespec-form {options}*) {decl}* {form}*)
Opens the specified file using OPTIONs, and evaluates FORMs with VAR bound to
a stream to/from the file.  The file is automatically closed on exit.  See
OPEN for the options."
  (multiple-value-bind (ds b)
      (find-declarations body)
    `(LET ((,stream (OPEN ,@filespec)))
       ,@ds
       (UNWIND-PROTECT
         (MULTIPLE-VALUE-PROG1 (PROGN ,@b) (WHEN ,stream (CLOSE ,stream)))
         (WHEN ,stream (CLOSE ,stream :ABORT T))))))

(defun y-or-n-p (&optional string &rest args)
  "Args: (&optional format-string &rest args)
Asks the user a Y-or-N question.  Does FRESH-LINE, prints a message as if
FORMAT-STRING and ARGs were given to FORMAT, and then prints \"(Y or N)\" is
printed.  If FORMAT-STRING is NIL, however, no prompt will appear."
  (do ((reply))
      (nil)
    (when string (format *query-io* "~&~?  (Y or N) " string args))
    (setq reply (read *query-io*))
    (cond ((string-equal (symbol-name reply) "Y")
           (return-from y-or-n-p t))
          ((string-equal (symbol-name reply) "N")
           (return-from y-or-n-p nil)))))

(defun yes-or-no-p (&optional string &rest args)
  "Args: (&optional format-string &rest args)
Asks the user an YES-or-NO question.  Does FRESH-LINE, prints a message as if
FORMAT-STRING and ARGs were given to FORMAT, and then prints \"(Y or N)\" is
printed.  If FORMAT-STRING is NIL, however, no prompt will appear."
  (do ((reply))
      (nil)
    (when string (format *query-io* "~&~?  (Yes or No) " string args))
    (setq reply (read *query-io*))
    (cond ((string-equal (symbol-name reply) "YES")
           (return-from yes-or-no-p t))
          ((string-equal (symbol-name reply) "NO")
           (return-from yes-or-no-p nil)))))

(defun sharp-a-reader (stream subchar arg)
  (declare (ignore subchar))
  (let ((initial-contents (read stream nil nil t)))
    (if *read-suppress*
        nil
        (do ((i 0 (1+ i))
             (d nil (cons (length ic) d))
             (ic initial-contents (if (zerop (length ic)) ic (elt ic 0))))
            ((>= i arg)
             (make-array (nreverse d) :initial-contents initial-contents))
	  (declare (fixnum i))))))

(set-dispatch-macro-character #\# #\a 'sharp-a-reader)
(set-dispatch-macro-character #\# #\A 'sharp-a-reader)

(defun sharp-s-reader (stream subchar arg)
  (declare (ignore subchar))
  (when (and arg (null *read-suppress*))
        (error "~S is an extra argument for the #s readmacro." arg))
  (let ((l (read stream)))
    (when *read-suppress*
      (return-from sharp-s-reader nil))
    (unless (get-sysprop (car l) 'is-a-structure)
            (error "~S is not a structure." (car l)))
    ;; Intern keywords in the keyword package.
    (do ((ll (cdr l) (cddr ll)))
        ((endp ll)
         ;; Find an appropriate construtor.
         (do ((cs (get-sysprop (car l) 'structure-constructors) (cdr cs)))
             ((endp cs)
              (error "The structure ~S has no structure constructor."
                     (car l)))
           (when (symbolp (car cs))
                 (return (apply (car cs) (cdr l))))))
      (rplaca ll (intern (string (car ll)) 'keyword)))))

(set-dispatch-macro-character #\# #\s 'sharp-s-reader)
(set-dispatch-macro-character #\# #\S 'sharp-s-reader)

(defvar *dribble-stream* nil)
(defvar *dribble-io* nil)
(defvar *dribble-namestring* nil)
(defvar *dribble-saved-terminal-io* nil)

(defun dribble (&optional (pathname "DRIBBLE.LOG" psp) (f :supersede))
  "Args: (&optional filespec)
If FILESPEC is given, starts recording the interaction to the specified file.
FILESPEC may be a symbol, a string, a pathname, or a file stream.  If FILESPEC
is not given, ends the recording."
  (cond ((not psp)
         (when (null *dribble-stream*) (error "Not in dribble."))
         (if (eq *dribble-io* *terminal-io*)
             (setq *terminal-io* *dribble-saved-terminal-io*)
             (warn "*TERMINAL-IO* was rebound while DRIBBLE is on.~%~
                   You may miss some dribble output."))
         (close *dribble-stream*)
         (setq *dribble-stream* nil)
         (format t "~&Finished dribbling to ~A." *dribble-namestring*))
        (*dribble-stream*
         (error "Already in dribble (to ~A)." *dribble-namestring*))
        (t
         (let* ((namestring (namestring pathname))
                (stream (open pathname :direction :output
                                       :if-exists f
                                       :if-does-not-exist :create)))
           (setq *dribble-namestring* namestring
                 *dribble-stream* stream
                 *dribble-saved-terminal-io* *terminal-io*
                 *dribble-io* (make-two-way-stream
                               (make-echo-stream *terminal-io* stream)
                               (make-broadcast-stream *terminal-io* stream))
                 *terminal-io* *dribble-io*)
           (multiple-value-bind (sec min hour day month year)
               (get-decoded-time)
             (format t "~&Starts dribbling to ~A (~d/~d/~d, ~d:~d:~d)."
                     namestring year month day hour min sec))))))

;(provide 'iolib)

(defmacro with-standard-io-syntax (&body body)
  "Syntax: ({forms}*)
The forms of the body are executed in a print environment that corresponds to
the one defined in the ANSI standard. *print-base* is 10, *print-array* is t,
*package* is \"CL-USER\", etc."
  `(let*((*package* (find-package :cl-user))
	 (*print-array* t)
	 (*print-base* 10)
	 (*print-case* :upcase)
	 (*print-circle* nil)
	 (*print-escape* t)
	 (*print-gensym* t)
	 (*print-length* nil)
	 (*print-level* nil)
	 (*print-lines* nil)
	 (*print-miser-width* nil)
	 (*print-pretty* nil)
	 (*print-radix* nil)
	 (*print-readably* t)
	 (*print-right-margin* nil)
	 (*read-base* 10)
	 (*read-default-float-format* 'single-float)
	 (*read-eval* t)
	 (*read-suppress* nil)
	 (*readtable* (copy-readtable (si::standard-readtable))))
    ,@body))
