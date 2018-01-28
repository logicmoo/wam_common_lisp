;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;                           DESCRIBE and INSPECT


(in-package 'lisp)

#+AKCL (import 'sys::arglist 'lisp)

(export '(arglist describe inspect))
(export '(documentation variable function structure type setf))

(in-package 'system)

(proclaim '(optimize (safety 2) (space 3)))

(defvar *inspect-level* 0)
(defvar *inspect-history* nil)
(defvar *inspect-mode* nil)

(defvar *old-print-level* nil)
(defvar *old-print-length* nil)


(defun inspect-read-line ()
  (do ((char (read-char *query-io*) (read-char *query-io*)))
      ((or (char= char #\Newline) (char= char #\Return)))))

(defun select-P (object)
  (let ((*print-pretty* t) (*print-level* nil) (*print-length* nil))
       (prin1 object)
       (terpri)))

(defun select-E ()
  (dolist (x (multiple-value-list
	       (multiple-value-prog1
		 (eval (read-preserving-whitespace *query-io*))
		 (inspect-read-line))))
	  (write x
		 :level *old-print-level*
		 :length *old-print-length*)
	  (terpri)))

(defun select-U ()
  (prog1
    (eval (read-preserving-whitespace *query-io*))
    (inspect-read-line)))

(defun select-? ()
  (terpri)
  (format t
	  "Inspect commands:~%~
                n (or N or Newline):    inspects the field (recursively).~%~
                s (or S):               skips the field.~%~
                p (or P):               pretty-prints the field.~%~
                a (or A):               aborts the inspection ~
                                        of the rest of the fields.~%~
                u (or U) form:          updates the field ~
                                        with the value of the form.~%~
                e (or E) form:          evaluates and prints the form.~%~
                q (or Q):               quits the inspection.~%~
                ?:                      prints this.~%~%"))

(defun read-inspect-command (label object allow-recursive)
  (unless *inspect-mode*
    (inspect-indent-1)
    (if allow-recursive
        (progn (princ label) (inspect-object object))
        (format t label object))
    (return-from read-inspect-command nil))
  (loop
    (inspect-indent-1)
    (if allow-recursive
        (progn (princ label)
               (inspect-indent)
               (prin1 object))
        (format t label object))
    (write-char #\Space)
    (force-output)
    (case (do ((char (read-char *query-io*) (read-char *query-io*)))
              ((and (char/= char #\Space) (char/= #\Tab)) char))
      ((#\Newline #\Return)
       (when allow-recursive (inspect-object object))
       (return nil))
      ((#\n #\N)
       (inspect-read-line)
       (when allow-recursive (inspect-object object))
       (return nil))
      ((#\s #\S)
       (inspect-read-line)
       (return nil))
      ((#\p #\P)
       (inspect-read-line)
       (select-P object))
      ((#\a #\A)
       (inspect-read-line)
       (throw 'ABORT-INSPECT nil))
      ((#\u #\U)
       (return (values t (select-U))))
      ((#\e #\E)
       (select-E))
      ((#\q #\Q)
       (inspect-read-line)
       (throw 'QUIT-INSPECT nil))
      ((#\?)
	(inspect-read-line)
	(select-?))
      (t
        (inspect-read-line))
      )))

(defmacro inspect-recursively (label object &optional place)
  (if place
      `(multiple-value-bind (update-flag new-value)
            (read-inspect-command ,label ,object t)
         (when update-flag (setf ,place new-value)))
      `(when (read-inspect-command ,label ,object t)
             (princ "Not updated.")
             (terpri))))

(defmacro inspect-print (label object &optional place)
  (if place
      `(multiple-value-bind (update-flag new-value)
           (read-inspect-command ,label ,object nil)
         (when update-flag (setf ,place new-value)))
      `(when (read-inspect-command ,label ,object nil)
             (princ "Not updated.")
             (terpri))))
          
(defun inspect-indent ()
  (fresh-line)
  (format t "~V@T"
          (* 4 (if (< *inspect-level* 8) *inspect-level* 8))))

(defun inspect-indent-1 ()
  (fresh-line)
  (format t "~V@T"
          (- (* 4 (if (< *inspect-level* 8) *inspect-level* 8)) 3)))


(defun inspect-symbol (symbol)
  (let ((p (symbol-package symbol)))
    (cond ((null p)
           (format t "~:@(~S~) - uninterned symbol" symbol))
          ((eq p (find-package "KEYWORD"))
           (format t "~:@(~S~) - keyword" symbol))
          (t
           (format t "~:@(~S~) - ~:[internal~;external~] symbol in ~A package"
                   symbol
                   (multiple-value-bind (b f)
                                        (find-symbol (symbol-name symbol) p)
                     (declare (ignore b))
                     (eq f :external))
                   (package-name p)))))

  (when (boundp symbol)
        (if *inspect-mode*
            (inspect-recursively "value:"
                                 (symbol-value symbol)
                                 (symbol-value symbol))
            (inspect-print "value:~%   ~S"
                           (symbol-value symbol)
                           (symbol-value symbol))))

  (do ((pl (symbol-plist symbol) (cddr pl)))
      ((endp pl))
    (unless (and (symbolp (car pl))
                 (or (eq (symbol-package (car pl)) (find-package 'SYSTEM))
                     (eq (symbol-package (car pl)) (find-package 'COMPILER))))
      (if *inspect-mode*
          (inspect-recursively (format nil "property ~S:" (car pl))
                               (cadr pl)
                               (get symbol (car pl)))
          (inspect-print (format nil "property ~:@(~S~):~%   ~~S" (car pl))
                         (cadr pl)
                         (get symbol (car pl))))))
  
  (when (print-doc symbol t)
        (format t "~&-----------------------------------------------------------------------------~%"))
  )

(defun inspect-package (package)
  (format t "~S - package" package)
  (when (package-nicknames package)
        (inspect-print "nicknames:  ~S" (package-nicknames package)))
  (when (package-use-list package)
        (inspect-print "use list:  ~S" (package-use-list package)))
  (when  (package-used-by-list package)
         (inspect-print "used-by list:  ~S" (package-used-by-list package)))
  (when (package-shadowing-symbols package)
        (inspect-print "shadowing symbols:  ~S"
                       (package-shadowing-symbols package))))

(defun inspect-character (character)
  (format t
          (cond ((standard-char-p character) "~S - standard character")
                ((string-char-p character) "~S - string character")
                (t "~S - character"))
          character)
  (inspect-print "code:  #x~X" (char-code character))
  (inspect-print "bits:  ~D" (char-bits character))
  (inspect-print "font:  ~D" (char-font character)))

(defun inspect-number (number)
  (case (type-of number)
    (FIXNUM (format t "~S - fixnum (32 bits)" number))
    (BIGNUM (format t "~S - bignum" number))
    (RATIO
     (format t "~S - ratio" number)
     (inspect-recursively "numerator:" (numerator number))
     (inspect-recursively "denominator:" (denominator number)))
    (COMPLEX
     (format t "~S - complex" number)
     (inspect-recursively "real part:" (realpart number))
     (inspect-recursively "imaginary part:" (imagpart number)))
    ((SHORT-FLOAT SINGLE-FLOAT)
     (format t "~S - short-float" number)
     (multiple-value-bind (signif expon sign)
          (integer-decode-float number)
       (declare (ignore sign))
       (inspect-print "exponent:  ~D" expon)
       (inspect-print "mantissa:  ~D" signif)))
    ((LONG-FLOAT DOUBLE-FLOAT)
     (format t "~S - long-float" number)
     (multiple-value-bind (signif expon sign)
          (integer-decode-float number)
       (declare (ignore sign))
       (inspect-print "exponent:  ~D" expon)
       (inspect-print "mantissa:  ~D" signif)))))

(defun inspect-cons (cons)
  (format t
          (case
	      #-LOCATIVE (car cons)
	      #+LOCATIVE
	      (let ((acar (car cons)))
		(cond ((locativep acar)
		       (dereference acar))
		      ((sl-boundp acar) acar)
		      (t nil)))
            ((LAMBDA LAMBDA-BLOCK LAMBDA-CLOSURE LAMBDA-BLOCK-CLOSURE)
             "~S - function")
            (QUOTE "~S - constant")
            (t "~S - cons"))
          cons)
  (when *inspect-mode*
        (do ((i 0 (1+ i))
             (l cons (cdr l)))
            ((atom l)
             (inspect-recursively (format nil "nthcdr ~D:" i)
                                  l (cdr (nthcdr (1- i) cons))))
          (inspect-recursively (format nil "nth ~D:" i)
                               (car l) (nth i cons)))))

(defun inspect-string (string)
  (format t (if (simple-string-p string) "~S - simple string" "~S - string")
          string)
  (inspect-print  "dimension:  ~D"(array-dimension string 0))
  (when (array-has-fill-pointer-p string)
        (inspect-print "fill pointer:  ~D"
                       (fill-pointer string)
                       (fill-pointer string)))
  (when *inspect-mode*
        (dotimes (i (array-dimension string 0))
                 (inspect-recursively (format nil "aref ~D:" i)
                                      (char string i)
                                      (char string i)))))

(defun inspect-vector (vector)
  (format t (if (simple-vector-p vector) "~S - simple vector" "~S - vector")
          vector)
  (inspect-print  "dimension:  ~D" (array-dimension vector 0))
  (when (array-has-fill-pointer-p vector)
        (inspect-print "fill pointer:  ~D"
                       (fill-pointer vector)
                       (fill-pointer vector)))
  (when *inspect-mode*
        (dotimes (i (array-dimension vector 0))
                 (inspect-recursively (format nil "aref ~D:" i)
                                      (aref vector i)
                                      (aref vector i)))))

(defun inspect-array (array)
  (format t (if (adjustable-array-p array)
                "~S - adjustable aray"
                "~S - array")
          array)
  (inspect-print "rank:  ~D" (array-rank array))
  (inspect-print "dimensions:  ~D" (array-dimensions array))
  (inspect-print "total size:  ~D" (array-total-size array)))

(defun select-ht-N (hashtable)
  (incf *inspect-level*)
  (maphash #'(lambda (key val)
	       (inspect-indent-1)
	       (format t "key  : ~S" key)
	       (inspect-recursively "value:" val (gethash key hashtable)))
	   hashtable)
  (decf *inspect-level*))

(defun select-ht-L (hashtable)
  (terpri)
  (format t "The keys of the hash table are:~%")
  (maphash #'(lambda (key val)
	       (declare (ignore val))
	       (format t "  ~S~%" key))
	   hashtable)
  (terpri))

(defun select-ht-J (hashtable)
  (let* ((key (prog1
		(read-preserving-whitespace *query-io*)
		(inspect-read-line)))
	 (val (gethash key hashtable)))
        (if val
	    (progn
	      (incf *inspect-level*)
	      (inspect-indent-1)
	      (format t "key  : ~S" key)
	      (inspect-recursively "value:" val (gethash key hashtable))
	      (decf *inspect-level*))
	    (progn
	      (terpri)
	      (format t "The key ~S is not present or the value associated is NIL." key)
	      (terpri)
	      (terpri)))))

(defun select-ht-? ()
  (terpri)
  (format t
	  "Inspect commands for hash tables:~%~
n (or N or Newline):  inspects the keys/values of the hashtable (recursively).~%~
s (or S):             skips the field.~%~
p (or P):             pretty-prints the field.~%~
a (or A):             aborts the inspection of the rest of the fields.~%~
e (or E) form:        evaluates and prints the form.~%~
l (or L):             show the keys of the hash table.~%~
j (or J) key:         inspect the value associated to the key requested.~%~
q (or Q):             quits the inspection.~%~
?:                    prints this.~%~%"
	  ))

(defun inspect-hashtable (hashtable)
  (if *inspect-mode*
      (progn
	(decf *inspect-level*)
        (loop
          (format t "~S - hash table: " hashtable)
	  (force-output)
          (case (do ((char (read-char *query-io*) (read-char *query-io*)))
	            ((and (char/= char #\Space) (char/= #\Tab)) char))
	        ((#\Newline #\Return)
		 (select-ht-N hashtable)
		 (return nil))
	        ((#\n #\N)
	         (inspect-read-line)
		 (select-ht-N hashtable)
		 (return nil))
	        ((#\s #\S)
	         (inspect-read-line)
	         (return nil))
		((#\p #\P)
		 (inspect-read-line)
		 (select-P hashtable))
		((#\a #\A)
		 (inspect-read-line)
		 (throw 'ABORT-INSPECT nil))
		((#\e #\E)
		 (select-E))
		((#\q #\Q)
		 (inspect-read-line)
		 (throw 'QUIT-INSPECT nil))
		((#\l #\L)
		 (inspect-read-line)
		 (select-ht-L hashtable))
		((#\j #\J)
		 (select-ht-J hashtable))
		((#\?)
		 (inspect-read-line)
		 (select-ht-?)))
          (inspect-indent)))
      (progn
	(format t "~S - hash table: " hashtable)
	(maphash #'(lambda (key val)
		     (inspect-indent-1)
		     (format t "key  : ~S" key)
		     (inspect-indent-1)
		     (format t "value:")
		     (inspect-object val))
	         hashtable))))

#+CLOS
(defun inspect-instance (instance)
  (if *inspect-mode*
      (clos::inspect-obj instance)
      (clos::describe-object instance)))

(defun inspect-object (object &aux (*inspect-level* *inspect-level*))
  (inspect-indent)
  (when (and (not *inspect-mode*)
             (or (> *inspect-level* 5)
                 (member object *inspect-history*)))
        (prin1 object)
        (return-from inspect-object))
  (incf *inspect-level*)
  (push object *inspect-history*)
  (catch 'ABORT-INSPECT
         (cond
	       #+LOCATIVE
               ((not (sys:sl-boundp object)) nil)
	       ((symbolp object) (inspect-symbol object))
               ((packagep object) (inspect-package object))
               ((characterp object) (inspect-character object))
               ((numberp object) (inspect-number object))
               ((consp object) (inspect-cons object))
               ((stringp object) (inspect-string object))
               ((vectorp object) (inspect-vector object))
               ((arrayp object) (inspect-array object))
               ((hash-table-p object) (inspect-hashtable object))
	       #+clos
	       ((sys:instancep object) (inspect-instance object))
	       #+LOCATIVE
	       ((sys:locativep object) (inspect-locative object))
               (t (format t "~S - ~S" object (type-of object))))))


(defun describe (object &aux (*inspect-mode* nil)
                             (*inspect-level* 0)
                             (*inspect-history* nil)
                             (*print-level* nil)
                             (*print-length* nil))
  "The lisp function DESCRIBE."
  (terpri)
  (catch 'QUIT-INSPECT (inspect-object object))
  (terpri)
  (values))

(defun inspect (object &aux (*inspect-mode* t)
                            (*inspect-level* 0)
                            (*inspect-history* nil)
                            (*old-print-level* *print-level*)
                            (*old-print-length* *print-length*)
                            (*print-level* 3)
                            (*print-length* 3))
  "The lisp function INSPECT."
  (read-line)
  (princ "Type ? and a newline for help.")
  (terpri)
  (catch 'QUIT-INSPECT (inspect-object object))
  (terpri)
  (values))

(defun print-doc (symbol &optional (called-from-apropos-doc-p nil)
                         &aux (f nil) x (*notify-gbc* nil))
  (flet ((doc1 (doc ind) ; &aux (arglist (get symbol 'ARGLIST))
           (setq f t)
           (format t
                   "~&-----------------------------------------------------------------------------~%~53S~24@A~%~A"
                   symbol ind doc))
         (good-package ()
           (if (eq (symbol-package symbol) (find-package "LISP"))
               (find-package "SYSTEM")
               *package*)))

    (cond ((special-form-p symbol)
           (doc1 (or (documentation symbol 'FUNCTION) "")
                 (if (macro-function symbol)
                     "[Special form and Macro]"
                     "[Special form]")))
          ((macro-function symbol)
           (doc1 (or (documentation symbol 'FUNCTION) "") "[Macro]"))
          ((fboundp symbol)
           (doc1
            (or (documentation symbol 'FUNCTION)
                (if (consp (setq x (symbol-function symbol)))
                    (case (car x)
                          (LAMBDA (format nil "~%Args: ~S" (cadr x)))
                          (LAMBDA-BLOCK (format nil "~%Args: ~S" (caddr x)))
                          (LAMBDA-CLOSURE
                           (format nil "~%Args: ~S" (car (cddddr x))))
                          (LAMBDA-BLOCK-CLOSURE
                           (format nil "~%Args: ~S" (cadr (cddddr x))))
                          (t ""))
                    ""))
            "[Function]"))
          ((setq x (documentation symbol 'FUNCTION))
           (doc1 x "[Macro or Function]")))

    (cond ((constantp symbol)
           (unless (and (eq (symbol-package symbol) (find-package "KEYWORD"))
                        (null (documentation symbol 'VARIABLE)))
             (doc1 (or (documentation symbol 'VARIABLE) "") "[Constant]")))
          ((sys:specialp symbol)
           (doc1 (or (documentation symbol 'VARIABLE) "")
                 "[Special variable]"))
          ((or (setq x (documentation symbol 'VARIABLE)) (boundp symbol))
           (doc1 (or x "") "[Variable]")))

    (cond ((setq x (documentation symbol 'TYPE))
           (doc1 x "[Type]"))
          ((setq x (get symbol 'DEFTYPE-FORM))
           (let ((*package* (good-package)))
             (doc1 (format nil "~%Defined as: ~S~%See the doc of DEFTYPE." x)
                   "[Type]"))))

    (cond ((setq x (documentation symbol 'STRUCTURE))
           (doc1 x "[Structure]"))
          ((setq x (get symbol 'DEFSTRUCT-FORM))
           (doc1 (format nil "~%Defined as: ~S~%See the doc of DEFSTRUCT." x)
                 "[Structure]")))

    (cond ((setq x (documentation symbol 'SETF))
           (doc1 x "[Setf]"))
          ((setq x (get symbol 'SETF-UPDATE-FN))
           (let ((*package* (good-package)))
             (doc1 (format nil "~%Defined as: ~S~%See the doc of DEFSETF."
                           `(defsetf ,symbol ,(get symbol 'SETF-UPDATE-FN)))
                   "[Setf]")))
          ((setq x (get symbol 'SETF-LAMBDA))
           (let ((*package* (good-package)))
             (doc1 (format nil "~%Defined as: ~S~%See the doc of DEFSETF."
                           `(defsetf ,symbol ,@(get symbol 'SETF-LAMBDA)))
                   "[Setf]")))
          ((setq x (get symbol 'SETF-METHOD))
           (let ((*package* (good-package)))
             (doc1
              (format nil
                "~@[~%Defined as: ~S~%See the doc of DEFINE-SETF-METHOD.~]"
                (if (consp x)
                    (case (car x)
                          (LAMBDA `(define-setf-method ,@(cdr x)))
                          (LAMBDA-BLOCK `(define-setf-method ,@(cddr x)))
                          (LAMBDA-CLOSURE `(define-setf-method ,@(cddddr x)))
                          (LAMBDA-BLOCK-CLOSURE
                           `(define-setf-method ,@(cdr (cddddr x))))
                          (t nil))
                    nil))
            "[Setf]"))))
    )

  ;; Format of entries in file help.doc:
  ;; ^_[F | V | T]<name>
  ;; description
  ;; [@[F | V | T]<name>
  ;; other description]
  ;;
  ;; where F means Function, V Variable and T Type.
  ;;
  (let* ((name (symbol-name symbol))
	 (path (merge-pathnames *system-directory* "help.doc"))
	 (pos 0))

    (labels ((bin-search (file start end &aux (delta 0) (middle 0) sym)
	       (declare (fixnum start end delta middle))
	       (when (< start end)
		 (setq middle (round (+ start end) 2))
		 (file-position file middle)
		 (if (and (plusp (setq delta (scan-for #\^_ file)))
			  (<= delta (- end middle)))
		     (if (string-equal name
				       (setq sym (symbol-name (read file))))
			 (+ middle delta (length name) 1) ; skip EOL
			 (if (string< name sym)
			     (bin-search file start (1- middle))
			     (bin-search file (+ middle delta) end)))
		     (bin-search file start (1- middle)))))
	     (scan-for (char file)
	       (do ((v #\space (read-char file nil nil))
		    (n 0 (1+ n)))
		   ((eql v #\^_)
		    (if (read-char file nil nil) n -1))	; skip V | F | T.
		 (declare (fixnum n)))))

      (if (probe-file path)
	  (with-open-file (file path)
	    (setq pos (bin-search file 0 (file-length file)))
	    (when pos
	      (setq f t)
	      (file-position file pos)
	      (do (v)
		  ((eql (setq v (read-char file nil #\^_)) #\^_))
		(if (eql v #\ )
		    (progn
		      (terpri)
		      (read-char file nil nil))	; skip V | F | T.
		    (princ v)))))
	  (format t "~&Cannot find the help file \"help.doc\""))))

  (if called-from-apropos-doc-p
      f
      (progn (if f
                 (format t "~&-----------------------------------------------------------------------------")
                 (format t "~&No documentation for ~:@(~S~)." symbol))
             (values))))

;(defun arglist (symbol)
;  (get symbol 'ARGLIST))

(defun apropos-doc (string &optional (package 'LISP) &aux (f nil))
  (setq string (string string))
  (if package
      (do-symbols (symbol package)
        (when (substringp string (string symbol))
          (setq f (or (print-doc symbol t) f))))
      (do-all-symbols (symbol)
        (when (substringp string (string symbol))
          (setq f (or (print-doc symbol t) f)))))
  (if f
      (format t "~&-----------------------------------------------------------------------------")
      (format t "~&No documentation for ~S in ~:[any~;~A~] package."
              string package
              (and package (package-name (coerce-to-package package)))))
  (values))

#+LOCATIVE
(defun inspect-locative (locative)
  (if (sys:sl-boundp (dereference locative))
      (if *inspect-mode*
	  (inspect-recursively "locative pointing to:"
			       (dereference locative))
	  (if (locativep (dereference locative))
	      (format t "~S - ~S" locative "UNBOUND-LOCATIVE")
	      (inspect-print "locative pointing to:~%   ~S"
			     (dereference locative)
			     )))
      (format t "~S - ~S" locative "UNBOUND-LOCATIVE")))

;;;----------------------------------------------------------------------
          
(defun documentation (symbol doc-type)
  (case doc-type
    (VARIABLE (get symbol 'VARIABLE-DOCUMENTATION))
    (FUNCTION (get symbol 'FUNCTION-DOCUMENTATION))
    (STRUCTURE (get symbol 'STRUCTURE-DOCUMENTATION))
    (TYPE (get symbol 'TYPE-DOCUMENTATION))
    (SETF (get symbol 'SETF-DOCUMENTATION))
    (t (error "~S is an illegal documentation type." doc-type))))

(defun find-documentation (body)
  (unless (or (endp body) (endp (cdr body)))
    (let ((form (macroexpand (car body))))
      (if (stringp form)
	  form
	  (when (and (consp form)
                     (eq (car form) 'DECLARE))
	    (find-documentation (cdr body)))))))

;(provide 'describe)
