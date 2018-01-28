;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package 'lisp)


(export '(check-type assert
          ecase ccase typecase etypecase ctypecase))


(in-package 'system)


(proclaim '(optimize (safety 2) (space 3)))


(defmacro check-type (place typespec &optional (string nil s))
  `(do ((*print-level* 4)
        (*print-length* 4))
       ((typep ,place ',typespec) nil)
       (cerror ""
               "The value of ~:@(~S~), ~:@(~S~), is not ~A."
               ',place ,place
               ,(if s string `',typespec))
       ,(ask-for-form place)
       (format *error-output* "Now continuing ...~%")))


(defmacro assert (test-form &optional places string &rest args)
  `(do ((*print-level* 4)
        (*print-length* 4))
       (,test-form nil)
       ,(if string
            `(cerror "" ,string ,@args)
            `(cerror "" "The assertion ~:@(~S~) is failed." ',test-form))
       ,@(mapcar #'ask-for-form places)
       (format *error-output* "Now continuing ...~%")))


(defun ask-for-form (place)
  `(progn (format  *error-output*
                   "Please input the new value for the place ~:@(~S~): "
                   ',place)
          (finish-output *error-output*)
          (setf ,place (read))))


(defmacro ecase (keyform &rest clauses &aux (key (gensym)))
   (do ((l (reverse clauses) (cdr l))
        (form `(let ((*print-level* 4)
                     (*print-length* 4))
                 (error
                  "The value of ~:@(~S~), ~:@(~S~), is ~
                  ~#[nonsense~;not ~:@(~S~)~;neither ~:@(~S~) nor ~:@(~S~)~
                  ~:;not ~@{~#[~;or ~]~:@(~S~)~^, ~}~]."
                  ',keyform
                  ,key
                  ,@(mapcan #'(lambda (x)
                                (if (listp (car x))
                                    (mapcar #'(lambda (y) `',y) (car x))
                                    `(',(car x))))
                            clauses)))))
       ((endp l) `(let ((,key ,keyform)) ,form))
       (when (caar l)
             (setq form `(if ,(if (listp (caar l))
                                  `(member ,key ',(caar l))
                                  `(eql ,key ',(caar l)))
                             (progn ,@(cdar l))
                             ,form))))
)

(defmacro ccase (keyplace &rest clauses &aux (key (gensym)))
   `(loop (let ((,key ,keyplace))
               ,@(mapcar #'(lambda (l)
                                  `(when ,(if (listp (car l))
                                              `(member ,key ',(car l))
                                              `(eql ,key ',(car l)))
                                         (return (progn ,@(cdr l)))))
                         clauses)
               (let ((*print-level* 4)
                     (*print-length* 4))
                    (cerror ""
                            "The value of ~:@(~S~), ~:@(~S~), is ~
                             ~#[nonsense~;not ~:@(~S~)~;neither ~
                             ~:@(~S~) nor ~:@(~S~)~
                             ~:;not ~@{~#[~;or ~]~:@(~S~)~^, ~}~]."
                             ',keyplace
                             ,key
                             ,@(mapcan
                                #'(lambda (x)
                                         (if (listp (car x))
                                             (mapcar #'(lambda (y) `',y)
                                                     (car x))
                                             `(',(car x))))
                                clauses))
                    ,(ask-for-form keyplace)
                    (format *error-output* "Now continuing ...~%"))))
   )

(defmacro typecase (keyform &rest clauses)
  (do ((l (reverse clauses) (cdr l))
       (form nil) (key (gensym)))
      ((endp l) `(let ((,key ,keyform)) ,form))
      (if (or (eq (caar l) 't) (eq (caar l) 'otherwise))
          (setq form `(progn ,@(cdar l)))
          (setq form
                `(if (typep ,key (quote ,(caar l)))
                     (progn ,@(cdar l))
                     ,form))))
  )

(defmacro etypecase (keyform &rest clauses &aux (key (gensym)))
   (do ((l (reverse clauses) (cdr l))	; Beppe
        (form `(error (typecase-error-string
                       ',keyform ,key
                       ',(mapcar #'(lambda (l) (car l)) clauses)))))
       ((endp l) `(let ((,key ,keyform)) ,form))
       (setq form `(if (typep ,key ',(caar l))
                       (progn ,@(cdar l))
                       ,form))
       )
   )

(defmacro ctypecase (keyplace &rest clauses &aux (key (gensym)))
  `(loop (let ((,key ,keyplace))
              ,@(mapcar #'(lambda (l)
                                 `(when (typep ,key ',(car l))
                                        (return (progn ,@(cdr l)))))
                        clauses)
              (cerror ""
                      (typecase-error-string
                       ',keyplace ,key
                       ',(mapcar #'(lambda (l) (car l)) clauses))))
         ,(ask-for-form keyplace)
         (format *error-output* "Now continuing ...~%"))
  )

(defun typecase-error-string
       (keyform keyvalue negs
                &aux (negs1 nil) (poss nil) (poss1 nil))
   (do ()
       ((endp negs))
       (if (symbolp (car negs))
           (progn (push (list (car negs)) negs1) (pop negs))
           (case (caar negs)
                 (or (setq negs (append (cdar negs) (cdr negs))))
                 (member (mapc #'(lambda (x) (push `(member ,x) negs1))
                               (cdar negs))
                         (pop negs))
                 (not (push (cadar negs) poss) (pop negs))
                 (otherwise (push (car negs) negs1) (pop negs)))))
   (do ()
       ((endp poss))
       (cond ((symbolp (car poss)) (push (list (car poss)) poss1) (pop poss))
             ((eq (caar poss) 'and)
              (setq poss (append (cdar poss) (cdr poss))))
             (t (push (car poss) poss1) (pop poss))))
   (format
    nil
    "The value of ~:@(~S~), ~:@(~S~), is ~?~?."
    keyform
    keyvalue
    "~#[~;~;~?~;~;~? and ~?~:;~%~@{~#[~;~;and ~]~?~^, ~}~]"
    (mapcan 'typecase-error-strings poss1)
    "~:[~[something~;~:;~%~]~;~[~:;, but~%~]~]~
     ~#[~;~;not ~?~;~;neither ~? nor ~?~:;not ~@{~#[~;~;or ~]~?~^, ~}~]"
    (cons poss1 (cons (length negs1)
                      (mapcan 'typecase-error-strings (nreverse negs1))))
    )
   )

(defun typecase-error-strings (type)
  (flet ((vowel-p (symbol)
	   (member (elt (symbol-name symbol) 0)
		   '(#\A #\I #\U #\E #\O #\a #\i #\u #\e #\o) :test #'eq)))
    (cond ((eq (car type) 'member)
	   (case (length (cdr type))
	     (0 `("one of none" nil))
	     (1 `("~:@(~S~)" (,(cadr type))))
	     (2 `("either ~:@(~S~) or ~:@(~S~)" ,(cdr type)))
	     (t `("one of ~:@(~S~)" (,(cdr type))))))
	  ((eq (car type) 'satisfies)
	   `("an object satisfying ~:@(~S~)" ,(cdr type)))
	  ((or (endp (cdr type)) (null (remove '* (cdr type))))
	   (let ((x (assoc (car type)
			   '((t "anything")
			     (nil "none")
			     (null "nil")
			     (common "an object of a standard data type")))))
             (if x
                 `(,(cadr x) nil)
                 `("~:[a~;an~] ~(~A~)" (,(vowel-p (car type)) ,(car type))))))
	  (t `("~:[a~;an~] ~:@(~S~)" (,(vowel-p (car type)) ,type)))))
 )

;;; (provide 'assert)
