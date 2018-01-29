;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;        The structure routines.


(in-package "COMMON-LISP")
(export 'defstruct)

(in-package "SYSTEM")

(proclaim '(optimize (safety 2) (space 3)))


(defun make-access-function (name conc-name type named slot-descr)
  (declare (ignore named))
  (let* ((slot-name (nth 0 slot-descr))
	 ;; (default-init (nth 1 slot-descr))
	 ;; (slot-type (nth 2 slot-descr))
	 (read-only (nth 3 slot-descr))
	 (offset (nth 4 slot-descr))
	 (access-function (intern (string-concatenate (string conc-name)
							 (string slot-name)))))
    (cond ((null type)
           ;; If TYPE is NIL,
           ;;  the slot is at the offset in the structure-body.
	   (fset access-function #'(lambda (x)
				     (sys:structure-ref x name offset))))
          ((or (eq type 'VECTOR)
               (and (consp type)
                    (eq (car type) 'VECTOR)))
	   ;; If TYPE is VECTOR or (VECTOR ... ), ELT is used.
           (fset access-function
		 #'(lambda (x) (elt x offset))))
          ((eq type 'LIST)
           ;; If TYPE is LIST, NTH is used.
	   (fset access-function
		 #'(lambda (x) (sys:list-nth offset x))))
          (t (error "~S is an illegal structure type." type)))
    (if read-only
	(progn
	  (rem-sysprop access-function 'SETF-UPDATE-FN)
	  (rem-sysprop access-function 'SETF-LAMBDA)
	  (rem-sysprop access-function 'SETF-DOCUMENTATION))
	(progn
	  ;; The following is used by the compiler to expand inline   ;; the accessor
	  (put-sysprop-r access-function (cons (or type name) offset)
		      'STRUCTURE-ACCESS)))))
  		     

(defun illegal-boa ()
  (error "An illegal BOA constructor."))


(defun make-predicate (name type named name-offset)
  (cond ((null type)
	 #'(lambda (x)
	     (structure-subtype-p x name)))
        ((or (eq type 'VECTOR)
             (and (consp type) (eq (car type) 'VECTOR)))
         ;; The name is at the NAME-OFFSET in the vector.
         (unless named (error "The structure should be named."))
	 #'(lambda (x)
	     (and (vectorp x)
		  (> (length x) name-offset)
		  ;; AKCL has (aref (the (vector t) x).)
		  ;; which fails with strings
		  (eq (elt x name-offset) name))))
        ((eq type 'LIST)
         ;; The name is at the NAME-OFFSET in the list.
         (unless named (error "The structure should be named."))
         (if (= name-offset 0)
	     #'(lambda (x)
		 (and (consp x) (eq (car x) name)))
	     #'(lambda (x)
		 (do ((i name-offset (1- i))
		      (y x (cdr y)))
		     ((= i 0) (and (consp y) (eq (car y) name)))
		   (declare (fixnum i))
		   (unless (consp y) (return nil))))))
        ((error "~S is an illegal structure type."))))


;;; PARSE-SLOT-DESCRIPTION parses the given slot-description
;;;  and returns a list of the form:
;;;        (slot-name default-init slot-type read-only offset)

(defun parse-slot-description (slot-description offset)
  (declare (si::c-local))
  (let* (slot-name default-init slot-type read-only)
    (cond ((atom slot-description)
           (setq slot-name slot-description))
          ((endp (cdr slot-description))
           (setq slot-name (car slot-description)))
          (t
           (setq slot-name (car slot-description))
           (setq default-init (cadr slot-description))
           (do ((os (cddr slot-description) (cddr os)) (o) (v))
               ((endp os))
             (setq o (car os))
             (when (endp (cdr os))
                   (error "~S is an illegal structure slot option."
                          os))
             (setq v (cadr os))
             (case o
               (:TYPE (setq slot-type v))
               (:READ-ONLY (setq read-only v))
               (t
                (error "~S is an illegal structure slot option."
                         os))))))
    (list slot-name default-init slot-type read-only offset)))


;;; OVERWRITE-SLOT-DESCRIPTIONS overwrites the old slot-descriptions
;;;  with the new descriptions which are specified in the
;;;  :include defstruct option.

(defun overwrite-slot-descriptions (news olds)
  (declare (si::c-local))
  (when olds
      (let ((sds (member (caar olds) news :key #'car)))
        (cond (sds
               (when (and (null (cadddr (car sds)))
                          (cadddr (car olds)))
                     ;; If read-only is true in the old
                     ;;  and false in the new, signal an error.
                     (error "~S is an illegal include slot-description."
                            sds))
               (cons (list (caar sds)
                           (cadar sds)
                           (caddar sds)
                           (cadddr (car sds))
                           ;; The offset if from the old.
                           (car (cddddr (car olds))))
                     (overwrite-slot-descriptions news (cdr olds))))
              (t
               (cons (car olds)
                     (overwrite-slot-descriptions news (cdr olds))))))))


(defun define-structure (name conc-name type named slots slot-descriptions
			      copier include print-function constructors
			      offset documentation)
  (put-sysprop name 'DEFSTRUCT-FORM `(defstruct ,name ,@slots))
  (put-sysprop name 'IS-A-STRUCTURE t)
  (put-sysprop name 'STRUCTURE-SLOT-DESCRIPTIONS slot-descriptions)
  (put-sysprop name 'STRUCTURE-INCLUDE include)
  (put-sysprop name 'STRUCTURE-PRINT-FUNCTION print-function)
  (put-sysprop name 'STRUCTURE-TYPE type)
  (put-sysprop name 'STRUCTURE-NAMED named)
  (put-sysprop name 'STRUCTURE-OFFSET offset)
  (put-sysprop name 'STRUCTURE-CONSTRUCTORS constructors)
  #+clos
  (when *keep-documentation*
    (sys:set-documentation name 'STRUCTURE documentation))
  (and (consp type) (eq (car type) 'VECTOR)
       (setq type 'VECTOR))
  (dolist (x slot-descriptions)
    (and x (car x)
	 (funcall #'make-access-function name conc-name type named x)))
  (when copier
    (fset copier
	  (ecase type
	    ((NIL) #'sys::copy-structure)
	    (LIST #'copy-list)
	    (VECTOR #'copy-seq))))
  )


;; Set the dispatch macro.
(set-dispatch-macro-character #\# #\s 'sharp-s-reader)
(set-dispatch-macro-character #\# #\S 'sharp-s-reader)


;; Examples from Common Lisp Reference Manual.

#|
(defstruct ship
  x-position
  y-position
  x-velocity
  y-velocity
  mass)

(defstruct person name age sex)

(defstruct (astronaut (:include person (age 45))
                      (:conc-name astro-))
  helmet-size
  (favorite-beverage 'tang))

(defstruct (foo (:constructor create-foo (a
                                          &optional b (c 'sea)
                                          &rest d
                                          &aux e (f 'eff))))
  a (b 'bee) c d e f)

(defstruct (binop (:type list) :named (:initial-offset 2))
  (operator '?)
  operand-1
  operand-2)

(defstruct (annotated-binop (:type list)
                            (:initial-offset 3)
                            (:include binop))
  commutative
  associative
  identity)
|#

