;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPBLOCK  Block and Return-from.

(in-package 'compiler)

#|
;;; Use a structure of type vector to avoid creating
;;; normal structures before booting CLOS:
(defstruct (blk (:type vector) :named)
   name			;;; Block name.
   (ref 0 :type fixnum)	;;; Number of references.
   ref-ccb		;;; Cross closure reference.
			;;; During Pass1, T or NIL.
   ref-clb		;;; Cross local function reference.
       			;;; During Pass1, T or NIL.
       			;;; block id, or NIL.
   exit			;;; Where to return.  A label.
   destination		;;; Where the value of the block to go.
   var			;;; variable containing the block ID.
) |#

(defvar *blocks* nil)

;;; During Pass 1, *blocks* holds a list of blk objects and the symbols 'CB'
;;; (Closure Boundary), 'LB' (Level Boundary) or 'UNWIND-PROTECT'.
;;; 'CB' will be pushed on *blocks* when the compiler begins to process
;;;   a closure.
;;; 'LB' will be pushed on *blocks* when *level* is incremented.
;;; 'UNWIND-PROTECT' is pushed when entering an unwind-protect.
;;;  A dummy variable is created to hold the block identifier.
;;;  When a reference to the block (via return-from) is found, the
;;;  var-ref count for that variable is incremented only if the reference
;;;  appears across a boundary (CB, LB or UNWIND-PROTECT), while
;;;  the blk-ref is always incremented.
;;;  Therefore blk-ref represents whether the block is used at all and var-ref
;;;  for the dummy variable represents whether a block identifier must be
;;;  created and stored in such variable.

(defun c1block (args)
  (when (endp args) (too-few-args 'BLOCK 1 0))
  (cmpck (not (symbolp (car args)))
         "The block name ~s is not a symbol." (car args))
  (let* ((blk (make-blk :name (car args)
			:var (make-var :name (car args) :kind 'LEXICAL)))
         (*blocks* (cons blk *blocks*))
         (body (c1progn (cdr args))))
    (when (or (blk-ref-ccb blk) (blk-ref-clb blk))
	  (incf *setjmps*))
    (if (plusp (blk-ref blk))
	(list 'BLOCK (reset-info-type (second body)) blk body)
	body))
  )

(defun c2block (blk body)
  (if (plusp (var-ref (blk-var blk)))
      (let* ((nr (list 'LCL (next-lcl)))
	     (VALUES_nr (list 'VALUES nr))
	     (blk-var (blk-var blk))
	     (*env-lvl* *env-lvl*))
	(setf (blk-exit blk) *exit*
	      (blk-destination blk) *destination*)
	(wt-nl "{ int " nr ";")
	(unless (or (blk-ref-ccb blk) (blk-ref-clb blk))
	  (setf (var-loc blk-var) (next-lcl)
		(var-kind blk-var) 'OBJECT)
	  (wt " object ") (wt-var blk-var) (wt ";"))
	(when (env-grows (blk-ref-ccb blk))
	  (let ((env-lvl *env-lvl*))
	    (wt-nl "object env" (incf *env-lvl*) " = env" env-lvl ";")))
	(bind "new_frame_id()" blk-var)
	(wt-nl "if ((" nr "=frs_push(FRS_CATCH,") (wt-var blk-var)
	(wt "))!=0) " nr "--;")
	(wt-nl "else {")
	(let ((*destination* VALUES_nr))
	  (c2expr* body))
	(wt "}")
	(wt-nl "frs_pop();")	; instead of binding *unwind-exit*
	(unwind-exit VALUES_nr 'JUMP)
	(when (blk-ref-ccb blk) (decf *env*))
	(wt-nl "}"))
      (progn
	(setf (blk-exit blk) *exit*)
	(setf (blk-destination blk) *destination*)
	(c2expr body)))
  )

(defun c1return-from (args)
  (cond ((endp args) (too-few-args 'RETURN-FROM 1 0))
        ((and (not (endp (cdr args))) (not (endp (cddr args))))
         (too-many-args 'RETURN-FROM 2 (length args)))
        ((not (symbolp (car args)))
         "The block name ~s is not a symbol." (car args)))
  (do ((blks *blocks* (cdr blks))
       (name (car args))
       (ccb) (clb) (unw) (blk) (type T))
      ((endp blks)
       (cmperr "The block ~s is undefined." name))
    (setq blk (car blks))
    (case blk
      (CB (setq ccb t))
      (LB (setq clb t))
      (UNWIND-PROTECT (setq unw T))
      (t (when (eq (blk-name blk) name)
           (let* ((val (c1expr (second args)))
		  (info (reset-info-type (second val)))
		  (var (blk-var blk)))
             (cond (ccb (setf (blk-ref-ccb blk) t
			      type 'CCB
			      (var-ref-ccb var) T)
			(incf (var-ref var)))
		   (clb (setf (blk-ref-clb blk) t
			      type 'CLB)
			(incf (var-ref var)))
		   (unw (setf type 'UNWIND-PROTECT)
			(incf (var-ref var))))
	     (incf (blk-ref blk))
	     (push var (info-local-referred info))
	     (pushnew var (info-referred-vars info))
;	     (push blk (info-referred-tags info))
             (return (list 'RETURN-FROM info blk type val)))))))
  )

(defun c2return-from (blk type val
			  &aux (nr (list 'LCL (next-lcl))))
  (case type
    (CCB
     (wt-nl "{frame_ptr fr; int " nr ";")
     (wt-nl "fr=frs_sch(") (wt-var (blk-var blk)) (wt ");")
     (wt-nl "if(fr==NULL) FEerror(\"The block ~s is missing.\",1,VV["
	    (add-symbol (blk-name blk)) "]);")
     (let ((*destination* (list 'VALUES nr))) (c2expr* val))
     (wt-nl "unwind(fr,Cnil," nr "+1);}"))
    ((CLB UNWIND-PROTECT)
     (wt-nl "{ int " nr ";")
     (let ((*destination* (list 'VALUES nr))) (c2expr* val))
     (wt-nl "unwind(frs_sch(") (wt-var (blk-var blk)) (wt "),Cnil," nr "+1);}"))
    (T (let ((*destination* (blk-destination blk))
	     (*exit* (blk-exit blk)))
	 (c2expr val))))
  )

;;; ----------------------------------------------------------------------

(setf (get 'BLOCK 'C1SPECIAL) 'c1block)
(setf (get 'BLOCK 'C2) 'c2block)

(setf (get 'RETURN-FROM 'C1SPECIAL) 'c1return-from)
(setf (get 'RETURN-FROM 'C2) 'c2return-from)
