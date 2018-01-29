;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPEXIT  Exit manager.

(in-package 'compiler)

(defvar *last-label* 0)
(defvar *exit*)
(defvar *unwind-exit*)

;;; *last-label* holds the label# of the last used label.
;;; *exit* holds an 'exit', which is
;;;	( label# . ref-flag ) or one of RETURNs (i.e. RETURN, RETURN-FIXNUM,
;;;	RETURN-CHARACTER, RETURN-LONG-FLOAT, RETURN-SHORT-FLOAT, or
;;;	RETURN-OBJECT).
;;; *unwind-exit* holds a list consisting of:
;;;	( label# . ref-flag ), one of RETURNs, TAIL-RECURSION-MARK, FRAME,
;;;	JUMP, BDS-BIND (each pushed for a single special binding), or a
;;;	LCL (which holds the bind stack pointer used to unbind).

(defun unwind-bds (bds-lcl bds-bind)
  (declare (fixnum bds-bind))
  (when bds-lcl (wt-nl "bds_unwind(") (wt-lcl bds-lcl) (wt ");"))
  (dotimes (n bds-bind) (declare (fixnum n)) (wt-nl "bds_unwind1;")))

(defun unwind-exit (loc &optional (jump-p nil)
                        &aux (bds-lcl nil) (bds-bind 0))
  (declare (fixnum bds-bind))
  (when (consp *destination*)
    (case (car *destination*)
      (JUMP-TRUE
       (set-jump-true loc (second *destination*))
       (when (eq loc t) (return-from unwind-exit)))
      (JUMP-FALSE
       (set-jump-false loc (second *destination*))
       (when (eq loc nil) (return-from unwind-exit)))))
  (flet ((single-valued-call (loc)
	   (member (car loc)
		   '(INLINE INLINE-COND INLINE-FIXNUM
		     INLINE-CHARACTER INLINE-LONG-FLOAT INLINE-SHORT-FLOAT)
		   :test #'eq)))
    (dolist (ue *unwind-exit* (baboon))
      ;; perform all unwind-exit's which precede *exit*
      (cond
	((consp ue)			; ( label# . ref-flag )
	 (cond ((eq ue *exit*)
		;; all body forms except the last (returning) are dealt here
		(if (and (consp *destination*)
			 (or (eq (car *destination*) 'JUMP-TRUE)
			     (eq (car *destination*) 'JUMP-FALSE)))
		    (unwind-bds bds-lcl bds-bind)
		    (if (or bds-lcl (plusp bds-bind))
			;; Save the value if LOC may possibly refer
			;; to special binding.
			(if (and (consp loc)
				 (or (and (eq (car loc) 'VAR)
					  (member (var-kind (second loc))
						  '(SPECIAL GLOBAL)
						  :test #'eq))
				     (single-valued-call loc))
				 ;; no need for temporary if we can use
				 ;; *destination* directly
				 (consp *destination*)
				 (member (car *destination*)
					 '(VAR BIND PUSH-CATCH-FRAME)
					 :test #'eq))
			    (let* ((*temp* *temp*)
				   (temp (list 'TEMP (next-temp))))
			      (let ((*destination* temp))
				(set-loc loc)) ; temp <- loc
			      (unwind-bds bds-lcl bds-bind)
			      (set-loc temp)) ; *destination* <- temp
			    (progn (set-loc loc)
				   (unwind-bds bds-lcl bds-bind)))
			(set-loc loc)))
		(when jump-p (wt-nl) (wt-go *exit*))
		(return))
	       (t (setq jump-p t))))
	((numberp ue) (setq bds-lcl ue
			    bds-bind 0))
	(t (case ue
	     (BDS-BIND (incf bds-bind))
	     (RETURN
	       (unless (eq *exit* 'RETURN) (baboon))
	       ;; *destination* must be either RETURN or TRASH.
	       (if (and (consp loc) (eq (car loc) 'VALUES))
		   ;; from multiple-value-prog1 or values
		   (progn
		     (when (or bds-lcl (plusp bds-bind))
		       (unwind-bds bds-lcl bds-bind))
		     (wt-nl "RETURN(" (second loc) ");"))
		   (if (or bds-lcl (plusp bds-bind))
		       (let* ((nr (list 'LCL (next-lcl))))
			 (wt-nl "{int " nr ";")
			 (set-values loc nr)
			 (unwind-bds bds-lcl bds-bind)
			 (wt-nl "RETURN(" nr ");}"))
		       (if (and (consp loc)
				(member (car loc) '(CALL CALL-LOCAL)
					:test #'eq))
			   (wt-nl "RETURN(" loc ");")
			   (progn
			     (set-loc loc)
			     (wt-nl "RETURN(1);")))))
	       (return))
	     (RETURN-FIXNUM
	      (when (eq *exit* ue)
		;; *destination* must be RETURN-FIXNUM
		(if (or bds-lcl (plusp bds-bind))
		    (let ((lcl (next-lcl)))
		      (wt-nl "{int ") (wt-lcl lcl) (wt "= ")
		      (wt-fixnum-loc loc) (wt ";")
		      (unwind-bds bds-lcl bds-bind)
		      (wt-nl "return(") (wt-lcl lcl) (wt ");}"))
		    (progn
		      (wt-nl "return(") (wt-fixnum-loc loc) (wt ");")))
		(return)))
	     (RETURN-CHARACTER
	      (when (eq *exit* ue)
		;; *destination* must be RETURN-CHARACTER
		(if (or bds-lcl (plusp bds-bind))
		    (let ((lcl (next-lcl)))
		      (wt-nl "{unsigned char ") (wt-lcl lcl) (wt "= ")
		      (wt-character-loc loc) (wt ";")
		      (unwind-bds bds-lcl bds-bind)
		      (wt-nl "return(") (wt-lcl lcl) (wt ");}"))
		    (progn
		      (wt-nl "return(") (wt-character-loc loc) (wt ");")))
		(return)))
	     (RETURN-LONG-FLOAT
	      (when (eq *exit* ue)
		;; *destination* must be RETURN-LONG-FLOAT
		(if (or bds-lcl (plusp bds-bind))
		    (let ((lcl (next-lcl)))
		      (wt-nl "{int ") (wt-lcl lcl) (wt "= ")
		      (wt-long-float-loc loc) (wt ";")
		      (unwind-bds bds-lcl bds-bind)
		      (wt-nl "return(") (wt-lcl lcl) (wt ");}"))
		    (progn
		      (wt-nl "return(") (wt-long-float-loc loc) (wt ");")))
		(return)))
	     (RETURN-SHORT-FLOAT
	      (when (eq *exit* ue)
		;; *destination* must be RETURN-SHORT-FLOAT
		(if (or bds-lcl (plusp bds-bind))
		    (let ((lcl (next-lcl)))
		      (wt-nl "{int ") (wt-lcl lcl) (wt "= ")
		      (wt-short-float-loc loc) (wt ";")
		      (unwind-bds bds-lcl bds-bind)
		      (wt-nl "return(") (wt-lcl lcl) (wt ");}"))
		    (progn
		      (wt-nl "return(") (wt-short-float-loc loc) (wt ");")))
		(return)))
	     (RETURN-OBJECT
	      (when (eq *exit* ue)
		;; *destination* must be RETURN-OBJECT
		(if (or bds-lcl (plusp bds-bind))
		    (progn
		      (wt-nl "{object x =" loc
			     (if (eq (car loc) 'CALL) ",VALUES(0)" "") ";")
		      (unwind-bds bds-lcl bds-bind)
		      (wt-nl "return(x);}"))
		    (wt-nl "return(" loc
			   (if (eq (car loc) 'CALL) ",VALUES(0)" "") ");"))
		(return)))
	     (FRAME
	      (when (and (consp loc) (single-valued-call loc))
		(if (and (consp *destination*)
			 (eq (car *destination*) 'LEX))
		    (progn
		      (set-loc loc)
		      (setq loc *destination*))
		    (let ((*destination* (list 'TEMP (next-temp))))
		      (set-loc loc)	; temp <- loc
		      (setq loc *destination*))))
	      (wt-nl "frs_pop();"))
	     (TAIL-RECURSION-MARK)
	     (JUMP (setq jump-p t))
	     (t (baboon)))))))
  ;;; Never reached
  )

(defun unwind-no-exit (exit &aux (bds-lcl nil) (bds-bind 0))
  (declare (fixnum bds-bind))
  (dolist (ue *unwind-exit* (baboon))
    (cond
       ((consp ue)
        (when (eq ue exit)
              (unwind-bds bds-lcl bds-bind)
              (return)))
       ((numberp ue) (setq bds-lcl ue bds-bind 0))
       ((eq ue 'BDS-BIND) (incf bds-bind))
       ((member ue '(RETURN RETURN-OBJECT RETURN-FIXNUM RETURN-CHARACTER
                            RETURN-LONG-FLOAT RETURN-SHORT-FLOAT))
        (if (eq exit ue)
          (progn (unwind-bds bds-lcl bds-bind)
                 (return))
          (baboon))
        ;;; Never reached
        )
       ((eq ue 'FRAME) (wt-nl "frs_pop();"))
       ((eq ue 'TAIL-RECURSION-MARK)
        (if (eq exit 'TAIL-RECURSION-MARK)
          (progn (unwind-bds bds-lcl bds-bind)
                 (return))
          (baboon))
        ;;; Never reached
        )
       ((eq ue 'JUMP))
       (t (baboon))
       ))
  ;;; Never reached
  )

;;; Tail-recursion optimization for a function F is possible only if
;;;	1. F receives only required parameters, and
;;;	2. no required parameter of F is enclosed in a closure.
;;;
;;; A recursive call (F e1 ... en) may be replaced by a loop only if
;;;	1. F is not declared as NOTINLINE,
;;;	2. n is equal to the number of required parameters of F,
;;;	3. the form is a normal function call (i.e. args are not ARGS-PUSHED),
;;;	4. (F e1 ... en) is not surrounded by a form that causes dynamic
;;;	   binding (such as LET, LET*, PROGV),
;;;	5. (F e1 ... en) is not surrounded by a form that that pushes a frame
;;;	   onto the frame-stack (such as BLOCK and TAGBODY whose tags are
;;;	   enclosed in a closure, and CATCH),

(defun tail-recursion-possible ()
  (dolist (ue *unwind-exit* (baboon))
    (cond ((eq ue 'TAIL-RECURSION-MARK) (return t))
          ((or (numberp ue) (eq ue 'BDS-BIND) (eq ue 'FRAME))
           (return nil))
          ((or (consp ue) (eq ue 'JUMP)))
          (t (baboon)))))
