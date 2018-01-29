;;				-[Thu Mar  1 10:54:27 1990 by jkf]-
;; pcl to quad translation
;; $Header: quadlap.cl,v 1.1 90/02/21 08:54:42 jkf Exp Locker: jkf $
;;
;; copyright (c) 1990 Franz Inc.
;;
(in-package :compiler)




(defvar *arg-to-treg* nil)
(defvar *cvar-to-index* nil)
(defvar *reg-array* nil)
(defvar *closure-treg* nil)
(defvar *nargs-treg* nil)

(defvar *debug-sparc* nil)

(defmacro pcl-make-lambda (&key required)
  `(list 'lambda nil :unknown-type 0 compiler::.function-level. 
	,required nil nil nil nil nil nil nil nil nil 
	nil 'compiler::none nil nil nil 
	nil nil nil nil nil nil 0 nil))

(defmacro pcl-make-varrec (&key name loc contour-level)
  `(list ,name nil 0 nil ,loc nil t compiler::.function-level. nil nil :unknown-type nil nil ,contour-level))

(defmacro pcl-make-lap (&key lap constants cframe-size locals)
  `(list nil ,constants ,lap nil nil ,cframe-size ,locals nil nil nil))


(defstruct (preg)
  ;; pseudo reg descritpor
  treg		; associated treg
  index 	; :index if this is an index type reg
  		; :vector if this is a vector type reg
  )


(defun pcl::excl-lap-closure-generator (closure-vars-names
				   arg-names
				   index-regs
				   vector-regs
				   fixnum-vector-regs
				   t-regs
				   lap-code)
  (let ((function (pcl::excl-lap-closure-gen closure-vars-names
				   arg-names
				   index-regs
				   (append vector-regs fixnum-vector-regs)
				   t-regs
				   lap-code)))
    #'(lambda (&rest closure-vals)
	(insert-closure-vals function closure-vals))))


(defun pcl::excl-lap-closure-gen
    (closure-vars-names arg-names index-regs vector-regs t-regs lap-code)
  (let ((*quads* nil)
	(*treg-num* 0)
	(*all-tregs* nil)
	(*bb-count* 0)
	*treg-bv-size*
	*treg-vector*
	(*next-catch-frame* 0)
	(*max-catch-frame* -1)
	*catch-labels*
	*top-label*
	*mv-treg*
	*mv-treg-target*
	*zero-treg*
	*nil-treg*
	*bbs* *bb* lap
	;; bbs
	*cross-block-regs*
	*const-tregs* *move-tregs*
	*actuals*
	*ignore-argcount*
	*binds-specs*
	*bvl-current-bv* ; for bitvector cacher
	*bvl-used-bvs*
	*bvl-index*
	(*inhibit-call-count* t)
	
	; this fcn
	*arg-to-treg*
	*cvar-to-index* 
	*reg-array*
	minargs
	maxargs
	*closure-treg*

	node
	otherargregs
	
	*nargs-treg*
	)

    (if* *debug-sparc* 
       then (format t ">>** << Generating sparc lap code~%"))
    
    (setq *nil-treg* 
      #+allegro-v4.0 (new-reg :global t)
      #-allegro-v4.0 (new-reg)
      *mv-treg* (new-reg)
      *mv-treg-target* (list *mv-treg*)
      *zero-treg* (comp::new-reg))
    
    ; examine given args
    
    (setq minargs 0  maxargs 0)
    (let (requireds)
      (dolist (arg arg-names)
	(if* (eq '&rest arg)
	   then (setq maxargs nil)
	   else (if* (null arg)
		   then ; we want a name even though we won't use it
			(setq arg (gensym)))
		(incf minargs)
		(incf maxargs)
		(push (cons arg (new-reg)) *arg-to-treg*)
		(push (pcl-make-varrec :name arg 
				   :loc (cdr (car *arg-to-treg*))
				   :contour-level 0)
		      requireds)
		))
      (setq node (pcl-make-lambda :required  (nreverse requireds))))
    (setq *arg-to-treg* (nreverse *arg-to-treg*))
    
    ; build closure vector list
    (let ((index -1))
      (dolist (cvar closure-vars-names)
	(push (cons cvar (incf index)) *cvar-to-index*)))
    
    (let ((maxreg (max (apply #'max (cons -1 index-regs))
		       (apply #'max (cons -1 vector-regs))
		       (apply #'max (cons -1 t-regs)))))
      (setq *reg-array* (make-array (1+ maxreg))))
    
    (dolist (index index-regs)
      (setf (svref *reg-array* index)
	(make-preg :treg (new-reg)
		   :index :index)))
    
    (dolist (vector vector-regs)
      (setf (svref *reg-array* vector) 
	(make-preg :treg (new-reg)
		   :index :vector)))
    
    (dolist (tr t-regs)
      (setf (svref *reg-array* tr) (make-preg :treg (new-reg))))
    

    (if* closure-vars-names
       then (setq *closure-treg* (new-reg)))
    (setq *nargs-treg* (new-reg))
        
    ;; (md-allocate-global-tregs)
    
    ; function entry
    (qe nop :arg :first-block)
    (qe entry)
    (qe argcount :arg (list minargs maxargs))
    (qe lambda :d (mapcar #'cdr *arg-to-treg*))
    (qe register :arg :nargs :d (list *nargs-treg*))

    (if* *closure-treg*
       then ; put the first closure vector in *closure-treg*
	    (qe extract-closure-vec :d (list *closure-treg*))
	    (let ((offsetreg (new-reg)))
	      (qe const :arg (mdparam 'md-cons-car-adj) :d (list offsetreg))
	      (qe ref :u (list *closure-treg* offsetreg) 
		  :d (list *closure-treg*)
		  :arg :long))
	    )

    (excl-gen-quads lap-code)

    (if* *debug-sparc*
       then (do-quad-list (quad next *quads*)
	      (format t "~a~%" quad))

	    (format t "basic blocks~%"))
    
    (setq *bbs* (qc-compute-basic-blocks *quads*))
    
    (excl::target-class-case
     ((:r :m) (setq *actuals* (qc-compute-actuals *bbs*))))
    
    (qc-live-variable-analysis *bbs*)
    
    (setq *treg-bv-size* (* 16 (truncate (+ *treg-num* 15) 16)))
      
    (qc-build-treg-vector)
    

    (let ((*dump-bbs* nil)
	  (r::*local-regs*
	   ; use the in registers that aren't in use
	   (append r::*local-regs*
		   (if* maxargs
		      then (nthcdr maxargs r::*in-regs* )))))
      (unwind-protect
	  (progn
	    ; machine specific code generation
	    (multiple-value-bind (lap-code literals size-struct locals)
		#+(target-class r m e)
		(progn
		  #+allegro-v4.0 
		  (md-codegen node *bbs*
			      nil otherargregs)
		  #-allegro-v4.0 
		  (md-codegen node *bbs*
			      *nil-treg* *mv-treg* *zero-treg*
			      nil otherargregs))
		  
		#-(target-class r m e) (md-codegen node *bbs*)
		(setq lap
		  (pcl-make-lap :lap lap-code
			    :constants literals
			    :cframe-size size-struct
			    :locals  locals)))

	     
	    lap)
	(giveback-bvs)))
    
    #+ignore 
    (progn (format t "sparc code pre optimization~%")
	   (dolist (instr (lap-lap lap))
	     (format t "> ~a~%" instr)))
    (md-optimize lap) ; peephole optimize
    (if* *debug-sparc*
       then (format t "sparc code post optimization~%")
	    (dolist (instr (lap-lap lap))
	      (format t "> ~a~%" instr)))
    (md-assemble lap)
    (setq last-lap lap)
 
    (nl-runtime-make-a-fcnobj lap)))

(defun qe-slot-access (operand offset dest)
  ;; access a slot in a structure
  (let ((temp (new-reg)))
    (qe const :arg offset :d (list temp))
    (qe ref :u (list (get-treg-of operand) temp) 
	:d (list (get-treg-of dest))
	:arg :long)))


(defun get-treg-of (operand &optional res-operand)
  ;; get the appropriate treg for the operand
  (let ((prefer-treg (and res-operand (simple-get-treg-of res-operand))))
    (if* (numberp operand)
       then (let ((treg (new-reg)))
	      (qe const :arg operand :d (list treg))
	      treg)
     elseif (consp operand)
       then (ecase (car operand)
	      (:reg 
	       (preg-treg (svref *reg-array* (cadr operand))))
	      (:arg 
	       (let ((x (cdr (assoc (cadr operand) *arg-to-treg* :test #'eq))))
		 (if* (null x)
		    then (error "where is arg ~s" operand)
		    else x)))
	      (:cvar
	       (let ((res-treg (or prefer-treg (new-reg)))
		     (temp-treg (new-reg)))
		 (qe const :arg (+ (mdparam 'md-svector-data0-adj)
				   (* 4 (cdr (assoc (cadr operand)
						    *cvar-to-index*
						    :test #'eq))))
		     :d (list temp-treg))
		 (qe ref :u (list *closure-treg* temp-treg)
		     :d (list res-treg)
		     :arg :long)
		 res-treg))
	      (:constant
	       (let ((treg (or prefer-treg (new-reg))))
		 (qe const :arg (if* (fixnump (cadr operand))
				   then (* 8 (cadr operand)) ; md!!
				   else (cadr operand))
		     :d (list treg))
		 treg))
	      (:index-constant
	       ; operand invented by jkf to denote an index type constant
	       (let ((treg (or prefer-treg (new-reg))))
		 (qe const :arg (if* (fixnump (cadr operand))
				   then (* 4 (cadr operand)) ; md!!
				   else (cadr operand))
		     :d (list treg))
		 treg)))
       else (error "bad operand: ~s" operand))))

(defun simple-get-treg-of (operand)
  ;; get the treg if it is so simple that we don't have to 
  ;; emit any instructions to access it.
  ;; return nil if we can't do it.
  (if* (numberp operand)
     then nil
   elseif (consp operand)
     then (case (car operand)
	    (:reg 
	     (preg-treg (svref *reg-array* (cadr operand))))
	    (:arg 
	     (let ((x (cdr (assoc (cadr operand) *arg-to-treg* :test #'eq))))
	       (if* (null x)
		  then nil
		  else x))))
	      
     else nil))

(defun index-p (operand)
  ;; determine if the result of this operand is an index value
  ;* it would be better if conversion between lisp values and
  ;  index values were made explicit in the lap code
  (and (consp operand)
       (or (and (eq :reg (car operand))
		(eq :index (preg-index (svref *reg-array* (cadr operand)))))
	   (member (car operand)
		   '(:i+ :i- :ilogand :ilogxor :i1+)
		   :test #'eq))
       t))

(defun gen-index-treg (operand)
  ;; return the non-index type operand in a index treg
  (if* (and (consp operand)
	    (eq ':constant (car operand)))
     then (get-treg-of `(:index-constant ,(cadr operand)))
     else (let ((treg (get-treg-of operand))
		(new-reg (new-reg))
		(shift-reg (new-reg)))
	    (qe const :arg 1 :d (list shift-reg))
	    (qe lsr :u (list treg shift-reg) :d (list new-reg))
	    new-reg)))

		
	    
  
  
(defun vector-preg-p (operand)
  (and (consp operand)
       (eq :reg (car operand))
       (eq :vector (preg-index (svref *reg-array* (cadr operand))))))
       
	    
	  
(defun excl-gen-quads (laps)
  ;; generate quads from the lap
  (dolist (lap laps)
    (if* *debug-sparc* then (format t ">> ~a~%" lap))
    (block again
      (let ((opcode (car lap))
	    (op1    (cadr lap))
	    (op2    (caddr lap)))
	(case opcode
	  (:move
	   ; can be either simple (both args registers)
	   ; or one arg can be complex and the other simple
	   (case (car op2)
	     (:iref
	      ;; assume that this is a lisp store
	      ;;(warn "assuming lisp store in ~s" lap)
	      (let (op1-treg)
		(if* (not (vector-preg-p (cadr op2)))
		   then ; must offset before store
			(error "must use vector register in ~s" lap)
		   else (setq op1-treg (get-treg-of (cadr op2))))
				       
				      
		
		(qe set :u (list op1-treg
				 (get-treg-of (caddr op2))
				 (get-treg-of op1))
		    :arg :lisp)
		(return-from again)))
	     (:cdr
	      ;; it certainly is a lisp stoer
	      (let (op1-treg const-reg)
		(setq op1-treg (get-treg-of (cadr op2)))
	        (setq const-reg (new-reg))
		(qe const :arg (mdparam 'md-cons-cdr-adj) 
		    :d (list const-reg))
				       
				      
		
		(qe set :u (list op1-treg
				 const-reg
				 (get-treg-of op1))
		    :arg :lisp)
		(return-from again))))
	 
	   ; the 'to'address is simple, the from address may not be
	 
	   (let ((index1 (index-p op1))
		 (index2 (index-p op2))
		 (vector1 (vector-preg-p op1))
		 (vector2 (vector-preg-p op2)))
	     (ecase (car op1)
	       ((:reg :cvar :arg :constant :lisp-symbol)
		(qe move 
		    :u (list (get-treg-of op1 op2))
		    :d (list (get-treg-of op2))))
	       (:std-wrapper
		(qe-slot-access (cadr op1) 
				(+ (* 1 4)
				   (comp::mdparam 'md-svector-data0-adj))
				op2))
	       (:std-slots
		(qe-slot-access (cadr op1) 
				(+ (* 2 4)
				   (comp::mdparam 'md-svector-data0-adj))
				op2))
	       (:fsc-wrapper
		(qe-slot-access (cadr op1) 
				(+ (* (- 15 1) 4)
				   (comp::mdparam 'md-function-const0-adj))
				op2))
	       (:fsc-slots
		(qe-slot-access (cadr op1) 
				(+ (* (- 15 2) 4)
				   (comp::mdparam 'md-function-const0-adj))
				op2))
	       ((:built-in-wrapper :structure-wrapper :built-in-or-structure-wrapper)
		(qe call :arg 'pcl::built-in-or-structure-wrapper-fun
		    :u (list (get-treg-of (cadr op1)))
		    :d (list (get-treg-of op2))))
               #+pcl-user-instances
               ((:user-wrapper :user-slots)
                (warn "Trying to use pcl-user-instances in Sun4 Allegro."))
	       (:other-wrapper
		(warn "do other-wrapper"))
	       ((:i+ :i- :ilogand :ilogxor)
		(qe arith :arg (cdr (assoc (car op1) 
					   '((:i+ . :+)
					     (:i- . :-)
					     (:ilogand . :logand)
					     (:ilogxor . :logxor))
					   :test #'eq))
		    :u (list (get-treg-of (cadr op1))
			     (get-treg-of (caddr op1)))
		    :d (list (get-treg-of op2))))
	       (:i1+
		(let ((const-reg (new-reg)))
		  (qe const :arg 4 ; an index value of 1
		      :d (list const-reg))
		  (qe arith :arg :+
		      :u (list const-reg
			       (get-treg-of (cadr op1)))
		      :d (list (get-treg-of op2)))))
		      
	       ((:iref :cref)
		(let (op1-treg)
		  (if* (not (vector-preg-p (cadr op1)))
		     then ; must offset before store
			  (error "must use vector register in ~s" lap)
		     else (setq op1-treg (get-treg-of (cadr op1))))
				       
		  (qe ref :u (list op1-treg
				   (get-treg-of (caddr op1) op2))
		      :d (list (get-treg-of op2))
		      :arg :long)))
	       (:cdr
		(let ((const-reg (new-reg)))
		  (qe const :arg (mdparam 'md-cons-cdr-adj)
		      :d (list const-reg))
		  (qe ref :arg :long
		      :u (list (get-treg-of (cadr op1))
			       const-reg)
		      :d (list (get-treg-of op2))))))
	     (if* (not (eq index1 index2))
		then (let ((shiftamt (new-reg)))
		       (qe const :arg 1 :d (list shiftamt))
		       (if* (and index1 (not index2))
			  then ; converting from index to non-index
			       (qe lsl :u (list (get-treg-of op2) shiftamt)
				   :d (list (get-treg-of op2)))
			elseif (and (not index1) index2)
			       ; converting to an index
			  then (qe lsr :u (list (get-treg-of op2) shiftamt)
				   :d (list (get-treg-of op2)))))
	      elseif (and vector2 (not vector1))
		then ; add vector offset
		     (let ((tempreg (new-reg))
			   (vreg (get-treg-of op2)))
		       (qe const :arg (mdparam 'md-svector-data0-adj)
			   :d (list tempreg))
		       (qe arith :arg :+ :u (list vreg tempreg)
			   :d (list vreg))))))
	  (:fix=
	   (let (tr1 tr2)
	     (if* (index-p op1)
		then (setq tr1 (get-treg-of op1))
		     (if* (not (index-p op2))
			then (setq tr2 (gen-index-treg op2))
			else (setq tr2 (get-treg-of op2)))
	      elseif (index-p op2)
		then ; assert: op1 isn't an index treg
		     (setq tr1 (gen-index-treg op1))
		     (setq tr2 (get-treg-of op2))
		else (setq tr1 (get-treg-of op1)
			   tr2 (get-treg-of op2)))
	   
		   
		   
	     (qe bcc :u (list tr1 tr2)
		 :arg (cadddr lap)
		 :arg2 :eq )))
	  ((:eq :neq :fix=)
	   (if* (not (eq (index-p op1) (index-p op2)))
	      then (error "non matching operands indexwise in: ~s" lap))
	   (qe bcc :u (list (get-treg-of op1)
			    (get-treg-of op2))
	       :arg (cadddr lap)
	       :arg2 (cdr (assoc opcode '((:eq . :eq)
					  (:neq . :ne))
				 :test #'eq))))
	  (:izerop 
	   (qe bcc :u (list (get-treg-of op1)
			    *zero-treg*)
	       :arg (caddr lap)
	       :arg2 :eq))
	  (:std-instance-p
	   (let ((treg (get-treg-of op1))
		 (tempreg (new-reg))
		 (temp2reg (new-reg))
		 (offsetreg (new-reg))
		 (nope (pc-genlab)))
	     (qe typecheck :u (list treg)
		 :arg nope
		 :arg2 '(not structure))
	     (qe const :arg 'pcl::std-instance :d (list tempreg))
	     (qe const :arg (mdparam 'md-svector-data0-adj) 
		 :d (list offsetreg))
	     (qe ref :u (list treg offsetreg) 
		 :d (list temp2reg)
		 :arg :long)
	     (qe bcc :arg2 :eq :u (list tempreg temp2reg)
		 :arg (caddr lap))
	     (qe label :arg nope)))
	  
	  (:fsc-instance-p
	   (let ((treg (get-treg-of op1))
		 (nope (pc-genlab))
		 (offsetreg (new-reg))
		 (tempreg (new-reg))
		 (checkreg (new-reg)))
	     (qe typecheck :u (list treg)
		 :arg nope
		 :arg2 '(not compiled-function))
	     (qe const :arg (mdparam 'md-function-flags-adj)
		 :d (list offsetreg))
	     (qe ref :u (list treg offsetreg) :d (list tempreg)
		 :arg :ubyte)
	     (qe const :arg pcl::funcallable-instance-flag-bit
		 :d (list checkreg))
	     (qe bcc :u (list checkreg tempreg)
		 :arg (caddr lap)
		 :arg2 :bit-and)
	     (qe label :arg nope)))
	  (:built-in-instance-p
	   ; always true
	   (qe bra :arg (caddr lap)))
	  (:jmp
	   (qe tail-funcall :u (list *nargs-treg* (get-treg-of op1))))
	  (:structure-instance-p
	   ; always true
	   (qe bra :arg (caddr lap)))
	  
	  (:return
	    (let (op-treg)
	      (if* (index-p op1)
		 then ; convert to lisp before returning
		      (let ((shiftamt (new-reg)))
			(setq op-treg (new-reg))
			(qe const :arg 1 :d (list shiftamt))
			(qe lsl :u (list (get-treg-of op1) shiftamt)
			    :d (list op-treg)))
		 else (setq op-treg (get-treg-of op1)))

	      (qe move :u (list op-treg) :d *mv-treg-target*)
	      (qe return :u *mv-treg-target*)))

	  (:go
	   (qe bra :arg (cadr lap)))
	   
	  (:label 
	   (qe label :arg (cadr lap)))
	     
	   
	   
	  (t (warn "ignoring ~s" lap)))))))


(defun insert-closure-vals (function closure-vals)
  ;;  build a fucntion from the lap and insert 
  (let ((newfun (sys::copy-function function)))
    (setf (excl::fn_closure newfun) (list (apply 'vector closure-vals)))
    newfun))

  
	     
; test case:
; (pcl::defclass foo () (a b c))
; (pcl::defmethod barx ((a foo) b c)  a )
; (apply 'pcl::excl-lap-closure-generator pcl::*tcase*)
;
; to turn it on

(if* (not (and (boundp 'user::noquad)
	       (symbol-value 'user::noquad)))
   then (setq pcl::*make-lap-closure-generator* 
	  'pcl::excl-lap-closure-generator))





  

