;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;; adopted to BABYLON 2.3 by Juergen Walther 30.5.94
;;; adopted to non SYMBOLICS machines for BABYLON 2.1 by Juergen Walther 10.5.89



;;; *********************************************************************************
;;; ************************ Miscellaneous Functions/Macros etc *********************
;;; ************************ to be used in K3-Code              *********************

(defmacro extend-list (variable list)
  `(setf ,variable (append ,variable ,list)))

;; ---------------------------------------------------------------------------------


(defun constraint-definition (name)
  
  "the constraint-object with name "name" if such an object exists.
otherwise the result is nil."
  
  (let ((constraint (send-constraint-processor :get name)))
    (if (null constraint)
      nil
      constraint)))


;; ---------------------------------------------------------------------------------


(defun constraint-expressions (all-constraint-names all-local-variables truth-value-variables)

  ;; all-constraint-names is a list of constraint names (cname-1 ... cname-N).
  ;; all-local-lists is a list of lists of the local variables of the cname-i's.
  ;; truth-value-variables is the list (WW-1 ... WW-N)
  ;;
  ;; constraint-expressions generates the structure
  ;;
  ;;     ((cname-1 <first element of all-local-variables> WW-1)
  ;;       ...
  ;;      (cname-N <N-th  element of all-local-variables> WW-N)).

  (mapcar #'(lambda (cname-i i-th-list ww-i)
	      `(,cname-i ,@i-th-list ,ww-i))
	  all-constraint-names all-local-variables truth-value-variables))


;; ---------------------------------------------------------------------------------


(defun get-constraint-names (attachments)

  ;; attachments is a list of attachments
  ;;
  ;;    (((cname1) (x1 y1) ... (xn yn)) ...
  ;;     ((cnamem) (z1 v1) ... (zm vm)))
  ;;
  ;; this function returns the list
  ;;
  ;;    (cname1 ... cnamem)
  

  (mapcar #'(lambda (an-attachment)
	      (caar an-attachment))
	  attachments))


;; ---------------------------------------------------------------------------------


(defun generate-variables (number)

  ;; generates number variables WW-1 ... WW-number

  (prog (v)
	(do ((i 0 (+ i 1)))
	    ((equal i number))
	  (extend-list v
		       (list (intern (format nil "WW-~S" i)))))
	(return v)))

;; ---------------------------------------------------------------------------------
 
(defun get-translated-variables (attachments)
  
  ;; attachments is a list of attachments
  ;;
  ;;    (((cname1) (x1 y1) ... (xn yn)) ...
  ;;     ((cnamem) (z1 v1) ... (zm vm)))
  ;;
  ;; this function returns the list
  ;;
  ;;    ((x1 y1) ... (xn yn) ... (z1 v1) ... (zm vm)) .
  
  (cond ((null (cdr attachments))
	 (cdar attachments))
	(t (append (cdar attachments)
		   (get-translated-variables (cdr attachments))))))


;; ---------------------------------------------------------------------------------


(defun get-all-variables (constraint-names)
  
  ;; constraint-names is a list of constraint names.
  ;;
  ;; get-all-variables returns a list of all lists of local variable-names defined
  ;; in constraints cname1 to cnamem.
  
  (enumerate-vars
    (mapcar #'(lambda (a-name)
		(prefix-name
		  (butlast
		    ($send (constraint-definition a-name) :INTERFACE) 1)
		  a-name))
	    
	    constraint-names)))


;; ---------------------------------------------------------------------------------


(defun enumerate-vars (list-of-lists)
  
  ;; for list-of-lists = ((a1 .. an) (b1 ... bp) ... (k1 .. km)),
  ;;
  ;; enumerate-vars generates a list ((a1-1 .. an-n) (b1-[n+1] ... bp-[n+p]) ...
  ;; (k1-[n+p+ ... +1] ... km-[n+p+ ... +m])
  ;;
  
  (prog ((n 0)
         (current-list nil)
         (current-list-of-lists nil))
    (dolist (clist list-of-lists)
      (dolist (elem clist)
        (setf current-list
              (append current-list 
                      (list (intern (generate-suffix elem n)))))
        (setf n (1+ n)))
      (setf current-list-of-lists
            (append current-list-of-lists
                    (list current-list)))
      (setf current-list nil))
    (return current-list-of-lists)))


;; ---------------------------------------------------------------------------------


(defun generate-suffix (string-arg suffix)

  ;; for string string-arg generate-suffix returns the string 
  ;; string-arg-suffix.

  (format nil "~S-~S" string-arg suffix))


;; ---------------------------------------------------------------------------------

(defun prefix-name (list-arg prefix)

  ;; if list-arg is 
  ;;      (e1 ... en)
  ;; and prefix is pref
  ;; then (prefix-name list-arg pref) is the list
  ;;      (pref-e1 ... pref-en)

  (mapcar #'(lambda (elem)
	      (intern (format nil "~S-~S" prefix elem)))
	  list-arg))


;; ---------------------------------------------------------------------------------


(defun flatten (list-arg)

  ;; flatten generates a flat list of elements in list-arg.
  ;;
  ;; example: if list-arg = ((a) b (c (d e)))
  ;;          then flatten(list-arg) = (a b c d e)

  (cond ((null list-arg) nil)
	((atom (car list-arg))
	 (append (list (car list-arg)) (flatten (cdr list-arg))))
	(t (append (flatten (car list-arg))
		   (flatten (cdr list-arg))))))

;; ---------------------------------------------------------------------------------


(defun translate-region-pattern-element
	 (old-relation old-variables constraint-name slot object)
  
  ;; old-relation is a list of patterns of a constraint-definition,
  ;; old-variables is a list of variables which should correspond to 
  ;; the list of variables in the single pattern element of the constraint.
  ;;
  ;;       ((:pattern (v1 ... vn) [:if (P v1 ... vn)]))
  ;; 
  ;; this function yields a list
  ;;
  ;;       ((:pattern (v1 ... vn 'T) :if (and (constrained-p v1 ... vn)
  ;;                                             (P v1 ... vn))
  ;;
  
  (prog ((predicate (cond ((null (fourth (car old-relation))) t)
			  (t (fourth (car old-relation))))))
	(if (or (> (length old-relation) 1)
		(null old-relation)
		(not (equal (caar old-relation) ':pattern))
		(not (equal old-variables (second (car old-relation)))))
	    (error "Constraint ~S in region ~S of ~S is not a usable :REGION constraint"
		   constraint-name slot object))
	(return `((:pattern ,(append old-variables (list ''T))
		   :if (and (constrained-p ,@old-variables)
			    ,predicate))
		  (:pattern ,(append old-variables (list ''nil))
		   :if (and (constrained-p ,@old-variables)
			    (not ,predicate)))))))


;;--------------------------------------------------------------------------------


(defun at-least-one-atom? (ref-list)
  
  ;; returns "t" if at least one element in ref-list is an atom
  
  (cond ((null ref-list) nil)
	((atom (car ref-list)) t)
	(t (at-least-one-atom? (cdr ref-list)))))

(defun all-elements-are-atoms? (ref-list)

  ;; returns "t" if all elements in a list are atoms.
  ;; note that this is the case if the ref-list is empty.

  (cond ((null ref-list) t)
	((not (atom (first ref-list))) nil)
	(t (all-elements-are-atoms? (cdr ref-list)))))

;;--------------------------------------------------------------------------------


(defun occurs-not-in-regions (cname exclude-list)
  
  ;; exclude-list is a list 
  ;;
  ;;     ( (slot-1 . (cname-1-1 ... cname-1-N))
  ;;       ...
  ;;       (slot-M . (cname-M-1 ... cname-M-N) )
  ;;
  ;; occurs-in-regions yields a list of all slots with a cname-i-j such that
  ;; cname-i-j = cname.
  
  (remove-if #'null 
	     (mapcar #'(lambda (exclude-elem)
			 (cond ((member cname (cdr exclude-elem) :test #'equal)
				(car exclude-elem)) 
			       (t  nil)))
		     exclude-list)))

;;--------------------------------------------------------------------------------


(defun make-compound-and-or-constraint (connector number-of-truth-variables)
  
  ;; connector is either OR or AND.
  ;; make-compound-and-or-constraint creates and evaluates a definition of a
  ;; compound constraint for AND or OR with number-of-truth-variables truth
  ;; value variables. If such a constraint already exists, make-compound-and-or-constraint
  ;; has no effect.
  ;; Example: (make-compound-and-or-constraint 'AND 4) has as side effect the definition of
  ;;
  ;;     (defconstraint K3-AND-4
  ;;        (:type compound)
  ;;        (:interface WW-1 WW-2 WW-3 WW-4 R)
  ;;        (:constraint-expressions
  ;;            (K3-AND-2 WW-1 WW-2 INTERN-R1)
  ;;            (K3-AND-2 INTERN-R1 WW-3 INTERN-R2)
  ;;            (K3-AND-2 INTERN-R2 WW-4 R)))
  ;;
  ;;      (K3-AND-2 INTERN-R1 WW-3 INTERN-R2) is generated by 'gen-constr-expressions.
  
  
  (prog* ((and-or-constraint-name (intern (format nil "K3-~S-~S"
                                                  connector number-of-truth-variables)))
          (existing-cdef (constraint-definition and-or-constraint-name))
          (truth-value-variables (generate-variables number-of-truth-variables))
          (and-or-2 (intern (format nil "K3-~S-2" connector)))
          (last-intern (intern (format nil "INTERN-R~S" (- number-of-truth-variables 2)))))
    
    (unless existing-cdef
      (eval `(defconstraint ,and-or-constraint-name
               (:type compound)
               (:interface ,@truth-value-variables R)
               (:constraint-expressions
                (,and-or-2 ,(first truth-value-variables)
                           ,(second truth-value-variables)
                           INTERN-R1)
                ,@(gen-constr-expressions connector 
                                          (butlast (cddr truth-value-variables)))
                
                (,and-or-2 ,last-intern ,(car (last truth-value-variables)) R)))))))


;;--------------------------------------------------------------------------------


(defun gen-constr-expressions (connector var-list)

  ;; gen-constr-expressions is strongly associated with make-compound-and-or-constraint.

  (gen-constr-expr-help connector var-list 1))

;;--------------------------------------------------------------------------------

(defun gen-constr-expr-help (connector var-list no)

  (let ((left-intern (intern (format nil "INTERN-R~S" no)))
	(right-intern (intern (format nil "INTERN-R~S" (+ no 1)))))

  (cond ((null var-list) nil)
	(t (cons (list (intern (format nil "K3-~S-2" connector))
		       left-intern 
		       (car var-list)
		       right-intern)
		 (gen-constr-expr-help connector (cdr var-list) (+ no 1)))))))

;;-------------------------------------------------------------------------------- 

(defun global-values (variable net-name)
  "gibt den Wert einer Netzvariablen zurueck."
  (get-var-info-values
   (assoc variable
          ($send (get-constraint net-name) :net-spec)
          :test 'equal)))

(defun send-restr-net (name &optional (method nil))
  (if (null method)
    (send-kb :get-restrictions name)
    ($send (send-kb :get-restrictions name)
          method)))


;;; eof

