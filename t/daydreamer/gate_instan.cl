;*******************************************************************************
;
; GATE
; Version 2.3
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
; This file contains the instantiator for obs
;
; 10/13/84: Original version written
;  6/30/85: Added *modify*, *expand*
;   1/6/86: Changed specials to obs
;  1/29/86: Added omit-proc
;  1/30/86: Added variables-in
;  9/24/86: Got rid of flavors
;  9/29/86: Updated to new instantiation algorithm with cycle preservation
;
;*******************************************************************************

(setq *found-obs* nil)
(setq *instan-obs* nil)

(setq *any-unbound?* nil)

(defun ob$instantiate (template bindings)
  (ob$instantiate1 template bindings 100 nil nil))

(defun ob$instantiate1 (template bindings depth omit-slots include-slots)
  (setq *instan-obs* nil)
  (setq *any-unbound?* nil)
  (ob$instantiate2 template bindings depth
                    omit-slots include-slots nil nil nil))

;
; substit: a binding list of pairs. Each pair has the thing (ob or otherwise)
; to substitute and the thing to substitute it with.
;
(defun ob$subst (ob substit depth omit-slots include-slots)
  (setq *instan-obs* nil)
  (setq *any-unbound?* nil)
  (ob$instantiate2 ob *empty-bd* depth
                    omit-slots include-slots substit nil nil))

;
; variabilize?: a predicate determining whether an ob should be abstracted
; and converted into a unique variable. Multiple occurences of the same ob
; will become the same variable.
;
(defun ob$variabilize (ob variabilize? depth omit-slots include-slots)
  (setq *instan-obs* nil)
  (setq *any-unbound?* nil)
  (ob$instantiate2 ob *empty-bd* depth omit-slots
                    include-slots '(t) variabilize? nil))

(defun ob$varize (ob variabilize?)
  (setq *instan-obs* nil)
  (setq *any-unbound?* nil)
  (ob$instantiate2 ob *empty-bd* 100 nil
                    nil '(t) variabilize? nil))

;
; omit-proc: a predicate determining whether an ob should be returned
; as is, without instantiation.
;
(defun ob$instan-omit (ob bd omit-proc depth omit-slots include-slots)
  (setq *instan-obs* nil)
  (setq *any-unbound?* nil)
  (ob$instantiate2 ob bd depth omit-slots include-slots nil nil omit-proc))

(setq *instantiate-omit-obs* nil)

(defun ob$instantiate-dbg (template bindings depth
                             omit-slots include-slots substit abstract
                             omit-proc)
  (ndbg-begin)
  (ndbg *gate-dbg* instantiate "Call ob$instantiate3: ~A ~A~%"
                                 template bindings)
  (let ((result (ob$instantiate3 template bindings depth
                                  omit-slots include-slots substit abstract
                                  omit-proc)))
    (ndbg *gate-dbg* instantiate "Return from ob$instantiate3: ~A~%" result)
    (ndbg-end)
    result))

;
; This should never be called from the top-level, at least without not
; first doing (setq *instan-obs* nil) and (setq *any-unbound?* nil).
;

(defun ob$instantiate3 (template bindings depth
                         omit-slots include-slots substit abstract
                         omit-proc)
  (cond
   ((let ((found (assq template *instan-obs*)))
         (if found
             (cdr found)
             nil)))
   ((and depth (< depth 0))
    template)
   ((and omit-proc (funcall omit-proc template)) template)
   ((not (ob? template)) template)
   ((var? template)
    (let ((found (bd-hyper-lookup (variable-name template) bindings)))
      (if found
          (cond
           ((var? found)
            (setq *any-unbound?* t)
;            (ndbg *gate-dbg* ob-warn "(?~A binding cycle)~%"
;                  (variable-name found))
            found)
           ((and (ob? found) (vars-in? found))
            (ob$instantiate2 found bindings (if depth (-1+ depth) nil)
                             omit-slots
                             include-slots substit abstract omit-proc))
           (else found))
          (progn
           (setq *any-unbound?* t)
;           (ndbg *gate-dbg* ob-warn "(?~A unbound)~%"
;                                      (variable-name template))
           template))))
   ((special? template)
    (ob$instan-special template bindings (if depth (-1+ depth) nil)
                           omit-slots include-slots
                       substit abstract omit-proc))
   (else ; (ob? template)
    (let ((result-ob (ob$create-empty)))
      (setq *instan-obs* (cons (cons template result-ob) *instan-obs*))
      (yloop
       (initial (rest (ob$pairs template))
                (substitution nil))
       (ywhile rest)
       (ydo (if (and (not (memq? (slots-name (car rest)) omit-slots))
                     (not (memq? (slots-name (car rest))
                          *permanent-ignore-slots*))
                     (not (null? (slots-value (car rest)))) ; todo
                     (if include-slots
                         (memq? (slots-name (car rest)) include-slots)
                         t))
               (progn
                (setq substitution (bd-lookup (slots-value (car rest)) substit))
                (ob$add result-ob (slots-name (car rest))
                     (cond
                      (substitution substitution)
                      ((and abstract
                            (funcall abstract (slots-value (car rest))))
                       (let ((uniqvar
                              (make-var (gen-id "var")
                                        (ty$get-major-type
                                         (ob$ty (slots-value (car rest)))))))
                         (setq substit (bd-bind! (slots-value (car rest))
                                                uniqvar substit))
                         uniqvar))
                      (else
                       (if (or (memq? (slots-value (car rest))
                                      *instantiate-omit-obs*)
                               (and (ob? (slots-value (car rest)))
                                    (ob$literal? (slots-value (car rest)))))
                           (slots-value (car rest))
                           (ob$instantiate2 (slots-value (car rest))
                                            bindings (if depth (-1+ depth)
                                                         nil)
                                            omit-slots include-slots substit
                                            abstract omit-proc)))))))
           (setq rest (cdr rest)))
       (yresult result-ob))))))


#|
Moved to gate_instan2.cl

(defun ob$instan-special 
 (template bindings depth omit-slots include-slots substit abstract omit-proc) 
 .. )
|#

;
;
; ob$instantiate!:
;
; This version of instantiate does not copy anything.
; It simply replaces all bound variables with their values.
;
                                                   
(defun ob$instantiate! (template bindings)
  (ob$instantiate1! template bindings nil))

(defun ob$instantiate1! (template bindings depth)
  (ob$instantiate2! template bindings depth nil nil))

(defun ob$instantiate2! (template bindings depth ob slot-name)
  (cond
   ((var? template)
    (let ((found (assq (variable-name template) (cdr bindings))))
      (if found
          (progn
           (ob$remove ob slot-name template)
           (ob$add ob slot-name (cadr found))
           (cadr found))
          (progn
           (ndbg *gate-dbg* ob-warn
             "Warning: No binding for ~A in instantiate.~%"
             template)
           template))))
   ((ob? template)
    (yloop
     (initial (rest (ob$pairs template)))
     (ywhile rest)
     (ydo (if (and (ob? (slots-value (car rest)))
                  (ob$literal? (slots-value (car rest))))
             (slots-value (car rest))
             (if (number? depth)
                 (if (> depth 1)
                     (ob$instantiate2! (slots-value (car rest))
                                        bindings
                                        (-1+ depth)
                                        template
                                        (slots-name (car rest)))
                     (slots-value (car rest)))
                 (ob$instantiate2! (slots-value (car rest))
                                   bindings
                                   nil
                                   template
                                   (slots-name (car rest)))))
         (setq rest (cdr rest)))
     (yresult template)))
   (else template)))

;
; Copies an ob down to the given depth. Does NOT replace variables
; with their values the way ob-instantiate does.
;
; (coding assistance from Sergio Alvarado)
;

(defun ob$copy (self)
  (setq *found-obs* nil)
  (copy-ob1 self 1 '(top-context)))

(defun ob$copy-deep (self)
  (setq *found-obs* nil)
  (copy-ob1 self 1000 nil))

(defun copy-ob (template)
  (setq *found-obs* nil)
  (copy-ob1 template 1 nil))

(defun ob$copy-omit (ob slots)
  (setq *found-obs* nil)
  (copy-ob1 ob 1 slots))

(defun copy-ob1 (template depth omit-slots)
  (cond
   ((var? template) template)
   ((ob? template)
    (cond    
      ((let ((found (assq template *found-obs*)))
            (if found
                (cadr found)
                nil)))
      (else
       (yloop
        (initial (new-ob (ob$create-empty)))
        (yfor sv in (ob$pairs template))
        (ydo (if (not (memq? (slots-name sv) omit-slots))
                (ob$add new-ob
                      (slots-name sv)
                      (if (and (ob? (slots-value sv))
                               (ob$literal? (slots-value sv)))
                          (slots-value sv)
                          (if (number? depth)
                              (if (> depth 1)
                                  (copy-ob1 (slots-value sv)
                                             (-1+ depth)
                                             omit-slots)
                                  (slots-value sv))
                              (copy-ob1 (slots-value sv)
                                         nil
                                         omit-slots))))))
        (yresult (progn
                  (push (list template new-ob) *found-obs*)
                   new-ob))))))
   (else template)))

(defun vars-in? (ob)
  (setq *found-vars* nil)
  (vars-in1? ob))

(setq *vars-in-ignores*
  '(linked-to linked-from linked-to-of linked-from-of
              analogical-episode main-motiv termination-context
              failure-context))

(defun vars-in1? (ob)
  (if (memq? ob *found-vars*)
      nil
      (progn
       (setq *found-vars* (cons ob *found-vars*))
       (cond
        ((and (ob? ob) (ob$literal? ob)) nil)
        ((ob? ob)
         (yloop (initial (result nil))
               (yfor sv in (ob$pairs ob))
               (ywhile (not result))
               (ydo (if (and (not (cx? (slots-value sv)))
                            (not (memq? (slots-name sv)
                                        *vars-in-ignores*))
                            (not (memq? (slots-name sv)
                                        *permanent-ignore-slots*)))
                       (if (and (var? (slots-value sv))
                                (not (memq? (slots-value sv) result)))
                           (setq result t)
                           (setq result (vars-in1? (slots-value sv))))))
               (yresult result)))
        (else nil)))))

(setq *found-vars* nil)

(defun variables-in (ob omit-slots)
  (setq *found-vars* nil)
  (variables-in1 ob omit-slots))

(defun variables-in1 (ob omit-slots)
  (if (memq? ob *found-vars*)
      nil
      (progn
       (setq *found-vars* (cons ob *found-vars*))
       (cond
        ((and (ob? ob) (ob$literal? ob)) nil)
        ((ob? ob)
         (yloop (initial (result nil))
             (yfor sv in (ob$pairs ob))
             (ydo (if (and (not (memq? (slots-name sv) omit-slots))
                          (not (cx? (slots-value sv))))
                  (if (and (var? (slots-value sv))
                          (not (memq? (slots-value sv) result)))
                     (setq result (cons (slots-value sv) result))
                     (setq result (union result (variables-in1 (slots-value
                                                         sv) omit-slots))))))
             (yresult result)))
        (else nil)))))

; End of file.
