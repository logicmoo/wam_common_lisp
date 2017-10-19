;*******************************************************************************
;
; GATE
; Version 2.3
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
;*******************************************************************************

(defmacro fixnum->string (n) `(format nil "~A" ,n))

(defmacro dbg (dbg-var . rest)
  `(progn
    (cond
     ((eq? ,dbg-var t)
      (format t ,@rest))
     (,dbg-var
      (format ,dbg-var ,@rest))
     (else nil))))

(defmacro if-interested-in (key . rest)
  `(if (and (assq ',key *ndbg-interests*)
            (or (memq? 'all (assq ',key *ndbg-interests*))
                (any? (lambda (x) (memq? x *ndbg-items*))
                      (cdr (assq ',key *ndbg-interests*)))))
       (progn ,@rest)
       nil))

(defmacro ndbg (dbg-stream key . rest)
  `(progn
    (if (and (assq ',key *ndbg-interests*)
             (or (memq? 'all (assq ',key *ndbg-interests*))
                 (any? (lambda (x) (memq? x *ndbg-items*))
                       (cdr (assq ',key *ndbg-interests*)))))
        (cond
         ((eq? ,dbg-stream t)
;          (format (standard-output) "~&")
;          (ndbg-indentation (standard-output))
          (format (standard-output) ,@rest)
          t)
         (,dbg-stream
;          (format ,dbg-stream "~&")
;          (ndbg-indentation ,dbg-stream)
          (format ,dbg-stream ,@rest)
          t)
         (else nil))
        nil)))

(defmacro ndbg-if (key form)
  `(if (and (assq ',key *ndbg-interests*)
            (or (memq? 'all (assq ',key *ndbg-interests*))
                (any? (lambda (x) (memq? x *ndbg-items*))
                      (cdr (assq ',key *ndbg-interests*)))))
       ,form))

(defmacro length-one? (x)
  `(and ,x (null? (cdr ,x))))

(defmacro nil? (x)
  `(or (null? ,x)
       (eq? ,x 'nil)))

(defun symbolconc (sym suffix) (intern (CONCATENATE 'string (string sym) (format () "~A" suffix))))

(defmacro pc (context-abbr)
  `(cx$print (eval (symbolconc 'CX. ,context-abbr))))

(defmacro pca (context-abbr)
  `(cx$print-ancestors (eval (symbolconc 'CX. ,context-abbr))))

(defmacro mem-empty-unify (ob obs context)
  `(mem (lambda (x y)
         (bd-and-empty-bd? (ob$unify-cx x y *empty-bd* ,context)))
        ,ob ,obs))

(defmacro mem-empty-unify? (ob obs context)
  `(mem? (lambda (x y)
         (bd-and-empty-bd? (ob$unify-cx x y *empty-bd* ,context)))
        ,ob ,obs))

(defmacro mem-unify (ob obs context)
  `(mem (lambda (x y) (ob$unify-cx x y *empty-bd* ,context)) ,ob ,obs))

(defmacro mem-unify? (ob obs context)
  `(mem? (lambda (x y) (ob$unify-cx x y *empty-bd* ,context)) ,ob ,obs))

(defmacro del-unify! (ob obs context)
  `(del! (lambda (x y) (ob$unify-cx x y *empty-bd* ,context)) ,ob ,obs))

(defmacro retrieve-bd->ob (bd)
  `(map 'list (lambda (x) (car x)) ,bd))

(defmacro cx? (x)
  `(and (ob? ,x)
        (eq? (ob$ty ,x) *cx-ob*)))

(defmacro touchable-fact? (fact)
  `(not (ty$instance? ,fact 'causal-link)))

(defmacro ob$instantiate2 (template bindings depth
                                 omit-slots include-slots substit abstract
                                 omit-proc)
  `(if *unify-debugging?*
       (ob$instantiate-dbg ,template ,bindings ,depth
                            ,omit-slots ,include-slots ,substit ,abstract
                            ,omit-proc)
       (ob$instantiate3 ,template ,bindings ,depth
                         ,omit-slots ,include-slots ,substit ,abstract
                         ,omit-proc)))

;
; (ob? obj):
;
; Determine if an arbitrary Lisp object is an ob.
;
(defmacro ob? (obj) `(typep ,obj 'obr))

(defmacro enforce-ob (obj routine)
  `(if (not (ob? ,obj))
       (setq ,obj (error "~A: ~A not ob" ,routine ,obj))))

(defmacro ob$ty (ob)
 `(ob$get ,ob 'type))

(defmacro ty? (x)
  `(and (ob? ,x)
        (eq? (ob$ty ,x) *ty-ob*)))

(defmacro path->slot-name (path)
  `(tlast ,path))

(defmacro var? (x)
  `(and (ob? ,x)
        (ty$instance? ,x 'uvar)))

(defmacro special? (x)
  `(and (ob? ,x)
        (ty$instance? ,x 'uspecial)))

(defmacro car-eq? (x y)
 `(and (pair? ,x) (eq? (car ,x) ,y)))

(defmacro variable-name (x)
 `(ob$get ,x 'name))

(defmacro variable-type (x)
 `(ob$get ,x 'unifies-with))

; Setters: For consistency, access to slots in obr is done through
; these macros.

(defmacro set-obr-obnames (ob val)
  `(setf (obr-obnames ,ob) ,val))

(defmacro set-obr-slots (ob val)
  `(setf (obr-slots ,ob) ,val))

(defmacro set-obr-literal (ob val)
  `(setf (obr-literal ,ob) ,val))

;
; Accessor functions for elements of the (obr-slots self) instance variable,
; which contains a triple of
;   slot-name
;   slot-value (a single value--multiple values for a slot require
;               multiple entries in (obr-slots self))
;

(defmacro slots-name (slots) `(car ,slots))

(defmacro slots-value (slots) `(cadr ,slots))

(defmacro with-unhidden-default (&rest body)
   `(unwind-protect
       (progn (setq *hidden-default* nil)
              ,@body)
       (setq *hidden-default* t)))

;
; ob$create-empty: create a new empty ob
;
(defmacro ob$create-empty ()
  '(ob$create-named-empty nil))

(defmacro ndbg-newline (stream key)
  `(if-interested-in ,key (do-newline ,stream)))

(defmacro ndbg-large-roman-font (stream key)
  `(if-interested-in ,key (begin-large-roman-font ,stream)))

(defmacro ndbg-large-bold-font (stream key)
  `(if-interested-in ,key (begin-large-bold-font ,stream)))

(defmacro ndbg-roman-font (stream key)
  `(if-interested-in ,key (begin-roman-font ,stream)))

(defmacro ndbg-bold-font (stream key)
  `(if-interested-in ,key (begin-bold-font ,stream)))

(defmacro ndbg-italic-font (stream key)
  `(if-interested-in ,key (begin-italic-font ,stream)))

(defmacro ndbg-slanted-font (stream key)
  `(if-interested-in ,key (begin-slanted-font ,stream)))

(defmacro ndbg-end-font (stream key)
  `(if-interested-in ,key (end-font ,stream)))

(defmacro ndbg-roman (stream key . rest)
  `(if-interested-in ,key
                     (begin-roman-font ,stream)
                     (ndbg ,stream ,key ,@rest)
                     (end-font ,stream)))

(defmacro ndbg-roman-nl (stream key . rest)
  `(if-interested-in ,key
                     (begin-roman-font ,stream)
                     (ndbg ,stream ,key ,@rest)
                     (end-font ,stream)
                     (do-newline ,stream)))

(defmacro ob$create (spec)
  `(ob$readlist ,spec))

(defmacro ob$fcreate (spec)
  `(ob$freadlist ,spec))

(defmacro special-priority? (ob1 ob2)
  `(cond
    ((not (special? ,ob1)) nil)
    ((not (special? ,ob2)) t)
    ((eq? (ob$ty ,ob1) (ob$ty ,ob2)) t)
    (else (memq? ,ob2 (memq ,ob1 *special-priorities*)))))
; REALLY: one should really be memq? and the other memq.

(defmacro old-special-priority? (ob1 ob2)
  `(cond
    ((not (special? ,ob1)) nil)
    ((not (special? ,ob2)) t)
    ((ty$instance? ,ob1 'uor) t)
    ((and (ty$instance? ,ob1 'uand)
          (ty$instance? ,ob2 'uor))
     nil)
    ((ty$instance? ,ob1 'uand) t)
    ((and (ty$instance? ,ob1 'unot)
          (or (ty$instance? ,ob2 'uor)
              (ty$instance? ,ob2 'uand)))
     nil)
    ((ty$instance? ,ob1 'unot) t)
    ((and (ty?instance? ,ob1 'uproc)
          (or (ty$instance? ,ob2 'uor)
              (ty$instance? ,ob2 'uand)
              (ty$instance? ,ob2 'unot)))
     nil)
    (else t)))

(defmacro var-ty$instance? (x y)
  `(if (null? ,y)
       t
       (and (ob? ,x)
            (ty$instance-of? ,x ,y))))

(defmacro type-compatible-vars? (var1 var2)
  `(or *relax-unify-var*
      (null? (variable-type ,var1))
      (null? (variable-type ,var2))
      (memq? (variable-type ,var1) (ty$supertypes* (variable-type ,var2)))
      (memq? (variable-type ,var2) (ty$supertypes* (variable-type ,var1)))))

(defmacro with-inverse-setting-default-off (&rest body)
  `(let ((result nil))
     (inverse-setting-default-off)
     (setq result (progn ,@body))
     (inverse-setting-default-on)
     result))

(defmacro bd-bind (var value bindings)
  `(if ,var ; this is for ob$unify-var which might pass a null var name.
       (cons 't (cons (list ,var ,value) (cdr ,bindings)))
       ,bindings))

(defmacro bd-bind! (var value bindings)
;  (if (null? bindings) (error "bd-bind!: null bindings))
  `(setf (cdr ,bindings) (cons (list ,var ,value) (cdr ,bindings))))

(defmacro bd-lookup (var bindings)
  `(and ,bindings
        (let ((found (assq ,var (cdr ,bindings))))
          (if found (cadr found) nil))))

(defmacro bd-hyper-lookup (var bd)
  `(bd-hyper-lookup1 ,var ,bd nil nil))

;
; Extra level to print debugging information.
; Should never be used from the top-level.
;
(defmacro ob$unify2 (ob1 ob2 bindings ignore-slots)
  `(if *unify-debugging?*
       (ob$unify-dbg ,ob1 ,ob2 ,bindings ,ignore-slots)
       (ob$unify0 ,ob1 ,ob2 ,bindings ,ignore-slots)))

;
; Top-level unifier call
;
(defmacro ob$unify1 (ob1 ob2 bindings ignore-slots)
  `(let ((already-matched *already-matched*)
         (result nil))
     (setq *diff?* nil)
     (setq *already-matched* (cons t nil))
     (setq result (ob$unify2 ,ob1 ,ob2 ,bindings ,ignore-slots))
     (setq *already-matched* already-matched)
     result))

;
; Top-level diffifier call
;
(defmacro ob$diff1 (ob1 ob2 bindings ignore-slots)
  `(let ((already-matched *already-matched*)
         (result nil))
     (setq *diff?* t)
     (setq *already-matched* (cons t nil))
     (setq result (ob$unify2 ,ob1 ,ob2 ,bindings ,ignore-slots))
     (setq *already-matched* already-matched)
     result))

;
; Top-level unifier call
;
(defmacro ob$unify (ob1 ob2 bindings)
  `(ob$unify1 ,ob1 ,ob2 ,bindings nil))

;
; Top-level diffifier call
;
(defmacro ob$diff (ob1 ob2 bindings)
  `(ob$diff1 ,ob1 ,ob2 ,bindings nil))

;
; Top-level unifier calls (with context)
;
(defmacro ob$unify-cx (ob1 ob2 bindings context)
  `(progn
    (setq *unify-context* ,context)
    (ob$unify1 ,ob1 ,ob2 ,bindings nil)))

(defmacro ob$unify-cx1 (ob1 ob2 bindings ignore-slots context)
  `(progn
    (setq *unify-context* ,context)
    (ob$unify1 ,ob1 ,ob2 ,bindings ,ignore-slots)))

;
; ob$compare: Compare two obs and produce a substitution
; binding list containing differences.
;

(defmacro ob$compare (source target ignore-slots)
  `(let ((already-matched *already-matched*)
         (result nil))
     (setq *already-matched* (cons t nil))
     (setq result (ob$compare1 ,source
                               ,target
                               *empty-bd*
                               ,ignore-slots
                               (lambda (source target)
                                 (cond
                                  ((and (ty? source)
                                        (ty? target))
                                   (ty$least-common-supertype source target))
                                  ((and (ob$ty source)
                                        (ob$ty target))
                                   (ty$least-common-supertype (ob$ty source)
                                                           (ob$ty target)))
                                  (else nil)))))
     (setq *already-matched* already-matched)
     result))

; End of file.
