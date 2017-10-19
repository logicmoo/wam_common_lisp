;;; CHANGES
;;; 19990429: Made this not a package. (erik)
;;; 19990506: I notice this is quite buggy: (1) if the YUNTIL clause contains
;;; the YFOR loop variable, it takes on the value NIL, (2) the YUNTIL clause
;;; cannot be placed before the YFOR clause. (erik)
;;; Version 1.1 released.
;;; Tidied up test code, comments cleaned up a bit, provide yloop as feature
;;;   and package, added version number, fixed ydo to be safer, included
;;;   hints from Jeni Tennison.  5-16-94 -FER
;;; fixed restrictive yfor i from x to y where x and y were required to be
;;;   numebrs at load time -fr 2/5/88
;;; made all clause keywords local to yloop package, and updated documentation
;;;    to reflect this fact.  -fr 10/27/86
;;; end tests also checked in the beginning to catch the case of no iteration
;;;     needed -fr 10/08/86
;;; note that I optimized the code in for so that it didn't eval its arg's 
;;;    twice -fr 7/19/86
;;; multiple end tests accomidated by use of or on end-tests
;;;   -fr 9/10/86
;;; Code originially in  conger:>jim>yale-loop>yale-loop.lisp. 
;;; Also available via anonymous ftp from 
;;;  ftp.cs.cmu.edu as 
;;;    /afs/cs/user/mkant/Public/Lisp/code/iter/loop/yloop/yloop.cl
;;;  and unicorn.ccc.nottingham.ac.uk as
;;;    pub/lpzfr/yloop.l
;;;          
;;; Questions or requests for later versions:
;;; Jim Panagos (jpanagos@world.std.com) or 
;;; Frank.Ritter@nottingham.ac.uk  (or Ritter@cs.cmu.edu) 
;;;
;;; 
;;; takes the following keywords:
;;;      YLOOP
;;;      INITIAL    (INITIAL (var1 val1) (var2 val2) ...)
;;;      BEFORE     (BEFORE (to-do-1) (to-do-2) ...)
;;;      AFTER      (AFTER  (to-do-1)(to-do-2) ...)
;;;      YFOR       (YFOR var1 {IN ON FROM} var2 TO var3)
;;;                    IN gets cars of the list, ON gets the cdrs
;;;      YDO        (YDO (to-do-1) (to-do-2) ...)
;;;      YRESULT    returns the rest of the clause in an implicet progn, or nil
;;;      NEXT       (NEXT   (var1 (+ var1 1)))
;;;      YWHILE     (YWHILE  {var1 (test)} )
;;;      YUNTIL     (YUNTIL  {var1 (test)} )
;;;      INCR
;;;      DECR
;;;      MAXIMIZE
;;;      SUM
;;;      YWHEN
;;;      LERROR
;;;
;;; Yale loop macro written in Common Lisp based on the loop construct in 
;;; McDermont, Charniak and Riesbeck's AI programming book.
;;;
;;; DESIGN APPROACH
;;; Each loop statement such as before or ywhile are macros themselves. Given
;;; their arguments they fill out a template of what should be stored in
;;; appropriate positions in the loop. The loop macro then fetches the
;;; collection of all templates and pieces them together the loop code (all 
;;; templates stored in the global *loop-alist*). The advantage of this 
;;; approach is that the syntax of the loop is order independent.
;;
;;; LOCAL LOOP VARIABLES
;;;
;;; Use INITIAL to define variables within the scope of the loop.
;;; E.g. (initial (foo 5) (bar 'baz)). This will be translated in 
;;; (let* ( (foo 5) (bar 'baz) ..) ..). Notice that 
;;; bindings are done sequentially via let*
;;;
;;; ITERATION DRIVING CLAUSES
;;;
;;; The iteration driving clauses are those discussed the sections of
;;; numeric iteration
;;; mapping of lists and the macros YWHILE and YUNTIL.  (YWHILE x) and (YUNTIL y)
;;; are translated to expand into (if (not x) nil (go loop)) and (if y nil 
;;; (go loop))
;;; 
;;; NUMERIC ITERATION
;;; There are 2 ways to perform numeric iteration. The first is via the YFOR 
;;; statement:
;;; e.g. (YFOR iteration-variable FROM begin-iteration TO  end-iteration STEP
;;;  inc-or-dec) [downward stepping can be implemented using negative steps]
;;; The second is via the (incr ..) and (decr ..) constructs. FROM and IN are 
;;; synonyms in this construct. If the .IN. type construct is desired 
;;; (see documentation), use IN not FROM. A step value is optional in both
;;; cases and defaults to 1.
;;;
;;; MAPPING OF LISTS
;;; Two constructs are provided for list mapping both accessible via the FOR
;;; statement. The IN construct permits mapping over successive elements of 
;;; the list. Eg. (yfor a in '(1 2 3 4))
;;; The ON constuct is similar to in except that it maps over successive cdrs.
;;;
;;; Examples 
;;;
;;; (yloop (incr a .in. 0 to 10) (ydo (print a))) ;print 0..10
;;; (yloop (ydo (print 'a)))
;;; 
;;; (yloop(initially a 0 b 5)(yfor x from 0 to 10)(ydo(print x))(yresult b))
;;;    will print 0..10 and return 5.
;;;
;;; 
;;;
;;; ADDING NEW LOOP MACROS
;;;
;;; Code has been provided to add the user define his/her own loop macros. 
;;; See explanation and code in the file.
;;;
;;; HINTS
;;;
;;; On Translation time syntax checking: as clauses are independent macros, 
;;; translation time syntax checking will be clumbersome. The  values in 
;;; *loop-alist* will have to be used after that list is fully constituted.
;;;
;;; EXPORT CONTROL
;;;
;;; Note that all symbols that will be used in trio, or some other package, 
;;; have to be exported.

;;  ;; fix 9-May-93 -FER  in DEFINE-AND-RENAME-LOOP-LOCALS NIL
;;  (if result (add-element-to-loop-alist (cons 'progn (list result)) 'result))
;;
;; Also need to fix yloop so that results get spliced in within a prog, list,
;;  or values (don't know how result is being used if there are multiple ones,  

(defvar *loop-alist* () "To contain translated loop information ")

(defmacro clear-loop-alist ()
  `(setf *loop-alist* nil))

(defmacro fetch-clauses (clause-key)
  `(car (rassoc ',clause-key *loop-alist*)))
        
(defmacro acons-setf (key datum list)  
  "Functions like acons accept changes list to the new value."
  `(setf ,list (acons ,key ,datum ,list)))

(defmacro before (&rest body)  
  `(add-element-to-loop-alist (cons 'progn ',body) 'before))

(defmacro initial (&rest body)
  `(dolist (clause ',body)
    (add-element-to-loop-alist clause 'initializations))) 

(defmacro next (&rest clauses)
  "Next clauses must be of the form (var exp). Eg (next (i (+ i 1)))."
  `(let ( (assignment-list nil)        )
     (dolist (clause ',clauses)
        (setf assignment-list (cons (cons 'setf clause) assignment-list)))
     (add-element-to-loop-alist (cons 'progn assignment-list) 'next)))

(defmacro yresult (&rest clauses)
  `(add-element-to-loop-alist (cons 'progn ',clauses) 'result))

(defmacro ydo (&rest clauses)
  `(add-element-to-loop-alist (cons 'progn ',clauses) 'do))

(defmacro ywhile (expression)
  `(add-element-to-loop-alist (list 'not ',expression) 'end-test))

(defmacro yuntil (expression)
  `(add-element-to-loop-alist  ',expression 'end-test))

(defmacro lerror (format-string &rest format-args)
  `(error ,format-string ,@format-args))

(defvar *stepping-variable* nil 
  "Dummy variable to nest macros without compiler barf.")
(defvar *what-to-do* nil 
  "Dummy variable to nest macros without compiler barf.")
(defvar *llist* nil 
  "Dummy variable to nest macros without compiler barf.")

(defmacro yfor  (variable what-to-do &rest llist)
  (let ((iteration-variable (gensym))
          (iteration-expression (gensym))
          stepping-variable)
    `(let ((,iteration-variable nil)
           (,iteration-expression nil) )
       ,(record-in-loop-alist `(,variable ,iteration-variable) 'iteration-variable)
       #| 
        #+ignore
           (format t "~% yfor variable is: ~a ~% and it is ~a to in"
                       (intern (symbol-name what-to-do))
                (eq 'in (intern (symbol-name what-to-do)))) |#
       ,(case (intern (symbol-name what-to-do))
          (in
            (record-in-loop-alist `(endp ,iteration-expression) 'end-test)
            (add-elements-to-clause 'next
            `(setf ,iteration-variable (car ,iteration-expression))
            `(setf ,iteration-expression (cdr ,iteration-expression)))
            (add-elements-to-clause 'initializations
                                    `(,iteration-variable  ;(car ,@llist))
                                                           (car ,iteration-expression))
                                    `(,iteration-expression  ,@llist))
            )   
          (on
            (record-in-loop-alist iteration-expression 'iteration-control-variable)
            (record-in-loop-alist `(endp ,iteration-expression) 'end-test)
            (add-elements-to-clause 'next
                             `(setf ,iteration-variable ,iteration-expression)
                             `(setf ,iteration-expression (cdr ,iteration-expression)))
            ; note that since you are in a let*, don't eval the expression twice, use
            ; the variable that it will be bound to
            (add-elements-to-clause 'initializations
                             `(,iteration-variable  (car ,iteration-expression))
                            `(,iteration-expression ,@llist)))
          (from     
            (if (null (fifth llist)) (setf stepping-variable 1)
                  (setf stepping-variable (fifth llist)))
            (cond
              ((> (length llist) 5) 
                (lerror "YL:Too many clauses in (yfor ~a ~a ..)" variable
                                       what-to-do))
              ((and (minusp stepping-variable)(<= (first llist) (third llist)))
               (lerror "YL:Cannot decrement from ~a to ~a" 
                        (first llist) (third llist)))
              (t 
               (add-element-to-loop-alist `(,iteration-variable ,(first llist))
                                          'initializations)
               (add-element-to-loop-alist `(setf ,iteration-variable
                                            (+ ,iteration-variable ,stepping-variable)) 'next)
               (if (minusp stepping-variable )
                   (add-element-to-loop-alist `(< ,iteration-variable ,(third llist))
                                              'end-test )
                   (add-element-to-loop-alist `(> ,iteration-variable ,(third llist))
                                              'end-test)))))
       ))) t)

(defmacro with-incr-or-decr-checking (&body body)
  "Very specialized code to fit in the incr and decr macros."
  `(progn
    (cond
      ((null args)
       (setf final t)
       (setf step 1))
      ((numberp (first args))
       (lerror "Syntax error in incr: expected a yloop keyword after ~a" init))
      ((not (numberp (second args)))
       (lerror "Syntax error in incr: ~a not a number" (second args)))      
      (t (setf final (second args))
         (if (null (fourth args))
             (setf step 1)
             (setf step (fourth args)))))
    ,@body))

(defmacro incr (variable from init &rest args) 
  (let* (final step (iteration-variable (gensym)))
    `(let ( (,iteration-variable nil) )
       ,(record-in-loop-alist `(,variable ,iteration-variable)
                              'iteration-variable)
    ,(with-incr-or-decr-checking
      (add-element-to-loop-alist `(setf ,iteration-variable
                                        (+ ,iteration-variable ,step)) 'next)
      (case (intern (symbol-name from))
        (.in. (record-in-loop-alist `(> ,iteration-variable ,final) 'end-test)
              (add-element-to-loop-alist `(,iteration-variable ,init)
                                         'initializations))
        (.in  (record-in-loop-alist `(= ,iteration-variable ,final) 'end-test)
              (add-element-to-loop-alist `(,iteration-variable ,init)
                                         'initializations))
        (in.  (record-in-loop-alist `(> ,iteration-variable ,final) 'end-test)
              (add-element-to-loop-alist `(,iteration-variable ,(1+ init))
                                         'initializations))
        (in   (record-in-loop-alist `(= ,iteration-variable ,final) 'end-test)
              (add-element-to-loop-alist `(,iteration-variable ,(1+ init))
                                         'initializations))
        (otherwise
          (record-in-loop-alist `(> ,iteration-variable ,final) 'end-test)
          (add-element-to-loop-alist `(,iteration-variable ,init) 
                                     'initializations))))))
    t)

(defmacro decr (variable from init &rest args)
  (let (final step (iteration-variable (gensym)))

    `(let ((,iteration-variable nil))
           ,(record-in-loop-alist `(,variable ,iteration-variable) 
                                  'iteration-variable)
    ,(with-incr-or-decr-checking
      (when (<= init final)
        (lerror
       "Cannot decrement from ~a downto ~a. Check the order of your arguments"
       init final))
      (add-element-to-loop-alist
       `(setf ,iteration-variable (- ,iteration-variable ,step))
       'next)
      (case (intern (symbol-name from))
      (.in. (record-in-loop-alist `(< ,iteration-variable ,final) 'end-test)
            (add-element-to-loop-alist `(,iteration-variable ,init) 'initializations))
      (.in  (record-in-loop-alist `(= ,iteration-variable ,final) 'end-test)
            (add-element-to-loop-alist `(,iteration-variable ,init) 'initializations))
      (in.  (record-in-loop-alist `(< ,iteration-variable ,final) 'end-test)
            (add-element-to-loop-alist `(,iteration-variable ,(1- init)) 'initializations))
      (in   (record-in-loop-alist `(= ,iteration-variable ,final) 'end-test)
            (add-element-to-loop-alist `(,iteration-variable ,(1- init)) 'initializations))
      (otherwise
        (record-in-loop-alist `(> ,iteration-variable ,final) 'end-test)
        (add-element-to-loop-alist `(,iteration-variable ,init) 'initializations)))
      ) )) t)
     
(defmacro after (&rest clauses)
  `(add-element-to-loop-alist (cons 'progn ',clauses) 'after))

(defun fetch-new-iteration-variable ()
  (second (car (fetch-clauses iteration-variable))))

(defun fetch-old-iteration-variable ()
  (first (car (fetch-clauses iteration-variable))))

(defun record-in-loop-alist (element key)
  "Adds new assoc pairs in *loop-alist*."
  (acons-setf (list element) key *loop-alist*))

(defun add-element-to-loop-alist (element clause-key)
  "Adds elements to a particular assoc sublist."
  (cond
    ((null (rassoc clause-key *loop-alist*))
     (record-in-loop-alist element clause-key))
    (t (rplaca (rassoc clause-key *loop-alist*)
               (cons element (car (rassoc clause-key *loop-alist*)))))))

(defun add-elements-to-end-of-clause (clause-key &rest elements)
  (dolist (element elements) (add-element-to-end-of-loop-alist element clause-key)))

(defun add-elements-to-clause (clause-key &rest elements)
  (dolist (element elements) (add-element-to-loop-alist element clause-key)))

(defun substitute-iteration-variable (list)
  "Substitutes iteration variables with those given by gensym."
  (let* (
         
        ;; Worring about the effect of (subst..) on ((#) . iteration-variable) 
        ;; the hard way  (sublis ..) may work better
         
        (saved-iteration-variable-clause (rassoc 'iteration-variable
                                                 *loop-alist*))
        (new-iteration-variable-symbol (fetch-new-iteration-variable))
        (old-iteration-variable-symbol (fetch-old-iteration-variable))
        (secured-list (remove (rassoc 'iteration-variable *loop-alist*)
                              list))
        )
    (cond
      ((null (or  new-iteration-variable-symbol old-iteration-variable-symbol))
       (lerror "No iteration variable defined"))  ;;; should not be required -fr
      (t (cons saved-iteration-variable-clause
               (subst new-iteration-variable-symbol
                      old-iteration-variable-symbol secured-list)))))) 

(defun iteration-variable-exists-p ()
  (fetch-clauses iteration-variable))

(defmacro yloop (&rest clauses)
  (setf *loop-alist* nil)
  (mapcar 'eval clauses)
  (when (iteration-variable-exists-p)
    ;; you have an iteration variuable to subsitute
    (setf *loop-alist* (substitute-iteration-variable *loop-alist*)))
  (let
    (
     (dos (fetch-clauses do))
     (afters (fetch-clauses after))
     (end-tests (fetch-clauses end-test))
     (bindings (fetch-clauses initializations))
     (result (fetch-clauses result))
     (nexts (fetch-clauses next))
     (befores (fetch-clauses before))
     (middle-stuff (fetch-clauses middle))
     (front-stuff (fetch-clauses front))
     (end-stuff (fetch-clauses end))
     (block-label (gensym))
    )
    (setf *loop-alist* nil)
    ;; if there are multiple end-test's, accomidate them
    ;; with an or wrapped around the end-test    
    `(unwind-protect
         (block ,block-label
           (let* (,@bindings)
             ,@befores ,@front-stuff
             ;if you have nothing to do, jump -fr
             (if (or ,@end-tests)
                 (return-from ,block-label ,@(or result '(nil))))
             (tagbody loop
                      ,@dos ,@middle-stuff ,@nexts
                      (if (or ,@end-tests) nil (go loop)))
             ,@afters ,@end-stuff
             ;; return results or nil              
             (return-from ,block-label ,@(or result '(nil)))))
       ,(clear-loop-alist))
      ))

(defmacro maximize (expression)
  (add-element-to-end-of-loop-alist `(maximum-variable ,expression) 
                                    'initializations) 
  (add-element-to-loop-alist
    `(if (> ,expression maximum-variable)
         (setf maximum-variable ,expression))
    'middle-stuff)
  (result maximum-variable)
  t)

(defun add-element-to-end-of-loop-alist (element clause-key)
  "Adds elements to a particular assoc sublist."
  (cond
    ((null (rassoc clause-key *loop-alist*))
     (record-in-loop-alist element clause-key))
    (t (rplaca (rassoc clause-key *loop-alist*)
               (reverse (cons element 
                              (car (rassoc clause-key *loop-alist*))))))))

(defun define-and-rename-loop-locals (where-to-add arg-list result body)
  (when arg-list
    (dolist (clause arg-list)
      (let* (
             (var nil)
             (new-var (gensym)))
        (if (listp clause) (setq var (car clause)) (setq var clause))
            ;; nsubst doesnt work on body as it isn't quite represented 
            ;; as a list on the function stack
        (setf arg-list (subst new-var var arg-list))
        (setf body (subst new-var var body))
        (setf result (subst new-var var result)))))
  
  (if result (add-element-to-loop-alist (cons 'progn (list result)) 'result))
  (add-element-to-loop-alist  body where-to-add)
  (when arg-list
    (dolist (new-var arg-list)
    (add-element-to-loop-alist new-var 'initializations))))

(defmacro add-to-loop-macro (where-to-add arg-list result &body body)  
  `(define-and-rename-loop-locals ',where-to-add ',arg-list ',result ',@body))

;;;     Examples of "programmer" defined loop macro functions. They are to
;;; function as their zetalisp loop counterparts. To define a yloop macro
;;; you must invoke the macro ADD-TO-LOOP-MACRO. This macro: 1)substitutes
;;; symbols (via gensym) so as to avoid symbol conflicts within the loop
;;; in the future; 2) provides requested local loop variables that will be
;;; within the lexical scope of the repeating statements (i.e. the loop),
;;; and ; 3)places the new code in the requested part of the
;;; loop. (Specifically the yloop macro is conceptually separated into 3
;;; parts: the FRONT, the MIDDLE and the end. Code that is in the FRONT of
;;; the yloop macro is executed after local bindings are made but before
;;; the executions of the statements to be repeated. Code that is in the
;;; MIDDLE of the yloop macro is executed after the FRONT code has been
;;; executed and is executed repeatedly until some termination condition
;;; is met. Code in the END of the yloop macro is executed after the loop
;;; terminates normally.)  The first argument to ADD-TO-LOOP-MACRO is to
;;; indicate where to place the new code.  It is to be one of FRONT MIDDLE
;;; END. The second argument is a list of desired local yloop
;;; variables. The syntax is to be the same as the car of let statements
;;; as that list will actually be placed at the position of the first
;;; argument in the let statement. The third argument is the variable
;;; which will given to the (return ) statement of the loop so that its
;;; value will be returned on normal termination of the loop. And the
;;; final arguments are to be the body of new macro to be inserted in the
;;; loop .
;;; 
;;; Hint When you want something returned, declare a new local loop
;;; variable, declare it as that which will be returned and set your
;;; answer to it.

;(defmacro sum (expression)
;  `(add-to-loop-macro middle ((sum 0)) sum
;    (setq sum (+ sum ,expression))))

;(defmacro ywhen (test &body clauses-to-execute)
;  `(add-to-loop-macro middle nil nil
;                      (when ,test ,@clauses-to-execute)))

(defun find-form (sequence form-to-find)
  (cond
    ((atom sequence) nil)
    ((null sequence) nil)
    ((equal (car sequence) form-to-find) sequence)
    (t (list-without-nils
         (find-form (car sequence) form-to-find)
         (find-form (cdr sequence) form-to-find)))))

(defun substitute-loop-return (label lisp-expressions)
  (dolist (subst-clause (find-form lisp-expressions 'loop-return))
    (nsublis `(,subst-clause  (return-from ,label (cdr subst-clause)))
     lisp-expressions)))

(defun list-without-nils (a b)
  (cond ((or (null a) (null b)) (append a b))
        (t (list a b))))

;;; *EOF*
