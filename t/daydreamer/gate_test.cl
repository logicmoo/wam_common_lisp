;*******************************************************************************
;
; GATE
; Version 2.3
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
;  2/23/87: First version written
; 20041220: Added tests
;
;*******************************************************************************

(setq *question-mark-atom* '?)
(load "gate_get")

(setq *test-succeeded* 0)
(setq *test-total* 0)

(defmacro test (a b)
  `(let ((a1 ,a))
    (setq *test-total* (+ *test-total* 1))
    (if (equal a1 ,b)
        (progn
         (format t "SUCCEEDED ~A~%" ',a)
         (setq *test-succeeded* (+ *test-succeeded* 1)))
        (progn
         (format t "FAILED ~A~%" ',a)
         (format t "==>~%")
         (format t "~A~%" a1)
         (format t "instead of~%")
         (format t "~A~%" ,b)))))

(test
 (ty$fcreate 'PERSON nil '(name age occupation))
 (ob$name->ob 'PERSON))

(test
 (ob$fcreate '(PERSON name "Karen" age 27 occupation 'DOCTOR obname Karen1))
 (ob$name->ob 'Karen1))

(test
 (ob$fcreate '(PERSON name "Jim" age 31 occupation 'COMPOSER obname Jim1))
 (ob$name->ob 'Jim1))

(test
 ^Karen1
 (ob$name->ob 'Karen1))

(test
 ^Jim1
 (ob$name->ob 'Jim1))

(test
 (ob$get ^Karen1 'name)
 "Karen")

(test
 (ob$get ^Jim1 'age)
 31)

(test
 (ob$get ^Jim1 'occupation)
 'COMPOSER)

(test
 (ob$set ^Jim1 'age 32)
 32)

(test
 (ob$get ^Jim1 'age)
 32)

(test
 (ty$fcreate 'ACTION nil '(actor from to obj))
 (ob$name->ob 'ACTION))

(test
 (ty$fcreate 'STATE nil nil)
 (ob$name->ob 'STATE))

(test
 (ty$fcreate 'ATRANS '(ACTION) '(actor from to obj))
 (ob$name->ob 'ATRANS))

(test
 (ty$fcreate 'MTRANS '(ACTION) '(actor from to obj))
 (ob$name->ob 'MTRANS))

(test
 (ty$fcreate 'PTRANS '(ACTION) '(actor from to obj))
 (ob$name->ob 'PTRANS))

(test
 (ty$fcreate 'LIVES-IN '(STATE) '(actor loc))
 (ob$name->ob 'LIVES-IN))

(test
 (ty$fcreate 'MAGAZINE nil '(name))
 (ob$name->ob 'MAGAZINE))

(test
 (ob$fcreate '(ATRANS actor Jim1
                      from Jim1
                      to Karen1
                      obj (MAGAZINE name "Ear Magazine")
                      obname Atrans1))
 (ob$name->ob 'Atrans1))

(test
 (ob$fcreate '(PERSON name "Peter" age 26 occupation 'MUSICIAN obname Peter1))
 (ob$name->ob 'Peter1))

(test
 (ob$fcreate '(MTRANS actor Jim1
                      from Jim1
                      to Peter1
                      obj Atrans1
                      obname Mtrans1))
 (ob$name->ob 'Mtrans1))

(test
 (ty$instance? ^Atrans1 'ACTION)
 t)

(test
 (ty$instance? ^Atrans1 'ATRANS)
 t)

(test
 (ty$instance? ^Atrans1 'MTRANS)
 nil)

(test
 (ty$instance? ^Atrans1 'PERSON)
 nil)

(test
 (ty$instance? ^Mtrans1 'MTRANS)
 t)

(progn
 (setq pattern (ob$fcreate '(MTRANS actor ?Person1
                                    from ?Person1
                                    to ?Person2
                                    obj ?Anything)))
 *repl-wont-print*)

(test
 (setq bd (ob$unify pattern ^Mtrans1 *empty-bd*))
 `(T (ANYTHING ,^Atrans1)
     (PERSON2 ,^Peter1)
     (PERSON1 ,^Jim1)))

(test
 (ob->list (ob$instantiate pattern bd))
 '(MTRANS actor Jim1
          from Jim1
          to Peter1
          obj Atrans1))

(test
 (ty$fcreate 'POSS '(STATE) '(actor obj))
 (ob$name->ob 'POSS))

(test
 (ty$fcreate 'INFERENCE nil '(if then))
 (ob$name->ob 'INFERENCE))

(progn
 (setq *infs*
       (list (ob$fcreate '(INFERENCE if (ATRANS actor ?Person1
                                                from ?Person1
                                                to ?Person2
                                                obj ?Object)
                                     then (POSS actor ?Person2
                                                obj ?Object)))))
 *repl-wont-print*)

(defun forward-inferences (cd)
  (yloop (initial (bd nil) (result nil))
         (yfor inf in *infs*)
         (ydo (if (setq bd (ob$unify (ob$get inf 'if) cd *empty-bd*))
                  (setq result (cons (ob$instantiate (ob$get inf 'then) bd)
                                     result))))
         (yresult result)))

(test
 (ob->list (car (forward-inferences ^Atrans1)))
 `(POSS actor Karen1
        obj Ob.39))

(test
 (ty$fcreate 'LOCATION nil '())
 (ob$name->ob 'LOCATION))

(test
 (ty$fcreate 'STORE '(LOCATION) '())
 (ob$name->ob 'STORE))

(test
 (ob$fcreate '(STORE obname Store1))
 (ob$name->ob 'Store1))

(test
 (ty$fcreate 'PROX '(STATE) '(actor loc))
 (ob$name->ob 'PROX))

(progn
 (setq *prules*
  (list
   (ob$create '(PRULE subgoal (ROR obj (PTRANS actor ?Person
                                               to ?Location)
                                       (LIVES-IN actor ?Person
                                                 loc ?Location))
                     goal (PROX actor ?Person
                                loc ?Location)))))
 *repl-wont-print*)

(progn
 (setq *pfacts* (cx$create))
 (cx$assert *pfacts* (ob$fcreate '(PTRANS actor Jim1 to Store1)))
 *repl-wont-print*)

(test
 (ob->list
  (ob$prove (ob$fcreate '(PROX actor ?Person loc Store1)) *empty-bd* 999))
 `((t (person Jim1))))

(ty$create 'FATHER-OF nil '(nil (actor obj) ()))
(ty$create 'MOTHER-OF nil '(nil (actor obj) ()))
(ty$create 'GRANDFATHER-OF nil '(nil (actor obj) ()))
(ty$create 'GRANDPARENT-OF nil '(nil (actor obj) ()))

(setq *prules* nil)

(ob$add-prule
 (ob$fcreate
  '(PRULE
    obname Grandfather-Rule
    subgoal (ROR
             obj (RAND obj (FATHER-OF actor ?Person1
                                      obj ?Person3)
                       obj (FATHER-OF actor ?Person3
                                      obj ?Person2))
             obj (RAND obj (FATHER-OF actor ?Person1
                                      obj ?Person3)
                       obj (MOTHER-OF actor ?Person3
                                      obj ?Person2)))
    goal (GRANDFATHER-OF actor ?Person1 obj ?Person2))))

(ob$add-prule
 (ob$fcreate
  '(PRULE
    obname Grandparent-Rule
    subgoal (GRANDFATHER-OF actor ?Person1 obj ?Person2)
    goal (GRANDPARENT-OF actor ?Person1 obj ?Person2))))
     
(setq *pfacts* (cx$create))

(ob$fcreate '(PERSON name "Roger" obname Schank))
(ob$fcreate '(PERSON name "Michael" obname Dyer))
(ob$fcreate '(PERSON name "Wendy" obname Lehnert))
(ob$fcreate '(PERSON name "Margot" obname Flowers))
(ob$fcreate '(PERSON name "Jack" obname Hodges))
(ob$fcreate '(PERSON name "Erik" obname Mueller))
(ob$fcreate '(PERSON name "Uri" obname Zernik))

(cx$assert *pfacts* (ob$fcreate '(FATHER-OF actor Schank
                                            obj Flowers)))

(cx$assert *pfacts* (ob$fcreate '(MOTHER-OF actor Flowers
                                            obj Hodges)))

(cx$assert *pfacts* (ob$fcreate '(FATHER-OF actor Dyer
                                            obj Mueller)))

(cx$assert *pfacts* (ob$fcreate '(FATHER-OF actor Dyer
                                            obj Zernik)))

(cx$assert *pfacts* (ob$fcreate '(MOTHER-OF actor Lehnert
                                            obj Dyer)))

(cx$assert *pfacts* (ob$fcreate '(FATHER-OF actor Schank
                                            obj Lehnert)))

(test
 (ob->list
  (ob$prove1 (ob$fcreate '(GRANDFATHER-OF actor Schank obj Dyer))
                          *empty-bd* 10 *prules* *pfacts* nil))
 `((t (person3 lehnert))))

(test
 (ob->list
  (ob$prove1 (ob$fcreate '(GRANDFATHER-OF actor Schank obj ?Person))
                          *empty-bd* 10 *prules* *pfacts* nil))
 `((t (person hodges) (person3 flowers))
   (t (person dyer) (person3 lehnert))))

(format t "~A of ~A tests succeeded.~%" *test-succeeded* *test-total*)

; End of file.
