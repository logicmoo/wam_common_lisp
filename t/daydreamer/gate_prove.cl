;*******************************************************************************
;
; GATE
; Version 2.3
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
; This file contains the OB theorem prover
;
;  2/23/87: First version written
;
;*******************************************************************************

(setq *prules* nil)

(defun ob$add-prule (prule)
  (setq *prules* (cons prule *prules*)))

(defun ob$remove-prule (prule)
  (setq *prules* (delq! prule *prules*)))

;*******************************************************************************
;
; ob$prove:
;
; pattern - concept, possibly containing variables (i.e., a query), to prove
; bd - binding list with respect to which the proof is to be performed
; max-number - the maximum number of solutions that are to be generated
;
; ob$prove1:
;
; pfacts - context containing the 'facts' which may be used in the proof
; prules - list of obs which are the 'prules' which may be used in the proof
; ignore-slots - list of slots to ignore
;
; Sample rules demonstrating the use of ROR, RAND, and RNOT:
;
; (ob$fcreate '(PRULE subgoal (ROR obj (PTRANS actor ?Person to ?Location)
;                                  obj (LIVES-IN actor ?Person loc ?Location))
;                     goal (PROX actor ?Person loc ?Location)))
;
; A solution is an augmented binding list. Since web-prove can generate
; several solutions, the result is a list of augmented binding lists. Thus,
; ob$prove returns either:
;
; 1) NIL if con cannot be proved
; 2) list of augmented binding lists if con can be proved
;
; Still to do:
; Add negation
;
;*******************************************************************************

(setq *proof-failures* nil)

(defun ob$prove (pattern bd max-number)
  (ob$prove1 pattern bd max-number *prules* *pfacts* nil))

(defun ob$prove1 (pattern bd max-number prules pfacts ignore-slots)
  (setq *proof-failures* nil)
  (ob$prove2 pattern bd max-number prules pfacts ignore-slots))

(defun ob$prove2 (pattern bd max-number prules pfacts ignore-slots)
  (cond
   ((ty$instance? pattern 'rand)
    (ob$prove-all (ob$gets pattern 'obj)
                  bd max-number prules pfacts ignore-slots))
   ((ty$instance? pattern 'ror)
    (ob$prove-any (ob$gets pattern 'obj)
                  bd max-number prules pfacts ignore-slots))
   ((ty$instance? pattern 'rnot)
    (if (ob$prove2 (ob$get pattern 'obj)
                   bd max-number
                   prules pfacts ignore-slots)
        nil
        bd))
   (else
    (yloop
     (initial (result
               (map 'list  (lambda (elem) (cons nil (cdr elem)))
                    (cx$retrieve-bd pfacts pattern bd)))
              (new-bd nil)
              (result1 nil))
     (yfor prule in prules)
     (ywhile (< (length result) max-number))
     (ydo
      (if (setq new-bd (ob$unify1 (ob$get prule 'goal) pattern bd
                                  ignore-slots))
          (progn
           (if (setq result1
                (ob$prove2 (ob$instantiate (ob$get prule 'subgoal) new-bd)
                           bd max-number prules pfacts ignore-slots))
               (setq result (append! result
                                     (map 'list 
                                      (lambda (elem)
                                       (cons t ;(cons prule (car elem))
                                             (cdr elem)))
                                      result1)))))))
     (yresult
      (if (and (null? result) (not (memq? pattern *proof-failures*)))
          (setq *proof-failures* (cons (list pattern bd) *proof-failures*)))
      result)))))

(defun ob$prove-all (prove-obs bd max-number prules pfacts ignore-slots)
  (let ((bd-list (ob$prove2 (car prove-obs)
                            bd max-number prules pfacts ignore-slots)))
    (if (null? (cdr prove-obs))
        bd-list
        (yloop (yfor bd in bd-list)
               (initial (result nil))
               (ydo
                (setq result
                  (append! result (ob$prove-all (cdr prove-obs)
                                                bd max-number prules
                                                pfacts ignore-slots))))
               (yresult result)))))

(defun ob$prove-any (prove-obs bd max-number prules pfacts ignore-slots)
  (yloop
   (initial (result nil))
   (yfor elem in prove-obs)
   (yuntil result)
   (ydo
    (setq result (ob$prove2 elem bd max-number prules pfacts ignore-slots)))
   (yresult result)))

; End of file.
