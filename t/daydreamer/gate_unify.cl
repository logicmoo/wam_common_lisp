;*******************************************************************************
;
; GATE
; Version 2.3
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
; This file contains the OB unifier
;
; 10/13/84: Original version written
;  1/24/85: Upgraded to full unifier
;  6/30/85: Added *instance-of*, ob$compare
;   9/3/85: Added loop checking in unifier
;   1/6/86: Changed special forms to obs
;  1/24/86: Commented out compile-web-pattern, added relaxation to ob-unify-var
;  1/26/86: Added variable-value
;  9/24/86: Removed flavors
;  9/29/86: Updated to new unification algorithm
;  11/2/86: Added UDIST, changed ob$unify-var
;
;*******************************************************************************

(setq *unify-debugging?* nil)
(setq *relax-unify-var* nil)

;
; Empty binding list
;
(setq *empty-bd* '(t))

(defun bd-and-empty-bd? (bd)
  (if bd
      (if (null? (cdr bd))
          bd
          nil)
      nil))

(defun empty-bd? (bd)
  (if (null? (cdr bd))
      bd
      nil))

(defun non-empty-bd? (bd)
  (if (cdr bd) bd nil))

;
; (bd-lookup var bindings):
; Look up the value of a variable in a binding list returned by ob$unify.
;
(defun bd-create () (cons t nil))

(defun bd-hyper-lookup1 (var bd vars first-level)
  (if (memq? var vars)
      first-level
      (let ((val (bd-lookup var bd)))
           (if (var? val)
               (bd-hyper-lookup1 (variable-name val)
                                 bd
                                 (cons var vars)
                                 (if first-level first-level val))
               val))))

(defun variable-hyper-lookup (variable bindings)
  (bd-hyper-lookup (variable-name variable) bindings))

;(defun variable-hyper-lookup (variable bindings)
;  (let ((found (assq (variable-name variable)
;                     (cdr bindings))))
;    (if found
;        (if (var? (cadr found))
;            (variable-hyper-lookup1 (cadr found) bindings
;                                    (list (variable-name variable)))
;            (cadr found))
;        nil)))

;(defun variable-hyper-lookup1 (variable bindings names)
;  (if (memq? (variable-name variable) names)
;      nil
;      (let ((found (assq (variable-name variable)
;                         (cdr bindings))))
;        (if found
;            (if (var? (cadr found))
;                (variable-hyper-lookup1 (cadr found) bindings
;                                        (cons (variable-name variable) names))
;                (cadr found))
;            variable))))

;(defun bd-hyper-lookup1 (var bindings)
;  (let ((found (assq var (cdr bindings))))
;    (if found
;        (if (var? (cadr found))
;            (bd-hyper-lookup (variable-name (cadr found)) bindings)
;            (cadr found))
;        var)))

;
; Variables
;
; Examples of macro translation:
; ?Self --> (UVAR name 'self unifies-with PERSON)
; ?Person1 --> (UVAR name 'person1 unifies-with PERSON))
; ?Silly:Person --> (UVAR name 'silly unifies-with PERSON)
; ?:Person --> (UVAR unifies-with PERSON)
; ?? --> (UVAR)
; ?Notatype --> (UVAR name 'notatype)
;

(defun make-var (name type)
  (cond
   ((and name type)
    (ob$fcreate `(UVAR
                    name (QUOTE ,name)
                    unifies-with ,type)))
   (type
    (ob$fcreate `(UVAR
                    unifies-with ,type)))
   (name
    (ob$fcreate `(UVAR
                    name (QUOTE ,name))))
   (else (ob$fcreate '(UVAR)))))

(defun variable-value (var bd)
  (bd-lookup (variable-name var) bd))

;
; (ob$unify ob1 ob2 bindings):
;
; Unifier for obs
; (Looping check code taken from the rhapsody matcher by Scott Turner).
;

(setq *already-matched* nil)

(setq *diff?* nil)

(defun ob$unify-dbg (ob1 ob2 bindings ignore-slots)
  (ndbg-begin)
  (ndbg *gate-dbg* unify "Call ob$unify: ~A ~A ~A ~A~%"
        ob1 ob2 bindings ignore-slots)
  (let ((result (ob$unify0 ob1 ob2 bindings ignore-slots)))
    (ndbg *gate-dbg* unify "Return from ob$unify: ~A~%" result)
    (ndbg-end)
    result))

;
; List of slots which unification should always ignore.
;
(setq *permanent-ignore-slots* '(top-context value weight offset decay
                                             plan-rule plan-subgoalnum
;;;; no no no linked-to-of linked-from-of
                                             input-state?
                                             inference-rule
                                             indexes))

(setq *unify-context* nil)

;
; This could be made faster by doing types first. Actually, types
; are done first anyway because they are the first slot.
;
(defun ob$unify0 (ob1 ob2 bindings ignore-slots)
  (if (memq? ob2 (bd-lookup ob1 *already-matched*))
      bindings
      (progn
       (bd-bind! ob1
                 (cons ob2 (bd-lookup ob1 *already-matched*))
                 *already-matched*)
; The below would introduce a semantics which does not conform
; to unification asymmetry.
;       (bd-bind! ob2
;                 (cons ob1 (bd-lookup ob2 *already-matched*))
;                 *already-matched*)
       (let ((result
        (cond
         ((eq? ob1 ob2) bindings)
         ((or (special? ob1) 
              (special? ob2))
          (if (special-priority? ob1 ob2)
              (ob$unify-special ob1 ob2 bindings ignore-slots nil)
              (ob$unify-special ob2 ob1 bindings ignore-slots t)))
         ((var? ob1)
          (ob$unify-var ob1 ob2 bindings ignore-slots nil))
         ((var? ob2)
          (ob$unify-var ob2 ob1 bindings ignore-slots t))
         ((and (ob? ob1) (ob$literal? ob1)) nil)
         ((and (ob? ob2) (ob$literal? ob2)) nil)
         ((and (ob? ob1) (ob? ob2))
          (yloop (initial (unified-slot-indices nil)
                         (ob2-slots (ob$pairs ob2))
                         (constant-slot-index nil)
                         (last-constant-value nil)
                         (new-bindings nil)
                         (found? nil))
                (yfor cur in (ob$pairs ob1)) ; was reverse
                (ywhile bindings)
                (ydo (if (and (not (memq? (car cur) ignore-slots))
                             (not (memq? (car cur) *permanent-ignore-slots*)))
                        (progn
                         (setq constant-slot-index 0)
                         (setq new-bindings nil)
                         (setq found? nil)
                         (setq last-constant-value nil)
                         (yloop (yfor constant-slot-value in ob2-slots)
                                (yuntil found?)
(ydo
 (if (and (eq? (car cur) (slots-name constant-slot-value))
          (not (memq? constant-slot-index unified-slot-indices))
          (setq last-constant-value (slots-value constant-slot-value))
          (setq new-bindings
                (if (eq? (cadr cur) (slots-value constant-slot-value))
                    bindings
                    (ob$unify2 (cadr cur) (slots-value constant-slot-value)
                               bindings ignore-slots))))
     (progn
      (setq found? t)
      (setq unified-slot-indices
            (cons constant-slot-index unified-slot-indices))))
 (increment-me constant-slot-index)))
                         (if found?
                             (setq bindings new-bindings)
                             (if *diff?*
                                 (setq bindings (bd-bind (slots-name cur)
                                                         (list
                                                          (cadr cur)
                                                          last-constant-value)
                                                         bindings))
                                 (setq bindings nil))))))
                (yresult bindings)))
         (else nil))))
        (if result
            result
            (progn
             (bd-bind! ob1
                       (delq! ob2 (bd-lookup ob1 *already-matched*))
                       *already-matched*)
             (bd-bind! ob2
                       (delq! ob1 (bd-lookup ob2 *already-matched*))
                       *already-matched*)
             nil))))))

(defun ob$unify-special (ob1 ob2 bindings ignore-slots reverse?)
  (cond
   ((ty$instance? ob1 'uand)
    (yloop (yfor item in (ob$gets ob1 'obj))
           (ywhile bindings)
           (ydo (setq bindings
                      (if reverse?
                          (ob$unify2 ob2 item bindings ignore-slots)
                          (ob$unify2 item ob2 bindings ignore-slots))))
           (yresult bindings)))
   ((ty$instance? ob1 'uor)
    (yloop (yfor item in (ob$gets ob1 'obj))
           (initial (new-bindings nil))
           (yuntil new-bindings)
           (ydo (setq new-bindings
                      (if reverse?
                          (ob$unify2 ob2 item bindings ignore-slots)
                          (ob$unify2 item ob2 bindings ignore-slots))))
           (yresult new-bindings)))
   ((ty$instance? ob1 'unot)
    (if (if reverse?
            (ob$unify2 ob2 (ob$get ob1 'obj) bindings ignore-slots)
            (ob$unify2 (ob$get ob1 'obj) ob2 bindings ignore-slots))
        nil
        bindings))
   ((ty$instance? ob1 'udist)
    (let ((val1 (if (not (var? (ob$get ob1 'obj)))
                    (ob$get ob1 'obj)
                    (bd-hyper-lookup (variable-name (ob$get ob1 'obj))
                                     bindings)))
          (val2 (if (not (var? ob2))
                    ob2
                    (bd-hyper-lookup (variable-name ob2) bindings))))
         (if (and (ob? val1) (ob? val2)
                  (not (var? val1)) (not (var? val2)))
             (if (neq? val1 val2) bindings nil)
             bindings)))
   ((ty$instance? ob1 'uproc)
    (if (eq? ob2 'uproc-answer-true)
        bindings
        (ob$unify-proc ob2 (ob$get ob1 'proc) bindings)))
   ((ty$instance? ob1 'uempty-slots)
    (if (every? (lambda (slot-name)
                 (null? (ob$gets ob2 slot-name)))
                (ob$get ob1 'slots))
        bindings
        nil))
   ((ty$instance? ob1 'uignore-slots)
    (if reverse?
        (ob$unify2 ob2 (ob$get ob1 'pattern) bindings
                   (append ignore-slots (ob$get ob1 'slots)))
        (ob$unify2 (ob$get ob1 'pattern) ob2 bindings
                   (append ignore-slots (ob$get ob1 'slots)))))
   ((ty$instance? ob1 'upath)
    (ob$path ob2 (ob$get ob1 'pattern)
                 (ob$get ob1 'path) bindings))
   ((ty$instance? ob1 'uolpath)
    (ol-path ob2 (ob$get ob1 'pattern) (ob$get ob1 'link)
                 (ob$get ob1 'direction)
                 *unify-context*
                 nil
                 bindings))
   ((ty$instance? ob1 'ueval)
    (ob$eval (ob$get ob1 'proc) bindings))
   ((ty$instance? ob1 'ucode)
    bindings) ; for now
   (else (error "ob$unify: unknown special!! ~A" ob1))))

; The (else t) above basically ignores prioritization of:
; (ty$instance? ,ob1 'uempty-slots)
; (ty$instance? ,ob1 'uignore-slots)
; (ty$instance? ,ob1 'upath)
; (ty$instance? ,ob1 'uolpath)

(defun ob$unify-proc (ob2 proc bd)
 (setq ob2 (ob$concretize ob2 bd))
 (if (concretized? ob2)
     (if (funcall proc ob2)
         bd
         nil)
     bd))

(defun ob$concretize (ob bd)
 (cond
  ((var? ob)
   (ob$concretize-var ob bd))
  ((and (ob? ob)
        (ty$instance? ob 'uand))
   (ob$concretize-and ob bd))
  (else ob)))

(defun ob$concretize-and (and-ptn bd)
  (yloop (yfor item in (ob$gets and-ptn 'obj))
         (initial (result nil))
         (yuntil result)
         (ydo (if (var? item)
                  (setq result (ob$concretize-var item bd))))
         (yresult (if (null? result)
                      (progn
                       (format *gate-output*
                               "Warning: ob$concretize-and unsuccessful.~%")
                       and-ptn)
                      result))))

(defun ob$concretize-var (var bd)
  (let ((found (bd-lookup (variable-name var) bd)))
    (if found found var)))

(defun concretized? (var)
  (not (var? var)))

;
; Question mark atom should never get to here.
;
; This routine no longer checks if the types match right even if the variable
; is already bound. This used to be used to handle prebound typed ?Self, but
; now the self-type slot of rules serves this function.
;

(defun ob$unify-var (ob1 ob2 bindings ignore-slots reverse?)
  (let ((val1 (bd-lookup (variable-name ob1) bindings))
        (val2 nil))
; was  (if val1 (setq ob1 val1))
       (if (and val1 (not (var? val1)))
           (setq ob1 val1))
       (if (var? ob2)
           (progn
            (setq val2 (bd-lookup (variable-name ob2) bindings))
            (if (and val2 (not (var? val2)))
                (setq ob2 val2))))
; was       (if val2 (setq ob2 val2))
       (cond
        ((and (var? ob1) (var? ob2))
         (if (type-compatible-vars? ob1 ob2)
             (if *diff?*
                 bindings
                 (bd-bind (variable-name ob2) ob1
                          (bd-bind (variable-name ob1) ob2 bindings)))
             nil))
        ((var? ob1)
         (if (var-ty$instance? ob2 (variable-type ob1))
             (if *diff?*
                 bindings
                 (bd-bind (variable-name ob1) ob2 bindings))
             nil))
        ((var? ob2)
         (if (var-ty$instance? ob1 (variable-type ob2))
             (if *diff?*
                 bindings
                 (bd-bind (variable-name ob2) ob1 bindings))
             nil))
        (else ;(and (not (var? ob1)) (not (var? ob2)))
         (if reverse?
             (ob$unify2 ob2 ob1 bindings ignore-slots)
             (ob$unify2 ob1 ob2 bindings ignore-slots))))))

;
; This is the old incomprehensible version.
;
(defun ob$old-unify-var (ob1 ob2 bindings ignore-slots reverse?)
  (if *relax-unify-var*
      (if (null? (variable-name ob1))
          bindings
          (let ((found (bd-lookup (variable-name ob1) bindings)))
               (if found
                   (ob$unify2 found ob2 bindings ignore-slots)
                   (if (var? ob2)
                       (progn
                        (setq found (bd-lookup (variable-name ob2) bindings))
                        (if found
                            (ob$unify2 ob1 found bindings ignore-slots) ; ?
                            (bd-bind (variable-name ob1) ob2 bindings)))
                       (bd-bind (variable-name ob1) ob2 bindings)))))
      (progn
  (if (and (variable-type ob1)
           (not (var? ob2))
           (not (ty$instance-of? ob2 (variable-type ob1))))
      nil
  (if (null? (variable-name ob1))
      bindings ; but should do type compatibility check
      (let ((found (bd-lookup (variable-name ob1) bindings)))
           (if found
           (ob$unify2 found ob2 bindings ignore-slots)
               (if (var? ob2)
                   (progn
                    (setq found (bd-lookup (variable-name ob2) bindings))
                    (if found
                        (ob$unify2 ob1 found bindings ignore-slots) ; ?
            (if (type-compatible-vars? ob1 ob2)
                        (bd-bind (variable-name ob1) ob2 bindings) nil)))
                   ; should check type compatibility in above line,
                   ; but only if both variables are typed.
                   (if (variable-type ob1)
                       (if (ty$instance-of? ob2 (variable-type ob1))
                   (bd-bind (variable-name ob1) ob2 bindings)
                   nil)
               (bd-bind (variable-name ob1) ob2 bindings))))))))))

(setq *max-breadth* 10)

(defun ob$path (from-constant to-ptn links bindings)
  (yloop (initial
         (result nil)
         (count 0)
         (next-obs (ob$get-many from-constant links)))
        (yuntil
         (or result
             (if (> count *max-breadth*)
                 (progn
                  (ndbg *gate-dbg* ob-warn
                        "Exceeded max breadth in ob$path.~%")
                  t)
                 nil)))
        (ywhile next-obs)
        (ydo
         (yloop (yfor next-ob in next-obs)
               (yuntil result)
               (ydo (setq result (ob$unify to-ptn next-ob bindings))))
         (if (null? result)
             (setq next-obs
                  (walk-append
                   (lambda (ob) (ob$get-many ob links))
                   next-obs)))
         (increment-me count))
        (yresult result)))

(setq *uniquified-obs* nil)

;
; The following function seems to be ineffectual. Maybe all references
; to these obs are not being deleted?
;
(defun gc-uniquified-obs ()
  (yloop (yfor ob in *uniquified-obs*)
        (ydo (ob$destroy ob)))
  (setq *uniquified-obs* nil))

(defun ob$compare1 (source target substit ignore-slots proc)
  (if (memq? target (bd-lookup source *already-matched*))
      substit
      (progn
       (bd-bind! source
                 (cons target (bd-lookup source *already-matched*))
                 *already-matched*)
       (bd-bind! target
                 (cons source (bd-lookup target *already-matched*))
                 *already-matched*)
       (let ((result
        (cond
         ((eq? source target) substit)
         ((and (ob? source)
               (and (ob$literal? source) (not (ty? source)))) nil)
         ((and (ob? target)
               (and (ob$literal? target) (not (ty? target)))) nil)
         ((eq? (bd-lookup source substit) target) substit)
         ((and (ob? source)
               (not (ty? source))
               (ob? target)
               (not (ty? target)))
          (yloop (initial (compared-slot-indices nil)
                         (target-slots (ob$pairs target))
                         (target-slot-index nil)
                         (new-substit nil)
                         (save-substit substit)
                         (found? nil)
                         (proc-result nil))
                (yfor cur in (ob$pairs source)) ; was reverse
                (ywhile substit)
                (ydo (if (and (not (memq? (car cur) ignore-slots))
                             (not (memq? (car cur) *permanent-ignore-slots*)))
                        (progn
                         (setq target-slot-index 0)
                         (setq new-substit nil)
                         (setq found? nil)
(yloop (yfor target-slot-value in target-slots)
      (yuntil found?)
      (ydo (if (and (eq? (car cur) (slots-name target-slot-value))
                   (not (memq? target-slot-index compared-slot-indices))
                   (setq new-substit (if (eq? (cadr cur)
                                               (slots-value target-slot-value))
                                        substit
                                        (ob$compare1 (cadr cur)
                                                      (slots-value
                                                      target-slot-value)
                                                      substit
                                                      ignore-slots
                                                      proc))))
                                       (progn
                                        (setq found? t)
                                        (setq compared-slot-indices
                                             (cons target-slot-index
                                                   compared-slot-indices))))
                                   (increment-me target-slot-index)))
                         (if found?
                             (setq substit new-substit)
                             (setq substit nil)))))
                (yresult (if (null? substit)
                            (if (setq proc-result (funcall proc source target))
                                (cons 't (cons (list source target proc-result)
                                          (cdr save-substit)))
                                nil)
                            substit))))
         ((and (ty? source) (ty? target))
          (let ((proc-result (funcall proc source target)))
            (if proc-result
                (cons 't (cons (list source target proc-result) (cdr substit)))
                nil)))
         (else nil))))
        (if result
            result
            (progn
             (bd-bind! source
                       (delq! target (bd-lookup source *already-matched*))
                       *already-matched*)
             (bd-bind! target
                       (delq! source (bd-lookup target *already-matched*))
                       *already-matched*)
             nil))))))

; End of file.
