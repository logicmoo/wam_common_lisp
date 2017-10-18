;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

; =============================================
; ================= TASK LAYER ================
; =============================================

; ----------------- Tasks ---------------------


(DEFFRAME task
  (SUPERS model-construct-mixin)
  (SLOTS
   (precondition           - :POSSIBLE-VALUES :any)
   (goal                   - :POSSIBLE-VALUES :any)
   (knowledge-sources-used - :POSSIBLE-VALUES (:instances-of knowledge-source))
   (body                   - :POSSIBLE-VALUES :any)))


(defmacro def-task (&rest task-spec)
  `(create-babylon-instance task ,task-spec))


(DEFFRAME task-interpreter-infos
  (SUPERS possible-values-mixin)
  (SLOTS 
   (all-task-goals         -)
   (all-knowledge-sources  - :POSSIBLE-VALUES (:instances-of knowledge-source))
   (all-tasks              - :POSSIBLE-VALUES (:instances-of task))
   (nesting-level          - :POSSIBLE-VALUES :NUMBER)
   (print-pretty-flag      -)
   (print-level-flag       -)))



; =============================================

(defun initialize-task-layer ()
  (declare (special task-info))
  (DEFINSTANCE task-info of task-interpreter-infos
    WITH nesting-level = 0)                                ; start of insertion in trace mode
  (<- task-info :put 'all-task-goals (select-goals-of-all-tasks))
  (<- task-info :put 'all-knowledge-sources (get-instance-list 'knowledge-source))
  (<- task-info :put 'all-tasks (get-instance-list 'task)))

; this function must first be called from the task-layer of the kbs

; =============================================


(DEFBEHAVIOR (task :activate) (&optional (trace nil))
  (let ((name (<- self :get 'name))
        (precondition (<- self :get 'precondition))
        (body (<- self :get 'body)))
    (cons name
          (prog2 (if (not (null trace)) (trace-print name :activate))
                 ; currently there is no check whether the optional keyword is the word "trace"
                 (if (eval-atom-or-list precondition)
                   (prog2 (if (not (null trace))
                            (prog2 (trace-print precondition :precondition)
                                   (trace-print 'statement :nest-in)))
                          (<- self :statement body trace)
                          (if (not (null trace)) (trace-print 'statement :nest-out)))
                   (print-error ':activate 26 name precondition))
                 (if (not (null trace)) (trace-print 'activate :nest-out))))))


(DEFBEHAVIOR (task :statement) (body trace)
  ; trace-print is set to activate because statement calls itself recursively
  (declare (special task-info))
  (if (not (null body))
    (let ((goals     (<- task-info :get 'all-task-goals))
          (ks        (<- task-info :get 'all-knowledge-sources))
          (all-tasks (<- task-info :get 'all-tasks))
          (first-body-element (first body)))
      (append (progn
                (if (not (null trace)) (trace-print first-body-element))
                (if (listp first-body-element)                               ; first-body-element is a list
                  (case (first first-body-element)
                    (call (if (member (second first-body-element) all-tasks)
                            (list (<- (second first-body-element) :activate trace))
                            (print-error ':statement 21 (second first-body-element) 'task)))
                    (satisfy (list (<- self :select-next-task 
                                       (cdr (assoc (second first-body-element) goals)) trace)))
                    ; test for modality-statement omitted
                    (t (<- self :control-statement first-body-element trace)))
                  (if (member first-body-element ks)             ; first-body-element must be a KS
                    (progn (if (not (null trace)) (trace-print first-body-element :ks))
                           (cons first-body-element
                                 (<- first-body-element :apply)))
                    (print-error ':statement 21 first-body-element 'knowledge-source))))
              (if (not (null (rest body)))
                (<- self :statement (rest body) trace))))))       ; call again recursively


(DEFBEHAVIOR (task :control-statement) (body trace)
  (prog2
   (if (not (null trace)) (trace-print 'control-statement :nest-in))
   (case (first body)
     (if (<- self :branching-statement (rest body) trace))
     (while (<- self :iteration-statement (rest body) trace))
     (t (print-error ':control-statement 22 body)))
   (if (not (null trace)) (trace-print 'control-statement :nest-out))))


(DEFBEHAVIOR (task :branching-statement) (body trace)
  ; the "IF" was yet read in :control-statement
  (prog2
   (if (not (null trace)) (trace-print 'branching-statement :nest-in))
   (let ((condition    (first body))
         (then-keyword (second body))
         (then-part    (third body))
         (else-keyword (fourth body))
         (else-part    (fifth body)))
     (if (equal then-keyword 'then)
       (if (eval-atom-or-list condition)
         (prog2 (progn (if (not (null trace)) (trace-print condition :condition))
                       (if (not (null trace)) (trace-print 'statement :nest-in)))
                (<- self :statement then-part trace)
                (if (not (null trace)) (trace-print 'statement :nest-out)))
         (if (not (null else-keyword))
           (if (equal else-keyword 'else)
             (if (null else-part)
               (print-error ':branching-statement 23 'else)
               (prog2   (if (not (null trace)) (trace-print 'statement :nest-in))
                        (<- self :statement else-part trace)
                        (if (not (null trace)) (trace-print 'statement :nest-out))))
             (print-error ':branching-statement 24 else-keyword 'else))))
       (print-error ':branching-statement 24 then-keyword 'then)))
   (if (not (null trace)) (trace-print 'branching-statement :nest-out))))


(DEFBEHAVIOR (task :iteration-statement) (body trace)
  ; the "WHILE" was yet read in :control-statement
  (prog2
   (if (not (null trace)) (trace-print 'iteration-statement :nest-in))
   (let ((do-keyword (second body))
         (while-body (third body)))
     (if (null while-body) (print-error ':iteration-statement 23 'do)
         (if (equal do-keyword 'do)
           (<- self :loop-and-test (first body) (third body) trace)
           (print-error ':iteration-statement 24 do-keyword 'do))))
   (if (not (null trace)) (trace-print 'iteration-statement :nest-out))))


(DEFBEHAVIOR (task :loop-and-test) (condition body trace)
  (if (eval-atom-or-list condition)
    (progn (if (not (null trace)) (trace-print condition :condition))
           (if (not (null trace)) (trace-print 'statement :nest-in))
           (append 
            (prog1
              (<- self :statement body trace)
              (if (not (null trace)) (trace-print 'statement :nest-out)))
            (<- self :loop-and-test condition body trace)))))     ; loop again over the while-body


(DEFBEHAVIOR (task :select-next-task) (task-instances trace)   ; tentative here comes a heuristics
  (prog2
   (if (not (null trace)) (trace-print 'select-next-task :nest-in))
   (cond ((null task-instances) (print-error ':select-next-task 27))
         ((= 1 (length task-instances))
          (<- (first task-instances) :activate trace))
         (t (let ((selected-task (do ((task-instance task-instances (cdr task-instance)))
                                     ((eval-atom-or-list (<- (car task-instance)
                                                             :get 'precondition))
                                      task-instance))))
              ; currently the first task that satisfies its precondition will be selected
              (if (null selected-task)
                (print-error ':select-next-task 25 task-instances)
                (<- (car selected-task) :activate trace)))))
   (if (not (null trace)) (trace-print 'select-next-task :nest-out))))


(defun select-goals-of-all-tasks ()
  ; returns an assoc-list of the form (<goal>  <task'1> ... <task'n>)
  (let ((tasks (get-instance-list 'task))
        (result nil))
    (dolist (instance tasks result)
      (let* ((goal (<- instance :get 'goal))
             (found-instances (cdr (assoc goal result))))
        (if (null found-instances)
          (setf result (cons (list goal instance) result))
          (rplacd (assoc goal result) (cons instance found-instances)))))))


; auxiliary functions for the task-layer

(defun trace-print (value &optional (mode :normal))
  (declare (special task-info))
  (if (equal mode :nest-out) (nest-out))
  (let ((nesting (<- task-info :get 'nesting-level)))
    (save-print-parameters)       ; save the current used print-parameters and override them ...
    (case mode
      (:ks        (format t "~%~VT==> applying knowledge-source '~:S' ..." nesting value))
      (:activate  (format t "~%~VT--> activating task '~:S'" nesting value) (nest-in))
      (:nest-in   (format t "~%~VT--> ~:S" nesting value) (nest-in))
      (:nest-out  (format t "~%~VT<-- ~:S" nesting value))
      (:normal    (pretty-print nesting value))
      (:condition (format t "~%~VTconditon '~:S' is satisfied" nesting value))
      (:precondition (format t "~%~VTit's preconditon '~:S' is satisfied" nesting value)))
    (restore-print-parameters)))  ; and restore them again

(defun pretty-print (nesting value)
  (if (< nesting 1)
    (format t "~%working on ~A" value)
    (let* ((body-string (write-to-string value))
           (new-string (do* ((first-pos (position '#\Newline body-string))
                             (blank-string (format nil "~%~VT" (+ 11 nesting))))
                            ((null first-pos) body-string)
                         (setf body-string (concatenate 'string (subseq body-string 0 first-pos)
                                                        blank-string 
                                                        (subseq body-string (1+ first-pos))))
                         (setf first-pos (position '#\Newline body-string :start (1+ first-pos))))))
      (format t "~%~VTworking on ~A" nesting new-string)))
    nil)

(defun save-print-parameters ()
  (declare (special task-info))
  (<- task-info :put 'print-pretty-flag *print-pretty*)         ; save the *print-pretty* - value
  (setq *print-pretty* t)                                       ; it's new value
  (<- task-info :put 'print-level-flag *print-level*)           ; save the *print-level* - value
  (setq *print-level* 3))                                        ; it's new value

(defun restore-print-parameters ()
  (declare (special task-info))
  (setq *print-pretty* (<- task-info :get 'print-pretty-flag))  ; restore the *print-pretty* - value
  (setq *print-level*  (<- task-info :get 'print-level-flag)))  ; restore the *print-level* - value

(defun nest-in ()
  (declare (special task-info))
  (<- task-info :put 'nesting-level (+ (<- task-info :get 'nesting-level) 4)))

(defun nest-out ()
  (declare (special task-info))
  (let ((nesting (<- task-info :get 'nesting-level)))
    (if (< nesting 4)
      (print-error 'nest-out 30 nesting)
      (<- task-info :put 'nesting-level (- nesting 4)))))

;;; eof

