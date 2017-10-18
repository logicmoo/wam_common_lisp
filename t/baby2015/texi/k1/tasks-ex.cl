;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;; adopted to non SYMBOLICS machines for BABYLON 2.1 by Juergen Walther 10.5.89

(def-kb-instance tasks&agendas-examples k1c :pkg :ks)

;; the kb uses the definitions of tasks&agendas knowledge base

(do ()
    ((member 'tasks&agendas *known-knowledge-bases*)
     (progn
       ($send tasks&agendas-examples :make-yourself-current)
       (send-kb :send-if-handles :import-kb tasks&agendas)
       "tasks&agendas imported"))
  (if (not (member 'tasks&agendas *known-knowledge-bases*))
    (cerror "Load << tasks&agendas >> before proceeding!"
            "Unknown Knowledge Base ~S" 'tasks&agendas)))

;; _________________________________________________________________________


;; ***************  EXAMPLES FOR TASKS AND AGENDAS *************************

;; this kb provides examples for using tasks and agendas from
;; the knowledge-base tasks&agendas it uses the task and agenda frames

;;_____________________________________________________________________________

;; this is a simple task

(DEFFRAME simple-task
  (SUPERS task))

(DEFBEHAVIOR (simple-task :start) (&optional arg (mode :REMEMBER))
  
  ;; starting an instance of task-tester1 only puts out that this
  ;; task is started with the arg as actual parameter and informs
  ;; when this task is finished. It returns finished
  (when (eq mode :REMEMBER)
    (send-kb :babylon-format "~%task ~S is started with arg = ~S" 
             ($send self :object-name) arg)
    (send-kb :babylon-format  "~%task ~S is finished" 
             ($send self :object-name))
    'finished))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; this is a task which finish normal

(definstance my-first-task of simple-task)

;; see what (<- my-first-task :start 100) does !!!

;; these are other simple tasks

(definstance simple-1 of simple-task)
(definstance simple-2 of simple-task)
(definstance simple-3 of simple-task)

;;_____________________________________________________________________________

;; this task stops itself if the stop-flag is set

(DEFFRAME stopping-itself-task
  (SUPERS task)
  (SLOTS  (stop-flag t)))

(DEFBEHAVIOR (stopping-itself-task :start)
  (arg &optional (mode :REMEMBER))
  
  ;; starting an instance of stopping-itself-task only puts out that this
  ;; task is started with the arg as actual parameter and informs
  ;; when this task is finished. If stop-flag is t it stops and
  ;; returns 'stopped otherwise it finishes normal returns finished.
  (when (eq mode :REMEMBER)
    (send-kb :babylon-format "~%task ~S is started with arg = ~S" 
             ($send self :object-name) arg)
    (if ($value 'stop-flag) (<- self :stop 'stopped))
    (send-kb :babylon-format  "~%task ~S is finished" 
             ($send self :object-name))
    'finished))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; instances

(definstance my-second-task of stopping-itself-task)

;; => see what (<- my-second-task :start 100) does !!! <=


(definstance stopping-1 of stopping-itself-task)
(definstance stopping-2 of stopping-itself-task)

;;_____________________________________________________________________________

;; a task that starts another task


(DEFFRAME task-starter-task
  (SUPERS task)
  (SLOTS  (task-to-start nil)))


(DEFBEHAVIOR (task-starter-task :start)
  (arg &optional (mode :REMEMBER))
  
  ;; starting an instance of task-starter-task only puts out that this
  ;; task is started with the arg as actual parameter. It then starts
  ;; the task-to-start if specified. After task-to-start has finished it
  ;; informs the user that this task (self) has finished and returns
  ;; 'finished
  (when (eq mode :REMEMBER)
    (send-kb :babylon-format "~%task ~S is started with arg = ~S" 
             ($send self :object-name) arg)
    (if ($value 'task-to-start)
      (<-  ($value 'task-to-start) :start 'started-from-task-tester-3))
    (send-kb :babylon-format  "~%task ~S is finished" 
             ($send self :object-name))
    'finished))


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; instances

(definstance my-third-task of task-starter-task 
  with task-to-start = my-first-task)

;; => see what (<- my-third-task :start 100) does !!! <=


;; these is a task which starts my-second-task (which will stop)

(definstance starter-1 of task-starter-task 
  with task-to-start = my-second-task)

;; => see what (<- starter-1 :start 100) does !!! <=


;; this is an example of sequencing tasks

(definstance starter-2 of task-starter-task 
  with task-to-start = simple-1)

(definstance starter-3 of task-starter-task
  with task-to-start = starter-2)

(definstance starter-4 of task-starter-task 
  with task-to-start = starter-3)

;; if starter-4 is started the control flow is as
;; follows:
;;
;;   starter-4 => starter-3
;;                starter-3 => starter-2
;;                             starter-2 => simple-1
;;                                       <= simple-1 "finishes"
;;                          <= starter-2 "finishes"
;;             <= starter-3 "finishes"
;;  starter-4 "finishes"

;; => see what (<- starter-4 :start 100) does !!! <=

;;_____________________________________________________________________________


;; this is a sequencer task

(DEFFRAME sequencer-task
  (SUPERS task)
  (SLOTS  (tasks-to-sequence nil
                             :DOC1 "must be a list in which all but the last element"
                             :DOC2 "must be instances of task-starter-task")))


(DEFBEHAVIOR (sequencer-task :after :initialize) (&rest ignore)
  (declare (ignore ignore))
  (let ((task-list ($value 'tasks-to-sequence)))
    (prog ()
      loop
      (if (null (rest task-list))
        (return t)
        (<- (pop task-list) :set 'task-to-start (first task-list)))
      (go loop))))

(DEFBEHAVIOR (sequencer-task :start) (arg &optional (mode :REMEMBER))
  (when (eq mode :REMEMBER)
    (<- (first ($value 'tasks-to-sequence)) :start arg)))
 
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; instances

(definstance starter-5 of task-starter-task)
(definstance starter-6 of task-starter-task)
(definstance starter-7 of task-starter-task)

(definstance sequencer-1 of sequencer-task
  with tasks-to-sequence = ((starter-5 starter-6 starter-7)))

;; => see what (<- sequencer-1 :start 100) does !!! <=

;; control-flow
;;
;;         sequencer-1 => starter-5 => starter-6 => starter-7
;;         sequencer-1 <= starter-5 <= starter-6 <= starter-7

;;_____________________________________________________________________________

;; this shows how to stop the current-caller of a task
;; with the message :stop-current-caller

(DEFFRAME caller-stopping-task
  (SUPERS task))


(DEFBEHAVIOR (caller-stopping-task :start) (arg &optional (mode :REMEMBER))
  (declare (ignore arg))
  (when (eq mode :REMEMBER)
    (<- self :stop-current-caller 'stopped-from-caller-stopping-task)))


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(definstance caller-stopping-1 of caller-stopping-task)

;; => see what (<- caller-stopping-1 :start 100) does !!! <=


;; other examples

(definstance caller-stopping-2 of caller-stopping-task)
(definstance starter-8 of task-starter-task)
(definstance starter-9 of task-starter-task)

(definstance sequencer-2 of sequencer-task
  with tasks-to-sequence = ((starter-9 starter-8 caller-stopping-2)))

;; => see what (<- sequencer-2 :start 100) does !!! <=

;; control flow
;;
;;  sequencer-2 => starter-9 => starter-8 => caller-stopping-2
;;  sequencer-2 <= starter-9 <=
       
;;_____________________________________________________________________________

;; this shows how some caller can be stopped
;; the message :get-current-activation-path is used

(DEFFRAME stopping-some-task
  (SUPERS task))

;; this behaviors stops the third started task
;; since the name of the current kb is always the root of
;; all starting activation, the task that will be stopped is the
;; second started task

(DEFBEHAVIOR (stopping-some-task :start) (arg &optional (mode :REMEMBER))
  (declare (ignore arg))
  (when (eq mode :REMEMBER)
    (<- (third (<- self :get-current-activation-path))
        :stop 'stopped-from-task-tester4)))


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; instances

(definstance stop-some-caller-task of stopping-some-task)

(definstance starter-10 of task-starter-task)
(definstance starter-11 of task-starter-task)
(definstance starter-12 of task-starter-task)

(definstance sequencer-3 of sequencer-task
  with tasks-to-sequence
  = ((starter-12 starter-11 starter-10 stop-some-caller-task)))

;; => see what (<- sequencer-3 :start 100) does !!! <=

;; control flow
;;
;; sequencer-3 => starter-12 => starter-11 => starter-10 => stop-some-caller-task
;; sequencer-3 <= 

;;;_____________________________________________________________________________


;; a task that stops all currently active tasks using
;; the message :stop-all-tasks

(DEFFRAME stopping-all-task
  (SUPERS task))

(DEFBEHAVIOR (stopping-all-task  :start) (arg &optional (mode :REMEMBER))
  (declare (ignore arg))
  (when (eq mode :REMEMBER)
    (<- self :stop-all-tasks 'all-aborted)))
 
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; instances


(definstance stopping-all-1 of stopping-all-task)

(definstance starter-13 of task-starter-task)
(definstance starter-14 of task-starter-task)
(definstance starter-15 of task-starter-task)

(definstance sequencer-4 of sequencer-task
  with tasks-to-sequence = ((starter-13 starter-14 starter-15 stopping-all-1)))

;; => see what (<- sequencer-4 :start 100) does !!! <=

;; control flow
;;
;; sequencer-4 => starter-13 => starter-14 => starter-15 => stopping-all-1
;; <=
;;;_____________________________________________________________________________


;; a task with user-interaction it allows to access the trace
;; information in the trace window

(defframe asking-task
  (SUPERS task))


(DEFBEHAVIOR (asking-task :start) (arg &optional (mode :REMEMBER))
  (declare (ignore arg))
  (when (eq mode :REMEMBER)
    (send-kb :babylon-format "~%task ~S is waiting to continue" 
             ($send self :object-name))
    (send-kb :type-end-to-continue "type ~C to continue")))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; instances

(definstance asking-1 of asking-task)

(definstance starter-16 of task-starter-task)
(definstance starter-17 of task-starter-task)
(definstance starter-18 of task-starter-task)

(definstance sequencer-5 of sequencer-task
  with tasks-to-sequence = ((starter-16 starter-17 starter-17 asking-1)))


;; => see what (<- sequencer-5 :start 100) does !!! <=


;; ***************************** AGENDA EXAMPLES ******************************

;; using agendas with simple-task

(definstance a1 of agenda
  with list-of-tasks = ((simple-1 simple-2 simple-3 simple-1)))

;; => see what (<- a1 :start 100) does !!! <=

;;_____________________________________________________________________________

;; using agendas with  stopping-itself-task


(definstance a2 of agenda
  with list-of-tasks = ((simple-1 stopping-1 simple-2 stopping-2 simple-3)))

;; => see what (<- a2 :start 100) does !!! <=


;;_____________________________________________________________________________

;; using agendas with agendas as elements


(definstance a3 of agenda
  with list-of-tasks = ((simple-1 stopping-1 a2 stopping-2 a1 simple-3)))

;; => see what (<- a3 :start 100) does !!! <=

;;_____________________________________________________________________________

;; stopping an agenda
;; each task which is started directly through an agenda can stop this agenda
;; with the message :stop-current-agenda. to stop the outermost agenda through
;; which a task has been activated use the message :stop-all-agendas. 


(DEFFRAME all-agenda-stopper
  (SUPERS task))

(DEFBEHAVIOR (all-agenda-stopper :start) (&optional arg (mode :REMEMBER))
  (declare (ignore arg))
  (when (eq mode :REMEMBER)
    (send-kb :babylon-format "~%all agenda stopper ~S is started" 
             ($send self :object-name))
    (<- self :stop-all-agendas 'all-agendas-stopped)))


(definstance agenda-stopper-1 of all-agenda-stopper)

(definstance a3 of agenda
  with list-of-tasks = ((simple-1 stopping-1 agenda-stopper-1)))

(definstance a4 of agenda
  with list-of-tasks = ((simple-2 a3 stopping-2)))

(definstance a5 of agenda
  with list-of-tasks = ((simple-3 a4 stopping-2)))


;; => see what (<- a5 :start 100) does !!! <=

;;_____________________________________________________________________________

;; agendas can be changed in a number of ways. normaly
;; the changing of a agenda is performed by the task which
;; is currently called from the agenda.

;; example: first a special task definition which is used for general purposes

(DEFFRAME decide-what-to-do
  (SUPERS task)
  (SLOTS  (answer - 
                  :possible-values (:ONE-OF yes no) 
                  :ask ("~%Shall I do ~S ?" (O)))))


;; this behavior is only for asking

(DEFBEHAVIOR (decide-what-to-do :get-answer) ()
  (<- self :ask 'answer)
  (if (eq ($value 'answer) 'yes)
    t
    nil))

;; this behavior is used to "compute" the possible values as
;; one-of the tasks currently contained in the associated agenda

(DEFBEHAVIOR (decide-what-to-do :get-contents-of-agenda) (&rest arg)
  (declare (ignore arg))
  (if (<- self :is-active)
    `(:ONE-OF . ,(<- (<- self :get-my-agenda) :get 'list-of-tasks))
    :ANY))


;; this behavior is used to "compute" the possible values as
;; one of all currently inactive tasks

(DEFBEHAVIOR (decide-what-to-do :get-all-inactive-tasks) (&rest arg)
  (declare (ignore arg))
  (if (<- self :is-active)
    `(:ONE-OF . ,(mapcar #'(lambda (a-task)
                             (unless (<- a-task :is-active)) a-task)
                         (send-kb :instances)))
    :ANY))

;;_____________________________________________________________________________

;; a task which clears the associated agenda

(DEFFRAME forget-next?
  (SUPERS decide-what-to-do))

(DEFBEHAVIOR (forget-next? :start) (&optional arg (mode :REMEMBER))
  (declare (ignore arg))
  (when (eq mode :REMEMBER)
    (if (<- self :get-answer)
      (<- (<- self :get-my-agenda) :forget-next-tasks))))

(definstance forget-next of forget-next?)

;;_____________________________________________________________________________

;; a task which deletes some other task from the associated agenda

(DEFFRAME delete-task?
  (SUPERS decide-what-to-do)
  (SLOTS  (task-to-delete (active-value -  :get-contents-of-agenda :no-update-permitted)
                          :ASK ("~%delete which object ? "))))

(DEFBEHAVIOR (delete-task? :start) (&optional arg (mode :REMEMBER))
  (declare (ignore arg))
  (when (eq mode :REMEMBER)
    (if (not (<- self :get-answer))
      nil
      (progn
        (<- self :ask 'task-to-delete)
        (<- (<- self :get-my-agenda) :delete-task ($value 'task-to-delete))))))

(definstance delete-task of delete-task?)

;;_____________________________________________________________________________

;; a task which changes the next task of the associated agenda

(DEFFRAME what-next?
  (SUPERS decide-what-to-do)
  (SLOTS  (task-to-do-next (active-value - :get-all-inactive-tasks :no-update-permitted)
                           :ASK ("~%what task next ? "))))

(DEFBEHAVIOR (what-next? :start) (&optional arg (mode :REMEMBER))
  (declare (ignore arg))
  (when (eq mode :REMEMBER)
    (if (not (<- self :get-answer))
      nil
      (progn
        (<- self :ask 'task-to-do-next)
        (<- (<- self :get-my-agenda) :do-next ($value 'task-to-do-next))))))

(definstance what-next of what-next?)

;;_____________________________________________________________________________

;; a task which changes the next task of the associated agenda
;; and immediately stops

(DEFFRAME what-immediately?
  (SUPERS decide-what-to-do)
  (SLOTS  (task-to-do-immediately (active-value - :get-all-inactive-tasks :no-update-permitted)
                                  :ASK ("~%what task immediately ? "))))

(DEFBEHAVIOR (what-immediately?  :start) (&optional arg (mode :REMEMBER))
  (declare (ignore arg))
  (when (eq mode :REMEMBER)
    (if (not (<- self :get-answer))
      nil
      (progn
        (<- self :ask 'task-to-do-immediately)
        (<- (<- self :get-my-agenda)
            :do-immediately ($value 'task-to-do-immediately))))))

(definstance what-immediately of what-immediately?)

;;_____________________________________________________________________________

(definstance test-agenda of agenda
  with list-of-tasks
  = ((simple-1 forget-next simple-1 delete-task simple-1
               what-immediately simple-1 what-next)))

;; ************************* A COMPLEX EXAMPLE ********************************

;; this example demonstrates the use of agendas and tasks in combination
;; with rules. Don't expect too much contents!

;;_____________________________________________________________________________

;; first we define SOME TASKS with an associated rule set:

;; The first one is intended to evaluate forward a rule set.

(DEFFRAME forward-rule-set-task
  (SUPERS task)
  (SLOTS  (rule-set nil :control-structure :do-all)))

;; we define then an appropriate start behavior:

(DEFBEHAVIOR (forward-rule-set-task :start) (&optional arg (mode :REMEMBER))
  (declare (ignore arg))
  (when (eq mode :REMEMBER)
    (let ((the-rule-set ($value 'rule-set)))
      (if (null the-rule-set)
        'NOTHING-TO-DO ;; else ask the kb to evaluate the-rule-set 
        (send-current-knowledge-base
         :find-implications the-rule-set
         ($value 'rule-set :control-structure))))))

;; here an instance is created and initialized:

(definstance suspect-generating-task of forward-rule-set-task
  with rule-set = :generate-suspect)

;; The second one is intended to evaluate backward a rule set.

(DEFFRAME backward-rule-set-task
  (SUPERS task)
  (SLOTS  (rule-set nil :goal nil)))

;; we define now the appropriate start behavior:

(DEFBEHAVIOR (backward-rule-set-task :start) (&optional arg (mode :REMEMBER))
  (declare (ignore arg))
  (when (eq mode :REMEMBER)
    (let ((the-rule-set ($value 'rule-set))
          (the-goal ($value 'rule-set :goal)))
      (if (or (null the-rule-set) (null the-goal))
        'NOTHING-TO-DO ;; else ask the kb to evaluate the-rule-set 
        (send-current-knowledge-base :obtain :all the-goal the-rule-set)))))

;; here an instance is created and initialized:

(definstance check-task of backward-rule-set-task
  with rule-set = :check)
 
;;_____________________________________________________________________________

;; Now we define a SPECIAL AGENDA which controls the execution of
;; our rule-set-tasks. For the control-agenda the default :START-BEHAVIOR
;; is used.

(DEFFRAME control-agenda
  (SUPERS agenda))

(DEFBEHAVIOR (control-agenda :inform) ()
  (send-kb :babylon-format
           "~%This agenda demonstrates how to use tasks in combination~@
             with rule sets. You should choose the trace configuration."))

(definstance the-controller of control-agenda
  with list-of-tasks = ((suspect-generating-task check-task)))

;;_____________________________________________________________________________

;; **************** Now the definition of the RULE SETS *********************

(DEFRULE-SET :generate-suspect
  (rule-1 (?AND (ist blau)
                (ist gross)
                (ist rund))
          ($CONCLUDE (check-task rule-set :goal = bayer)))
  (rule-2 (?AND (ist rot)
                (ist klein)
                (ist rund))
          ($CONCLUDE (check-task rule-set :goal = berliner)))
  (rule-2 (?AND (ist gruen))
          ;; hier wird die nichtzustaendigkeit erkannt
          ($EXECUTE (the-controller :stop 'IST-NICHT-MEIN-FALL))))

(DEFRULE-SET :check
  (rule-1 ($AND (isst kraut)
                (trinkt bier)
                (traegt lederhose)
                (ist ludwig-fan))
          ($CONCLUDE (bayer)))
  (rule-2 ($AND (isst currywurst)
                (trinkt berliner-weisse))
          ($CONCLUDE (berliner))))

;; ****************************** INSTRUCTIONS ******************************


;;;  Instructions for normal-Babylon
;;;

(instructions
 (<- the-controller :inform)
 (<- the-controller :start)
 ($send self :print-hypotheses-verified)
 ($send self :print-true-facts)
 )


;;;  Task and instruction part for tex-i-bylon :
;;;  you can start this task after RESET with
;;;      (send-kb :reset-kb)
;;;      (send-kb :start-kb)


;;; eof

